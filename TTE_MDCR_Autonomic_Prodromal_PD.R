###############################################################################
# Expanded prodromal-PD covariate sensitivity analysis
# Reuses saved TTE objects whenever available and processes large claims in batches
###############################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(arrow)
library(readr)
library(WeightIt)
library(cobalt)
library(survey)
library(survival)
library(broom)
library(ggplot2)

# =============================================================================
# 0. CONFIGURATION
# =============================================================================
DATASET <- "MDCR"
BASE_PATH <- "/data/MarketScan_data/hypertension_cohort_update"
OUTPUT_PATH <- paste0("$HOME/TTE/HTN_final/", DATASET, "_results/")
EXPANDED_PATH <- paste0(OUTPUT_PATH, "expanded_prodromal_PS/")
PLOT_PATH <- paste0(EXPANDED_PATH, "plots/")
dir.create(EXPANDED_PATH, recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_PATH, recursive = TRUE, showWarnings = FALSE)

BASELINE_DAYS <- 365
MAX_FOLLOWUP_YEARS <- 10
MAX_FOLLOWUP_DAYS <- MAX_FOLLOWUP_YEARS * 365.25
BATCH_SIZE <- 25000

DATA_FILES <- list(
  inpatient = file.path(BASE_PATH, paste0(DATASET, "_I.parquet")),
  outpatient = file.path(BASE_PATH, paste0(DATASET, "_O_limit.parquet")),
  medication = file.path(BASE_PATH, paste0(DATASET, "_D.parquet"))
)

REDBOOK_PATH <- "/data/MarketScan_data/dictionary/REDBOOK.csv"

log_file <- paste0(EXPANDED_PATH, DATASET, "_expanded_prodromal_PS_log.txt")
cat("Analysis started: ", as.character(Sys.time()), "\n", file = log_file)

log_message <- function(x) {
  cat(x, "\n")
  cat(x, "\n", file = log_file, append = TRUE)
}

# =============================================================================
# 1. REUSE THE FINAL PD-ELIGIBLE COHORT FROM THE ORIGINAL TTE
# =============================================================================
# Preferred object: outcome_ready_dataset.parquet.
# It already contains the original treatment assignment, 90-day landmark,
# prevalent-PD exclusion, censoring dates, outcomes, and original covariates.
#
# If it was not saved, baseline_characteristics is NOT sufficient for this
# sensitivity analysis because the final PD exclusion occurs later. In that case,
# save outcome_ready_dataset from the original script first.

outcome_ready_dataset <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/outcome_ready_dataset.parquet")

outcome_ready_dataset <- outcome_ready_dataset %>%
  mutate(
    ENROLID = as.character(ENROLID),
    index_date = as.Date(index_date),
    risk_start_date = as.Date(risk_start_date),
    end_date = as.Date(end_date),
    last_enroll = as.Date(last_enroll),
    PD_date = as.Date(PD_date)
  )

# Final IDs only: this avoids carrying baseline claims for people later excluded for PD.
analysis_ids <- outcome_ready_dataset %>%
  select(ENROLID, treatment_combined, index_date, risk_start_date, end_date) %>%
  distinct() %>%
  mutate(
    baseline_start = index_date - days(BASELINE_DAYS),
    baseline_end = index_date - days(1)
  )

write_parquet(analysis_ids, paste0(EXPANDED_PATH, "analysis_ids_final_PD_eligible.parquet"))
log_message(paste("Final PD-eligible patients:", nrow(analysis_ids)))

# =============================================================================
# 2. ICD-9-CM / ICD-10-CM DEFINITIONS
# =============================================================================
# Codes are normalized by removing dots/spaces and converting to uppercase.

normalize_dx <- function(x) {
  x <- toupper(as.character(x))
  x <- gsub("[^A-Z0-9]", "", x)
  x
}

flag_prodromal_dx <- function(code) {
  code <- normalize_dx(code)
  
  orthostatic_hypotension <- code %in% c("4580", "I951")
  syncope <- code %in% c("7802", "R55")
  
  falls_icd9 <- grepl("^E88[0-8]", code)
  falls_icd10 <- grepl("^W(0[0-9]|1[0-9])", code)
  falls <- falls_icd9 | falls_icd10
  
  stroke_icd9 <- code %in% c("430", "431", "436") |
    grepl("^433[0-9]1$", code) |
    grepl("^434[0-9]1$", code)
  stroke_icd10 <- grepl("^I60", code) |
    grepl("^I61", code) |
    grepl("^I63", code) |
    code == "I64" |
    grepl("^I69", code)
  stroke <- stroke_icd9 | stroke_icd10
  
  tremor <- code %in% c("3331", "7810", "G250", "G251", "G252", "R251")
  
  constipation <- code %in% c(
    "56032", "5640", "56400", "56401", "56409",
    "K5641", "K581", "K5900", "K5901", "K5904", "K5909"
  )
  
  rbd <- code %in% c("32742", "G4752")
  
  olfactory_impairment <- code %in% c(
    "7811", "R430", "R431", "R432", "R438", "R439"
  )
  
  tibble(
    orthostatic_hypotension = as.integer(orthostatic_hypotension),
    syncope = as.integer(syncope),
    falls = as.integer(falls),
    stroke = as.integer(stroke),
    tremor = as.integer(tremor),
    constipation = as.integer(constipation),
    rbd = as.integer(rbd),
    olfactory_impairment = as.integer(olfactory_impairment)
  )
}

# =============================================================================
# 3. PROCESS DIAGNOSIS CLAIMS IN BATCHES
# =============================================================================
# This intentionally does NOT collect the full outpatient file.
# Each batch first restricts to final cohort IDs and then collects.

outpatient_ds <- open_dataset(DATA_FILES$outpatient)

# Original script used read_parquet() for inpatient. We retain that strategy because
# the inpatient file was already manageable there.
inpatient_raw <- read_parquet(DATA_FILES$inpatient) %>%
  mutate(ENROLID = as.character(ENROLID))

id_batches <- split(
  analysis_ids$ENROLID,
  ceiling(seq_along(analysis_ids$ENROLID) / BATCH_SIZE)
)

process_dx_batch <- function(batch_ids, batch_number) {
  batch_periods <- analysis_ids %>%
    filter(ENROLID %in% batch_ids)
  
  # -------------------- outpatient --------------------
  out_wide <- outpatient_ds %>%
    select(ENROLID, SVCDATE, DX1, DX2, DX3, DX4) %>%
    mutate(ENROLID = cast(ENROLID, string())) %>%
    filter(ENROLID %in% batch_ids) %>%
    collect() %>%
    mutate(
      ENROLID = as.character(ENROLID),
      diagnosis_date = as.Date(SVCDATE)
    ) %>%
    select(ENROLID, diagnosis_date, DX1, DX2, DX3, DX4)
  
  out_long <- out_wide %>%
    pivot_longer(
      cols = c(DX1, DX2, DX3, DX4),
      names_to = "dx_position",
      values_to = "diagnosis_code"
    ) %>%
    filter(!is.na(diagnosis_code), diagnosis_code != "") %>%
    select(ENROLID, diagnosis_date, diagnosis_code)
  
  rm(out_wide)
  gc()
  
  # -------------------- inpatient --------------------
  in_wide <- inpatient_raw %>%
    filter(ENROLID %in% batch_ids) %>%
    select(ENROLID, ADMDATE, starts_with("DX"))
  
  in_long <- in_wide %>%
    pivot_longer(
      cols = starts_with("DX"),
      names_to = "dx_position",
      values_to = "diagnosis_code"
    ) %>%
    filter(!is.na(diagnosis_code), diagnosis_code != "") %>%
    transmute(
      ENROLID = as.character(ENROLID),
      diagnosis_date = as.Date(ADMDATE),
      diagnosis_code = diagnosis_code
    )
  
  rm(in_wide)
  gc()
  
  dx <- bind_rows(out_long, in_long) %>%
    inner_join(
      batch_periods %>%
        select(
          ENROLID, treatment_combined, baseline_start, baseline_end,
          risk_start_date, end_date
        ),
      by = "ENROLID"
    ) %>%
    mutate(code_clean = normalize_dx(diagnosis_code))
  
  flags <- flag_prodromal_dx(dx$code_clean)
  dx <- bind_cols(dx, flags)
  
  # Baseline binary covariates: >=1 claim during 365-day baseline.
  baseline_dx <- dx %>%
    filter(diagnosis_date >= baseline_start, diagnosis_date <= baseline_end) %>%
    group_by(ENROLID) %>%
    summarise(
      orthostatic_hypotension = as.integer(any(orthostatic_hypotension == 1)),
      syncope = as.integer(any(syncope == 1)),
      falls = as.integer(any(falls == 1)),
      stroke = as.integer(any(stroke == 1)),
      tremor = as.integer(any(tremor == 1)),
      constipation = as.integer(any(constipation == 1)),
      rbd = as.integer(any(rbd == 1)),
      olfactory_impairment = as.integer(any(olfactory_impairment == 1)),
      .groups = "drop"
    )
  
  # Reviewer-requested autonomic validation during follow-up.
  # We count claims only inside each participant's actual risk/follow-up interval.
  followup_autonomic <- dx %>%
    filter(diagnosis_date >= risk_start_date, diagnosis_date <= end_date) %>%
    group_by(ENROLID) %>%
    summarise(
      followup_oh = as.integer(any(orthostatic_hypotension == 1)),
      followup_syncope = as.integer(any(syncope == 1)),
      followup_falls = as.integer(any(falls == 1)),
      followup_oh_claims = sum(orthostatic_hypotension),
      followup_syncope_claims = sum(syncope),
      followup_fall_claims = sum(falls),
      .groups = "drop"
    )
  
  rm(dx, out_long, in_long, flags)
  gc()
  
  list(
    baseline = baseline_dx,
    followup = followup_autonomic
  )
}

baseline_dx_list <- vector("list", length(id_batches))
followup_autonomic_list <- vector("list", length(id_batches))

for (i in seq_along(id_batches)) {
  log_message(paste("Diagnosis batch", i, "of", length(id_batches)))
  tmp <- process_dx_batch(id_batches[[i]], i)
  baseline_dx_list[[i]] <- tmp$baseline
  followup_autonomic_list[[i]] <- tmp$followup
  rm(tmp)
  gc()
}

baseline_prodromal_dx <- bind_rows(baseline_dx_list)
followup_autonomic <- bind_rows(followup_autonomic_list)

rm(baseline_dx_list, followup_autonomic_list)
gc()

# Add patients with no qualifying diagnosis as zeros.
baseline_prodromal_dx <- analysis_ids %>%
  select(ENROLID) %>%
  left_join(baseline_prodromal_dx, by = "ENROLID") %>%
  mutate(across(-ENROLID, ~replace_na(.x, 0L)))

followup_autonomic <- analysis_ids %>%
  select(ENROLID, treatment_combined) %>%
  left_join(followup_autonomic, by = "ENROLID") %>%
  mutate(
    across(
      c(
        followup_oh, followup_syncope, followup_falls,
        followup_oh_claims, followup_syncope_claims, followup_fall_claims
      ),
      ~replace_na(.x, 0L)
    )
  )

write_parquet(
  baseline_prodromal_dx,
  paste0(EXPANDED_PATH, "baseline_prodromal_diagnosis_covariates.parquet")
)
write_parquet(
  followup_autonomic,
  paste0(EXPANDED_PATH, "followup_autonomic_validation_patient_level.parquet")
)

# =============================================================================
# 4. DOPAMINE-BLOCKING MEDICATIONS + OVERALL MEDICATION BURDEN
# =============================================================================
REDBOOK <- read_csv(REDBOOK_PATH, show_col_types = FALSE) %>%
  transmute(
    GENERID = as.character(GENERID),
    GENNME = tolower(as.character(GENNME))
  ) %>%
  distinct(GENERID, .keep_all = TRUE)

dopamine_blocking_drugs_core <- c(
  "chlorpromazine",
  "thioridazine",
  "perphenazine",
  "fluphenazine",
  "trifluoperazine",
  "haloperidol",
  "droperidol",
  "pimozide",
  "thiothixene",
  "loxapine",
  "olanzapine",
  "risperidone",
  "paliperidone",
  "ziprasidone",
  "aripiprazole",
  "brexpiprazole",
  "quetiapine",
  "clozapine",
  "asenapine",
  "iloperidone",
  "lurasidone",
  "metoclopramide",
  "prochlorperazine"
)

drba_regex <- paste0(
  "(^|[^a-z])(",
  paste(dopamine_blocking_drugs_core, collapse = "|"),
  ")([^a-z]|$)"
)

drba_dictionary <- REDBOOK %>%
  filter(str_detect(GENNME, regex(drba_regex, ignore_case = TRUE))) %>%
  mutate(drba = 1L)

write_csv(
  drba_dictionary,
  paste0(EXPANDED_PATH, "dopamine_blocking_drug_dictionary_QC.csv")
)

medication_ds <- open_dataset(DATA_FILES$medication)

process_med_batch <- function(batch_ids) {
  batch_periods <- analysis_ids %>%
    filter(ENROLID %in% batch_ids) %>%
    select(ENROLID, baseline_start, baseline_end)
  
  meds <- medication_ds %>%
    select(ENROLID, GENERID, SVCDATE) %>%
    mutate(
      ENROLID = cast(ENROLID, string()),
      GENERID = cast(GENERID, string())
    ) %>%
    filter(ENROLID %in% batch_ids) %>%
    collect() %>%
    mutate(
      ENROLID = as.character(ENROLID),
      GENERID = as.character(GENERID),
      SVCDATE = as.Date(SVCDATE)
    ) %>%
    inner_join(batch_periods, by = "ENROLID") %>%
    filter(SVCDATE >= baseline_start, SVCDATE <= baseline_end)
  
  # Distinct GENERID is the medication-burden unit.
  burden <- meds %>%
    filter(!is.na(GENERID), GENERID != "") %>%
    distinct(ENROLID, GENERID) %>%
    count(ENROLID, name = "n_baseline_medications")
  
  drba <- meds %>%
    inner_join(
      drba_dictionary %>% select(GENERID, GENNME),
      by = "GENERID"
    ) %>%
    group_by(ENROLID) %>%
    summarise(
      dopamine_blocker = 1L,
      n_drba_prescriptions = n(),
      n_distinct_drba = n_distinct(GENERID),
      .groups = "drop"
    )
  
  # Drug-level QC counts.
  drba_qc <- meds %>%
    inner_join(
      drba_dictionary %>% select(GENERID, GENNME),
      by = "GENERID"
    ) %>%
    distinct(ENROLID, GENERID, GENNME) %>%
    count(GENNME, name = "n_patients")
  
  rm(meds)
  gc()
  
  list(burden = burden, drba = drba, drba_qc = drba_qc)
}

med_burden_list <- vector("list", length(id_batches))
drba_list <- vector("list", length(id_batches))
drba_qc_list <- vector("list", length(id_batches))

for (i in seq_along(id_batches)) {
  log_message(paste("Medication batch", i, "of", length(id_batches)))
  tmp <- process_med_batch(id_batches[[i]])
  med_burden_list[[i]] <- tmp$burden
  drba_list[[i]] <- tmp$drba
  drba_qc_list[[i]] <- tmp$drba_qc
  rm(tmp)
  gc()
}

baseline_medication_covariates <- analysis_ids %>%
  select(ENROLID) %>%
  left_join(bind_rows(med_burden_list), by = "ENROLID") %>%
  left_join(bind_rows(drba_list), by = "ENROLID") %>%
  mutate(
    n_baseline_medications = replace_na(n_baseline_medications, 0L),
    dopamine_blocker = replace_na(dopamine_blocker, 0L),
    n_drba_prescriptions = replace_na(n_drba_prescriptions, 0L),
    n_distinct_drba = replace_na(n_distinct_drba, 0L)
  )

drba_drug_counts <- bind_rows(drba_qc_list) %>%
  group_by(GENNME) %>%
  summarise(n_patients = sum(n_patients), .groups = "drop") %>%
  arrange(desc(n_patients))

write_parquet(
  baseline_medication_covariates,
  paste0(EXPANDED_PATH, "baseline_medication_covariates.parquet")
)
write_csv(
  drba_drug_counts,
  paste0(EXPANDED_PATH, "dopamine_blocker_drug_counts_QC.csv")
)

rm(med_burden_list, drba_list, drba_qc_list)
gc()

# =============================================================================
# 5. MERGE EXPANDED COVARIATES WITH THE ORIGINAL OUTCOME-READY DATASET
# =============================================================================

expanded_dataset <- outcome_ready_dataset %>%
  left_join(
    baseline_prodromal_dx,
    by = "ENROLID"
  ) %>%
  left_join(
    baseline_medication_covariates,
    by = "ENROLID"
  ) %>%
  mutate(
    across(
      c(
        orthostatic_hypotension,
        syncope,
        falls,
        stroke,
        tremor,
        constipation,
        rbd,
        olfactory_impairment,
        dopamine_blocker,
        n_baseline_medications
      ),
      ~replace_na(.x, 0)
    ),
    
    # Cap follow-up at 10 years
    followup_time_10y =
      pmin(
        followup_time,
        MAX_FOLLOWUP_DAYS
      ),
    
    # PD counts as an event only if it occurs within the 10-year horizon
    event_PD_10y =
      ifelse(
        event_PD == 1 &
          !is.na(PD_date) &
          PD_date <= risk_start_date + MAX_FOLLOWUP_DAYS,
        1L,
        0L
      )
  )

write_parquet(
  expanded_dataset,
  paste0(EXPANDED_PATH, "outcome_ready_dataset_expanded_covariates.parquet")
)

# =============================================================================
# 6. BASELINE + FOLLOW-UP AUTONOMIC VALIDATION BY TREATMENT ARM
# =============================================================================
autonomic_validation <- expanded_dataset %>%
  select(
    ENROLID, treatment_combined,
    orthostatic_hypotension, syncope, falls
  ) %>%
  left_join(
    followup_autonomic %>%
      select(
        ENROLID, followup_oh, followup_syncope, followup_falls,
        followup_oh_claims, followup_syncope_claims, followup_fall_claims
      ),
    by = "ENROLID"
  ) %>%
  mutate(
    across(
      c(
        followup_oh, followup_syncope, followup_falls,
        followup_oh_claims, followup_syncope_claims, followup_fall_claims
      ),
      ~replace_na(.x, 0)
    )
  ) %>%
  group_by(treatment_combined) %>%
  summarise(
    N = n(),
    baseline_OH_n = sum(orthostatic_hypotension),
    baseline_OH_pct = 100 * mean(orthostatic_hypotension),
    baseline_syncope_n = sum(syncope),
    baseline_syncope_pct = 100 * mean(syncope),
    baseline_falls_n = sum(falls),
    baseline_falls_pct = 100 * mean(falls),
    followup_OH_n = sum(followup_oh),
    followup_OH_pct = 100 * mean(followup_oh),
    followup_OH_claims = sum(followup_oh_claims),
    followup_syncope_n = sum(followup_syncope),
    followup_syncope_pct = 100 * mean(followup_syncope),
    followup_syncope_claims = sum(followup_syncope_claims),
    followup_falls_n = sum(followup_falls),
    followup_falls_pct = 100 * mean(followup_falls),
    followup_fall_claims = sum(followup_fall_claims),
    .groups = "drop"
  )

write_csv(
  autonomic_validation,
  paste0(EXPANDED_PATH, "autonomic_validation_baseline_followup_by_arm.csv")
)
print(autonomic_validation)

# =============================================================================
# 7. UNWEIGHTED EXPANDED-COVARIATE SUMMARY BY ARM
# =============================================================================
expanded_covariates <- c(
  "orthostatic_hypotension",
  "syncope",
  "falls",
  "stroke",
  "tremor",
  "constipation",
  "rbd",
  "olfactory_impairment",
  "dopamine_blocker"
)

expanded_binary_summary <- expanded_dataset %>%
  select(ENROLID, treatment_combined, all_of(expanded_covariates)) %>%
  pivot_longer(
    cols = all_of(expanded_covariates),
    names_to = "covariate",
    values_to = "value"
  ) %>%
  group_by(treatment_combined, covariate) %>%
  summarise(
    N = n(),
    n = sum(value == 1, na.rm = TRUE),
    pct = 100 * mean(value == 1, na.rm = TRUE),
    .groups = "drop"
  )

med_burden_summary <- expanded_dataset %>%
  group_by(treatment_combined) %>%
  summarise(
    N = n(),
    mean_medications = mean(n_baseline_medications, na.rm = TRUE),
    sd_medications = sd(n_baseline_medications, na.rm = TRUE),
    median_medications = median(n_baseline_medications, na.rm = TRUE),
    q1_medications = quantile(n_baseline_medications, 0.25, na.rm = TRUE),
    q3_medications = quantile(n_baseline_medications, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  expanded_binary_summary,
  paste0(EXPANDED_PATH, "expanded_covariates_unweighted_by_arm.csv")
)
write_csv(
  med_burden_summary,
  paste0(EXPANDED_PATH, "medication_burden_unweighted_by_arm.csv")
)

# =============================================================================
# 8. EXPANDED PS + IPTW + TRIMMED-WEIGHT BALANCE + WEIGHTED COX
# =============================================================================
# Same original PS variables + the reviewer-requested expanded covariates.
original_ps_vars <- c(
  "age_at_index",
  "sex_factor",
  "enrollment_year",
  "n_outpatient_visits",
  "n_hospitalizations",
  "chf",
  "carit",
  "valv",
  "pvd",
  "ond",
  "cpd",
  "diabunc",
  "diabc",
  "rf",
  "depre"
)

new_ps_vars <- c(
  "orthostatic_hypotension",
  "syncope",
  "falls",
  "stroke",
  "tremor",
  "constipation",
  "rbd",
  "olfactory_impairment",
  "dopamine_blocker",
  "n_baseline_medications"
)

ps_covariates <- c(original_ps_vars, new_ps_vars)

missing_ps_vars <- setdiff(ps_covariates, names(expanded_dataset))
if (length(missing_ps_vars) > 0) {
  stop(
    paste(
      "These PS variables are missing from outcome_ready_dataset:",
      paste(missing_ps_vars, collapse = ", ")
    )
  )
}

run_expanded_pair <- function(data, comparator, exposed, label) {
  log_message(paste("Running expanded PS:", exposed, "vs", comparator))
  
  dat <- data %>%
    filter(
      treatment_combined %in% c(comparator, exposed),
      followup_time_10y > 0
    ) %>%
    mutate(
      treatment_combined = factor(
        treatment_combined,
        levels = c(comparator, exposed)
      ),
      sex_factor = factor(sex_factor)
    )
  
  # Complete-case check. These variables should normally be complete.
  cc <- complete.cases(dat[, ps_covariates])
  if (any(!cc)) {
    log_message(paste("Removing", sum(!cc), "rows with missing PS covariates for", label))
    dat <- dat[cc, ]
  }
  
  ps_formula <- as.formula(
    paste(
      "treatment_combined ~",
      paste(ps_covariates, collapse = " + ")
    )
  )
  
  # WeightIt PS model. keep.mparts=TRUE retains model components where supported.
  wobj <- weightit(
    ps_formula,
    data = dat,
    method = "ps",
    estimand = "ATE",
    stabilize = TRUE,
    keep.mparts = TRUE
  )
  
  dat$iptw <- wobj$weights
  
  # Same 1st/99th percentile trimming strategy as the original analysis.
  trim_limits <- quantile(dat$iptw, probs = c(0.01, 0.99), na.rm = TRUE)
  dat <- dat %>%
    mutate(
      iptw_trimmed = pmin(
        pmax(iptw, trim_limits[[1]]),
        trim_limits[[2]]
      )
    )
  
  # IMPORTANT: assess balance using the ACTUAL trimmed weights used in Cox.
  balance_trimmed <- bal.tab(
    ps_formula,
    data = dat,
    weights = dat$iptw_trimmed,
    method = "weighting",
    estimand = "ATE",
    binary = "std",
    un = TRUE,
    thresholds = c(m = 0.1)
  )
  
  balance_df <- as.data.frame(balance_trimmed$Balance) %>%
    tibble::rownames_to_column("covariate")
  
  write_csv(
    balance_df,
    paste0(EXPANDED_PATH, "balance_", label, "_trimmed_weights.csv")
  )
  
  pdf(
    paste0(PLOT_PATH, "love_plot_", label, "_trimmed_weights.pdf"),
    width = 10,
    height = 9
  )
  print(
    love.plot(
      ps_formula,
      data = dat,
      weights = dat$iptw_trimmed,
      method = "weighting",
      estimand = "ATE",
      abs = TRUE,
      threshold = 0.1,
      binary = "std",
      var.order = "unadjusted",
      title = paste("Expanded PS balance:", exposed, "vs", comparator)
    )
  )
  dev.off()
  
  # PS overlap plot. For binary treatment WeightIt stores one estimated PS/person.
  ps_vector <- wobj$ps
  if (!is.null(ps_vector)) {
    ps_plot_data <- dat %>%
      mutate(propensity_score = as.numeric(ps_vector))
    
    p_ps <- ggplot(
      ps_plot_data,
      aes(x = propensity_score, fill = treatment_combined)
    ) +
      geom_density(alpha = 0.35) +
      labs(
        x = "Propensity score",
        y = "Density",
        fill = "Treatment",
        title = paste("Propensity-score overlap:", exposed, "vs", comparator)
      ) +
      theme_bw()
    
    ggsave(
      paste0(PLOT_PATH, "PS_overlap_", label, ".pdf"),
      p_ps,
      width = 8,
      height = 6
    )
  }
  
  # Weight distribution.
  weight_summary <- dat %>%
    group_by(treatment_combined) %>%
    summarise(
      n = n(),
      mean_untrimmed = mean(iptw),
      median_untrimmed = median(iptw),
      max_untrimmed = max(iptw),
      mean_trimmed = mean(iptw_trimmed),
      median_trimmed = median(iptw_trimmed),
      max_trimmed = max(iptw_trimmed),
      .groups = "drop"
    )
  
  write_csv(
    weight_summary,
    paste0(EXPANDED_PATH, "weight_summary_", label, ".csv")
  )
  
  # Save the fitted PS coefficients if the underlying model is available.
  ps_coef <- NULL
  if (!is.null(wobj$obj)) {
    ps_coef <- tryCatch(
      broom::tidy(wobj$obj, conf.int = TRUE),
      error = function(e) NULL
    )
  }
  if (!is.null(ps_coef)) {
    write_csv(
      ps_coef,
      paste0(EXPANDED_PATH, "PS_coefficients_", label, ".csv")
    )
  }
  
  # Robust/sandwich SE through survey::svycoxph.
  weighted_design <- svydesign(
    ids = ~1,
    weights = ~iptw_trimmed,
    data = dat
  )
  
  cox_model <- svycoxph(
    Surv(followup_time_10y / 365.25, event_PD_10y) ~ treatment_combined,
    design = weighted_design
  )
  
  cox_result <- tidy(
    cox_model,
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    mutate(
      comparison = paste(exposed, "vs", comparator),
      analysis = "Expanded prodromal-PD covariate IPTW sensitivity analysis",
      max_followup_years = MAX_FOLLOWUP_YEARS,
      weight_trim_lower = trim_limits[[1]],
      weight_trim_upper = trim_limits[[2]]
    )
  
  write_csv(
    cox_result,
    paste0(EXPANDED_PATH, "weighted_cox_", label, ".csv")
  )
  
  # Unweighted and weighted descriptive summaries for all PS covariates.
  # svymean() produces weighted means/proportions; binary variables are interpretable as proportions.
  design_by_group <- split(dat, dat$treatment_combined)
  
  weighted_summary_list <- lapply(names(design_by_group), function(g) {
    dg <- design_by_group[[g]]
    des_g <- svydesign(ids = ~1, weights = ~iptw_trimmed, data = dg)
    
    numeric_vars <- ps_covariates[
      sapply(dg[, ps_covariates, drop = FALSE], is.numeric)
    ]
    
    out <- lapply(numeric_vars, function(v) {
      m <- svymean(as.formula(paste0("~", v)), des_g, na.rm = TRUE)
      tibble(
        treatment_group = g,
        covariate = v,
        weighted_mean = as.numeric(coef(m))[1],
        weighted_se = as.numeric(SE(m))[1]
      )
    })
    
    bind_rows(out)
  })
  
  weighted_summary <- bind_rows(weighted_summary_list)
  
  write_csv(
    weighted_summary,
    paste0(EXPANDED_PATH, "weighted_covariate_summary_", label, ".csv")
  )
  
  write_parquet(
    dat,
    paste0(EXPANDED_PATH, "analysis_dataset_", label, ".parquet")
  )
  
  list(
    data = dat,
    weights = wobj,
    balance = balance_trimmed,
    cox_model = cox_model,
    cox_result = cox_result
  )
}

# Primary pairwise comparisons, matching the original TTE structure.
result_ARB <- run_expanded_pair(
  data = expanded_dataset,
  comparator = "Other_FirstLine",
  exposed = "ARB",
  label = "ARB_vs_Other"
)

result_CCB <- run_expanded_pair(
  data = expanded_dataset,
  comparator = "Other_FirstLine",
  exposed = "CCB",
  label = "CCB_vs_Other"
)

expanded_cox_results <- bind_rows(
  result_ARB$cox_result,
  result_CCB$cox_result
)

write_csv(
  expanded_cox_results,
  paste0(EXPANDED_PATH, "expanded_PS_weighted_Cox_results_all.csv")
)

print(expanded_cox_results)

# =============================================================================
# 9. FINAL QC
# =============================================================================
qc_final <- expanded_dataset %>%
  group_by(treatment_combined) %>%
  summarise(
    N = n(),
    PD_events_10y = sum(event_PD_10y),
    OH_baseline = sum(orthostatic_hypotension),
    syncope_baseline = sum(syncope),
    falls_baseline = sum(falls),
    stroke_baseline = sum(stroke),
    tremor_baseline = sum(tremor),
    constipation_baseline = sum(constipation),
    RBD_baseline = sum(rbd),
    olfactory_baseline = sum(olfactory_impairment),
    dopamine_blocker_baseline = sum(dopamine_blocker),
    mean_medication_burden = mean(n_baseline_medications),
    median_medication_burden = median(n_baseline_medications),
    .groups = "drop"
  )

write_csv(
  qc_final,
  paste0(EXPANDED_PATH, "final_QC_by_treatment_group.csv")
)

print(qc_final)
log_message("Expanded prodromal-PD covariate sensitivity analysis completed.")

###############################################################################
# 10. RESIDUAL-IMBALANCE ADJUSTED WEIGHTED COX MODELS
#     Pairwise only:
#       1) ARB vs Other_FirstLine
#       2) CCB vs Other_FirstLine
###############################################################################

cat("\n")
cat("============================================================\n")
cat("RESIDUAL-IMBALANCE ADJUSTED WEIGHTED COX MODELS\n")
cat("============================================================\n")

# =============================================================================
# Step 10.1. File paths
# =============================================================================

expanded_dataset_file <- paste0(
  EXPANDED_PATH,
  "outcome_ready_dataset_expanded_covariates.parquet"
)

arb_weighted_file <- paste0(
  EXPANDED_PATH,
  "analysis_dataset_ARB_vs_Other.parquet"
)

ccb_weighted_file <- paste0(
  EXPANDED_PATH,
  "analysis_dataset_CCB_vs_Other.parquet"
)

# =============================================================================
# Step 10.2. Reload expanded dataset if object is no longer in memory
# =============================================================================

if (!exists("expanded_dataset")) {
  
  if (!file.exists(expanded_dataset_file)) {
    
    stop(
      paste0(
        "Cannot find: ",
        expanded_dataset_file,
        "\nPlease run through Step 5 once to generate this file."
      )
    )
  }
  
  expanded_dataset <- read_parquet(
    expanded_dataset_file
  ) %>%
    mutate(
      ENROLID = as.character(ENROLID),
      treatment_combined = as.character(
        treatment_combined
      )
    )
  
  cat(
    "Reloaded expanded dataset: ",
    nrow(expanded_dataset),
    " patients\n",
    sep = ""
  )
}

# =============================================================================
# Step 10.3. Define the same expanded PS covariates
# =============================================================================

original_ps_vars_residual <- c(
  "age_at_index",
  "sex_factor",
  "enrollment_year",
  "n_outpatient_visits",
  "n_hospitalizations",
  "chf",
  "carit",
  "valv",
  "pvd",
  "ond",
  "cpd",
  "diabunc",
  "diabc",
  "rf",
  "depre"
)

new_ps_vars_residual <- c(
  "orthostatic_hypotension",
  "syncope",
  "falls",
  "stroke",
  "tremor",
  "constipation",
  "rbd",
  "olfactory_impairment",
  "dopamine_blocker",
  "n_baseline_medications"
)

ps_covariates_residual <- c(
  original_ps_vars_residual,
  new_ps_vars_residual
)

# =============================================================================
# Step 10.4. Helper function
# Reuse saved pairwise weighted data if available.
# If missing, rebuild ONLY PS/IPTW from expanded_dataset.
# =============================================================================

get_pairwise_weighted_data <- function(
    exposed,
    comparator,
    saved_file
) {
  
  if (file.exists(saved_file)) {
    
    cat(
      "\nReading saved weighted dataset:\n",
      saved_file,
      "\n"
    )
    
    dat <- read_parquet(
      saved_file
    ) %>%
      mutate(
        ENROLID = as.character(ENROLID),
        treatment_combined = factor(
          as.character(treatment_combined),
          levels = c(
            comparator,
            exposed
          )
        )
      )
    
    if (!"iptw_trimmed" %in% names(dat)) {
      
      stop(
        paste0(
          "The saved file does not contain iptw_trimmed: ",
          saved_file
        )
      )
    }
    
    return(dat)
  }
  
  cat(
    "\nSaved weighted dataset not found.\n",
    "Rebuilding PS/IPTW only for ",
    exposed,
    " vs ",
    comparator,
    "\n",
    sep = ""
  )
  
  dat <- expanded_dataset %>%
    filter(
      treatment_combined %in%
        c(
          comparator,
          exposed
        ),
      followup_time_10y > 0
    ) %>%
    mutate(
      treatment_combined = factor(
        treatment_combined,
        levels = c(
          comparator,
          exposed
        )
      ),
      sex_factor = factor(
        sex_factor
      )
    )
  
  cc <- complete.cases(
    dat[
      ,
      ps_covariates_residual
    ]
  )
  
  if (any(!cc)) {
    
    cat(
      "Removing ",
      sum(!cc),
      " patients with missing PS covariates.\n",
      sep = ""
    )
    
    dat <- dat[
      cc,
    ]
  }
  
  ps_formula_residual <- as.formula(
    paste(
      "treatment_combined ~",
      paste(
        ps_covariates_residual,
        collapse = " + "
      )
    )
  )
  
  ps_weights_residual <- weightit(
    ps_formula_residual,
    data = dat,
    method = "ps",
    estimand = "ATE",
    stabilize = TRUE
  )
  
  dat$iptw <- ps_weights_residual$weights
  
  trim_limits_residual <- quantile(
    dat$iptw,
    probs = c(
      0.01,
      0.99
    ),
    na.rm = TRUE
  )
  
  dat <- dat %>%
    mutate(
      iptw_trimmed = pmin(
        pmax(
          iptw,
          trim_limits_residual[[1]]
        ),
        trim_limits_residual[[2]]
      )
    )
  
  write_parquet(
    dat,
    saved_file
  )
  
  cat(
    "Rebuilt and saved:\n",
    saved_file,
    "\n"
  )
  
  dat
}

# =============================================================================
# Step 10.5. ARB vs Other_FirstLine
# =============================================================================

arb_dat_residual <- get_pairwise_weighted_data(
  exposed = "ARB",
  comparator = "Other_FirstLine",
  saved_file = arb_weighted_file
)

cat(
  "\nARB vs Other cohort sizes:\n"
)

print(
  arb_dat_residual %>%
    count(
      treatment_combined
    )
)

# Survey design using the same trimmed IPTW
arb_design_residual <- svydesign(
  ids = ~1,
  weights = ~iptw_trimmed,
  data = arb_dat_residual
)

# Weighted Cox + additional adjustment for residual imbalance
arb_cox_residual <- svycoxph(
  Surv(
    followup_time_10y / 365.25,
    event_PD_10y
  ) ~
    treatment_combined +
    n_hospitalizations +
    chf +
    carit,
  design = arb_design_residual
)

arb_cox_residual_result <- tidy(
  arb_cox_residual,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    comparison =
      "ARB vs Other_FirstLine",
    analysis =
      paste0(
        "Expanded IPTW + additional adjustment for ",
        "hospitalizations, CHF, and cardiac arrhythmia"
      )
  )

cat(
  "\n================ ARB vs Other ================\n"
)

print(
  arb_cox_residual_result
)

write_csv(
  arb_cox_residual_result,
  paste0(
    EXPANDED_PATH,
    "weighted_cox_ARB_vs_Other_residual_adjustment.csv"
  )
)

arb_treatment_effect_residual <-
  arb_cox_residual_result %>%
  filter(
    grepl(
      "^treatment_combined",
      term
    )
  )

cat(
  "\nARB treatment effect:\n"
)

print(
  arb_treatment_effect_residual
)

# =============================================================================
# Step 10.6. CCB vs Other_FirstLine
# =============================================================================

ccb_dat_residual <- get_pairwise_weighted_data(
  exposed = "CCB",
  comparator = "Other_FirstLine",
  saved_file = ccb_weighted_file
)

cat(
  "\nCCB vs Other cohort sizes:\n"
)

print(
  ccb_dat_residual %>%
    count(
      treatment_combined
    )
)

# Separate survey design
ccb_design_residual <- svydesign(
  ids = ~1,
  weights = ~iptw_trimmed,
  data = ccb_dat_residual
)

# Separate CCB vs Other Cox model
ccb_cox_residual <- svycoxph(
  Surv(
    followup_time_10y / 365.25,
    event_PD_10y
  ) ~
    treatment_combined +
    n_hospitalizations +
    chf +
    carit,
  design = ccb_design_residual
)

ccb_cox_residual_result <- tidy(
  ccb_cox_residual,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    comparison =
      "CCB vs Other_FirstLine",
    analysis =
      paste0(
        "Expanded IPTW + additional adjustment for ",
        "hospitalizations, CHF, and cardiac arrhythmia"
      )
  )

cat(
  "\n================ CCB vs Other ================\n"
)

print(
  ccb_cox_residual_result
)

write_csv(
  ccb_cox_residual_result,
  paste0(
    EXPANDED_PATH,
    "weighted_cox_CCB_vs_Other_residual_adjustment.csv"
  )
)

ccb_treatment_effect_residual <-
  ccb_cox_residual_result %>%
  filter(
    grepl(
      "^treatment_combined",
      term
    )
  )

cat(
  "\nCCB treatment effect:\n"
)

print(
  ccb_treatment_effect_residual
)

# =============================================================================
# Step 10.7. Final pairwise summary
# Only combine the final result table for display.
# The Cox models themselves were fitted SEPARATELY.
# =============================================================================

residual_adjustment_treatment_effects <- bind_rows(
  arb_treatment_effect_residual,
  ccb_treatment_effect_residual
)

cat("\n")
cat("============================================================\n")
cat("PAIRWISE RESIDUAL-ADJUSTED RESULTS\n")
cat("============================================================\n")

print(
  residual_adjustment_treatment_effects
)

write_csv(
  residual_adjustment_treatment_effects,
  paste0(
    EXPANDED_PATH,
    "residual_adjustment_pairwise_treatment_effects.csv"
  )
)