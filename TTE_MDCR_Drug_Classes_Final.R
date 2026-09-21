###############################################################################
# CLASS-SPECIFIC ACTIVE-COMPARATOR TTE
# ARB vs ACEi and ARB vs CCB
# Revised: original-study treatment-deviation censoring; BB restriction only
# through the 90-day grace/stability period.
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
library(tibble)

# =============================================================================
# 0. PATHS / PARAMETERS
# =============================================================================
DATASET <- "MDCR"
BASE_PATH <- "/data/MarketScan_data/hypertension_cohort_update"
REVISED_OUTPUT_PATH <- "$HOME/TTE/HTN_final/MDCR_results/"
CLASS_OUTPUT_PATH <- paste0(REVISED_OUTPUT_PATH, "class_specific_ARB_ACEi_CCB/")
PLOT_PATH <- paste0(CLASS_OUTPUT_PATH, "plots/")
dir.create(CLASS_OUTPUT_PATH, recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_PATH, recursive = TRUE, showWarnings = FALSE)

MEDICATION_FILE <- file.path(BASE_PATH, paste0(DATASET, "_D.parquet"))
REDBOOK_PATH <- "/data/MarketScan_data/dictionary/REDBOOK.csv"
BASELINE_DAYS <- 365
EXPOSURE_WINDOW_DAYS <- 90
MAX_FOLLOWUP_YEARS <- 10
MAX_FOLLOWUP_DAYS <- MAX_FOLLOWUP_YEARS * 365.25
BATCH_SIZE <- 25000

log_file <- paste0(CLASS_OUTPUT_PATH, DATASET, "_class_specific_log.txt")
cat("Started: ", as.character(Sys.time()), "\n", file = log_file)

log_message <- function(x) {
  cat(x, "\n")
  cat(x, "\n", file = log_file, append = TRUE)
}


# =============================================================================
# 1. REUSE FINAL ORIGINAL OUTCOME-READY COHORT
# =============================================================================
outcome_ready_dataset <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/outcome_ready_dataset.parquet")

outcome_ready_dataset <- outcome_ready_dataset %>%
  mutate(
    ENROLID = as.character(ENROLID),
    treatment_combined = as.character(treatment_combined),
    index_date = as.Date(index_date),
    risk_start_date = as.Date(risk_start_date),
    censor_date = as.Date(censor_date),
    event_date = as.Date(event_date),
    PD_date = as.Date(PD_date),
    last_enroll = as.Date(last_enroll)
  )

candidate_cohort <- outcome_ready_dataset %>%
  filter(treatment_combined %in% c("ARB", "CCB", "Other_FirstLine")) %>%
  mutate(
    baseline_start = index_date - days(BASELINE_DAYS),
    baseline_end = index_date - days(1)
  )

log_message(paste("Candidate final TTE cohort:", nrow(candidate_cohort)))

# =============================================================================
# 2. DRUG DICTIONARY
#    - Preserve original ARB / CCB / ACEi / diuretic definitions
#    - Build a FULL BB-component dictionary from REDBOOK (no ARB/CCB exclusion)
# =============================================================================
drug_dictionary <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/drug_dictionary.parquet")

drug_dictionary <- drug_dictionary %>%
  mutate(GENERID = as.character(GENERID))

ACEI_GENERIDS <- drug_dictionary %>%
  filter(drug_class == "ACEi") %>%
  distinct(GENERID) %>%
  pull(GENERID)

REDBOOK <- read_csv(REDBOOK_PATH, show_col_types = FALSE) %>%
  transmute(
    GENERID = as.character(GENERID),
    GENNME = tolower(as.character(GENNME))
  ) %>%
  distinct(GENERID, .keep_all = TRUE)

BB_PATTERN <- paste(c(
  "alprenolol","oxprenolol","pindolol","propranolol","timolol","sotalol",
  "nadolol","mepindolol","carteolol","tertatolol","bopindolol","bupranolol",
  "penbutolol","cloranolol","practolol","metoprolol","atenolol","acebutolol",
  "betaxolol","bevantolol","bisoprolol","celiprolol","esmolol","epanolol",
  "s-atenolol","nebivolol","talinolol","landiolol","labetalol","carvedilol"
), collapse = "|")

BB_DICTIONARY_FULL <- REDBOOK %>%
  filter(str_detect(GENNME, regex(BB_PATTERN, ignore_case = TRUE))) %>%
  mutate(BB_component = 1L)

BB_GENERIDS_FULL <- BB_DICTIONARY_FULL %>%
  distinct(GENERID) %>%
  pull(GENERID)

write_csv(
  BB_DICTIONARY_FULL,
  paste0(CLASS_OUTPUT_PATH, "BB_component_dictionary_full_REDBOOK_QC.csv")
)

# Antihypertensive classes used to detect class-specific treatment deviation.
# IMPORTANT:
# - ARB/CCB/ACEi remain the three target treatment groups.
# - BBL and Diuretic are NOT comparators, but starting them after index is a
#   treatment deviation/add-on under the same per-protocol logic as the
#   original study.
# - Fixed-dose products already classified as ARB/CCB/ACEi by the original
#   dictionary remain in that core class (e.g., allowed thiazide FDCs).
SWITCH_DICTIONARY <- drug_dictionary %>%
  filter(drug_class %in% c("ARB", "CCB", "ACEi", "BBL", "Diuretic", "Combo")) %>%
  select(GENERID, drug_class) %>%
  distinct()

SWITCH_GENERIDS <- unique(c(SWITCH_DICTIONARY$GENERID, BB_GENERIDS_FULL))

# =============================================================================
# 3. SCAN LARGE MEDICATION DATA IN BATCHES
#    We need:
#      1) ACEi at index
#      2) BB during baseline / at index
#      3) all post-index antihypertensive classes for grace-period exclusion
#         and subsequent class-specific switching censoring
# =============================================================================
medication_ds <- open_dataset(MEDICATION_FILE)

candidate_ids <- candidate_cohort %>%
  select(ENROLID, index_date, baseline_start, baseline_end, censor_date) %>%
  distinct()

id_batches <- split(
  candidate_ids$ENROLID,
  ceiling(seq_along(candidate_ids$ENROLID) / BATCH_SIZE)
)

process_med_batch <- function(batch_ids) {
  
  periods <- candidate_ids %>% filter(ENROLID %in% batch_ids)
  
  meds <- medication_ds %>%
    select(ENROLID, GENERID, SVCDATE) %>%
    mutate(
      ENROLID = cast(ENROLID, string()),
      GENERID = cast(GENERID, string())
    ) %>%
    filter(
      ENROLID %in% batch_ids,
      GENERID %in% SWITCH_GENERIDS
    ) %>%
    collect() %>%
    mutate(
      ENROLID = as.character(ENROLID),
      GENERID = as.character(GENERID),
      SVCDATE = as.Date(SVCDATE),
      is_BB = as.integer(GENERID %in% BB_GENERIDS_FULL)
    ) %>%
    left_join(SWITCH_DICTIONARY, by = "GENERID") %>%
    mutate(
      # Full REDBOOK BB detection overrides the old BBL dictionary so that
      # BB-containing products are never missed.
      med_class = case_when(
        is_BB == 1 ~ "BBL",
        drug_class == "ARB" ~ "ARB",
        drug_class == "CCB" ~ "CCB",
        drug_class == "ACEi" ~ "ACEi",
        drug_class == "Diuretic" ~ "Diuretic",
        drug_class == "Combo" ~ "Combo",
        TRUE ~ NA_character_
      )
    ) %>%
    inner_join(periods, by = "ENROLID")
  
  index_acei <- meds %>%
    filter(SVCDATE == index_date, GENERID %in% ACEI_GENERIDS) %>%
    distinct(ENROLID) %>%
    mutate(index_ACEi = 1L)
  
  baseline_bb <- meds %>%
    filter(
      is_BB == 1,
      SVCDATE >= baseline_start,
      SVCDATE <= baseline_end
    ) %>%
    distinct(ENROLID) %>%
    mutate(baseline_BB = 1L)
  
  index_bb <- meds %>%
    filter(is_BB == 1, SVCDATE == index_date) %>%
    distinct(ENROLID) %>%
    mutate(index_BB = 1L)
  
  postindex_classes <- meds %>%
    filter(
      SVCDATE > index_date,
      SVCDATE <= censor_date,
      !is.na(med_class)
    ) %>%
    select(ENROLID, SVCDATE, med_class) %>%
    distinct()
  
  rm(meds)
  gc()
  
  list(
    index_acei = index_acei,
    baseline_bb = baseline_bb,
    index_bb = index_bb,
    postindex_classes = postindex_classes
  )
}

idx_acei_list <- vector("list", length(id_batches))
base_bb_list <- vector("list", length(id_batches))
idx_bb_list <- vector("list", length(id_batches))
postclass_list <- vector("list", length(id_batches))

for (i in seq_along(id_batches)) {
  log_message(paste("Medication batch", i, "of", length(id_batches)))
  tmp <- process_med_batch(id_batches[[i]])
  idx_acei_list[[i]] <- tmp$index_acei
  base_bb_list[[i]] <- tmp$baseline_bb
  idx_bb_list[[i]] <- tmp$index_bb
  postclass_list[[i]] <- tmp$postindex_classes
  rm(tmp)
  gc()
}

index_acei_all <- bind_rows(idx_acei_list) %>% distinct(ENROLID, .keep_all = TRUE)
baseline_bb_all <- bind_rows(base_bb_list) %>% distinct(ENROLID, .keep_all = TRUE)
index_bb_all <- bind_rows(idx_bb_list) %>% distinct(ENROLID, .keep_all = TRUE)
postindex_classes_all <- bind_rows(postclass_list) %>% distinct()

write_parquet(index_acei_all, paste0(CLASS_OUTPUT_PATH, "index_ACEi_users.parquet"))
write_parquet(baseline_bb_all, paste0(CLASS_OUTPUT_PATH, "baseline_BB_users.parquet"))
write_parquet(index_bb_all, paste0(CLASS_OUTPUT_PATH, "index_BB_users.parquet"))
write_parquet(postindex_classes_all,
              paste0(CLASS_OUTPUT_PATH, "postindex_antihypertensive_classes.parquet"))

rm(idx_acei_list, base_bb_list, idx_bb_list, postclass_list)
gc()

# =============================================================================
# 4. BUILD ARB / ACEi / CCB COHORT
#    Symmetric BB restriction applies ONLY before risk-set entry:
#      - baseline BB -> exclude
#      - index-date BB -> exclude
#      - BB or any other treatment deviation during days 1-90 -> exclude
# =============================================================================
class_base <- candidate_cohort %>%
  left_join(index_acei_all, by = "ENROLID") %>%
  left_join(baseline_bb_all, by = "ENROLID") %>%
  left_join(index_bb_all, by = "ENROLID") %>%
  mutate(
    index_ACEi = replace_na(index_ACEi, 0L),
    baseline_BB = replace_na(baseline_BB, 0L),
    index_BB = replace_na(index_BB, 0L),
    class_specific_group = case_when(
      treatment_combined == "ARB" ~ "ARB",
      treatment_combined == "CCB" ~ "CCB",
      treatment_combined == "Other_FirstLine" & index_ACEi == 1 ~ "ACEi",
      TRUE ~ NA_character_
    )
  )

# Start with the intended three classes and remove baseline/index BB.
class_pregrace <- class_base %>%
  filter(
    !is.na(class_specific_group),
    baseline_BB == 0,
    index_BB == 0
  )

# Join each post-index antihypertensive claim to the assigned index class.
postindex_deviation <- postindex_classes_all %>%
  inner_join(
    class_pregrace %>%
      select(ENROLID, class_specific_group, index_date, risk_start_date),
    by = "ENROLID"
  ) %>%
  mutate(
    # Any class different from the assigned index class is a treatment
    # deviation/add-on, matching the original study logic.
    is_deviation = med_class != class_specific_group
  ) %>%
  filter(is_deviation)

# During the 90-day grace/stability window: exclude rather than censor.
grace_violations <- postindex_deviation %>%
  filter(
    SVCDATE > index_date,
    SVCDATE <= risk_start_date
  ) %>%
  group_by(ENROLID) %>%
  summarise(
    first_grace_deviation_date = min(SVCDATE),
    first_grace_deviation_class = med_class[which.min(SVCDATE)],
    .groups = "drop"
  )

class_base_clean <- class_pregrace %>%
  filter(!ENROLID %in% grace_violations$ENROLID)

# =============================================================================
# 5. ORIGINAL-STUDY CENSORING LOGIC, NOW CLASS-SPECIFIC
#    After the 90-day risk start:
#      - first treatment deviation/switch -> censor
#      - insurance disenrollment / study end remain embedded in original
#        censor_date
#      - BB has NO special post-landmark censoring rule
# =============================================================================
switch_dates_class_specific <- postindex_deviation %>%
  semi_join(class_base_clean %>% select(ENROLID), by = "ENROLID") %>%
  filter(SVCDATE > risk_start_date) %>%
  group_by(ENROLID) %>%
  summarise(
    class_switch_date = min(SVCDATE),
    first_switch_class = med_class[which.min(SVCDATE)],
    .groups = "drop"
  )

# The original censor_date already contains original switching + last_enroll +
# administrative study end.  Taking pmin() with the newly reconstructed
# class-specific switch date preserves every original censoring mechanism and
# adds the ACEi-specific deviations that were previously hidden inside
# Other_FirstLine.
class_base_clean <- class_base_clean %>%
  left_join(switch_dates_class_specific, by = "ENROLID") %>%
  mutate(
    pair_censor_date = case_when(
      !is.na(class_switch_date) ~ pmin(censor_date, class_switch_date, na.rm = TRUE),
      TRUE ~ censor_date
    ),
    pair_event = as.integer(
      !is.na(event_date) & event_date <= pair_censor_date
    ),
    pair_end_date = case_when(
      pair_event == 1 ~ event_date,
      TRUE ~ pair_censor_date
    ),
    pair_followup_time = as.numeric(pair_end_date - risk_start_date)
  ) %>%
  filter(!is.na(pair_followup_time), pair_followup_time > 0) %>%
  mutate(
    pair_followup_time_10y = pmin(pair_followup_time, MAX_FOLLOWUP_DAYS),
    pair_event_10y = as.integer(
      pair_event == 1 &
        !is.na(event_date) &
        event_date <= risk_start_date + MAX_FOLLOWUP_DAYS
    )
  )

# Flow / QC tables
class_flow <- bind_rows(
  tibble(
    Step = c(
      "Original candidate cohort",
      "Baseline BB excluded",
      "Index-date BB excluded",
      "Grace-period treatment deviation excluded"
    ),
    N = c(
      nrow(candidate_cohort),
      sum(class_base$baseline_BB == 1, na.rm = TRUE),
      sum(class_base$baseline_BB == 0 & class_base$index_BB == 1, na.rm = TRUE),
      nrow(grace_violations)
    )
  ),
  class_base_clean %>%
    count(class_specific_group, name = "N") %>%
    transmute(Step = paste("Final eligible", class_specific_group), N = N)
)

print(class_flow)
write_csv(class_flow, paste0(CLASS_OUTPUT_PATH, "class_specific_cohort_flow.csv"))

grace_qc <- grace_violations %>%
  left_join(
    class_pregrace %>% select(ENROLID, class_specific_group),
    by = "ENROLID"
  ) %>%
  count(class_specific_group, first_grace_deviation_class, name = "N") %>%
  arrange(class_specific_group, desc(N))

write_csv(
  grace_qc,
  paste0(CLASS_OUTPUT_PATH, "grace_period_deviation_QC.csv")
)

switch_qc <- class_base_clean %>%
  group_by(class_specific_group) %>%
  summarise(
    N = n(),
    post_landmark_class_switch_censored = sum(!is.na(class_switch_date)),
    pct_post_landmark_class_switch_censored =
      100 * mean(!is.na(class_switch_date)),
    PD_events_10y = sum(pair_event_10y),
    person_years_10y = sum(pair_followup_time_10y) / 365.25,
    .groups = "drop"
  )

print(switch_qc)
write_csv(
  switch_qc,
  paste0(CLASS_OUTPUT_PATH, "class_specific_switch_censoring_QC.csv")
)

switch_type_qc <- class_base_clean %>%
  filter(!is.na(class_switch_date)) %>%
  count(class_specific_group, first_switch_class, name = "N") %>%
  arrange(class_specific_group, desc(N))

write_csv(
  switch_type_qc,
  paste0(CLASS_OUTPUT_PATH, "post_landmark_switch_type_QC.csv")
)

write_parquet(
  grace_violations,
  paste0(CLASS_OUTPUT_PATH, "grace_period_treatment_deviations.parquet")
)

write_parquet(
  switch_dates_class_specific,
  paste0(CLASS_OUTPUT_PATH, "class_specific_switch_dates.parquet")
)

write_parquet(
  class_base_clean,
  paste0(CLASS_OUTPUT_PATH, "class_specific_outcome_ready_base.parquet")
)

# =============================================================================
# 6. REUSE SAVED PRODROMAL-PD COVARIATES
# =============================================================================
EXPANDED_FILE <- paste0(
  REVISED_OUTPUT_PATH,
  "expanded_prodromal_PS/outcome_ready_dataset_expanded_covariates.parquet"
)

if (!file.exists(EXPANDED_FILE)) {
  stop(
    paste0(
      "Missing saved expanded covariates: ", EXPANDED_FILE,
      "\nRun the previous expanded prodromal-PD script through Step 5 once."
    )
  )
}

expanded_covs <- read_parquet(EXPANDED_FILE) %>%
  mutate(ENROLID = as.character(ENROLID)) %>%
  select(
    ENROLID,
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
  ) %>%
  distinct(ENROLID, .keep_all = TRUE)

class_analysis <- class_base_clean %>%
  left_join(expanded_covs, by = "ENROLID") %>%
  mutate(
    across(
      c(
        orthostatic_hypotension, syncope, falls, stroke, tremor,
        constipation, rbd, olfactory_impairment, dopamine_blocker,
        n_baseline_medications
      ),
      ~replace_na(.x, 0)
    )
  )

write_parquet(
  class_analysis,
  paste0(CLASS_OUTPUT_PATH,
         "class_specific_outcome_ready_with_prodromal_covariates.parquet")
)

# =============================================================================
# 7. PS COVARIATES
# =============================================================================
primary_ps_vars <- c(
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

prodromal_ps_vars <- c(
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

expanded_ps_vars <- c(primary_ps_vars, prodromal_ps_vars)

# =============================================================================
# 8. TABLE 1 HELPERS
# =============================================================================
weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

weighted_sd_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) <= 1) return(NA_real_)
  x <- x[ok]
  w <- w[ok]
  mu <- weighted.mean(x, w)
  sqrt(sum(w * (x - mu)^2) / sum(w))
}

labels <- c(
  age_at_index = "Age at index, years",
  enrollment_year = "Index year",
  n_outpatient_visits = "Outpatient visits in baseline",
  n_hospitalizations = "Hospitalizations in baseline",
  female = "Female sex",
  chf = "Congestive heart failure",
  carit = "Cardiac arrhythmias",
  valv = "Valvular disease",
  pvd = "Peripheral vascular disease",
  ond = "Other neurological disorders",
  cpd = "Chronic pulmonary disease",
  diabunc = "Diabetes mellitus, uncomplicated",
  diabc = "Diabetes mellitus, complicated",
  rf = "Renal failure",
  depre = "Depression",
  orthostatic_hypotension = "Orthostatic hypotension",
  syncope = "Syncope",
  falls = "Falls",
  stroke = "Stroke",
  tremor = "Tremor",
  constipation = "Constipation",
  rbd = "REM sleep behavior disorder",
  olfactory_impairment = "Olfactory impairment",
  dopamine_blocker = "Dopamine-blocking medication",
  n_baseline_medications = "Number of distinct baseline medications"
)

make_table1 <- function(dat, bal, ps_vars, comparator, exposure, label) {
  x <- dat %>%
    mutate(female = as.integer(sex_factor == "Female"))
  
  cont <- intersect(
    c("age_at_index", "enrollment_year", "n_outpatient_visits",
      "n_hospitalizations", "n_baseline_medications"),
    ps_vars
  )
  
  bin <- intersect(
    c("female", "chf", "carit", "valv", "pvd", "ond", "cpd", "diabunc",
      "diabc", "rf", "depre", "orthostatic_hypotension", "syncope", "falls",
      "stroke", "tremor", "constipation", "rbd", "olfactory_impairment",
      "dopamine_blocker"),
    c("female", ps_vars)
  )
  
  d0 <- x %>% filter(pair_treatment == comparator)
  d1 <- x %>% filter(pair_treatment == exposure)
  
  cont_rows <- bind_rows(lapply(cont, function(v) {
    tibble(
      Variable = v,
      Comparator_unweighted = sprintf("%.2f (%.2f)", mean(d0[[v]], na.rm=TRUE), sd(d0[[v]], na.rm=TRUE)),
      Exposure_unweighted = sprintf("%.2f (%.2f)", mean(d1[[v]], na.rm=TRUE), sd(d1[[v]], na.rm=TRUE)),
      Comparator_weighted = sprintf("%.2f (%.2f)", weighted_mean_safe(d0[[v]], d0$iptw_trimmed), weighted_sd_safe(d0[[v]], d0$iptw_trimmed)),
      Exposure_weighted = sprintf("%.2f (%.2f)", weighted_mean_safe(d1[[v]], d1$iptw_trimmed), weighted_sd_safe(d1[[v]], d1$iptw_trimmed))
    )
  }))
  
  bin_rows <- bind_rows(lapply(bin, function(v) {
    tibble(
      Variable = v,
      Comparator_unweighted = sprintf("%.1f%%", 100 * mean(d0[[v]], na.rm=TRUE)),
      Exposure_unweighted = sprintf("%.1f%%", 100 * mean(d1[[v]], na.rm=TRUE)),
      Comparator_weighted = sprintf("%.1f%%", 100 * weighted_mean_safe(d0[[v]], d0$iptw_trimmed)),
      Exposure_weighted = sprintf("%.1f%%", 100 * weighted_mean_safe(d1[[v]], d1$iptw_trimmed))
    )
  }))
  
  b <- as.data.frame(bal$Balance) %>%
    rownames_to_column("BalanceName") %>%
    transmute(
      Variable = ifelse(BalanceName == "sex_factor_Female", "female", BalanceName),
      SMD_unweighted = Diff.Un,
      SMD_weighted = Diff.Adj
    )
  
  out <- bind_rows(cont_rows, bin_rows) %>%
    left_join(b, by = "Variable") %>%
    mutate(
      Characteristic = recode(Variable, !!!labels, .default = Variable)
    ) %>%
    select(
      Characteristic,
      Comparator_unweighted,
      Exposure_unweighted,
      Comparator_weighted,
      Exposure_weighted,
      SMD_unweighted,
      SMD_weighted,
      Variable
    )
  
  nrow_out <- tibble(
    Characteristic = "N",
    Comparator_unweighted = as.character(nrow(d0)),
    Exposure_unweighted = as.character(nrow(d1)),
    Comparator_weighted = NA_character_,
    Exposure_weighted = NA_character_,
    SMD_unweighted = NA_real_,
    SMD_weighted = NA_real_,
    Variable = "N"
  )
  
  out <- bind_rows(nrow_out, out)
  write_csv(out, paste0(CLASS_OUTPUT_PATH, "Table1_", label, ".csv"))
  out
}

# =============================================================================
# 9. PAIRWISE ANALYSIS FUNCTION
# =============================================================================
run_pair <- function(data, comparator, exposure, ps_vars, model_name, pair_name) {
  label <- paste0(pair_name, "_", model_name)
  
  dat <- data %>%
    filter(class_specific_group %in% c(comparator, exposure)) %>%
    mutate(
      pair_treatment = factor(class_specific_group, levels = c(comparator, exposure)),
      sex_factor = factor(sex_factor)
    )
  
  cc <- complete.cases(dat[, ps_vars, drop = FALSE])
  if (any(!cc)) {
    log_message(paste(label, "- removing", sum(!cc), "rows with missing PS variables"))
    dat <- dat[cc, ]
  }
  
  form <- as.formula(
    paste("pair_treatment ~", paste(ps_vars, collapse = " + "))
  )
  
  w <- weightit(
    form,
    data = dat,
    method = "ps",
    estimand = "ATE",
    stabilize = TRUE,
    keep.mparts = TRUE
  )
  
  dat$iptw <- w$weights
  trim <- quantile(dat$iptw, c(0.01, 0.99), na.rm = TRUE)
  
  dat <- dat %>%
    mutate(
      iptw_trimmed = pmin(pmax(iptw, trim[[1]]), trim[[2]])
    )
  
  bal <- bal.tab(
    form,
    data = dat,
    weights = dat$iptw_trimmed,
    method = "weighting",
    estimand = "ATE",
    un = TRUE,
    binary = "std",
    thresholds = c(m = 0.1)
  )
  
  bal_df <- as.data.frame(bal$Balance) %>%
    rownames_to_column("covariate")
  
  write_csv(bal_df, paste0(CLASS_OUTPUT_PATH, "balance_", label, ".csv"))
  
  residual <- bal_df %>%
    filter(!is.na(Diff.Adj), abs(Diff.Adj) >= 0.1)
  
  write_csv(residual, paste0(CLASS_OUTPUT_PATH, "residual_imbalance_", label, ".csv"))
  
  pdf(paste0(PLOT_PATH, "love_plot_", label, ".pdf"), width = 10, height = 9)
  print(love.plot(
    bal,
    abs = TRUE,
    threshold = 0.1,
    var.order = "unadjusted",
    title = paste("Covariate balance:", exposure, "vs", comparator, "-", model_name)
  ))
  dev.off()
  
  if (!is.null(w$ps)) {
    p <- dat %>%
      mutate(propensity_score = as.numeric(w$ps)) %>%
      ggplot(aes(x = propensity_score, fill = pair_treatment)) +
      geom_density(alpha = 0.35) +
      labs(
        x = "Propensity score",
        y = "Density",
        fill = "Treatment",
        title = paste("PS overlap:", exposure, "vs", comparator, "-", model_name)
      ) +
      theme_bw()
    
    ggsave(paste0(PLOT_PATH, "PS_overlap_", label, ".pdf"), p, width = 8, height = 6)
  }
  
  table1 <- make_table1(dat, bal, ps_vars, comparator, exposure, label)
  
  incidence <- dat %>%
    group_by(pair_treatment) %>%
    summarise(
      n = n(),
      events = sum(pair_event_10y),
      person_years = sum(pair_followup_time_10y) / 365.25,
      ir_per_1000py = 1000 * events / person_years,
      .groups = "drop"
    )
  
  write_csv(incidence, paste0(CLASS_OUTPUT_PATH, "incidence_", label, ".csv"))
  
  design <- svydesign(ids = ~1, weights = ~iptw_trimmed, data = dat)
  
  fit <- svycoxph(
    Surv(pair_followup_time_10y / 365.25, pair_event_10y) ~ pair_treatment,
    design = design
  )
  
  fit_res <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      comparison = paste(exposure, "vs", comparator),
      PS_model = model_name,
      max_followup_years = MAX_FOLLOWUP_YEARS,
      trim_lower = trim[[1]],
      trim_upper = trim[[2]],
      N = nrow(dat),
      events = sum(dat$pair_event_10y)
    )
  
  write_csv(fit_res, paste0(CLASS_OUTPUT_PATH, "Cox_", label, ".csv"))
  write_parquet(dat, paste0(CLASS_OUTPUT_PATH, "analysis_dataset_", label, ".parquet"))
  
  log_message(
    paste(
      label,
      "| N =", nrow(dat),
      "| events =", sum(dat$pair_event_10y),
      "| residual |SMD|>=0.1 =", nrow(residual)
    )
  )
  
  list(
    data = dat,
    weights = w,
    balance = bal,
    residual = residual,
    table1 = table1,
    incidence = incidence,
    cox = fit,
    cox_result = fit_res
  )
}

# =============================================================================
# 10. FOUR MODELS: TWO PAIRS × TWO PS SPECIFICATIONS
# =============================================================================
ARB_ACEi_primary <- run_pair(
  class_analysis, "ACEi", "ARB",
  primary_ps_vars, "Primary_PS", "ARB_vs_ACEi"
)

ARB_CCB_primary <- run_pair(
  class_analysis, "CCB", "ARB",
  primary_ps_vars, "Primary_PS", "ARB_vs_CCB"
)

ARB_ACEi_expanded <- run_pair(
  class_analysis, "ACEi", "ARB",
  expanded_ps_vars, "Expanded_prodromal_PS", "ARB_vs_ACEi"
)

ARB_CCB_expanded <- run_pair(
  class_analysis, "CCB", "ARB",
  expanded_ps_vars, "Expanded_prodromal_PS", "ARB_vs_CCB"
)

# =============================================================================
# 11. FINAL SUMMARY
# =============================================================================
final_results <- bind_rows(
  ARB_ACEi_primary$cox_result,
  ARB_ACEi_expanded$cox_result,
  ARB_CCB_primary$cox_result,
  ARB_CCB_expanded$cox_result
)

write_csv(
  final_results,
  paste0(CLASS_OUTPUT_PATH, "FINAL_class_specific_Cox_results.csv")
)

max_smd <- function(obj, comparison, model) {
  b <- as.data.frame(obj$balance$Balance)
  tibble(
    comparison = comparison,
    PS_model = model,
    max_abs_SMD_unweighted = max(abs(b$Diff.Un), na.rm = TRUE),
    max_abs_SMD_weighted = max(abs(b$Diff.Adj), na.rm = TRUE),
    n_covariates_SMD_ge_0_1 = sum(abs(b$Diff.Adj) >= 0.1, na.rm = TRUE)
  )
}

final_balance <- bind_rows(
  max_smd(ARB_ACEi_primary, "ARB vs ACEi", "Primary PS"),
  max_smd(ARB_ACEi_expanded, "ARB vs ACEi", "Expanded prodromal PS"),
  max_smd(ARB_CCB_primary, "ARB vs CCB", "Primary PS"),
  max_smd(ARB_CCB_expanded, "ARB vs CCB", "Expanded prodromal PS")
)

write_csv(
  final_balance,
  paste0(CLASS_OUTPUT_PATH, "FINAL_balance_summary.csv")
)

cat("\n============================================================\n")
cat("FINAL CLASS-SPECIFIC COX RESULTS\n")
cat("============================================================\n")
print(final_results)

cat("\n============================================================\n")
cat("FINAL BALANCE SUMMARY\n")
cat("============================================================\n")
print(final_balance)

log_message("Class-specific ARB vs ACEi / ARB vs CCB analyses completed.")