###############################################################################
# ARB SUBGROUP ANALYSES IN TARGET-TRIAL EMULATION
# 1) BBB-crossing vs non-BBB-crossing ARB initiators
# 2) BBB-crossing / non-BBB-crossing ARB vs Other_FirstLine and vs ACEi
# 3) Individual ARB initiators vs Other_FirstLine and vs ACEi
#
# IMPORTANT DESIGN CHOICE:
# - ARB subgroup assignment is based ONLY on the ARB initiated at index.
# - Subsequent switching between individual ARBs is NOT treated as a new
#   censoring event. Follow-up/censoring otherwise follows the parent analyses.
# - "Other_FirstLine" comparisons inherit the original primary TTE cohort and
#   censoring.
# - ACEi comparisons inherit the finalized class-specific ACEi cohort and
#   censoring framework.
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
library(purrr)

# =============================================================================
# 0. PATHS / PARAMETERS
# =============================================================================
DATASET <- "MDCR"
BASE_PATH <- "/data/MarketScan_data/hypertension_cohort_update"
REVISED_OUTPUT_PATH <- "$HOME/TTE/HTN_final/MDCR_results/"
CLASS_OUTPUT_PATH <- paste0(
  REVISED_OUTPUT_PATH,
  "class_specific_ARB_ACEi_CCB/"
)
OUTPUT_PATH <- paste0(
  REVISED_OUTPUT_PATH,
  "ARB_BBB_and_individual_agent_subgroups/"
)
PLOT_PATH <- paste0(OUTPUT_PATH, "plots/")

dir.create(OUTPUT_PATH, recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_PATH, recursive = TRUE, showWarnings = FALSE)

MEDICATION_FILE <- file.path(BASE_PATH, paste0(DATASET, "_D.parquet"))
REDBOOK_PATH <- "/data/MarketScan_data/dictionary/REDBOOK.csv"
EXPANDED_FILE <- paste0(
  REVISED_OUTPUT_PATH,
  "expanded_prodromal_PS/outcome_ready_dataset_expanded_covariates.parquet"
)
CLASS_ANALYSIS_FILE <- paste0(
  CLASS_OUTPUT_PATH,
  "class_specific_outcome_ready_with_prodromal_covariates.parquet"
)

MAX_FOLLOWUP_YEARS <- 10
MAX_FOLLOWUP_DAYS <- MAX_FOLLOWUP_YEARS * 365.25
BATCH_SIZE <- 5000

# Prespecified minimums for individual-agent reporting.
# Models are attempted when there are >=5 exposed PD events and >=200 exposed
# initiators. The manuscript-reportable flag is stricter: >=10 exposed PD
# events and >=500 exposed initiators.
MIN_N_TO_MODEL <- 200
MIN_EVENTS_TO_MODEL <- 5
MIN_N_TO_REPORT <- 500
MIN_EVENTS_TO_REPORT <- 10

log_file <- paste0(OUTPUT_PATH, DATASET, "_ARB_subgroup_log.txt")
cat("Started: ", as.character(Sys.time()), "\n", file = log_file)

log_message <- function(x) {
  cat(x, "\n")
  cat(x, "\n", file = log_file, append = TRUE)
}

# =============================================================================
# 1. LOAD ORIGINAL PRIMARY TTE COHORT
# =============================================================================
original <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/outcome_ready_dataset.parquet")
original <- original %>%
  mutate(
    ENROLID = as.character(ENROLID),
    treatment_combined = as.character(treatment_combined),
    index_date = as.Date(index_date),
    risk_start_date = as.Date(risk_start_date),
    censor_date = as.Date(censor_date),
    event_date = as.Date(event_date),
    PD_date = as.Date(PD_date),
    sex_factor = factor(sex_factor),
    analysis_followup_time_10y = pmin(as.numeric(followup_time), MAX_FOLLOWUP_DAYS),
    analysis_event_10y = as.integer(
      event_PD == 1 &
        !is.na(event_date) &
        event_date <= risk_start_date + MAX_FOLLOWUP_DAYS
    )
  ) %>%
  filter(
    treatment_combined %in% c("ARB", "Other_FirstLine"),
    !is.na(analysis_followup_time_10y),
    analysis_followup_time_10y > 0
  )

log_message(paste("Original ARB + Other cohort N =", nrow(original)))

# =============================================================================
# 2. LOAD FINAL CLASS-SPECIFIC COHORT FOR ACEi COMPARISONS
# =============================================================================
if (!file.exists(CLASS_ANALYSIS_FILE)) {
  stop(
    paste0(
      "Missing finalized class-specific file: ", CLASS_ANALYSIS_FILE,
      "\nRun TTE_class_specific_ARB_vs_ACEi_CCB_original_censoring_v2.R first."
    )
  )
}

class_analysis <- read_parquet(CLASS_ANALYSIS_FILE) %>%
  mutate(
    ENROLID = as.character(ENROLID),
    class_specific_group = as.character(class_specific_group),
    index_date = as.Date(index_date),
    risk_start_date = as.Date(risk_start_date),
    sex_factor = factor(sex_factor),
    analysis_followup_time_10y = as.numeric(pair_followup_time_10y),
    analysis_event_10y = as.integer(pair_event_10y)
  ) %>%
  filter(
    class_specific_group %in% c("ARB", "ACEi"),
    !is.na(analysis_followup_time_10y),
    analysis_followup_time_10y > 0
  )

log_message(paste("Final class-specific ARB + ACEi cohort N =", nrow(class_analysis)))

# =============================================================================
# 3. LOAD EXPANDED PRODROMAL COVARIATES FOR ORIGINAL ARB/OTHER COHORT
# =============================================================================
if (!file.exists(EXPANDED_FILE)) {
  stop(
    paste0(
      "Missing expanded covariate file: ", EXPANDED_FILE,
      "\nRun the expanded prodromal-PD sensitivity script first."
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

original <- original %>%
  select(-any_of(names(expanded_covs)[-1])) %>%
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

# =============================================================================
# 4. DEFINE ARB AGENTS AND BBB CLASSIFICATION
# =============================================================================
# User-specified BBB classification.
BBB_CROSSING <- c(
  "Valsartan",
  "Telmisartan",
  "Fimasartan",
  "Candesartan",
  "Azilsartan"
)

NON_BBB_CROSSING <- c(
  "Irbesartan",
  "Losartan",
  "Olmesartan",
  "Eprosartan"
)

UNKNOWN_BBB <- c("Tasosartan")

ALL_ARB_AGENTS <- c(BBB_CROSSING, NON_BBB_CROSSING, UNKNOWN_BBB)

arb_classification_table <- tibble(
  arb_agent = ALL_ARB_AGENTS,
  BBB_group = case_when(
    arb_agent %in% BBB_CROSSING ~ "BBB_crossing",
    arb_agent %in% NON_BBB_CROSSING ~ "Non_BBB_crossing",
    TRUE ~ "Unknown"
  )
)

write_csv(
  arb_classification_table,
  paste0(OUTPUT_PATH, "ARB_BBB_classification_prespecified.csv")
)

# =============================================================================
# 5. MAP ARB GENERIDs TO SPECIFIC AGENTS USING REDBOOK
# =============================================================================
drug_dictionary <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/drug_dictionary.parquet")

drug_dictionary <- drug_dictionary %>%
  mutate(GENERID = as.character(GENERID))

ARB_GENERIDS <- drug_dictionary %>%
  filter(drug_class == "ARB") %>%
  distinct(GENERID) %>%
  pull(GENERID)

redbook <- read_csv(REDBOOK_PATH, show_col_types = FALSE) %>%
  transmute(
    GENERID = as.character(GENERID),
    GENNME = tolower(as.character(GENNME))
  ) %>%
  distinct(GENERID, .keep_all = TRUE)

# Specific-agent identification is component based, so an ARB+diuretic FDC is
# still assigned to its ARB component (e.g., losartan/HCTZ -> Losartan).
ARB_GENERID_AGENT_MAP <- redbook %>%
  filter(GENERID %in% ARB_GENERIDS) %>%
  mutate(
    arb_agent = case_when(
      str_detect(GENNME, regex("valsartan", ignore_case = TRUE)) ~ "Valsartan",
      str_detect(GENNME, regex("telmisartan", ignore_case = TRUE)) ~ "Telmisartan",
      str_detect(GENNME, regex("fimasartan", ignore_case = TRUE)) ~ "Fimasartan",
      str_detect(GENNME, regex("candesartan", ignore_case = TRUE)) ~ "Candesartan",
      str_detect(GENNME, regex("azilsartan", ignore_case = TRUE)) ~ "Azilsartan",
      str_detect(GENNME, regex("irbesartan", ignore_case = TRUE)) ~ "Irbesartan",
      str_detect(GENNME, regex("losartan", ignore_case = TRUE)) ~ "Losartan",
      str_detect(GENNME, regex("olmesartan", ignore_case = TRUE)) ~ "Olmesartan",
      str_detect(GENNME, regex("eprosartan", ignore_case = TRUE)) ~ "Eprosartan",
      str_detect(GENNME, regex("tasosartan", ignore_case = TRUE)) ~ "Tasosartan",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(arb_classification_table, by = "arb_agent")

write_csv(
  ARB_GENERID_AGENT_MAP,
  paste0(OUTPUT_PATH, "ARB_GENERID_to_agent_REDBOOK_QC.csv")
)

# =============================================================================
# 6. IDENTIFY THE ARB INITIATED AT INDEX
# =============================================================================
# IMPORTANT: assignment is based only on index-date ARB initiation.
# Subsequent within-ARB switching is intentionally ignored.
medication_ds <- open_dataset(MEDICATION_FILE)

arb_index_dates <- original %>%
  filter(treatment_combined == "ARB") %>%
  select(ENROLID, index_date) %>%
  distinct()

id_batches <- split(
  arb_index_dates$ENROLID,
  ceiling(seq_along(arb_index_dates$ENROLID) / BATCH_SIZE)
)

extract_index_arb_batch <- function(batch_ids) {
  periods <- arb_index_dates %>% filter(ENROLID %in% batch_ids)
  
  x <- medication_ds %>%
    select(ENROLID, GENERID, SVCDATE) %>%
    mutate(
      ENROLID = cast(ENROLID, string()),
      GENERID = cast(GENERID, string())
    ) %>%
    filter(
      ENROLID %in% batch_ids,
      GENERID %in% ARB_GENERIDS
    ) %>%
    collect() %>%
    mutate(
      ENROLID = as.character(ENROLID),
      GENERID = as.character(GENERID),
      SVCDATE = as.Date(SVCDATE)
    ) %>%
    inner_join(periods, by = "ENROLID") %>%
    filter(SVCDATE == index_date) %>%
    left_join(
      ARB_GENERID_AGENT_MAP %>% select(GENERID, arb_agent, BBB_group),
      by = "GENERID"
    ) %>%
    select(ENROLID, index_date, GENERID, arb_agent, BBB_group)
  
  x
}

log_message(paste("Scanning index ARB prescriptions in", length(id_batches), "batches"))

arb_index_records <- bind_rows(lapply(seq_along(id_batches), function(i) {
  log_message(paste("Index ARB batch", i, "of", length(id_batches)))
  extract_index_arb_batch(id_batches[[i]])
}))

write_parquet(
  arb_index_records,
  paste0(OUTPUT_PATH, "ARB_index_prescription_records.parquet")
)

# One index patient should map to one specific ARB component. If multiple
# distinct ARB components are found on the index date, classify as Ambiguous
# rather than selecting one arbitrarily.
arb_index_assignment <- arb_index_records %>%
  group_by(ENROLID) %>%
  summarise(
    n_index_arb_records = n(),
    n_distinct_mapped_agents = n_distinct(arb_agent[!is.na(arb_agent)]),
    arb_agent = case_when(
      n_distinct_mapped_agents == 1 ~ first(arb_agent[!is.na(arb_agent)]),
      n_distinct_mapped_agents > 1 ~ "Ambiguous_multiple_ARB",
      TRUE ~ "Unmapped_ARB"
    ),
    .groups = "drop"
  ) %>%
  left_join(arb_classification_table, by = "arb_agent") %>%
  mutate(
    BBB_group = case_when(
      arb_agent == "Ambiguous_multiple_ARB" ~ "Ambiguous",
      arb_agent == "Unmapped_ARB" ~ "Unmapped",
      TRUE ~ BBB_group
    )
  )

write_csv(
  arb_index_assignment,
  paste0(OUTPUT_PATH, "ARB_index_agent_assignment.csv")
)

# =============================================================================
# 7. ATTACH INDEX-ARB SUBGROUP TO BOTH ANALYSIS BASES
# =============================================================================
original_analysis <- original %>%
  left_join(arb_index_assignment, by = "ENROLID") %>%
  mutate(
    arb_agent = ifelse(treatment_combined == "ARB", arb_agent, NA_character_),
    BBB_group = ifelse(treatment_combined == "ARB", BBB_group, NA_character_)
  )

class_analysis <- class_analysis %>%
  left_join(arb_index_assignment, by = "ENROLID") %>%
  mutate(
    arb_agent = ifelse(class_specific_group == "ARB", arb_agent, NA_character_),
    BBB_group = ifelse(class_specific_group == "ARB", BBB_group, NA_character_)
  )

# ARB assignment QC uses the original ARB cohort; ARB follow-up is unchanged in
# the finalized ACEi class-specific analysis.
arb_assignment_qc <- original_analysis %>%
  filter(treatment_combined == "ARB") %>%
  group_by(arb_agent, BBB_group) %>%
  summarise(
    N = n(),
    PD_events_10y = sum(analysis_event_10y, na.rm = TRUE),
    person_years_10y = sum(analysis_followup_time_10y, na.rm = TRUE) / 365.25,
    .groups = "drop"
  ) %>%
  arrange(desc(N))

print(arb_assignment_qc, n = Inf)
write_csv(
  arb_assignment_qc,
  paste0(OUTPUT_PATH, "ARB_index_agent_and_BBB_QC.csv")
)

bbb_qc <- original_analysis %>%
  filter(treatment_combined == "ARB") %>%
  group_by(BBB_group) %>%
  summarise(
    N = n(),
    PD_events_10y = sum(analysis_event_10y, na.rm = TRUE),
    person_years_10y = sum(analysis_followup_time_10y, na.rm = TRUE) / 365.25,
    .groups = "drop"
  ) %>%
  arrange(desc(N))

print(bbb_qc)
write_csv(bbb_qc, paste0(OUTPUT_PATH, "ARB_BBB_group_QC.csv"))

# =============================================================================
# 8. PS COVARIATES
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
# 9. GENERIC PAIRWISE IPTW + WEIGHTED COX FUNCTION
# =============================================================================
safe_filename <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

run_pair <- function(
    dat,
    group_var,
    comparator,
    exposure,
    ps_vars,
    model_name,
    analysis_family,
    save_ps_plot = TRUE,
    min_exposed_n = 1,
    min_exposed_events = 1) {
  
  label <- safe_filename(paste(analysis_family, exposure, "vs", comparator, model_name, sep = "__"))
  
  d <- dat %>%
    filter(.data[[group_var]] %in% c(comparator, exposure)) %>%
    mutate(
      pair_treatment = factor(.data[[group_var]], levels = c(comparator, exposure)),
      sex_factor = factor(sex_factor)
    )
  
  exposed_qc <- d %>%
    filter(.data[[group_var]] == exposure) %>%
    summarise(
      exposed_N = n(),
      exposed_events = sum(analysis_event_10y, na.rm = TRUE),
      exposed_PY = sum(analysis_followup_time_10y, na.rm = TRUE) / 365.25
    )
  
  comparator_qc <- d %>%
    filter(.data[[group_var]] == comparator) %>%
    summarise(
      comparator_N = n(),
      comparator_events = sum(analysis_event_10y, na.rm = TRUE),
      comparator_PY = sum(analysis_followup_time_10y, na.rm = TRUE) / 365.25
    )
  
  qc <- bind_cols(exposed_qc, comparator_qc) %>%
    mutate(
      analysis_family = analysis_family,
      comparison = paste(exposure, "vs", comparator),
      PS_model = model_name,
      reportable_prespecified =
        exposed_N >= MIN_N_TO_REPORT & exposed_events >= MIN_EVENTS_TO_REPORT
    )
  
  if (
    exposed_qc$exposed_N < min_exposed_n ||
    exposed_qc$exposed_events < min_exposed_events ||
    comparator_qc$comparator_N == 0 ||
    comparator_qc$comparator_events == 0
  ) {
    log_message(
      paste(
        "SKIPPED", label,
        "| exposed N =", exposed_qc$exposed_N,
        "| exposed events =", exposed_qc$exposed_events
      )
    )
    
    return(list(
      qc = qc %>% mutate(model_status = "Skipped: insufficient N/events"),
      result = NULL,
      balance_summary = NULL,
      balance = NULL
    ))
  }
  
  cc <- complete.cases(d[, ps_vars, drop = FALSE])
  if (any(!cc)) {
    log_message(paste(label, "- removing", sum(!cc), "rows with missing PS covariates"))
    d <- d[cc, ]
  }
  
  # Confirm both treatment levels remain after complete-case restriction.
  if (n_distinct(d$pair_treatment) < 2) {
    return(list(
      qc = qc %>% mutate(model_status = "Skipped: one treatment group after complete cases"),
      result = NULL,
      balance_summary = NULL,
      balance = NULL
    ))
  }
  
  ps_formula <- as.formula(
    paste("pair_treatment ~", paste(ps_vars, collapse = " + "))
  )
  
  fit_out <- tryCatch({
    w <- weightit(
      ps_formula,
      data = d,
      method = "ps",
      estimand = "ATE",
      stabilize = TRUE,
      keep.mparts = TRUE
    )
    
    d$iptw <- as.numeric(w$weights)
    trim_limits <- quantile(d$iptw, c(0.01, 0.99), na.rm = TRUE)
    d$iptw_trimmed <- pmin(
      pmax(d$iptw, trim_limits[[1]]),
      trim_limits[[2]]
    )
    
    bal <- bal.tab(
      ps_formula,
      data = d,
      weights = d$iptw_trimmed,
      method = "weighting",
      estimand = "ATE",
      un = TRUE,
      binary = "std",
      thresholds = c(m = 0.1)
    )
    
    bal_df <- as.data.frame(bal$Balance) %>%
      rownames_to_column("covariate")
    
    write_csv(
      bal_df,
      paste0(OUTPUT_PATH, "balance_", label, ".csv")
    )
    
    balance_summary <- tibble(
      analysis_family = analysis_family,
      comparison = paste(exposure, "vs", comparator),
      PS_model = model_name,
      max_abs_SMD_unweighted = max(abs(bal_df$Diff.Un), na.rm = TRUE),
      max_abs_SMD_weighted = max(abs(bal_df$Diff.Adj), na.rm = TRUE),
      n_covariates_SMD_ge_0_1 = sum(abs(bal_df$Diff.Adj) >= 0.1, na.rm = TRUE)
    )
    
    incidence <- d %>%
      group_by(pair_treatment) %>%
      summarise(
        N = n(),
        PD_events_10y = sum(analysis_event_10y, na.rm = TRUE),
        person_years_10y = sum(analysis_followup_time_10y, na.rm = TRUE) / 365.25,
        incidence_per_1000_PY = 1000 * PD_events_10y / person_years_10y,
        .groups = "drop"
      ) %>%
      mutate(
        analysis_family = analysis_family,
        comparison = paste(exposure, "vs", comparator),
        PS_model = model_name
      )
    
    write_csv(
      incidence,
      paste0(OUTPUT_PATH, "incidence_", label, ".csv")
    )
    
    design <- svydesign(ids = ~1, weights = ~iptw_trimmed, data = d)
    
    cox <- svycoxph(
      Surv(analysis_followup_time_10y / 365.25, analysis_event_10y) ~ pair_treatment,
      design = design
    )
    
    result <- tidy(cox, exponentiate = TRUE, conf.int = TRUE) %>%
      mutate(
        analysis_family = analysis_family,
        comparison = paste(exposure, "vs", comparator),
        exposure = exposure,
        comparator = comparator,
        PS_model = model_name,
        max_followup_years = MAX_FOLLOWUP_YEARS,
        trim_lower = trim_limits[[1]],
        trim_upper = trim_limits[[2]],
        total_N = nrow(d),
        total_events = sum(d$analysis_event_10y, na.rm = TRUE),
        exposed_N = sum(d$pair_treatment == exposure),
        exposed_events = sum(
          d$analysis_event_10y[d$pair_treatment == exposure], na.rm = TRUE
        ),
        comparator_N = sum(d$pair_treatment == comparator),
        comparator_events = sum(
          d$analysis_event_10y[d$pair_treatment == comparator], na.rm = TRUE
        ),
        reportable_prespecified =
          exposed_N >= MIN_N_TO_REPORT & exposed_events >= MIN_EVENTS_TO_REPORT
      )
    
    write_csv(
      result,
      paste0(OUTPUT_PATH, "Cox_", label, ".csv")
    )
    
    weight_summary <- d %>%
      group_by(pair_treatment) %>%
      summarise(
        N = n(),
        mean_weight = mean(iptw_trimmed, na.rm = TRUE),
        median_weight = median(iptw_trimmed, na.rm = TRUE),
        min_weight = min(iptw_trimmed, na.rm = TRUE),
        max_weight = max(iptw_trimmed, na.rm = TRUE),
        .groups = "drop"
      )
    
    write_csv(
      weight_summary,
      paste0(OUTPUT_PATH, "weights_", label, ".csv")
    )
    
    if (save_ps_plot && !is.null(w$ps)) {
      p <- d %>%
        mutate(propensity_score = as.numeric(w$ps)) %>%
        ggplot(aes(x = propensity_score, fill = pair_treatment)) +
        geom_density(alpha = 0.35) +
        labs(
          x = "Propensity score",
          y = "Density",
          fill = "Treatment",
          title = paste(exposure, "vs", comparator, "-", model_name)
        ) +
        theme_bw()
      
      ggsave(
        paste0(PLOT_PATH, "PS_overlap_", label, ".pdf"),
        p,
        width = 8,
        height = 6
      )
    }
    
    pdf(
      paste0(PLOT_PATH, "love_plot_", label, ".pdf"),
      width = 10,
      height = 9
    )
    print(
      love.plot(
        bal,
        abs = TRUE,
        threshold = 0.1,
        binary = "std",
        var.order = "unadjusted",
        title = paste("Balance:", exposure, "vs", comparator, "-", model_name)
      )
    )
    dev.off()
    
    list(
      qc = qc %>% mutate(model_status = "Completed"),
      result = result,
      balance_summary = balance_summary,
      balance = bal
    )
  }, error = function(e) {
    log_message(paste("ERROR", label, ":", conditionMessage(e)))
    list(
      qc = qc %>% mutate(model_status = paste0("Error: ", conditionMessage(e))),
      result = NULL,
      balance_summary = NULL,
      balance = NULL
    )
  })
  
  fit_out
}

# =============================================================================
# 10. BUILD BBB ANALYSIS DATASETS
# =============================================================================
# 10A. Direct BBB-crossing vs non-BBB-crossing comparison among ARB initiators.
# Tasosartan (Unknown), ambiguous, and unmapped ARBs are excluded from the
# direct BBB comparison.
bbb_direct <- original_analysis %>%
  filter(
    treatment_combined == "ARB",
    BBB_group %in% c("BBB_crossing", "Non_BBB_crossing")
  )

# 10B. BBB subgroup vs original Other_FirstLine comparator.
bbb_vs_other <- original_analysis %>%
  mutate(
    subgroup_for_model = case_when(
      treatment_combined == "Other_FirstLine" ~ "Other_FirstLine",
      treatment_combined == "ARB" & BBB_group == "BBB_crossing" ~ "BBB_crossing",
      treatment_combined == "ARB" & BBB_group == "Non_BBB_crossing" ~ "Non_BBB_crossing",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(subgroup_for_model))

# 10C. BBB subgroup vs finalized ACEi active comparator.
bbb_vs_acei <- class_analysis %>%
  mutate(
    subgroup_for_model = case_when(
      class_specific_group == "ACEi" ~ "ACEi",
      class_specific_group == "ARB" & BBB_group == "BBB_crossing" ~ "BBB_crossing",
      class_specific_group == "ARB" & BBB_group == "Non_BBB_crossing" ~ "Non_BBB_crossing",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(subgroup_for_model))

# =============================================================================
# 11. BBB SUBGROUP MODELS
# =============================================================================
# Direct BBB-group comparison: non-BBB is reference.
BBB_direct_primary <- run_pair(
  dat = bbb_direct,
  group_var = "BBB_group",
  comparator = "Non_BBB_crossing",
  exposure = "BBB_crossing",
  ps_vars = primary_ps_vars,
  model_name = "Primary_PS",
  analysis_family = "BBB_direct"
)

BBB_direct_expanded <- run_pair(
  dat = bbb_direct,
  group_var = "BBB_group",
  comparator = "Non_BBB_crossing",
  exposure = "BBB_crossing",
  ps_vars = expanded_ps_vars,
  model_name = "Expanded_prodromal_PS",
  analysis_family = "BBB_direct"
)

BBB_crossing_vs_Other_primary <- run_pair(
  dat = bbb_vs_other,
  group_var = "subgroup_for_model",
  comparator = "Other_FirstLine",
  exposure = "BBB_crossing",
  ps_vars = primary_ps_vars,
  model_name = "Primary_PS",
  analysis_family = "BBB_vs_Other"
)

Non_BBB_crossing_vs_Other_primary <- run_pair(
  dat = bbb_vs_other,
  group_var = "subgroup_for_model",
  comparator = "Other_FirstLine",
  exposure = "Non_BBB_crossing",
  ps_vars = primary_ps_vars,
  model_name = "Primary_PS",
  analysis_family = "BBB_vs_Other"
)

BBB_crossing_vs_Other_expanded <- run_pair(
  dat = bbb_vs_other,
  group_var = "subgroup_for_model",
  comparator = "Other_FirstLine",
  exposure = "BBB_crossing",
  ps_vars = expanded_ps_vars,
  model_name = "Expanded_prodromal_PS",
  analysis_family = "BBB_vs_Other"
)

Non_BBB_crossing_vs_Other_expanded <- run_pair(
  dat = bbb_vs_other,
  group_var = "subgroup_for_model",
  comparator = "Other_FirstLine",
  exposure = "Non_BBB_crossing",
  ps_vars = expanded_ps_vars,
  model_name = "Expanded_prodromal_PS",
  analysis_family = "BBB_vs_Other"
)

BBB_crossing_vs_ACEi_primary <- run_pair(
  dat = bbb_vs_acei,
  group_var = "subgroup_for_model",
  comparator = "ACEi",
  exposure = "BBB_crossing",
  ps_vars = primary_ps_vars,
  model_name = "Primary_PS",
  analysis_family = "BBB_vs_ACEi"
)

Non_BBB_crossing_vs_ACEi_primary <- run_pair(
  dat = bbb_vs_acei,
  group_var = "subgroup_for_model",
  comparator = "ACEi",
  exposure = "Non_BBB_crossing",
  ps_vars = primary_ps_vars,
  model_name = "Primary_PS",
  analysis_family = "BBB_vs_ACEi"
)

BBB_crossing_vs_ACEi_expanded <- run_pair(
  dat = bbb_vs_acei,
  group_var = "subgroup_for_model",
  comparator = "ACEi",
  exposure = "BBB_crossing",
  ps_vars = expanded_ps_vars,
  model_name = "Expanded_prodromal_PS",
  analysis_family = "BBB_vs_ACEi"
)

Non_BBB_crossing_vs_ACEi_expanded <- run_pair(
  dat = bbb_vs_acei,
  group_var = "subgroup_for_model",
  comparator = "ACEi",
  exposure = "Non_BBB_crossing",
  ps_vars = expanded_ps_vars,
  model_name = "Expanded_prodromal_PS",
  analysis_family = "BBB_vs_ACEi"
)

bbb_objects <- list(
  BBB_direct_primary,
  BBB_direct_expanded,
  BBB_crossing_vs_Other_primary,
  Non_BBB_crossing_vs_Other_primary,
  BBB_crossing_vs_Other_expanded,
  Non_BBB_crossing_vs_Other_expanded,
  BBB_crossing_vs_ACEi_primary,
  Non_BBB_crossing_vs_ACEi_primary,
  BBB_crossing_vs_ACEi_expanded,
  Non_BBB_crossing_vs_ACEi_expanded
)

BBB_final_results <- bind_rows(lapply(bbb_objects, `[[`, "result"))
BBB_final_balance <- bind_rows(lapply(bbb_objects, `[[`, "balance_summary"))
BBB_model_QC <- bind_rows(lapply(bbb_objects, `[[`, "qc"))

write_csv(BBB_final_results, paste0(OUTPUT_PATH, "FINAL_BBB_Cox_results.csv"))
write_csv(BBB_final_balance, paste0(OUTPUT_PATH, "FINAL_BBB_balance_summary.csv"))
write_csv(BBB_model_QC, paste0(OUTPUT_PATH, "FINAL_BBB_model_QC.csv"))

# Direct BBB-crossing vs non-BBB-crossing result is the formal between-group
# comparison. Extract it into a compact file for easy manuscript use.
BBB_between_group_test <- BBB_final_results %>%
  filter(
    analysis_family == "BBB_direct",
    exposure == "BBB_crossing",
    comparator == "Non_BBB_crossing"
  ) %>%
  select(
    PS_model, estimate, conf.low, conf.high, p.value,
    exposed_N, exposed_events, comparator_N, comparator_events,
    total_N, total_events
  )

write_csv(
  BBB_between_group_test,
  paste0(OUTPUT_PATH, "BBB_crossing_vs_non_BBB_between_group_test.csv")
)

# =============================================================================
# 12. INDIVIDUAL ARB ANALYSES
# =============================================================================
# Run all mapped ARBs against each reference. Model estimation is attempted only
# after the prespecified minimum N/events rule. Main reporting should use the
# reportable_prespecified flag rather than significance.
individual_agents_to_test <- arb_assignment_qc %>%
  filter(arb_agent %in% ALL_ARB_AGENTS) %>%
  arrange(match(arb_agent, ALL_ARB_AGENTS)) %>%
  pull(arb_agent)

individual_model_objects <- list()

for (agent in individual_agents_to_test) {
  log_message(paste("Individual ARB analysis:", agent))
  
  # -----------------------------------------------------------
  # Agent vs original Other_FirstLine
  # -----------------------------------------------------------
  dat_other <- original_analysis %>%
    mutate(
      individual_group = case_when(
        treatment_combined == "Other_FirstLine" ~ "Other_FirstLine",
        treatment_combined == "ARB" & arb_agent == agent ~ agent,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(individual_group))
  
  obj_other <- run_pair(
    dat = dat_other,
    group_var = "individual_group",
    comparator = "Other_FirstLine",
    exposure = agent,
    ps_vars = primary_ps_vars,
    model_name = "Primary_PS",
    analysis_family = "Individual_ARB_vs_Other",
    save_ps_plot = TRUE,
    min_exposed_n = MIN_N_TO_MODEL,
    min_exposed_events = MIN_EVENTS_TO_MODEL
  )
  
  individual_model_objects[[paste0(agent, "__Other")]] <- obj_other
  
  # -----------------------------------------------------------
  # Agent vs finalized ACEi comparator
  # -----------------------------------------------------------
  dat_acei <- class_analysis %>%
    mutate(
      individual_group = case_when(
        class_specific_group == "ACEi" ~ "ACEi",
        class_specific_group == "ARB" & arb_agent == agent ~ agent,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(individual_group))
  
  obj_acei <- run_pair(
    dat = dat_acei,
    group_var = "individual_group",
    comparator = "ACEi",
    exposure = agent,
    ps_vars = primary_ps_vars,
    model_name = "Primary_PS",
    analysis_family = "Individual_ARB_vs_ACEi",
    save_ps_plot = TRUE,
    min_exposed_n = MIN_N_TO_MODEL,
    min_exposed_events = MIN_EVENTS_TO_MODEL
  )
  
  individual_model_objects[[paste0(agent, "__ACEi")]] <- obj_acei
}

Individual_ARB_results <- bind_rows(
  lapply(individual_model_objects, `[[`, "result")
)

Individual_ARB_balance <- bind_rows(
  lapply(individual_model_objects, `[[`, "balance_summary")
)

Individual_ARB_model_QC <- bind_rows(
  lapply(individual_model_objects, `[[`, "qc")
)

write_csv(
  Individual_ARB_results,
  paste0(OUTPUT_PATH, "FINAL_individual_ARB_Cox_results.csv")
)

write_csv(
  Individual_ARB_balance,
  paste0(OUTPUT_PATH, "FINAL_individual_ARB_balance_summary.csv")
)

write_csv(
  Individual_ARB_model_QC,
  paste0(OUTPUT_PATH, "FINAL_individual_ARB_model_QC.csv")
)

# Compact manuscript-facing table: all successfully modeled agents, with the
# prespecified reportability flag retained. Do NOT select rows by P value.
Individual_ARB_manuscript_table <- Individual_ARB_results %>%
  select(
    analysis_family,
    exposure,
    comparator,
    estimate,
    conf.low,
    conf.high,
    p.value,
    exposed_N,
    exposed_events,
    comparator_N,
    comparator_events,
    reportable_prespecified,
    trim_lower,
    trim_upper
  ) %>%
  arrange(analysis_family, exposure)

write_csv(
  Individual_ARB_manuscript_table,
  paste0(OUTPUT_PATH, "Individual_ARB_manuscript_table_all_modeled.csv")
)

Individual_ARB_reportable <- Individual_ARB_manuscript_table %>%
  filter(reportable_prespecified)

write_csv(
  Individual_ARB_reportable,
  paste0(OUTPUT_PATH, "Individual_ARB_manuscript_table_reportable_only.csv")
)

# =============================================================================
# 13. FINAL COMPACT SUMMARY ACROSS BBB + INDIVIDUAL ANALYSES
# =============================================================================
compact_bbb <- BBB_final_results %>%
  transmute(
    analysis_family,
    comparison,
    exposure,
    comparator,
    PS_model,
    HR = estimate,
    CI_lower = conf.low,
    CI_upper = conf.high,
    P_value = p.value,
    exposed_N,
    exposed_events,
    comparator_N,
    comparator_events,
    reportable_prespecified
  )

compact_individual <- Individual_ARB_results %>%
  transmute(
    analysis_family,
    comparison,
    exposure,
    comparator,
    PS_model,
    HR = estimate,
    CI_lower = conf.low,
    CI_upper = conf.high,
    P_value = p.value,
    exposed_N,
    exposed_events,
    comparator_N,
    comparator_events,
    reportable_prespecified
  )

FINAL_ALL_ARB_SUBGROUP_RESULTS <- bind_rows(compact_bbb, compact_individual)

write_csv(
  FINAL_ALL_ARB_SUBGROUP_RESULTS,
  paste0(OUTPUT_PATH, "FINAL_ALL_ARB_SUBGROUP_RESULTS.csv")
)

cat("\n============================================================\n")
cat("ARB INDEX AGENT / BBB QC\n")
cat("============================================================\n")
print(arb_assignment_qc, n = Inf)

cat("\n============================================================\n")
cat("BBB SUBGROUP COX RESULTS\n")
cat("============================================================\n")
print(BBB_final_results, n = Inf)

cat("\n============================================================\n")
cat("BBB BALANCE SUMMARY\n")
cat("============================================================\n")
print(BBB_final_balance, n = Inf)

cat("\n============================================================\n")
cat("INDIVIDUAL ARB RESULTS\n")
cat("============================================================\n")
print(Individual_ARB_manuscript_table, n = Inf)

cat("\n============================================================\n")
cat("INDIVIDUAL ARB BALANCE\n")
cat("============================================================\n")
print(Individual_ARB_balance, n = Inf)

log_message("ARB BBB-crossing and individual-agent subgroup analyses completed.")
log_message(paste("Finished:", as.character(Sys.time())))