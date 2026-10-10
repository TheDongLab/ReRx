###############################################################################
# Target Trial Emulation for Parkinson's Disease (PD)
###############################################################################
library(cobalt)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(tidyr)
library(tidyverse)
library(data.table)
library(survival)
library(survminer)
library(arrow)
library(comorbidity)
library(WeightIt)
library(survey)
library(broom)

# =============================================================================
# CONFIGURATION SECTION - Soft Coding for Flexibility
# =============================================================================

# Dataset selection (can be switched for sensitivity analysis)
DATASET <- "MDCR"  # Options: "MDCR", "CCAE", "Medicaid"

# Analysis parameters
WASHOUT_DAYS <- 365  # Washout period in 1 year
BASELINE_DAYS <- 365  # Baseline period for comorbidities and characteristics (1 years)
MIN_OUTPATIENT_COUNT <- 1  # Minimum outpatient visits for PD diagnosis (sensitivity analysis: 1 or 2)
MIN_COMORBIDITY_CLAIMS <- 1  # Minimum claims for comorbidity (sensitivity analysis: 1 or 2)
MIN_AGE <- 18  # Minimum age for inclusion
# MAX_AGE <- 100  # Maximum age for inclusion

# File paths (adjust based on data set)
BASE_PATH <- "/data/MarketScan_data/hypertension_cohort_update"
OUTPUT_PATH <- paste0("$HOME/TTE/HTN_final/", DATASET, "_results/")
PLOT_PATH <- paste0(OUTPUT_PATH, "plots/")

# Create output directories
dir.create(OUTPUT_PATH, recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_PATH, recursive = TRUE, showWarnings = FALSE)

# Data file paths (can be adjusted for different datasets)
DATA_FILES <- list(
  patient_final = paste0(DATASET, "_hypertension_pt_final.parquet"),
  enrollment = paste0(DATASET, "_T.parquet"),
  inpatient = paste0(DATASET, "_I.parquet"),
  outpatient = paste0(DATASET, "_O_limit.parquet"),
  medication = paste0(DATASET, "_D.parquet")
)

# Log file for tracking analysis
log_file <- paste0(OUTPUT_PATH, DATASET, "_analysis_log.txt")
cat("Analysis started at:", as.character(Sys.time()), "\n", file = log_file)
cat("Dataset:", DATASET, "\n", file = log_file, append = TRUE)
cat("Washout period:", WASHOUT_DAYS, "days\n", file = log_file, append = TRUE)

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

log_message <- function(message) {
  cat(message, "\n")
  cat(message, "\n", file = log_file, append = TRUE)
}

# =============================================================================
# DATA PREPARATION
# =============================================================================

setwd(BASE_PATH)

log_message("=== Data Preparation Phase ===")

# Prepare enrollment/follow-up data
log_message("Processing enrollment data...")
enrollment_raw <- open_dataset(DATA_FILES$enrollment)
follow_up_data <- enrollment_raw %>%
  select(ENROLID, DOBYR, SEX, DTSTART, DTEND) %>%
  collect() %>%
  mutate(
    DTSTART = as.Date(DTSTART, format = "%Y-%m-%d"),
    DTEND   = as.Date(DTEND, format = "%Y-%m-%d")
  ) %>%
  group_by(ENROLID, DOBYR, SEX) %>%
  summarise(
    first_enroll = min(DTSTART, na.rm = TRUE),
    last_enroll  = max(DTEND, na.rm = TRUE),
    .groups = "drop"
  )

follow_up_data_update <- follow_up_data %>%
  group_by(ENROLID) %>%
  summarise(
    birth_year_fixed = min(DOBYR, na.rm = TRUE),
    SEX             = first(SEX),
    first_enroll    = min(first_enroll, na.rm = TRUE),
    last_enroll     = max(last_enroll,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ENROLID = as.character(ENROLID))

write_parquet(follow_up_data_update, paste0(OUTPUT_PATH, "follow_up_data_update.parquet"))
# follow_up_data_update <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/follow_up_data_update.parquet")
log_message(paste("Follow-up data processed:", nrow(follow_up_data_update), "all unique patients for analysis"))

# Load and prepare diagnosis data (will be used later for comorbidities and outcomes)
# Inpatient data
inpatient_raw <- read_parquet(DATA_FILES$inpatient)

# Outpatient data
outpatient_raw <- open_dataset(DATA_FILES$outpatient)

# =============================================================================
# MEDICATION CLASSIFICATION
# =============================================================================

log_message("=== Medication Classification ===")

# Load drug dictionary
REDBOOK <- read_csv("/data/MarketScan_data/dictionary/REDBOOK.csv")

# Define drug classes with soft coding approach
drug_patterns <- list(
  ARB = "losartan|eprosartan|valsartan|irbesartan|tasosartan|candesartan|telmisartan|olmesartan|azilsartan|fimasartan",
  
  CCB = "amlodipine|felodipine|isradipine|nicardipine|nifedipine|nimodipine|nisoldipine|nitrendipine|lacidipine|nilvadipine|manidipine|barnidipine|lercanidipine|cilnidipine|benidipine|clevidipine|levamlodipine|mibefradil|verapamil|gallopamil|etripamil|diltiazem|fendiline|bepridil|lidoflazine|perhexiline",
  
  ACEi = "captopril|enalapril|lisinopril|perindopril|ramipril|quinapril|benazepril|cilazapril|fosinopril|trandolapril|spirapril|delapril|moexipril|temocapril|zofenopril|imidapril",
  
  BBL = "alprenolol|oxprenolol|pindolol|propranolol|timolol|sotalol|nadolol|mepindolol|carteolol|tertatolol|bopindolol|bupranolol|penbutolol|cloranolol|practolol|metoprolol|atenolol|acebutolol|betaxolol|bevantolol|bisoprolol|celiprolol|esmolol|epanolol|s-atenolol|nebivolol|talinolol|landiolol|labetalol|carvedilol",
  
  Diuretic = "bendroflumethiazide|hydroflumethiazide|hydrochlorothiazide|chlorothiazide|polythiazide|trichlormethiazide|cyclopenthiazide|methyclothiazide|cyclothiazide|mebutizide",
  
  AntiPD = "trihexyphenidyl|biperiden|metixene|procyclidine|profenamine|dexetimide|phenglutarimide|mazaticol|bornaprine|tropatepine|etanautine|orphenadrine|benzatropine|etybenzatropine|levodopa|melevodopa|etilevodopa|foslevodopa|amantadine|bromocriptine|pergolide|dihydroergocryptine|ropinirole|pramipexole|cabergoline|apomorphine|piribedil|rotigotine|selegiline|rasagiline|safinamide|tolcapone|entacapone|budipine|opicapone|istradefylline",
  
  Antidementia = "donepezil|memantine|rivastigmine|galantamine|aducanumab",
  
  Combo = "(?=.*sartan)(?=.*dipine)"
)

# Create drug classification dictionary
create_drug_class <- function(drug_class, pattern, exclude_patterns = NULL) {
  drugs <- REDBOOK %>%
    filter(grepl(pattern, GENNME, ignore.case = TRUE))
  
  # Apply exclusions if specified
  if (!is.null(exclude_patterns)) {
    for (exclude_pattern in exclude_patterns) {
      drugs <- drugs %>%
        filter(!grepl(exclude_pattern, GENNME, ignore.case = TRUE))
    }
  }
  
  drugs %>%
    select(GENERID, GENNME, STRNGTH) %>%
    mutate(drug_class = drug_class)
}

# Generate all drug classes
ARB_drugs <- create_drug_class("ARB", drug_patterns$ARB, c("dipine")) # Exclude CCBs
CCB_drugs <- create_drug_class("CCB", drug_patterns$CCB, c("sartan"))  # Exclude ARBs
ACEi_drugs <- create_drug_class("ACEi", drug_patterns$ACEi, c(drug_patterns$ARB, drug_patterns$CCB))
BBL_drugs <- create_drug_class("BBL", drug_patterns$BBL, c(drug_patterns$ARB, drug_patterns$CCB))
Diuretic_drugs <- create_drug_class("Diuretic", drug_patterns$Diuretic, c(drug_patterns$ARB, drug_patterns$CCB))
AntiPD_drugs <- create_drug_class("AntiPD", drug_patterns$AntiPD)
Antidementia_drugs <- create_drug_class("Antidementia", drug_patterns$Antidementia)
Combo_drugs <- REDBOOK %>%
  filter(str_detect(GENNME, regex("sartan", ignore_case = TRUE)) &
           str_detect(GENNME, regex("dipine", ignore_case = TRUE))) %>%
  select(GENERID, GENNME, STRNGTH) %>%
  mutate(drug_class = "Combo")

# Combine all drug classes
all_drugs <- bind_rows(ARB_drugs, CCB_drugs, ACEi_drugs, BBL_drugs, 
                       Diuretic_drugs, AntiPD_drugs, Antidementia_drugs, Combo_drugs) %>%
  mutate(GENERID = as.character(GENERID)) %>%
  select(GENERID, drug_class)

# Define drug groups for three-way comparison
# Now ACEi, BBL, Diuretic are combined as "Other_FirstLine"
first_line_drugs <- c("ARB", "CCB", "ACEi", "BBL", "Diuretic", "Combo")
outcome_drugs <- c("AntiPD")  # Outcome-related drugs (may be also add "Antidementia")

# Save drug dictionaries
write_parquet(all_drugs, paste0(OUTPUT_PATH, "drug_dictionary.parquet"))
# all_drugs <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/drug_dictionary.parquet")

# =============================================================================
# ACTIVE-COMPARATOR NEW-USER DESIGN - MODIFIED FOR THREE-WAY COMPARISON
# WITH DRUG SWITCHING FILTER FOR OTHER_FIRSTLINE GROUP
# =============================================================================
STUDY_END_DATE <- as.Date("2024-09-30")
MIN_INDEX_DATE <- STUDY_END_DATE - years(5)

# Load medication data
medication_raw <- open_dataset(DATA_FILES$medication) %>%
  mutate(GENERID = cast(GENERID, string()),
         ENROLID = cast(ENROLID, string()))

drug_exposure <- medication_raw %>%
  select(ENROLID, GENERID, SVCDATE) %>%
  inner_join(all_drugs, by = "GENERID") %>%   # still lazy
  filter(drug_class %in% first_line_drugs)

# Step 1: Create treatment groups by combining ACEi, BBL, Diuretic into Other_FirstLine
drug_exposure_grouped <- drug_exposure %>%
  mutate(
    treatment_group = case_when(
      drug_class == "ARB" ~ "ARB",
      drug_class == "CCB" ~ "CCB",
      drug_class %in% c("ACEi", "BBL", "Diuretic") ~ "Other_FirstLine",
      drug_class == "Combo" ~ "Combo",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(treatment_group))

# Step 2: Identify first prescription of each treatment group
first_line_first_rx <- drug_exposure_grouped %>%
  group_by(ENROLID, treatment_group) %>%
  summarise(
    first_rx_date = min(as.Date(SVCDATE)),
    .groups = "drop"
  )

# Step 3: For patients with multiple treatment groups, select the earliest
earliest_rx_date <- first_line_first_rx %>%
  group_by(ENROLID) %>%
  summarise(
    earliest_date = min(first_rx_date),
    .groups = "drop"
  )

index_candidates <- first_line_first_rx %>%
  inner_join(earliest_rx_date, by = "ENROLID") %>%
  filter(first_rx_date == earliest_date) %>%
  group_by(ENROLID) %>%
  mutate(n_groups_same_date = n()) %>%
  ungroup() %>%
  filter(first_rx_date <= MIN_INDEX_DATE) # At least 5-years period for outcome assessment

# Remove patients with simultaneous initiation of multiple treatment groups
index_drug_users_combined <- index_candidates %>%
  filter(n_groups_same_date == 1) %>%
  select(
    ENROLID,
    treatment_combined = treatment_group,
    index_date = first_rx_date
  ) %>%
  mutate(ENROLID = as.character(ENROLID)) %>%
  collect()

# write_parquet(index_drug_users_combined, paste0(OUTPUT_PATH, "index_drug_users_combined.parquet"))
# index_drug_users_combined <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/index_drug_users_combined.parquet")
log_message(paste("Remaining index drug users:", nrow(index_drug_users_combined)))

# Step 4: Apply new-user design - exclude prior use of outcome drugs
# Create list of all drugs to check in washout period
washout_drug_classes <- c(outcome_drugs)

washout_violations <- medication_raw %>%
  select(ENROLID, GENERID, SVCDATE) %>%
  inner_join(all_drugs, by = "GENERID") %>%
  filter(drug_class %in% washout_drug_classes) %>%
  collect()

#index_drug_users_combined <- index_drug_users_combined %>%
#  mutate(ENROLID = as.character(ENROLID))

washout_violations_baseline <- washout_violations %>%
  mutate(SVCDATE = as.Date(SVCDATE)) %>%
  mutate(ENROLID = as.character(ENROLID)) %>%
  inner_join(index_drug_users_combined %>% select(ENROLID, index_date), by = "ENROLID") %>%
  filter(SVCDATE < index_date & SVCDATE >= (index_date - WASHOUT_DAYS)) %>%
  distinct(ENROLID)

new_users_combined_1 <- index_drug_users_combined %>%
  filter(!ENROLID %in% washout_violations_baseline$ENROLID)

new_users_combined <- new_users_combined_1 %>%
  filter(treatment_combined != "Combo")

log_message(paste("Excluded due to baseline AntiPD use:", nrow(washout_violations_baseline)))
log_message(paste("New users before drug switching filter:", nrow(new_users_combined)))

# Summary by treatment group before switching filter
treatment_summary_before <- new_users_combined %>%
  group_by(treatment_combined) %>%
  summarise(n = n(), .groups = "drop")

log_message("Treatment group distribution before switching filter:")
for (i in 1:nrow(treatment_summary_before)) {
  log_message(paste("-", treatment_summary_before$treatment_combined[i], ":", treatment_summary_before$n[i]))
}

# =============================================================================
# cohort filtering
# =============================================================================

log_message("=== Applying Baseline Enrollment Filters ===")

#follow_up_data_update <- follow_up_data_update %>%
#  mutate(ENROLID = as.character(ENROLID))

cohort_with_enrollment <- new_users_combined %>%
  left_join(
    follow_up_data_update %>% 
      select(ENROLID, birth_year_fixed, SEX, first_enroll, last_enroll),
    by = "ENROLID"
  ) %>%
  mutate(
    baseline_period_available = as.numeric(index_date - first_enroll),
    age_at_index = year(index_date) - birth_year_fixed
  )

log_message(paste("Cohort before enrollment filters:", nrow(cohort_with_enrollment)))

# filter criteria
cohort_filtered <- cohort_with_enrollment %>%
  filter(
    baseline_period_available >= as.numeric(BASELINE_DAYS),
    age_at_index >= as.numeric(MIN_AGE),
    index_date < last_enroll,
    !is.na(SEX)
  )

n_excluded_baseline <- sum(cohort_with_enrollment$baseline_period_available < BASELINE_DAYS, na.rm = TRUE)
n_excluded_age <- sum(cohort_with_enrollment$age_at_index < MIN_AGE, na.rm = TRUE)
n_excluded_followup <- sum(cohort_with_enrollment$index_date >= cohort_with_enrollment$last_enroll, na.rm = TRUE)
n_excluded_sex <- sum(is.na(cohort_with_enrollment$SEX))

log_message(paste("Excluded due to insufficient baseline period (<", BASELINE_DAYS, "days):", n_excluded_baseline))
log_message(paste("Excluded due to age <", MIN_AGE, ":", n_excluded_age))
log_message(paste("Excluded due to no valid follow-up:", n_excluded_followup))
log_message(paste("Excluded due to missing sex:", n_excluded_sex))
log_message(paste("Final cohort after filters:", nrow(cohort_filtered)))

treatment_summary_filtered <- cohort_filtered %>%
  group_by(treatment_combined) %>%
  summarise(
    n = n(),
    mean_age = round(mean(age_at_index), 1),
    mean_baseline_days = round(mean(baseline_period_available), 0),
    .groups = "drop"
  )

log_message("=== Cohort Summary After Filters ===")
print(treatment_summary_filtered)

# =============================================================================
# DRUG SWITCHING FILTER IN EXPOSURE WINDOW
# =============================================================================

# -------------------------------
# Parameters
# -------------------------------
EXPOSURE_WINDOW_DAYS <- 90

# -------------------------------
# Step 1: Pull antihypertensive prescriptions
# ONLY for index cohort patients
# -------------------------------
exposure_window_drugs <- medication_raw %>%
  select(ENROLID, GENERID, SVCDATE) %>%
  inner_join(all_drugs, by = "GENERID") %>%
  filter(drug_class %in% first_line_drugs) %>%      # antihypertensives only
  mutate(
    treatment_group = case_when(
      drug_class == "ARB" ~ "ARB",
      drug_class == "CCB" ~ "CCB",
      drug_class %in% c("ACEi", "BBL", "Diuretic") ~ "Other_FirstLine",
      drug_class == "Combo" ~ "Combo",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(treatment_group)) %>%
  semi_join(
    cohort_filtered %>% select(ENROLID),
    by = "ENROLID"
  ) %>%
  group_by(ENROLID, treatment_group) %>%
  summarise(
    first_rx_date = min(SVCDATE),
    .groups = "drop"
  ) %>%
  collect() %>%                                     # now small & safe
  mutate(
    ENROLID = as.character(ENROLID),
    first_rx_date = as.Date(first_rx_date)
  )

# write_parquet(exposure_window_drugs, paste0(OUTPUT_PATH, "exposure_window_drugs.parquet"))
# exposure_window_drugs <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/exposure_window_drugs.parquet")
log_message(
  paste(
    "Antihypertensive prescriptions pulled for exposure window:",
    nrow(exposure_window_drugs)
  )
)

# -------------------------------
# Step 2: Identify switching / add-on events
# Definition:
# Any antihypertensive drug class ≠ index drug, no Combo
# within 90 days after index date
# -------------------------------
switching_violations <- exposure_window_drugs %>%
  inner_join(
    cohort_filtered %>%
      select(ENROLID, treatment_combined, index_date),
    by = "ENROLID"
  ) %>%
  filter(
    first_rx_date > index_date,
    first_rx_date <= index_date + days(EXPOSURE_WINDOW_DAYS)
  ) %>%
  filter(treatment_group != treatment_combined) %>%
  distinct(ENROLID)

log_message(
  paste(
    "Patients excluded due to switching or add-on within",
    EXPOSURE_WINDOW_DAYS,
    "days:",
    nrow(switching_violations)
  )
)

# -------------------------------
# Step 3: Construct per-protocol cohort
# -------------------------------
final_new_users_three_way <- cohort_filtered %>%
  filter(!ENROLID %in% switching_violations$ENROLID)

# -------------------------------
# Step 4: Summarize final cohort
# -------------------------------
final_treatment_summary <- final_new_users_three_way %>%
  group_by(treatment_combined) %>%
  summarise(n = n(), .groups = "drop")

print(final_treatment_summary)

log_message("Final per-protocol treatment group distribution:")
for (i in seq_len(nrow(final_treatment_summary))) {
  log_message(
    paste(
      "-",
      final_treatment_summary$treatment_combined[i],
      ":",
      final_treatment_summary$n[i]
    )
  )
}

log_message("=== Per-protocol exposure definition completed ===")

###############################################################
## REVIEWER QUICK 4
## DETAILED FLOWCHART COUNTS
###############################################################

log_message("=== Reviewer: Detailed TTE flowchart counts ===")


###############################################################
## Any patient with first-line antihypertensive exposure
###############################################################

n_any_firstline <- earliest_rx_date %>%
  summarise(
    N = n()
  ) %>%
  collect() %>%
  pull(N)


###############################################################
## Index too late for potential >=5-y administrative window
###############################################################

n_index_too_late <- earliest_rx_date %>%
  filter(
    earliest_date > MIN_INDEX_DATE
  ) %>%
  summarise(
    N = n()
  ) %>%
  collect() %>%
  pull(N)


###############################################################
## Simultaneous initiation of >=2 treatment groups
###############################################################

n_multigroup_same_day <- index_candidates %>%
  filter(
    n_groups_same_date > 1
  ) %>%
  distinct(
    ENROLID
  ) %>%
  summarise(
    N = n()
  ) %>%
  collect() %>%
  pull(N)


###############################################################
## ARB + CCB fixed-dose Combo group
###############################################################

n_combo_ARB_CCB <- index_drug_users_combined %>%
  filter(
    treatment_combined == "Combo"
  ) %>%
  summarise(
    N = n()
  ) %>%
  pull(N)


###############################################################
## Baseline AntiPD exclusion
###############################################################

n_AntiPD_baseline_excluded <- nrow(
  washout_violations_baseline
)


###############################################################
## Enrollment filters
###############################################################

n_before_enrollment <- nrow(
  cohort_with_enrollment
)

n_after_enrollment <- nrow(
  cohort_filtered
)

n_enrollment_excluded <-
  n_before_enrollment -
  n_after_enrollment


###############################################################
## 90-day switching/add-on
###############################################################

n_switch_90d <- nrow(
  switching_violations
)


###############################################################
## Primary cohort before PD landmark exclusion
###############################################################

n_primary_pre_PD <- nrow(
  final_new_users_three_way
)


flowchart_QC <- tibble(
  
  Flow_item = c(
    "Patients with first observed eligible antihypertensive treatment",
    "Excluded: initiation too late for >=5-y potential administrative follow-up window",
    "Excluded: simultaneous initiation of >=2 treatment groups",
    "Excluded: prespecified ARB+CCB fixed-dose combination",
    "Excluded: baseline AntiPD medication",
    "Excluded: baseline enrollment / demographic criteria",
    "Excluded: switch/add-on during 90-day exposure window",
    "Primary cohort before early-PD landmark exclusion"
  ),
  
  N = c(
    n_any_firstline,
    n_index_too_late,
    n_multigroup_same_day,
    n_combo_ARB_CCB,
    n_AntiPD_baseline_excluded,
    n_enrollment_excluded,
    n_switch_90d,
    n_primary_pre_PD
  )
)


print(
  flowchart_QC,
  n = nrow(flowchart_QC)
)


write.csv(
  flowchart_QC,
  paste0(
    OUTPUT_PATH,
    "reviewer_flowchart_detailed_counts.csv"
  ),
  row.names = FALSE
)

# -------------------------------
# Step 5: Drug switching pattern
# -------------------------------

switch_dates <- exposure_window_drugs %>%
  inner_join(
    final_new_users_three_way %>%
      select(ENROLID, treatment_combined, index_date),
    by = "ENROLID"
  ) %>%
  filter(first_rx_date > index_date) %>%
  filter(treatment_group != treatment_combined) %>%
  group_by(ENROLID) %>%
  summarise(
    switch_date = min(first_rx_date),
    .groups = "drop"
  )

# save again
# write_parquet(final_new_users_three_way, paste0(OUTPUT_PATH, "final_new_users_three_way_filtered.parquet"))

log_message("=== Baseline Enrollment Filters Applied Successfully ===")

# =============================================================================
# BASELINE COMORBIDITIES CALCULATION - ELIXHAUSER ONLY (FIXED)
# =============================================================================

log_message("=== Baseline Comorbidities Calculation (Elixhauser Only) ===")

# -----------------------------------------------------------------------------
# Step 1: Prepare baseline periods
# -----------------------------------------------------------------------------
log_message("Step 1: Preparing baseline periods...")

# final_new_users_three_way <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/final_new_users_three_way_filtered.parquet")

baseline_periods <- final_new_users_three_way %>%
  select(ENROLID, index_date) %>%
  mutate(
    baseline_start = index_date - BASELINE_DAYS,
    baseline_end = index_date - 1
  )

log_message(paste("Baseline periods created for", nrow(baseline_periods), "patients"))

# -----------------------------------------------------------------------------
# Step 2: Extract inpatient diagnoses
# -----------------------------------------------------------------------------
log_message("Step 2: Extracting inpatient diagnoses...")

inpatient_diagnoses <- inpatient_raw %>%
  select(ENROLID, ADMDATE, starts_with("DX")) %>%
  pivot_longer(cols = starts_with("DX"), names_to = "dx_position", values_to = "diagnosis_code") %>%
  filter(!is.na(diagnosis_code) & diagnosis_code != "") %>%
  mutate(diagnosis_date = as.Date(ADMDATE)) %>%
  mutate(ENROLID = as.character(ENROLID)) %>%
  select(ENROLID, diagnosis_date, diagnosis_code)

# write_parquet(inpatient_diagnoses, paste0(OUTPUT_PATH, "inpatient_diagnoses.parquet"))
# inpatient_diagnoses <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/inpatient_diagnoses.parquet")
log_message(paste("Inpatient diagnosis records:", nrow(inpatient_diagnoses)))

# -----------------------------------------------------------------------------
# Step 3: Extract outpatient diagnoses
# -----------------------------------------------------------------------------
log_message("Step 3: Extracting outpatient diagnoses...")

outpatient_diagnoses <- outpatient_raw %>%
  select(ENROLID, SVCDATE, DX1, DX2, DX3, DX4) %>%
  mutate(
    ENROLID = as.character(ENROLID),
    SVCDATE = as.Date(SVCDATE)
  ) %>%
  inner_join(baseline_periods, by = "ENROLID") %>%
  collect() %>%
  pivot_longer(cols = c(DX1, DX2, DX3, DX4), names_to = "dx_position", values_to = "diagnosis_code") %>%
  filter(!is.na(diagnosis_code) & diagnosis_code != "") %>%
  mutate(diagnosis_date = as.Date(SVCDATE)) %>%
  select(ENROLID, diagnosis_date, diagnosis_code)

# write_parquet(outpatient_diagnoses, paste0(OUTPUT_PATH, "outpatient_diagnoses.parquet"))
# outpatient_diagnoses <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/outpatient_diagnoses.parquet")
log_message(paste("Outpatient diagnosis records:", nrow(outpatient_diagnoses)))

# -----------------------------------------------------------------------------
# Step 4: Combine all diagnoses
# -----------------------------------------------------------------------------
log_message("Step 4: Combining all diagnoses...")

all_diagnoses <- bind_rows(inpatient_diagnoses, outpatient_diagnoses)
log_message(paste("Total diagnosis records:", nrow(all_diagnoses)))

# -----------------------------------------------------------------------------
# Step 5: Filter diagnoses within baseline period
# -----------------------------------------------------------------------------
log_message("Step 5: Filtering diagnoses within baseline period...")

baseline_diagnoses <- all_diagnoses %>%
  inner_join(baseline_periods, by = "ENROLID") %>%
  filter(diagnosis_date >= baseline_start & diagnosis_date <= baseline_end)

log_message(paste("Baseline diagnosis records:", nrow(baseline_diagnoses)))

# -----------------------------------------------------------------------------
# Step 6: Apply minimum claims filter (if specified)
# -----------------------------------------------------------------------------
if (MIN_COMORBIDITY_CLAIMS > 1) {
  log_message(paste("Step 6: Applying minimum claims filter (>= ", MIN_COMORBIDITY_CLAIMS, " claims)..."))
  
  diagnosis_counts <- baseline_diagnoses %>%
    group_by(ENROLID, diagnosis_code) %>%
    summarise(n_claims = n(), .groups = "drop") %>%
    filter(n_claims >= MIN_COMORBIDITY_CLAIMS)
  
  baseline_diagnoses <- baseline_diagnoses %>%
    semi_join(diagnosis_counts, by = c("ENROLID", "diagnosis_code"))
  
  log_message(paste("Diagnosis records after filtering:", nrow(baseline_diagnoses)))
} else {
  log_message("Step 6: Skipping minimum claims filter (threshold = 1)")
}

# -----------------------------------------------------------------------------
# Step 7: Prepare data for comorbidity package
# -----------------------------------------------------------------------------
log_message("Step 7: Preparing data for comorbidity package...")

comorbidity_data <- baseline_diagnoses %>%
  select(id = ENROLID, code = diagnosis_code) %>%
  distinct()

log_message(paste("Unique patient-diagnosis combinations:", nrow(comorbidity_data)))

# -----------------------------------------------------------------------------
# Step 8: Determine ICD version for each code
# -----------------------------------------------------------------------------
log_message("Step 8: Determining ICD version for each code...")

comorbidity_data <- comorbidity_data %>%
  mutate(icd_version = ifelse(grepl("^[A-Z]", code), "icd10", "icd9"))

icd_version_summary <- comorbidity_data %>%
  count(icd_version)
print(icd_version_summary)

# -----------------------------------------------------------------------------
# Step 9: Split data by ICD version
# -----------------------------------------------------------------------------
log_message("Step 9: Splitting data by ICD version...")

icd9_data <- comorbidity_data %>% filter(icd_version == "icd9")
icd10_data <- comorbidity_data %>% filter(icd_version == "icd10")

# -----------------------------------------------------------------------------
# Step 10: Calculate Elixhauser comorbidities (ICD-9)
# -----------------------------------------------------------------------------
elixhauser_icd9 <- NULL
if (nrow(icd9_data) > 0) {
  elixhauser_icd9 <- comorbidity(
    x = icd9_data %>% select(id, code),
    id = "id",
    code = "code",
    map = "elixhauser_icd9_quan",
    assign0 = FALSE
  )
  log_message(paste("Elixhauser ICD-9 patients:", nrow(elixhauser_icd9)))
}

# -----------------------------------------------------------------------------
# Step 11: Calculate Elixhauser comorbidities (ICD-10)
# -----------------------------------------------------------------------------
elixhauser_icd10 <- NULL
if (nrow(icd10_data) > 0) {
  elixhauser_icd10 <- comorbidity(
    x = icd10_data %>% select(id, code),
    id = "id",
    code = "code",
    map = "elixhauser_icd10_quan",
    assign0 = FALSE
  )
  log_message(paste("Elixhauser ICD-10 patients:", nrow(elixhauser_icd10)))
}

# -----------------------------------------------------------------------------
# Step 12: Combine ICD-9 + ICD-10 results
# -----------------------------------------------------------------------------
log_message("Step 12: Combining ICD-9 and ICD-10 results...")

if (!is.null(elixhauser_icd9) && !is.null(elixhauser_icd10)) {
  elixhauser_combined <- elixhauser_icd9 %>%
    rename(ENROLID = id) %>%
    full_join(elixhauser_icd10 %>% rename(ENROLID = id), by = "ENROLID")
} else if (!is.null(elixhauser_icd9)) {
  elixhauser_combined <- elixhauser_icd9 %>% rename(ENROLID = id)
} else if (!is.null(elixhauser_icd10)) {
  elixhauser_combined <- elixhauser_icd10 %>% rename(ENROLID = id)
} else {
  elixhauser_combined <- NULL
}

elixhauser_vars <- gsub("\\.x$", "", grep("\\.x$", names(elixhauser_combined), value = TRUE))

for (v in elixhauser_vars) {
  col_x <- paste0(v, ".x")
  col_y <- paste0(v, ".y")
  
  elixhauser_combined[[v]] <- pmax(coalesce(elixhauser_combined[[col_x]], 0),
                                   coalesce(elixhauser_combined[[col_y]], 0),
                                   na.rm = TRUE)
}

elixhauser_combined <- elixhauser_combined %>%
  select(ENROLID, all_of(elixhauser_vars))


# -----------------------------------------------------------------------------
# Step 13: Merge with all patients
# -----------------------------------------------------------------------------
log_message("Step 13: Merging comorbidities with all patients...")

baseline_comorbidities <- baseline_periods %>% select(ENROLID)

if (!is.null(elixhauser_combined)) {
  baseline_comorbidities <- baseline_comorbidities %>%
    left_join(elixhauser_combined, by = "ENROLID") %>%
    mutate(across(-ENROLID, ~replace_na(., 0)))
  
  log_message(paste("Elixhauser variables:", ncol(elixhauser_combined) - 1))
}

# -----------------------------------------------------------------------------
# Step 14: Baseline comorbidity summary
# -----------------------------------------------------------------------------
log_message("Step 14: Generating baseline comorbidity summary...")

if (!is.null(elixhauser_combined)) {
  comorbidity_vars <- names(elixhauser_combined)[names(elixhauser_combined) != "ENROLID"]
  
  baseline_summary <- baseline_comorbidities %>%
    select(all_of(comorbidity_vars)) %>%
    summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "comorbidity", values_to = "n_patients") %>%
    mutate(
      proportion = round(n_patients / nrow(baseline_comorbidities) * 100, 2)
    ) %>%
    arrange(desc(n_patients))
  
  log_message("=== Baseline Comorbidity Counts and Proportions ===")
  print(baseline_summary)
  
  baseline_comorbidity_summary <- baseline_summary
} else {
  log_message("No comorbidity data available to summarize.")
}

log_message(paste("Summary statistics generated for", nrow(baseline_comorbidities), "patients"))
log_message("=== Baseline Comorbidity Calculation Complete ===")

# =============================================================================
# FINAL BASELINE CHARACTERISTICS TABLE
# =============================================================================
log_message("=== Generating final baseline characteristics table ===")

# Calculating baseline healthcare utilization
baseline_outpatient <- outpatient_diagnoses %>%
  inner_join(baseline_periods, by = "ENROLID") %>%
  filter(diagnosis_date >= baseline_start & diagnosis_date <= baseline_end) %>%
  distinct(ENROLID, diagnosis_date) %>%
  count(ENROLID, name = "n_outpatient_visits")

baseline_hospital <- inpatient_diagnoses %>%
  inner_join(baseline_periods, by = "ENROLID") %>%
  filter(diagnosis_date >= baseline_start & diagnosis_date <= baseline_end) %>%
  distinct(ENROLID, diagnosis_date) %>% 
  count(ENROLID, name = "n_hospitalizations")

baseline_utilization <- baseline_periods %>%
  select(ENROLID) %>%
  left_join(baseline_outpatient, by = "ENROLID") %>%
  left_join(baseline_hospital, by = "ENROLID") %>%
  mutate(
    n_outpatient_visits = coalesce(n_outpatient_visits, 0),
    n_hospitalizations = coalesce(n_hospitalizations, 0)
  )

# Merge all baseline info
baseline_characteristics <- final_new_users_three_way %>%
  left_join(baseline_comorbidities, by = "ENROLID") %>%
  left_join(baseline_utilization, by = "ENROLID") %>%
  rename(DOBYR = birth_year_fixed) %>%
  mutate(
    # Age at index date
    age_at_index = year(index_date) - DOBYR,
    age_group = case_when(
      age_at_index < 50 ~ "<50",
      age_at_index < 65 ~ "50-64",
      TRUE ~ "65+"
    ),
    sex_factor = factor(SEX, levels = c(1,2), labels = c("Male","Female")),
    enrollment_year = year(index_date),
    treatment_factor = factor(treatment_combined,
                              levels = c("Other_FirstLine","ARB","CCB"))
  )

# write_parquet(baseline_characteristics, paste0(OUTPUT_PATH, "baseline_characteristics.parquet"))
# baseline_characteristics <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/baseline_characteristics.parquet")

log_message(paste("Baseline characteristics prepared for", nrow(baseline_characteristics), "patients"))
log_message("=== Baseline characteristics table generated successfully ===")

# =============================================================================
# OUTCOME DEFINITION — Parkinson’s Disease (PD)
# =============================================================================

log_message("=== Outcome Definition: Parkinson's Disease ===")

# -----------------------------------------------------------------------------
# Step 1. Define ICD codes for PD
# -----------------------------------------------------------------------------
PD_ICD <- c("3320", "G20")      # ICD-9-CM / ICD-10-CM codes; sensitivity analysis: PD_ICD <- c("3320", "G20", "G20A1", "G20A2", "G20B1", "G20B2", "G20C")
MIN_OUTPATIENT_COUNT <- 1       # Require ≥1 PD outpatient visits to reduce false positives
study_end_date <- as.Date("2024-09-30")
# MIN_OUTPATIENT_INTERVAL <- 30   # Require visits separated by ≥30 days

# -----------------------------------------------------------------------------
# Step 2. Process INPATIENT PD diagnoses
# -----------------------------------------------------------------------------
inpatient_PD <- inpatient_raw %>%
  select(ENROLID, ADMDATE, DISDATE, starts_with("DX")) %>%
  mutate(
    ENROLID = as.character(ENROLID),
    ADMDATE = as.Date(ADMDATE)
  ) %>%
  pivot_longer(cols = starts_with("DX"),
               names_to = "DX_type", values_to = "Diagnosis_Code") %>%
  filter(Diagnosis_Code %in% PD_ICD) %>%
  group_by(ENROLID) %>%
  summarise(
    inpatient_count = n(),
    PD_date_inpatient = min(ADMDATE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(PD_status_inpatient = 1)

# -----------------------------------------------------------------------------
# Step 3. Process OUTPATIENT PD diagnoses
# -----------------------------------------------------------------------------
outpatient_raw <- open_dataset(DATA_FILES$outpatient)
outpatient_PD <- outpatient_raw %>%
  select(ENROLID, SVCDATE, DX1, DX2, DX3, DX4) %>%
  filter(DX1 %in% PD_ICD | DX2 %in% PD_ICD | DX3 %in% PD_ICD | DX4 %in% PD_ICD) %>%
  mutate(
    ENROLID = as.character(ENROLID),
    SVCDATE = as.Date(SVCDATE)
  ) %>%
  collect() %>%
  arrange(ENROLID, SVCDATE) %>%
  group_by(ENROLID) %>%
  summarise(
    outpatient_count = n(),
    PD_date_outpatient = min(SVCDATE, na.rm = TRUE),
    PD_date_last = max(SVCDATE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PD_status_outpatient = ifelse(
      outpatient_count >= MIN_OUTPATIENT_COUNT,
      1, 0)
  ) # sensitivity analysis & (as.numeric(PD_date_last - PD_date_outpatient) >= MIN_OUTPATIENT_INTERVAL)

# -----------------------------------------------------------------------------
# Step 4. Combine inpatient and outpatient diagnoses
# -----------------------------------------------------------------------------
PD_outcomes <- full_join(inpatient_PD, outpatient_PD, by = "ENROLID") %>%
  mutate(
    PD_status = ifelse(
      coalesce(PD_status_inpatient, 0) == 1 |
        coalesce(PD_status_outpatient, 0) == 1,
      1, 0
    ),
    PD_date = pmin(PD_date_inpatient, PD_date_outpatient, na.rm = TRUE),
    PD_count_total = coalesce(inpatient_count, 0) + coalesce(outpatient_count, 0)
  )

log_message(paste("Total PD cases identified:", sum(PD_outcomes$PD_status, na.rm = TRUE)))

# -----------------------------------------------------------------------------
# Step 5. Link PD outcomes with baseline cohort (index date + 90 days)
# -----------------------------------------------------------------------------

outcome_ready_dataset <- baseline_characteristics %>%
  left_join(
    PD_outcomes %>% select(ENROLID, PD_status, PD_date),
    by = "ENROLID"
  ) %>%
  left_join(
    switch_dates,
    by = "ENROLID"
  ) %>%
  mutate(
    # -------------------------------------------------
    # Define risk start (induction / lag period)
    # -------------------------------------------------
    risk_start_date = index_date + days(EXPOSURE_WINDOW_DAYS),
    
    PD_status = coalesce(PD_status, 0)
  ) %>%
  # -----------------------------------------------
# Exclude prevalent PD (before risk window)
# -------------------------------------------------
filter(
  is.na(PD_date) | PD_date >= risk_start_date
) %>%
  mutate(
    # -------------------------------------------------
    # Define censor date (treatment deviation / admin)
    # -------------------------------------------------
    censor_date = pmin(
      switch_date,
      last_enroll,
      study_end_date,
      na.rm = TRUE
    ),
    
    # -------------------------------------------------
    # Define event date (PD only if after risk start)
    # -------------------------------------------------
    event_date = ifelse(
      PD_status == 1 & !is.na(PD_date) & PD_date >= risk_start_date,
      PD_date,
      as.Date(NA)
    ),
    event_date = as.Date(event_date),
    
    # -------------------------------------------------
    # Final follow-up end date
    # -------------------------------------------------
    end_date = pmin(
      event_date,
      censor_date,
      na.rm = TRUE
    ),
    
    # -------------------------------------------------
    # Event indicator: PD before censor
    # -------------------------------------------------
    event_PD = ifelse(
      !is.na(event_date) & event_date <= censor_date,
      1, 0
    ),
    
    # -------------------------------------------------
    # Follow-up time (days)
    # -------------------------------------------------
    followup_time = as.numeric(
      end_date - risk_start_date
    )
  ) %>%
  filter(
    !is.na(followup_time) & followup_time > 0
  )

###############################################################
## REVIEWER QUICK 5
## PD CODE VS ANTIPD MEDICATION TIMING
###############################################################

log_message(
  "=== Reviewer: PD diagnosis vs AntiPD timing ==="
)


PD_cases_validation <- outcome_ready_dataset %>%
  filter(
    event_PD == 1
  ) %>%
  select(
    ENROLID,
    PD_date
  )


PD_case_ids <- PD_cases_validation %>%
  select(
    ENROLID
  )


###############################################################
## First broad AntiPD medication
###############################################################
AntiPD_drugs <- AntiPD_drugs %>%
  mutate(GENERID = as.character(GENERID))

PD_first_AntiPD <- medication_raw %>%
  select(
    ENROLID,
    GENERID,
    SVCDATE
  ) %>%
  semi_join(
    PD_case_ids,
    by = "ENROLID"
  ) %>%
  inner_join(
    AntiPD_drugs %>%
      select(
        GENERID
      ),
    by = "GENERID"
  ) %>%
  group_by(
    ENROLID
  ) %>%
  summarise(
    first_AntiPD_date =
      min(
        SVCDATE
      ),
    .groups = "drop"
  ) %>%
  collect() %>%
  mutate(
    ENROLID =
      as.character(ENROLID),
    first_AntiPD_date =
      as.Date(first_AntiPD_date)
  )


PD_timing_validation <- PD_cases_validation %>%
  left_join(
    PD_first_AntiPD,
    by = "ENROLID"
  ) %>%
  mutate(
    days_AntiPD_minus_PD =
      as.numeric(
        first_AntiPD_date -
          PD_date
      ),
    
    timing_group =
      case_when(
        is.na(first_AntiPD_date) ~
          "No recorded AntiPD medication",
        first_AntiPD_date < PD_date ~
          "Before first PD code",
        first_AntiPD_date == PD_date ~
          "Same day",
        first_AntiPD_date > PD_date ~
          "After first PD code"
      )
  )


PD_AntiPD_timing_summary <- PD_timing_validation %>%
  count(
    timing_group,
    name = "N"
  ) %>%
  mutate(
    Percent =
      round(
        100 *
          N /
          sum(N),
        1
      )
  )


PD_AntiPD_lead_time <- PD_timing_validation %>%
  filter(
    timing_group ==
      "Before first PD code"
  ) %>%
  summarise(
    N_before = n(),
    
    median_days_before =
      median(
        -days_AntiPD_minus_PD,
        na.rm = TRUE
      ),
    
    IQR_25 =
      quantile(
        -days_AntiPD_minus_PD,
        0.25,
        na.rm = TRUE
      ),
    
    IQR_75 =
      quantile(
        -days_AntiPD_minus_PD,
        0.75,
        na.rm = TRUE
      ),
    
    Percent_gt_30_days =
      100 *
      mean(
        -days_AntiPD_minus_PD > 30
      ),
    
    Percent_gt_90_days =
      100 *
      mean(
        -days_AntiPD_minus_PD > 90
      ),
    
    Percent_gt_180_days =
      100 *
      mean(
        -days_AntiPD_minus_PD > 180
      ),
    
    Percent_gt_365_days =
      100 *
      mean(
        -days_AntiPD_minus_PD > 365
      ),
    
    Percent_gt_730_days =
      100 *
      mean(
        -days_AntiPD_minus_PD > 730
      )
  )


print(
  PD_AntiPD_timing_summary
)

print(
  PD_AntiPD_lead_time
)


write.csv(
  PD_AntiPD_timing_summary,
  paste0(
    OUTPUT_PATH,
    "reviewer_PD_AntiPD_timing_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  PD_AntiPD_lead_time,
  paste0(
    OUTPUT_PATH,
    "reviewer_PD_AntiPD_lead_time.csv"
  ),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# Step 6. QC summary table
# -----------------------------------------------------------------------------
summary_table <- outcome_ready_dataset %>%
  summarise(
    total_n = n(),
    PD_cases = sum(event_PD == 1),
    mean_followup_years = mean(followup_time, na.rm = TRUE) / 365.25,
    median_followup_years = median(followup_time, na.rm = TRUE) / 365.25
  )

log_message("Summary of PD outcome dataset:")
print(summary_table)

final_treatment_summary_update <- outcome_ready_dataset %>%
  group_by(treatment_combined) %>%
  summarise(n = n(), .groups = "drop")

print(final_treatment_summary_update)
# write_parquet(outcome_ready_dataset, paste0(OUTPUT_PATH, "outcome_ready_dataset.parquet"))
# outcome_ready_dataset <- read_parquet("$HOME/TTE/HTN_final/MDCR_results/outcome_ready_dataset.parquet")

# =============================================================================
# IPTW ESTIMATION AND COX PROPORTIONAL HAZARDS MODEL
# Target Trial Emulation: ARB vs CCB vs Other_FirstLine for PD Risk
# =============================================================================

# =============================================================================
# STEP 1: DATA PREPARATION AND QUALITY CHECKS
# =============================================================================

log_message("=== IPTW and Cox Analysis Phase ===")

# -----------------------------------------------------------------------------
# OPTIONAL STEP: Restrict follow-up time to n_years (sensitivity analysis -- 2,5,10,total years)
# -----------------------------------------------------------------------------
MAX_FOLLOWUP_YEARS <- 10
MAX_FOLLOWUP_DAYS <- MAX_FOLLOWUP_YEARS * 365.25
comparison_group_1 <- c("Other_FirstLine", "ARB")
comparison_group_2 <- c("Other_FirstLine", "CCB")
comparison_group_all <- c("Other_FirstLine", "ARB", "CCB")

outcome_ready_dataset_update <- outcome_ready_dataset %>%
  mutate(
    # Truncate follow-up at MAX_FOLLOWUP_YEARS years
    followup_time = pmin(followup_time, MAX_FOLLOWUP_DAYS),
    # If event occurred after n years, treat as censored
    event_PD = ifelse(event_PD == 1 & PD_date <= risk_start_date + MAX_FOLLOWUP_DAYS, 1, 0)
  )

# Apply additional filters
analysis_cohort <- outcome_ready_dataset_update %>%
  filter(
    # Valid follow-up time
    followup_time > 0,
    # Exclude Combo group (or analyze separately)
    treatment_combined %in% comparison_group_1
  ) %>%
  mutate(
    treatment_combined = factor(treatment_combined, 
                                levels = comparison_group_1)
  )

log_message(paste("Analysis cohort after filters:", nrow(analysis_cohort)))

# Check for missing data
missing_check <- analysis_cohort %>%
  summarise(across(everything(), ~sum(is.na(.))))

log_message("Missing data check completed")
print(colSums(missing_check > 0))

# =============================================================================
# STEP 2: DEFINE COVARIATES FOR PS MODEL
# =============================================================================

# Demographic variables
demo_vars <- c("age_at_index", "sex_factor", "enrollment_year")

# Healthcare utilization
utilization_vars <- c("n_outpatient_visits", "n_hospitalizations")

# Elixhauser comorbidities (automatically get all columns starting with specific patterns)
elixhauser_vars <- names(analysis_cohort)[names(analysis_cohort) %in% 
                                            c("chf", "carit", "valv", "pvd", "ond", 
                                              "cpd", "diabunc", "diabc", "rf", "depre")]

# Combine all covariates
covariate_names <- c(demo_vars, utilization_vars, elixhauser_vars)

log_message(paste("Total covariates for PS model:", length(covariate_names)))

# Create formula
ps_formula <- as.formula(paste("treatment_combined ~", 
                               paste(covariate_names, collapse = " + ")))

# =============================================================================
# STEP 3: ESTIMATE PROPENSITY SCORES AND IPTW
# =============================================================================

log_message("=== Estimating propensity scores ===")

# Fit propensity score model using multinomial logistic regression
ps_weights <- weightit(
  ps_formula,
  data = analysis_cohort,
  method = "ps",           # Propensity score weighting
  estimand = "ATE",        # Average Treatment Effect
  stabilize = TRUE         # Stabilized weights
)

log_message("Propensity score model fitted")

# Add weights to dataset
analysis_cohort$iptw <- ps_weights$weights

# Check weight distribution
weight_summary <- analysis_cohort %>%
  group_by(treatment_combined) %>%
  summarise(
    n = n(),
    mean_weight = mean(iptw),
    median_weight = median(iptw),
    min_weight = min(iptw),
    max_weight = max(iptw),
    sd_weight = sd(iptw),
    se_weight = sd(iptw) / sqrt(n())
  )

log_message("=== Weight Distribution by Treatment Group ===")
print(weight_summary)

###############################################################
## REVIEWER QUICK PLOT
## PROPENSITY SCORE OVERLAP
###############################################################

analysis_cohort$ps_report <- predict(
  ps_reporting_model,
  type = "response"
)


ps_overlap_plot <- ggplot(
  analysis_cohort,
  aes(
    x = ps_report,
    fill = treatment_combined
  )
) +
  geom_density(
    alpha = 0.35
  ) +
  labs(
    x = "Propensity score",
    y = "Density",
    fill = "Treatment group",
    title = "Propensity Score Overlap"
  ) +
  theme_classic()


ggsave(
  filename = paste0(
    PLOT_PATH,
    "PS_overlap_",
    paste(levels(analysis_cohort$treatment_combined), collapse = "_vs_"),
    ".pdf"
  ),
  plot = ps_overlap_plot,
  width = 8,
  height = 6
)

###############################################################
## REVIEWER QUICK 1
## FULL PROPENSITY-SCORE MODEL REPORT
###############################################################

log_message("=== Reviewer: Full PS model coefficients ===")

ps_reporting_model <- glm(
  ps_formula,
  data = analysis_cohort,
  family = binomial(link = "logit")
)

ps_coefficients <- broom::tidy(
  ps_reporting_model,
  conf.int = TRUE
) %>%
  mutate(
    OR = exp(estimate),
    OR_lower = exp(conf.low),
    OR_upper = exp(conf.high)
  )

print(
  ps_coefficients,
  n = nrow(ps_coefficients)
)

write.csv(
  ps_coefficients,
  paste0(
    OUTPUT_PATH,
    "SI_PS_model_coefficients_",
    paste(levels(analysis_cohort$treatment_combined), collapse = "_vs_"),
    ".csv"
  ),
  row.names = FALSE
)

log_message("PS model coefficients saved.")

# Trim extreme weights (optional but recommended)
weight_trim <- quantile(analysis_cohort$iptw, c(0.01, 0.99))
analysis_cohort <- analysis_cohort %>%
  mutate(iptw_trimmed = pmin(pmax(iptw, weight_trim[1]), weight_trim[2]))

log_message(paste("Weights trimmed at 1st and 99th percentiles:",
                  round(weight_trim[1], 3), "to", round(weight_trim[2], 3)))

weight_summary_trimmed <- analysis_cohort %>%
  group_by(treatment_combined) %>%
  summarise(
    n = n(),
    mean_weight = mean(iptw_trimmed),
    median_weight = median(iptw_trimmed),
    min_weight = min(iptw_trimmed),
    max_weight = max(iptw_trimmed),
    sd_weight = sd(iptw_trimmed),
    se_weight = sd(iptw_trimmed) / sqrt(n())
  )
log_message("=== Trimmed Weight Distribution by Treatment Group ===")
print(weight_summary_trimmed)

# =============================================================================
# STEP 4: ASSESS COVARIATE BALANCE
# =============================================================================

# log_message("=== Assessing covariate balance ===")

# Balance assessment before and after weighting
# bal_tab <- bal.tab(ps_weights, 
#                    un = TRUE,           # Include unweighted balance
#                    thresholds = c(m = 0.1))  # SMD threshold

# print(bal_tab)

# Create love plot
# pdf(paste0(PLOT_PATH, "balance_plot_CCB_vs_other.pdf"), width = 10, height = 8)
# love.plot(ps_weights, 
#           threshold = 0.1,
#           abs = TRUE,
#           var.order = "unadjusted",
#           title = "Covariate Balance Before and After IPTW")
# dev.off()

# log_message("Balance plot saved")

###############################################################
## REVIEWER QUICK 2A
## BALANCE USING THE EXACT TRIMMED WEIGHTS USED IN COX
###############################################################

log_message("=== Reviewer: Covariate balance using trimmed IPTW ===")

balance_formula <- as.formula(
  paste(
    "treatment_combined ~",
    paste(covariate_names, collapse = " + ")
  )
)

bal_tab_trimmed <- cobalt::bal.tab(
  balance_formula,
  data = analysis_cohort,
  weights = analysis_cohort$iptw_trimmed,
  un = TRUE,
  estimand = "ATE",
  binary = "std",
  thresholds = c(m = 0.1)
)

print(
  bal_tab_trimmed
)

balance_table_trimmed <- as.data.frame(
  bal_tab_trimmed$Balance
)

balance_table_trimmed$Variable <- rownames(
  balance_table_trimmed
)

rownames(
  balance_table_trimmed
) <- NULL

write.csv(
  balance_table_trimmed,
  paste0(
    OUTPUT_PATH,
    "SI_balance_trimmed_IPTW_",
    paste(levels(analysis_cohort$treatment_combined), collapse = "_vs_"),
    ".csv"
  ),
  row.names = FALSE
)

###############################################################
## REVIEWER QUICK 2B
## LOVE PLOT USING TRIMMED IPTW
###############################################################

pdf(
  paste0(
    PLOT_PATH,
    "love_plot_trimmed_IPTW_",
    paste(levels(analysis_cohort$treatment_combined), collapse = "_vs_"),
    ".pdf"
  ),
  width = 9,
  height = 7
)

cobalt::love.plot(
  bal_tab_trimmed,
  abs = TRUE,
  threshold = 0.1,
  binary = "std",
  var.order = "unadjusted",
  title = "Covariate Balance Before and After Trimmed IPTW"
)

dev.off()

###############################################################
## REVIEWER QUICK 2C
## PRE- AND POST-WEIGHTED COHORT CHARACTERISTICS
###############################################################

weighted_mean_safe <- function(x, w) {
  
  ok <- !is.na(x) & !is.na(w)
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  weighted.mean(
    x[ok],
    w[ok]
  )
}


weighted_sd_safe <- function(x, w) {
  
  ok <- !is.na(x) & !is.na(w)
  
  if (sum(ok) <= 1) {
    return(NA_real_)
  }
  
  x <- x[ok]
  w <- w[ok]
  
  mu <- weighted.mean(
    x,
    w
  )
  
  sqrt(
    sum(
      w * (x - mu)^2
    ) /
      sum(w)
  )
}


analysis_cohort_balance <- analysis_cohort %>%
  mutate(
    female =
      as.integer(
        sex_factor == "Female"
      )
  )


continuous_balance_vars <- c(
  "age_at_index",
  "enrollment_year",
  "n_outpatient_visits",
  "n_hospitalizations"
)


binary_balance_vars <- c(
  "female",
  elixhauser_vars
)


make_continuous_balance_row <- function(var) {
  
  groups <- levels(
    analysis_cohort_balance$treatment_combined
  )
  
  ref <- groups[1]
  exp <- groups[2]
  
  dat_ref <- analysis_cohort_balance %>%
    filter(
      treatment_combined == ref
    )
  
  dat_exp <- analysis_cohort_balance %>%
    filter(
      treatment_combined == exp
    )
  
  tibble(
    Variable = var,
    
    Comparator_unweighted =
      sprintf(
        "%.2f (%.2f)",
        mean(
          dat_ref[[var]],
          na.rm = TRUE
        ),
        sd(
          dat_ref[[var]],
          na.rm = TRUE
        )
      ),
    
    Exposure_unweighted =
      sprintf(
        "%.2f (%.2f)",
        mean(
          dat_exp[[var]],
          na.rm = TRUE
        ),
        sd(
          dat_exp[[var]],
          na.rm = TRUE
        )
      ),
    
    Comparator_weighted =
      sprintf(
        "%.2f (%.2f)",
        weighted_mean_safe(
          dat_ref[[var]],
          dat_ref$iptw_trimmed
        ),
        weighted_sd_safe(
          dat_ref[[var]],
          dat_ref$iptw_trimmed
        )
      ),
    
    Exposure_weighted =
      sprintf(
        "%.2f (%.2f)",
        weighted_mean_safe(
          dat_exp[[var]],
          dat_exp$iptw_trimmed
        ),
        weighted_sd_safe(
          dat_exp[[var]],
          dat_exp$iptw_trimmed
        )
      )
  )
}


make_binary_balance_row <- function(var) {
  
  groups <- levels(
    analysis_cohort_balance$treatment_combined
  )
  
  ref <- groups[1]
  exp <- groups[2]
  
  dat_ref <- analysis_cohort_balance %>%
    filter(
      treatment_combined == ref
    )
  
  dat_exp <- analysis_cohort_balance %>%
    filter(
      treatment_combined == exp
    )
  
  tibble(
    Variable = var,
    
    Comparator_unweighted =
      sprintf(
        "%.1f%%",
        100 *
          mean(
            dat_ref[[var]],
            na.rm = TRUE
          )
      ),
    
    Exposure_unweighted =
      sprintf(
        "%.1f%%",
        100 *
          mean(
            dat_exp[[var]],
            na.rm = TRUE
          )
      ),
    
    Comparator_weighted =
      sprintf(
        "%.1f%%",
        100 *
          weighted_mean_safe(
            dat_ref[[var]],
            dat_ref$iptw_trimmed
          )
      ),
    
    Exposure_weighted =
      sprintf(
        "%.1f%%",
        100 *
          weighted_mean_safe(
            dat_exp[[var]],
            dat_exp$iptw_trimmed
          )
      )
  )
}


weighted_characteristics_table <- bind_rows(
  
  lapply(
    continuous_balance_vars,
    make_continuous_balance_row
  ) %>%
    bind_rows(),
  
  lapply(
    binary_balance_vars,
    make_binary_balance_row
  ) %>%
    bind_rows()
)


###############################################################
## Add SMD before and after IPTW
###############################################################

balance_for_merge <- balance_table_trimmed %>%
  select(
    Variable,
    any_of(
      c(
        "Diff.Un",
        "Diff.Adj"
      )
    )
  ) %>%
  mutate(
    Variable = case_when(
      Variable == "sex_factor_Female" ~ "female",
      TRUE ~ Variable
    )
  )

weighted_characteristics_table <- weighted_characteristics_table %>%
  left_join(
    balance_for_merge,
    by = "Variable"
  )


print(
  weighted_characteristics_table,
  n = nrow(weighted_characteristics_table)
)


write.csv(
  weighted_characteristics_table,
  paste0(
    OUTPUT_PATH,
    "SI_pre_post_weighted_characteristics_",
    paste(levels(analysis_cohort$treatment_combined), collapse = "_vs_"),
    ".csv"
  ),
  row.names = FALSE
)

# =============================================================================
# STEP 5: WEIGHTED COX PROPORTIONAL HAZARDS MODEL
# =============================================================================

log_message("=== Fitting weighted Cox model ===")

# Create survey design object
weighted_design <- svydesign(
  ids = ~1,
  weights = ~iptw_trimmed,
  data = analysis_cohort
)

# Fit Cox model
cox_model <- svycoxph(
  Surv(followup_time / 365.25, event_PD) ~ treatment_combined,
  design = weighted_design
)

# Extract results
cox_summary <- summary(cox_model)
cox_results <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

log_message("=== Weighted Cox Model Results ===")
print(cox_results)

# Calculate incidence rates per 1000 person-years
incidence_rates <- analysis_cohort %>%
  group_by(treatment_combined) %>%
  summarise(
    n = n(),
    events = sum(event_PD),
    person_years = sum(followup_time) / 365.25,
    ir_per_1000py = (events / person_years) * 1000,
    ir_95ci_lower = (qchisq(0.025, 2*events) / (2*person_years)) * 1000,
    ir_95ci_upper = (qchisq(0.975, 2*events + 2) / (2*person_years)) * 1000
  )

print(incidence_rates)

# =============================================================================
# STEP 6: SURVIVAL CURVES
# =============================================================================

log_message("=== Generating survival curves ===")

# Fit weighted KM model (time in years or months = 30.4375 days)
km_fit <- survfit(
  Surv(followup_time / 365.25, event_PD) ~ treatment_combined,
  data = analysis_cohort,
  weights = iptw_trimmed
)

# Plot Kaplan-Meier curves (follow-up up to 10 years - draw)
km_plot <- ggsurvplot(
  km_fit,
  data = analysis_cohort,
  fun = "event",
  censor.shape = FALSE,
  risk.table = TRUE,
  conf.int = TRUE,
  surv.scale = "default",
  xlim = c(0, 10),          # 10 years
  break.x.by = 1,          # tick every year
  ylim = c(0, 0.04),
  xlab = "Follow-up, years",
  ylab = "Adjusted Cumulative Incidence of Parkinson's Disease",
  title = "Weighted Cumulative Incidence Curves by Treatment Group",
  legend.title = "Treatment Group",
  legend.labs = c("Other First-line", "ARB"),
  palette = c("#1f87be", "#cc5650"),
  risk.table.height = 0.25,
  risk.table.fontsize = 4,
  risk.table.y.text.col = TRUE,
  risk.table.y.text = FALSE
)

keep_layers <- c("GeomStep", "GeomConfint")
km_plot$plot$layers <- km_plot$plot$layers[
  sapply(km_plot$plot$layers, function(x) class(x$geom)[1]) %in% keep_layers
]

pdf(paste0(PLOT_PATH, "km_curve_allyears_CCB_10years.pdf"), width = 10, height = 8)
print(km_plot)
dev.off()

log_message("K–M survival curve (all-year follow-up) saved.")

###############################################################
## REVIEWER: ADJUSTED ABSOLUTE PD RISKS
## IPTW + 1st/99th percentile trimmed weights
###############################################################

risk_times <- c(
  1,
  3,
  5,
  10
)

km_summary_adjusted <- summary(
  km_fit,
  times = risk_times,
  extend = TRUE
)

adjusted_risk_long <- tibble(
  
  time_years =
    km_summary_adjusted$time,
  
  treatment_group =
    sub(
      "treatment_combined=",
      "",
      km_summary_adjusted$strata
    ),
  
  n_at_risk =
    km_summary_adjusted$n.risk,
  
  adjusted_survival =
    km_summary_adjusted$surv,
  
  adjusted_cumulative_PD_risk =
    1 -
    km_summary_adjusted$surv
)

print(
  adjusted_risk_long,
  n = nrow(adjusted_risk_long)
)

adjusted_risk_wide <- adjusted_risk_long %>%
  select(
    time_years,
    treatment_group,
    adjusted_cumulative_PD_risk
  ) %>%
  pivot_wider(
    names_from =
      treatment_group,
    values_from =
      adjusted_cumulative_PD_risk
  )

adjusted_risk_wide <- adjusted_risk_wide %>%
  mutate(
    
    ###########################################################
    ## Absolute risk difference
    ###########################################################
    
    ARD =
      ARB -
      Other_FirstLine,
    
    ###########################################################
    ## Percentage-point scale
    ###########################################################
    
    ARD_percent =
      100 *
      ARD,
    
    ###########################################################
    ## Per 1,000 persons
    ###########################################################
    
    ARD_per_1000 =
      1000 *
      ARD,
    
    ###########################################################
    ## Display risk as %
    ###########################################################
    
    Other_FirstLine_percent =
      100 *
      Other_FirstLine,
    
    ARB_percent =
      100 *
      ARB
  )

print(
  adjusted_risk_wide,
  n = nrow(adjusted_risk_wide)
)

###############################################################
## OBSERVED CUMULATIVE PD EVENT COUNTS
###############################################################

observed_PD_counts <- expand_grid(
  treatment_group =
    levels(analysis_cohort$treatment_combined),
  time_years =
    risk_times
) %>%
  rowwise() %>%
  mutate(
    
    baseline_N =
      sum(
        analysis_cohort$treatment_combined ==
          treatment_group,
        na.rm = TRUE
      ),
    
    observed_cumulative_PD_events =
      sum(
        analysis_cohort$treatment_combined ==
          treatment_group &
          analysis_cohort$event_PD == 1 &
          !is.na(analysis_cohort$PD_date) &
          analysis_cohort$PD_date <=
          analysis_cohort$risk_start_date +
          days(round(time_years * 365.25)),
        na.rm = TRUE
      ),
    
    observed_event_percent =
      100 *
      observed_cumulative_PD_events /
      baseline_N
  ) %>%
  ungroup()

print(
  observed_PD_counts,
  n = nrow(observed_PD_counts)
)

# =============================================================================
# STEP 7: SUBGROUP ANALYSES
# =============================================================================

log_message("=== Subgroup Analyses ===")

### Age subgroups
## Age_at_index: < 50
# treatment_combined     n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>              <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine      169      0        855.              0             0          4.31
# 2 ARB                   26      0         82.4             0             0         44.8 

# treatment_combined     n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>              <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine      169      0         855.             0             0          4.31
# 2 CCB                   29      0         132.             0             0         27.9 

## Age_at_index: 50-64
# treatment_combined     n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>              <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine     4426     41       21198.          1.93         1.39           2.62
# 2 ARB                  523      5        1955.          2.56         0.830          5.97

# treatment_combined     n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>              <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine     4426     41       21198.         1.93        1.39             2.62
# 2 CCB                  747      1        2833.         0.353       0.00894          1.97

# [ARB: aHR = 1.55, 95%CI (0.583, 4.14), p = 0.0378] [CCB: aHR = 0.164, 95%CI (0.0225, 1.19), p = 0.0738]

## Age_at_index: 65+
# treatment_combined      n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>               <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine    202184   2281      691209.          3.30          3.17          3.44
# 2 ARB                 25416    148       70362.          2.10          1.78          2.47

# treatment_combined      n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>               <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine    202184   2281      691209.          3.30          3.17          3.44
# 2 CCB                 36097    290       96806.          3.00          2.66          3.36

# [ARB: aHR = 0.691, 95%CI (0.580, 0.823), p = 0.0000355] [CCB: aHR = 0.893, 95%CI (0.787, 1.01), p = 0.0805]

### Sex subgroups
## Male (SEX == 1) 
# treatment_combined     n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>              <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine    99357   1465      353956.          4.14          3.93          4.36
# 2 ARB                11735     93       33005.          2.82          2.27          3.45

# treatment_combined     n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>              <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine    99357   1465      353956.          4.14          3.93          4.36
# 2 CCB                16151    158       44393.          3.56          3.03          4.16

# [ARB: aHR = 0.749, 95%CI (0.600, 0.936), p = 0.0109] [CCB: aHR = 0.873, 95%CI (0.737, 1.04), p = 0.119]

## Female (SEX == 2)
# treatment_combined      n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>               <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine    107422    857      359306.          2.39          2.23          2.55
# 2 ARB                 14230     60       39395.          1.52          1.16          1.96

# treatment_combined      n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>               <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine    107422    857      359306.          2.39          2.23          2.55
# 2 CCB                 20722    133       55379.          2.40          2.01          2.85

# [ARB: aHR = 0.653, 95%CI (0.497, 0.859), p = 0.00231] [CCB: aHR = 0.919, 95%CI (0.761, 1.11), p = 0.382]

### CCB: non-DHP vs DHP
index_drug_users_combined <- read_parquet(
  paste0(OUTPUT_PATH, "index_drug_users_combined.parquet")
)

ccb_index_drug_lazy <- medication_raw %>%
  select(ENROLID, GENERID, SVCDATE) %>%
  inner_join(
    index_drug_users_combined %>%
      filter(treatment_combined == "CCB") %>%
      select(ENROLID, index_date),
    by = "ENROLID"
  ) %>%
  filter(
    as.Date(SVCDATE) == index_date
  )

ccb_index_drug_lazy <- ccb_index_drug_lazy %>%
  left_join(
    REDBOOK %>% select(GENERID, GENNME),
    by = "GENERID"
  )

ccb_index_subtype_lazy <- ccb_index_drug_lazy %>%
  mutate(
    CCB_subtype = ifelse(
      grepl(
        "amlodipine|felodipine|isradipine|nicardipine|nifedipine|nimodipine|nisoldipine|nitrendipine|lacidipine|nilvadipine|manidipine|barnidipine|lercanidipine|cilnidipine|benidipine|clevidipine|levamlodipine",
        GENNME,
        ignore.case = TRUE
      ),
      "CCB_DHP",
      "CCB_nonDHP"
    )
  ) %>%
  select(ENROLID, CCB_subtype) %>%
  distinct()

ccb_index_subtype <- ccb_index_subtype_lazy %>%
  collect() %>%
  group_by(ENROLID) %>%
  summarise(
    CCB_subtype = first(CCB_subtype),
    .groups = "drop"
  )

analysis_cohort_CCBsplit <- analysis_cohort %>%
  left_join(ccb_index_subtype, by = "ENROLID") %>%
  mutate(
    treatment_final = case_when(
      treatment_combined == "CCB" ~ CCB_subtype,
      TRUE ~ treatment_combined
    )
  ) %>%
  filter(
    treatment_final %in% c(
      "Other_FirstLine", "CCB_DHP", "CCB_nonDHP"
    )
  )

## CCB_DHP
analysis_cohort_CCBsplit_CCB_DHP <- analysis_cohort_CCBsplit %>%
  filter(
    treatment_final %in% c(
      "Other_FirstLine", "CCB_DHP"
    )
  ) %>%
  mutate(
    treatment_final = factor(
      treatment_final,
      levels = c(
        "Other_FirstLine",
        "CCB_DHP"
      )
    )
  )

# treatment_final      n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>            <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine 206779   2322      713262.          3.26          3.12          3.39
# 2 CCB_DHP          20645    167       57386.          2.91          2.49          3.39
# [CCB_DHP: aHR = 0.939, 95%CI (0.796, 1.11), p = 0.451]

## CCB_nonDHP
analysis_cohort_CCBsplit_CCB_nonDHP <- analysis_cohort_CCBsplit %>%
  filter(
    treatment_final %in% c(
      "Other_FirstLine", "CCB_nonDHP"
    )
  ) %>%
  mutate(
    treatment_final = factor(
      treatment_final,
      levels = c(
        "Other_FirstLine",
        "CCB_nonDHP"
      )
    )
  )

# treatment_final      n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>            <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine 206779   2322      713262.          3.26          3.12          3.39
# 2 CCB_nonDHP       16228    124       42385.          2.93          2.43          3.49
# [CCB_nonDHP: aHR = 0.831, 95%CI (0.684, 1.01), p = 0.0607]

# =============================================================================
# STEP 8: SENSITIVITY ANALYSES
# =============================================================================

log_message("=== Sensitivity Analysis: Unweighted Cox Model ===")

# Unweighted Cox model for comparison [ARB: aHR = 0.760, 95%CI (0.644, 0.896), p = 1.08e-3] [CCB: aHR = 0.898, 95%CI (0.795, 1.02), p = 0.0861]
unweighted_cox <- coxph(
  Surv(followup_time / 365.25, event_PD) ~ treatment_combined + 
    age_at_index + sex_factor + enrollment_year + 
    n_outpatient_visits + n_hospitalizations,
  data = analysis_cohort
)
unweighted_results <- tidy(unweighted_cox, exponentiate = TRUE, conf.int = TRUE)
log_message("=== Unweighted Cox Model Results ===")
print(unweighted_results)
# all years [ARB: aHR = 0.695, 95%CI (0.585, 0.825), p = 0.0000314] [CCB: aHR = 0.881, 95%CI (0.777, 0.998), p = 0.0459]
# 5 years [ARB: aHR = 0.628, 95%CI (0.515, 0.766), p = 0.00000438] [CCB: aHR = 0.889, 95%CI (0.774, 1.020), p = 0.0939]
# for ARB, add a unbalanced covariates - n_hospitalizations, [ARB: aHR = 0.727, 95%CI (0.611, 0.864), p = 0.000295] n_hospitalizations 1.35 (1.27, 1.43) p = 1.01e-22
# risk_start_date = index_date + days(EXPOSURE_WINDOW_DAYS)-- EXPOSURE_WINDOW_DAYS = 0 [ARB: aHR = 0.716, 95%CI (0.608, 0.843), p = 0.0000627] [CCB: aHR = 0.874, 95%CI (0.775, 0.986), p = 0.0285]
# MIN_OUTPATIENT_COUNT <- 2 [ARB: aHR = 0.659, 95%CI (0.546, 0.797), p = 0.0000163] [CCB: aHR = 0.896, 95%CI (0.783, 1.03), p = 0.0689]
# To mimic ITT, remove drug_switch in censor part [ARB: aHR = 0.844, 95%CI (0.741, 0.960), p = 0.00990] [CCB: aHR = 1.01, 95%CI (0.915, 1.11), p = 0.872]
# PSM [ARB: aHR = 0.752, 95%CI (0.616, 0.918), p = 0.00509] [CCB: aHR = 0.853, 95%CI (0.734, 0.991), p = 0.0374]
library(MatchIt)
psm_model <- matchit(
  treatment_combined ~ age_at_index + sex_factor + enrollment_year +
    n_outpatient_visits + n_hospitalizations +
    chf + carit + valv + pvd + ond +
    cpd + diabunc + diabc + rf + depre,
  data = analysis_cohort,
  method = "nearest",
  ratio = 1,
  caliper = 0.2
)
matched_data <- match.data(psm_model)
cox_psm <- coxph(
  Surv(followup_time / 365.25, event_PD) ~ treatment_combined,
  data = matched_data
)
summary(cox_psm)
tidy(cox_psm, exponentiate = TRUE, conf.int = TRUE)
love.plot(psm_model, threshold = 0.1)

# Remove BBLs in the Other_first group as a comparator
BBL_index_users <- medication_raw %>%
  select(ENROLID, GENERID, SVCDATE) %>%
  inner_join(
    index_drug_users_combined %>%
      filter(treatment_combined == "Other_FirstLine") %>%
      select(ENROLID, index_date),
    by = "ENROLID"
  ) %>%
  filter(as.Date(SVCDATE) == index_date) %>%
  inner_join(
    all_drugs %>% filter(drug_class == "BBL"),
    by = "GENERID"
  ) %>%
  select(ENROLID) %>%
  distinct() %>%
  collect()


analysis_cohort_noBBL <- outcome_ready_dataset_update %>%
  filter(
    !(treatment_combined == "Other_FirstLine" &
        ENROLID %in% BBL_index_users$ENROLID)
  )

analysis_cohort <- analysis_cohort_noBBL %>%
  filter(
    # Valid follow-up time
    followup_time > 0,
    # Exclude Combo group (or analyze separately)
    treatment_combined %in% comparison_group_1
  ) %>%
  mutate(
    treatment_combined = factor(treatment_combined, 
                                levels = comparison_group_1)
  )

# treatment_combined      n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>               <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine    110951   1042      374081.          2.79          2.62          2.96
# 2 ARB                 25965    153       72399.          2.11          1.79          2.48

# treatment_combined      n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>               <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine    110951   1042      374081.          2.79          2.62          2.96
# 2 CCB                 36873    291       99771.          2.92          2.59          3.27

# [ARB: aHR = 0.782, 95%CI (0.656, 0.932), p = 0.00610] [CCB: aHR = 0.941, 95%CI (0.819, 1.08), p = 0.389]

# EXPOSURE_WINDOW_DAYS: 90 days -> 180 days

# treatment_combined      n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>               <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine    191096   2213      667218.          3.32          3.18          3.46
# 2 ARB                 23603    144       66455.          2.17          1.83          2.55

# treatment_combined      n events person_years ir_per_1000py ir_95ci_lower ir_95ci_upper
# <fct>               <int>  <dbl>        <dbl>         <dbl>         <dbl>         <dbl>
# 1 Other_FirstLine    191096   2213      667218.          3.32          3.18          3.46
# 2 CCB                 32830    266       91449.          2.91          2.57          3.28

# [ARB: aHR = 0.697, 95%CI (0.584, 0.833), p = 0.0000690] [CCB: aHR = 0.855, 95%CI (0.750, 0.976), p = 0.0203]

# =============================================================================
# STEP 9: Table 1 - Baseline Chracteristics
# =============================================================================
table1_data <- outcome_ready_dataset_update %>%
  filter(
    treatment_combined %in% c("Other_FirstLine", "ARB", "CCB")
  ) %>%
  mutate(
    treatment_combined = factor(
      treatment_combined,
      levels = c("Other_FirstLine", "ARB", "CCB")
    )
  )

mean_sd <- function(x) {
  sprintf(
    "%.1f (%.1f)",
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE)
  )
}

n_pct <- function(x, total) {
  n <- sum(x, na.rm = TRUE)
  sprintf(
    "%d (%.1f%%)",
    n,
    100 * n / total
  )
}

table1_demo_followup <- table1_data %>%
  group_by(treatment_combined) %>%
  summarise(
    N = n(),
    
    `Age, mean (SD)` =
      mean_sd(age_at_index),
    
    `Female sex, n (%)` =
      n_pct(sex_factor == "Female", n()),
    
    `Follow-up time (years), mean (SD)` =
      mean_sd(followup_time / 365.25),
    
    .groups = "drop"
  )

comorbidity_vars <- c(
  "chf", "carit", "valv", "pvd", "ond",
  "cpd", "diabunc", "diabc", "rf", "depre"
)

table1_comorbidities <- lapply(comorbidity_vars, function(var) {
  
  table1_data %>%
    group_by(treatment_combined) %>%
    summarise(
      value = n_pct(.data[[var]] == 1, n()),
      .groups = "drop"
    ) %>%
    mutate(variable = var)
  
}) %>%
  bind_rows() %>%
  pivot_wider(
    names_from = treatment_combined,
    values_from = value
  )

table1_demo_long <- table1_demo_followup %>%
  pivot_longer(
    cols = -c(treatment_combined, N),
    names_to = "variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = treatment_combined,
    values_from = value
  )

table1_final <- bind_rows(
  tibble(
    variable = "N",
    Other_FirstLine = as.character(table1_demo_followup$N[table1_demo_followup$treatment_combined == "Other_FirstLine"]),
    ARB             = as.character(table1_demo_followup$N[table1_demo_followup$treatment_combined == "ARB"]),
    CCB             = as.character(table1_demo_followup$N[table1_demo_followup$treatment_combined == "CCB"])
  ),
  
  table1_demo_long %>% select(-N),
  
  table1_comorbidities
)

label_map <- c(
  chf     = "Congestive heart failure",
  carit   = "Cardiac arrhythmias",
  valv    = "Valvular disease",
  pvd     = "Peripheral vascular disease",
  ond     = "Other neurological disorders",
  cpd     = "Chronic pulmonary disease",
  diabunc = "Diabetes mellitus (uncomplicated)",
  diabc   = "Diabetes mellitus (complicated)",
  rf      = "Renal failure",
  depre   = "Depression"
)

table1_final <- table1_final %>%
  mutate(
    variable = recode(variable, !!!label_map)
  )

print(table1_final)

###############################################################
## REVIEWER QUICK 6
## DISCONTINUATION-CENSORING SENSITIVITY
###############################################################

DISCONTINUATION_GRACE_DAYS <- 90


###############################################################
## Find last prescription in the ORIGINAL treatment strategy
###############################################################

last_strategy_rx <- medication_raw %>%
  select(
    ENROLID,
    GENERID,
    SVCDATE
  ) %>%
  inner_join(
    all_drugs,
    by = "GENERID"
  ) %>%
  filter(
    drug_class %in%
      first_line_drugs
  ) %>%
  mutate(
    rx_treatment_group =
      case_when(
        drug_class == "ARB" ~
          "ARB",
        
        drug_class == "CCB" ~
          "CCB",
        
        drug_class %in%
          c(
            "ACEi",
            "BBL",
            "Diuretic"
          ) ~
          "Other_FirstLine",
        
        drug_class == "Combo" ~
          "Combo",
        
        TRUE ~
          NA_character_
      )
  ) %>%
  filter(
    !is.na(rx_treatment_group)
  ) %>%
  inner_join(
    outcome_ready_dataset %>%
      select(
        ENROLID,
        treatment_combined,
        index_date
      ),
    by = "ENROLID"
  ) %>%
  filter(
    rx_treatment_group ==
      treatment_combined,
    SVCDATE >= index_date
  ) %>%
  group_by(
    ENROLID
  ) %>%
  summarise(
    last_strategy_rx_date =
      max(
        SVCDATE
      ),
    .groups = "drop"
  ) %>%
  collect() %>%
  mutate(
    ENROLID =
      as.character(ENROLID),
    last_strategy_rx_date =
      as.Date(last_strategy_rx_date),
    
    discontinuation_proxy_date =
      last_strategy_rx_date +
      days(
        DISCONTINUATION_GRACE_DAYS
      )
  )

discontinuation_dataset <- outcome_ready_dataset %>%
  left_join(
    last_strategy_rx %>%
      select(
        ENROLID,
        discontinuation_proxy_date
      ),
    by = "ENROLID"
  ) %>%
  mutate(
    max_followup_date =
      risk_start_date + MAX_FOLLOWUP_DAYS,
    
    censor_date_discontinuation =
      pmin(
        switch_date,
        discontinuation_proxy_date,
        last_enroll,
        study_end_date,
        max_followup_date,
        na.rm = TRUE
      ),
    
    event_PD_disc =
      ifelse(
        PD_status == 1 &
          !is.na(PD_date) &
          PD_date >= risk_start_date &
          PD_date <= censor_date_discontinuation,
        1,
        0
      ),
    
    end_date_disc =
      if_else(
        event_PD_disc == 1,
        PD_date,
        censor_date_discontinuation
      ),
    
    followup_time_disc =
      as.numeric(
        end_date_disc - risk_start_date
      )
  ) %>%
  filter(
    followup_time_disc > 0
  )

analysis_disc <- discontinuation_dataset %>%
  filter(
    treatment_combined %in%
      comparison_group_1
  ) %>%
  mutate(
    treatment_combined =
      factor(
        treatment_combined,
        levels =
          comparison_group_1
      )
  )


disc_formula <- as.formula(
  paste(
    "treatment_combined ~",
    paste(
      covariate_names,
      collapse = " + "
    )
  )
)


disc_weights <- weightit(
  disc_formula,
  data = analysis_disc,
  method = "ps",
  estimand = "ATE",
  stabilize = TRUE
)


analysis_disc$iptw <-
  disc_weights$weights


disc_trim <- quantile(
  analysis_disc$iptw,
  c(
    0.01,
    0.99
  )
)


analysis_disc <- analysis_disc %>%
  mutate(
    iptw_trimmed =
      pmin(
        pmax(
          iptw,
          disc_trim[1]
        ),
        disc_trim[2]
      )
  )


disc_design <- svydesign(
  ids = ~1,
  weights = ~iptw_trimmed,
  data = analysis_disc
)


disc_cox <- svycoxph(
  Surv(
    followup_time_disc / 365.25,
    event_PD_disc
  ) ~
    treatment_combined,
  design = disc_design
)


disc_results <- tidy(
  disc_cox,
  exponentiate = TRUE,
  conf.int = TRUE
)


print(
  disc_results
)

###############################################################
## REVIEWER QUICK 7
## 1 / 2 / 5-YEAR LAGGED PD ANALYSES
###############################################################

run_lagged_PD_analysis <- function(
    lag_years,
    comparison_groups,
    data,
    covariate_names
) {
  
  lag_start_days <-
    lag_years *
    365.25
  
  
  lag_data <- data %>%
    mutate(
      lag_risk_start =
        index_date +
        days(
          round(
            lag_start_days
          )
        )
    ) %>%
    filter(
      is.na(PD_date) |
        PD_date >=
        lag_risk_start
    ) %>%
    mutate(
      lag_censor_date =
        pmin(
          switch_date,
          last_enroll,
          study_end_date,
          lag_risk_start + years(MAX_FOLLOWUP_YEARS),
          na.rm = TRUE
        )
    ) %>%
    filter(
      lag_censor_date >
        lag_risk_start
    ) %>%
    mutate(
      lag_event_PD =
        ifelse(
          PD_status == 1 &
            !is.na(PD_date) &
            PD_date >= lag_risk_start &
            PD_date <= lag_censor_date,
          1,
          0
        ),
      
      lag_end_date =
        if_else(
          lag_event_PD == 1,
          PD_date,
          lag_censor_date
        ),
      
      lag_followup_time =
        as.numeric(
          lag_end_date -
            lag_risk_start
        )
    ) %>%
    filter(
      lag_followup_time > 0,
      treatment_combined %in%
        comparison_groups
    ) %>%
    mutate(
      treatment_combined =
        factor(
          treatment_combined,
          levels =
            comparison_groups
        )
    )
  
  
  lag_formula <- as.formula(
    paste(
      "treatment_combined ~",
      paste(
        covariate_names,
        collapse = " + "
      )
    )
  )
  
  
  lag_weights <- weightit(
    lag_formula,
    data = lag_data,
    method = "ps",
    estimand = "ATE",
    stabilize = TRUE
  )
  
  
  lag_data$iptw <-
    lag_weights$weights
  
  
  trim <- quantile(
    lag_data$iptw,
    c(
      0.01,
      0.99
    )
  )
  
  
  lag_data <- lag_data %>%
    mutate(
      iptw_trimmed =
        pmin(
          pmax(
            iptw,
            trim[1]
          ),
          trim[2]
        )
    )
  
  
  lag_design <- svydesign(
    ids = ~1,
    weights = ~iptw_trimmed,
    data = lag_data
  )
  
  
  lag_model <- svycoxph(
    Surv(
      lag_followup_time / 365.25,
      lag_event_PD
    ) ~
      treatment_combined,
    design = lag_design
  )
  
  
  result <- tidy(
    lag_model,
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    mutate(
      lag_years =
        lag_years,
      N =
        nrow(
          lag_data
        ),
      PD_events =
        sum(
          lag_data$lag_event_PD
        )
    )
  
  
  return(
    result
  )
}

lag_0y <- run_lagged_PD_analysis(
  lag_years = 0.2464,
  comparison_groups = comparison_group_2,
  data = outcome_ready_dataset,
  covariate_names = covariate_names
)

lag_1y <- run_lagged_PD_analysis(
  lag_years = 1,
  comparison_groups = comparison_group_2,
  data = outcome_ready_dataset,
  covariate_names = covariate_names
)

lag_2y <- run_lagged_PD_analysis(
  lag_years = 2,
  comparison_groups = comparison_group_2,
  data = outcome_ready_dataset,
  covariate_names = covariate_names
)

lag_3y <- run_lagged_PD_analysis(
  lag_years = 3,
  comparison_groups = comparison_group_2,
  data = outcome_ready_dataset,
  covariate_names = covariate_names
)

lag_4y <- run_lagged_PD_analysis(
  lag_years = 4,
  comparison_groups = comparison_group_2,
  data = outcome_ready_dataset,
  covariate_names = covariate_names
)

lag_5y <- run_lagged_PD_analysis(
  lag_years = 5,
  comparison_groups = comparison_group_2,
  data = outcome_ready_dataset,
  covariate_names = covariate_names
)


lag_results <- bind_rows(
  lag_0y,
  lag_1y,
  lag_2y,
  lag_3y,
  lag_4y,
  lag_5y
)


print(
  lag_results,
  n = nrow(lag_results)
)
