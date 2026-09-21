###############################################################
## MGB BIOBANK
## FINAL TARGETED SENSITIVITY ANALYSES
##
## Run AFTER primary drug-wide screening.
##
## Key drugs:
##   Losartan
##   Amlodipine
##
## Primary index:
##   PD = first qualifying PD diagnosis
##   HC = last recorded EHR encounter/history
##
## Reverse-causation analyses:
##
## PANEL A
## Exclude exposure during final 1 / 2 / 5 years before index
##
## PANEL B
## Shift index date back by 1 / 2 / 5 years
##
###############################################################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(MatchIt)
library(broom)
library(openxlsx)
library(ggplot2)

###############################################################
## Step S1. Settings
###############################################################

setwd("$HOME/PROJECT_FOLDER")

KEY_DRUGS <- c("Losartan", "Amlodipine")
LAG_YEARS <- c(1, 2, 5)

SENSITIVITY_OUTPUT <- "./data/results/MGB_Losartan_Amlodipine_sensitivity_FINAL.xlsx"

###############################################################
## Step S2. Primary reference
###############################################################

primary_sensitivity_reference <- key_drug_results %>%
  transmute(
    Analysis = "Primary analysis",
    Analysis_group = "Primary",
    Lag_years = 0,
    DrugName = DrugName,
    OR = OR,
    CI_lower = CI_lower,
    CI_upper = CI_upper,
    p_value = p_value,
    Y_case = Y_case,
    Y_ctrl = Y_ctrl,
    N_case = N_case,
    N_ctrl = N_ctrl,
    case_exposure_percent = case_exposure_percent,
    control_exposure_percent = control_exposure_percent,
    N_PD = Y_case + N_case,
    N_HC = Y_ctrl + N_ctrl
  )

###############################################################
## Step S3. Helper: fit Losartan / Amlodipine
###############################################################

fit_key_drugs <- function(cohort, exposure_table, analysis_name, analysis_group = NA_character_, lag_years = NA_real_) {
  
  result_list <- list()
  
  cohort <- cohort %>%
    mutate(EMPI = as.character(EMPI))
  
  exposure_table <- exposure_table %>%
    mutate(EMPI = as.character(EMPI))
  
  for (drug in KEY_DRUGS) {
    
    exposed_ids <- exposure_table %>%
      filter(DrugName == drug) %>%
      pull(EMPI) %>%
      unique()
    
    model_data <- cohort %>%
      mutate(drug_used = as.integer(EMPI %in% exposed_ids))
    
    Y_case <- sum(model_data$drug_used == 1 & model_data$PD_status == 1)
    Y_ctrl <- sum(model_data$drug_used == 1 & model_data$PD_status == 0)
    N_case <- sum(model_data$drug_used == 0 & model_data$PD_status == 1)
    N_ctrl <- sum(model_data$drug_used == 0 & model_data$PD_status == 0)
    
    model <- glm(
      PD_status ~ drug_used + Age + Gender + factor(Race_Category),
      data = model_data,
      family = binomial()
    )
    
    model_result <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
      filter(term == "drug_used")
    
    result_list[[drug]] <- tibble(
      Analysis = analysis_name,
      Analysis_group = analysis_group,
      Lag_years = lag_years,
      DrugName = drug,
      OR = model_result$estimate,
      CI_lower = model_result$conf.low,
      CI_upper = model_result$conf.high,
      p_value = model_result$p.value,
      Y_case = Y_case,
      Y_ctrl = Y_ctrl,
      N_case = N_case,
      N_ctrl = N_ctrl,
      case_exposure_percent = round(100 * Y_case / (Y_case + N_case), 2),
      control_exposure_percent = round(100 * Y_ctrl / (Y_ctrl + N_ctrl), 2),
      N_PD = Y_case + N_case,
      N_HC = Y_ctrl + N_ctrl
    )
  }
  
  bind_rows(result_list)
}

###############################################################
## Step S4. Helper: rematch restricted PD phenotype
###############################################################

rematch_PD_subset <- function(PD_ids, seed = 42) {
  
  PD_ids <- tibble(EMPI = unique(as.character(PD_ids)))
  
  PD_subset <- PD_follow_up_analysis %>%
    semi_join(PD_ids, by = "EMPI") %>%
    select(EMPI, Date_of_Birth, Age, Gender, Race_Category, Age_Category, PD_status)
  
  HC_subset <- health_follow_up_analysis %>%
    select(EMPI, Date_of_Birth, Age, Gender, Race_Category, Age_Category, PD_status)
  
  rematch_source <- bind_rows(PD_subset, HC_subset) %>%
    distinct(EMPI, .keep_all = TRUE) %>%
    filter(!is.na(Age), !is.na(Gender), !is.na(Race_Category))
  
  set.seed(seed)
  
  rematch_model <- matchit(
    PD_status ~ Age + Gender + Race_Category,
    data = rematch_source,
    method = "nearest",
    ratio = 2,
    caliper = 0.1
  )
  
  match.data(rematch_model) %>%
    mutate(EMPI = as.character(EMPI))
}

###############################################################
## Step S5. Initialize results
###############################################################

sensitivity_results <- list()
sensitivity_results[["Primary"]] <- primary_sensitivity_reference

###############################################################
## SENSITIVITY 1. Neurologist-supported PD
###############################################################

neurology_clinic_table <- read.xlsx("./data/processed_data/MGB_Biobank/Neurologist_clinic_classification.xlsx") %>%
  mutate(
    Clinic = str_trim(as.character(Clinic)),
    Neurologist_valid = suppressWarnings(as.numeric(Neurologist_valid))
  )

valid_neurology_clinics <- neurology_clinic_table %>%
  filter(Neurologist_valid == 1) %>%
  distinct(Clinic)

neurologist_PD_ids <- PD_codes %>%
  mutate(Clinic = str_trim(as.character(Clinic))) %>%
  semi_join(valid_neurology_clinics, by = "Clinic") %>%
  semi_join(PD_follow_up_analysis %>% select(EMPI), by = "EMPI") %>%
  distinct(EMPI)

matched_neurologist_PD <- rematch_PD_subset(neurologist_PD_ids$EMPI)

sensitivity_results[["Neurologist"]] <- fit_key_drugs(
  matched_neurologist_PD,
  drug_exposure_primary,
  "Neurologist-supported PD",
  "PD phenotype"
)

###############################################################
## SENSITIVITY 2. >=2 distinct PD diagnosis dates
###############################################################

repeated_PD_ids <- PD_codes %>%
  semi_join(PD_follow_up_analysis %>% select(EMPI), by = "EMPI") %>%
  group_by(EMPI) %>%
  summarise(n_PD_dates = n_distinct(Date), .groups = "drop") %>%
  filter(n_PD_dates >= 2)

matched_repeated_PD <- rematch_PD_subset(repeated_PD_ids$EMPI)

sensitivity_results[["Repeated_PD"]] <- fit_key_drugs(
  matched_repeated_PD,
  drug_exposure_primary,
  "PD with >=2 distinct diagnosis dates",
  "PD phenotype"
)

###############################################################
## SENSITIVITY 3. PD + levodopa / dopamine agonist
###############################################################

dopaminergic_drugs <- c(
  "Levodopa",
  "Pramipexole",
  "Ropinirole",
  "Rotigotine",
  "Apomorphine",
  "Bromocriptine",
  "Cabergoline",
  "Pergolide",
  "Piribedil"
)

dopaminergic_PD_ids <- drug_library_separated_MGB %>%
  filter(DrugName %in% dopaminergic_drugs) %>%
  semi_join(PD_follow_up_analysis %>% select(EMPI), by = "EMPI") %>%
  distinct(EMPI)

matched_dopaminergic_PD <- rematch_PD_subset(dopaminergic_PD_ids$EMPI)

sensitivity_results[["Dopaminergic_PD"]] <- fit_key_drugs(
  matched_dopaminergic_PD,
  drug_exposure_primary,
  "PD + levodopa/dopamine agonist",
  "PD phenotype"
)

###############################################################
## SENSITIVITY 4. Exclude post-index secondary parkinsonism
###############################################################

secondary_parkinsonism_records <- PD_diagnosis %>%
  filter(
    (Code_Type == "ICD9" & Code == "332.1") |
      (Code_Type == "ICD10" & grepl("^G21", Code))
  ) %>%
  filter(!is.na(Date))

post_index_secondary_PD <- secondary_parkinsonism_records %>%
  inner_join(PD_index_primary %>% select(EMPI, index_date), by = "EMPI") %>%
  filter(Date > index_date) %>%
  distinct(EMPI)

stable_PD_ids <- PD_follow_up_analysis %>%
  anti_join(post_index_secondary_PD, by = "EMPI") %>%
  select(EMPI)

matched_stable_PD <- rematch_PD_subset(stable_PD_ids$EMPI)

sensitivity_results[["No_secondary"]] <- fit_key_drugs(
  matched_stable_PD,
  drug_exposure_primary,
  "Exclude post-index secondary parkinsonism",
  "PD phenotype"
)

###############################################################
## Step S6. Build PD and HC index dates
###############################################################

PD_index_sensitivity <- PD_index_primary %>%
  transmute(
    EMPI = as.character(EMPI),
    index_date = as.Date(index_date)
  )

###############################################################
## HC index = last recorded encounter/history
##
## Construct one EHR-history endpoint from available
## diagnosis + medication history.
###############################################################

HC_diagnosis_history <- health_diagnosis %>%
  transmute(
    EMPI = as.character(EMPI),
    Encounter_Date = as.Date(Date)
  ) %>%
  filter(!is.na(Encounter_Date))

HC_medication_history <- drug_library_separated_MGB %>%
  transmute(
    EMPI = as.character(EMPI),
    Encounter_Date = as.Date(Medication_Date)
  ) %>%
  filter(!is.na(Encounter_Date))

HC_encounter_history <- bind_rows(
  HC_diagnosis_history,
  HC_medication_history
) %>%
  semi_join(health_follow_up_primary %>% select(EMPI), by = "EMPI") %>%
  filter(!is.na(Encounter_Date))

HC_index_primary <- HC_encounter_history %>%
  group_by(EMPI) %>%
  summarise(
    index_date = max(Encounter_Date),
    .groups = "drop"
  )

###############################################################
## QC: HC index
###############################################################

HC_index_QC <- health_follow_up_primary %>%
  transmute(EMPI = as.character(EMPI)) %>%
  left_join(HC_index_primary, by = "EMPI") %>%
  summarise(
    N_HC = n(),
    N_with_index = sum(!is.na(index_date)),
    N_missing_index = sum(is.na(index_date))
  )

###############################################################
## Patient-specific index in PRIMARY MATCHED cohort
###############################################################

primary_matched_index <- PD_risk_all_matched %>%
  mutate(EMPI = as.character(EMPI)) %>%
  left_join(
    PD_index_sensitivity %>% rename(PD_index_date = index_date),
    by = "EMPI"
  ) %>%
  left_join(
    HC_index_primary %>% rename(HC_index_date = index_date),
    by = "EMPI"
  ) %>%
  mutate(
    index_date = case_when(
      PD_status == 1 ~ PD_index_date,
      PD_status == 0 ~ HC_index_date,
      TRUE ~ as.Date(NA)
    )
  ) %>%
  select(-PD_index_date, -HC_index_date)

index_date_QC <- primary_matched_index %>%
  group_by(PD_status) %>%
  summarise(
    N = n(),
    N_with_index = sum(!is.na(index_date)),
    N_missing_index = sum(is.na(index_date)),
    .groups = "drop"
  )

###############################################################
## ============================================================
## PANEL A
## EXCLUDE EXPOSURE WITHIN 1 / 2 / 5 YEARS BEFORE INDEX
##
## Keep original index and original matched cohort.
##
## Only exposure definition changes.
###############################################################

run_exposure_exclusion <- function(lag_years) {
  
  analysis_name <- paste0(
    "Exclude exposure within ",
    lag_years,
    "-year pre-index window"
  )
  
  cohort_i <- primary_matched_index %>%
    filter(!is.na(index_date)) %>%
    mutate(exposure_cutoff = index_date - years(lag_years))
  
  exposure_i <- drug_library_separated_MGB %>%
    transmute(
      EMPI = as.character(EMPI),
      Medication_Date = as.Date(Medication_Date),
      DrugName = DrugName
    ) %>%
    inner_join(
      cohort_i %>% select(EMPI, exposure_cutoff),
      by = "EMPI"
    ) %>%
    filter(
      !is.na(Medication_Date),
      Medication_Date < exposure_cutoff,
      DrugName %in% KEY_DRUGS
    ) %>%
    distinct(EMPI, DrugName)
  
  result_i <- fit_key_drugs(
    cohort_i,
    exposure_i,
    analysis_name,
    "Pre-index exposure exclusion",
    lag_years
  )
  
  exposure_QC_i <- cohort_i %>%
    select(EMPI, PD_status) %>%
    tidyr::crossing(DrugName = KEY_DRUGS) %>%
    left_join(
      exposure_i %>% mutate(exposed = 1L),
      by = c("EMPI", "DrugName")
    ) %>%
    mutate(exposed = coalesce(exposed, 0L)) %>%
    group_by(PD_status, DrugName) %>%
    summarise(
      N = n(),
      N_exposed = sum(exposed),
      exposure_percent = round(100 * mean(exposed), 2),
      .groups = "drop"
    ) %>%
    mutate(
      Analysis = analysis_name,
      Lag_years = lag_years,
      Group = if_else(PD_status == 1, "PD cases", "Controls")
    )
  
  list(
    result = result_i,
    exposure_QC = exposure_QC_i,
    cohort = cohort_i
  )
}

exclusion_1y <- run_exposure_exclusion(1)
exclusion_2y <- run_exposure_exclusion(2)
exclusion_5y <- run_exposure_exclusion(5)

sensitivity_results[["Exposure_exclusion_1y"]] <- exclusion_1y$result
sensitivity_results[["Exposure_exclusion_2y"]] <- exclusion_2y$result
sensitivity_results[["Exposure_exclusion_5y"]] <- exclusion_5y$result

exposure_exclusion_QC <- bind_rows(
  exclusion_1y$exposure_QC,
  exclusion_2y$exposure_QC,
  exclusion_5y$exposure_QC
)

###############################################################
## ============================================================
## PANEL B
## SHIFT INDEX DATE BACK BY 1 / 2 / 5 YEARS
##
## Index changes for BOTH PD and HC.
##
## Then:
##   reapply medication eligibility
##   redefine exposure
##   rebuild cohort
##   rematch
###############################################################

###############################################################
## PANEL B source cohorts
##
## PD_follow_up_primary ALREADY contains PD index_date.
## Do NOT join PD_index_primary again.
###############################################################

PD_shift_source <- PD_follow_up_primary %>%
  mutate(
    EMPI = as.character(EMPI),
    index_date = as.Date(index_date)
  ) %>%
  filter(
    !is.na(index_date)
  ) %>%
  distinct(
    EMPI,
    .keep_all = TRUE
  )


###############################################################
## HC does NOT already contain an explicit index_date,
## so join HC_index_primary here.
###############################################################

HC_shift_source <- health_follow_up_primary %>%
  mutate(
    EMPI = as.character(EMPI)
  ) %>%
  inner_join(
    HC_index_primary %>%
      mutate(
        EMPI = as.character(EMPI),
        index_date = as.Date(index_date)
      ),
    by = "EMPI"
  ) %>%
  filter(
    !is.na(index_date)
  ) %>%
  distinct(
    EMPI,
    .keep_all = TRUE
  )

run_shifted_index <- function(lag_years, seed = 42) {
  
  analysis_name <- paste0(
    "Index shifted ",
    lag_years,
    ifelse(lag_years == 1, " year earlier", " years earlier")
  )
  
  PD_shift_i <- PD_shift_source %>%
    mutate(shifted_index_date = index_date - years(lag_years))
  
  HC_shift_i <- HC_shift_source %>%
    mutate(shifted_index_date = index_date - years(lag_years))
  
  PD_med_shift_i <- drug_library_separated_MGB %>%
    transmute(
      medication_record_id = medication_record_id,
      EMPI = as.character(EMPI),
      Medication_Date = as.Date(Medication_Date),
      DrugName = DrugName
    ) %>%
    inner_join(
      PD_shift_i %>% select(EMPI, shifted_index_date),
      by = "EMPI"
    ) %>%
    filter(
      !is.na(Medication_Date),
      Medication_Date < shifted_index_date
    )
  
  HC_med_shift_i <- drug_library_separated_MGB %>%
    transmute(
      medication_record_id = medication_record_id,
      EMPI = as.character(EMPI),
      Medication_Date = as.Date(Medication_Date),
      DrugName = DrugName
    ) %>%
    inner_join(
      HC_shift_i %>% select(EMPI, shifted_index_date),
      by = "EMPI"
    ) %>%
    filter(
      !is.na(Medication_Date),
      Medication_Date < shifted_index_date
    )
  
  PD_med_eligible_i <- PD_med_shift_i %>%
    distinct(EMPI)
  
  HC_med_eligible_i <- HC_med_shift_i %>%
    distinct(EMPI)
  
  PD_analysis_i <- PD_shift_i %>%
    semi_join(PD_med_eligible_i, by = "EMPI")
  
  HC_analysis_i <- HC_shift_i %>%
    semi_join(HC_med_eligible_i, by = "EMPI")
  
  matching_source_i <- bind_rows(
    PD_analysis_i %>%
      select(EMPI, Date_of_Birth, Age, Gender, Race_Category, Age_Category, PD_status),
    HC_analysis_i %>%
      select(EMPI, Date_of_Birth, Age, Gender, Race_Category, Age_Category, PD_status)
  ) %>%
    distinct(EMPI, .keep_all = TRUE) %>%
    filter(!is.na(Age), !is.na(Gender), !is.na(Race_Category))
  
  set.seed(seed)
  
  match_i <- matchit(
    PD_status ~ Age + Gender + Race_Category,
    data = matching_source_i,
    method = "nearest",
    ratio = 2,
    caliper = 0.1
  )
  
  matched_i <- match.data(match_i) %>%
    mutate(EMPI = as.character(EMPI))
  
  exposure_i <- bind_rows(
    PD_med_shift_i %>% select(EMPI, DrugName),
    HC_med_shift_i %>% select(EMPI, DrugName)
  ) %>%
    semi_join(matched_i %>% select(EMPI), by = "EMPI") %>%
    filter(DrugName %in% KEY_DRUGS) %>%
    distinct(EMPI, DrugName)
  
  result_i <- fit_key_drugs(
    matched_i,
    exposure_i,
    analysis_name,
    "Shifted index",
    lag_years
  )
  
  cohort_QC_i <- tibble(
    Analysis = analysis_name,
    Lag_years = lag_years,
    PD_before_medication_requirement = n_distinct(PD_shift_i$EMPI),
    PD_medication_eligible = n_distinct(PD_analysis_i$EMPI),
    HC_before_medication_requirement = n_distinct(HC_shift_i$EMPI),
    HC_medication_eligible = n_distinct(HC_analysis_i$EMPI),
    Matched_PD = sum(matched_i$PD_status == 1),
    Matched_HC = sum(matched_i$PD_status == 0)
  )
  
  exposure_QC_i <- matched_i %>%
    select(EMPI, PD_status) %>%
    tidyr::crossing(DrugName = KEY_DRUGS) %>%
    left_join(
      exposure_i %>% mutate(exposed = 1L),
      by = c("EMPI", "DrugName")
    ) %>%
    mutate(exposed = coalesce(exposed, 0L)) %>%
    group_by(PD_status, DrugName) %>%
    summarise(
      N = n(),
      N_exposed = sum(exposed),
      exposure_percent = round(100 * mean(exposed), 2),
      .groups = "drop"
    ) %>%
    mutate(
      Analysis = analysis_name,
      Lag_years = lag_years,
      Group = if_else(PD_status == 1, "PD cases", "Controls")
    )
  
  list(
    result = result_i,
    matched_cohort = matched_i,
    cohort_QC = cohort_QC_i,
    exposure_QC = exposure_QC_i
  )
}

shifted_index_1y <- run_shifted_index(1)
shifted_index_2y <- run_shifted_index(2)
shifted_index_5y <- run_shifted_index(5)

sensitivity_results[["Shifted_index_1y"]] <- shifted_index_1y$result
sensitivity_results[["Shifted_index_2y"]] <- shifted_index_2y$result
sensitivity_results[["Shifted_index_5y"]] <- shifted_index_5y$result

shifted_index_cohort_QC <- bind_rows(
  shifted_index_1y$cohort_QC,
  shifted_index_2y$cohort_QC,
  shifted_index_5y$cohort_QC
)

shifted_index_exposure_QC <- bind_rows(
  shifted_index_1y$exposure_QC,
  shifted_index_2y$exposure_QC,
  shifted_index_5y$exposure_QC
)

###############################################################
## Step S7. Antihypertensive definitions
###############################################################

ARB <- c(
  "losartan",
  "eprosartan",
  "valsartan",
  "irbesartan",
  "tasosartan",
  "candesartan",
  "telmisartan",
  "olmesartan",
  "azilsartan",
  "fimasartan"
)

CCB <- c(
  "amlodipine",
  "felodipine",
  "isradipine",
  "nicardipine",
  "nifedipine",
  "nimodipine",
  "nisoldipine",
  "nitrendipine",
  "lacidipine",
  "nilvadipine",
  "manidipine",
  "barnidipine",
  "lercanidipine",
  "cilnidipine",
  "benidipine",
  "clevidipine",
  "levamlodipine",
  "mibefradil",
  "verapamil",
  "gallopamil",
  "etripamil",
  "diltiazem",
  "fendiline",
  "bepridil",
  "lidoflazine",
  "perhexiline"
)

ACEi <- c(
  "captopril",
  "enalapril",
  "lisinopril",
  "perindopril",
  "ramipril",
  "quinapril",
  "benazepril",
  "cilazapril",
  "fosinopril",
  "trandolapril",
  "spirapril",
  "delapril",
  "moexipril",
  "temocapril",
  "zofenopril",
  "imidapril"
)

BBL <- c(
  "alprenolol",
  "oxprenolol",
  "pindolol",
  "propranolol",
  "timolol",
  "sotalol",
  "nadolol",
  "mepindolol",
  "carteolol",
  "tertatolol",
  "bopindolol",
  "bupranolol",
  "penbutolol",
  "cloranolol",
  "practolol",
  "metoprolol",
  "atenolol",
  "acebutolol",
  "betaxolol",
  "bevantolol",
  "bisoprolol",
  "celiprolol",
  "esmolol",
  "epanolol",
  "s-atenolol",
  "nebivolol",
  "talinolol",
  "landiolol",
  "labetalol",
  "carvedilol"
)

Diuretic <- c(
  "bendroflumethiazide",
  "hydroflumethiazide",
  "hydrochlorothiazide",
  "chlorothiazide",
  "polythiazide",
  "trichlormethiazide",
  "cyclopenthiazide",
  "methyclothiazide",
  "cyclothiazide",
  "mebutizide"
)

antiHTN_dictionary <- bind_rows(
  tibble(DrugName_key = ARB, antiHTN_class = "ARB"),
  tibble(DrugName_key = CCB, antiHTN_class = "CCB"),
  tibble(DrugName_key = ACEi, antiHTN_class = "ACEi"),
  tibble(DrugName_key = BBL, antiHTN_class = "BBL"),
  tibble(DrugName_key = Diuretic, antiHTN_class = "Diuretic")
) %>%
  distinct()

###############################################################
## Step S8. Primary medication dataset
###############################################################

primary_medication_patient <- Medication_history_primary %>%
  semi_join(PD_risk_all_matched %>% select(EMPI), by = "EMPI")

antiHTN_patient_class <- primary_medication_patient %>%
  transmute(
    EMPI = EMPI,
    DrugName_key = str_to_lower(str_trim(DrugName))
  ) %>%
  inner_join(antiHTN_dictionary, by = "DrugName_key") %>%
  distinct(EMPI, antiHTN_class) %>%
  mutate(exposed = 1L) %>%
  pivot_wider(
    names_from = antiHTN_class,
    values_from = exposed,
    values_fill = 0
  )

medication_burden <- primary_medication_patient %>%
  group_by(EMPI) %>%
  summarise(
    total_distinct_medications = n_distinct(DrugName),
    .groups = "drop"
  )

key_drug_indicator <- primary_medication_patient %>%
  filter(DrugName %in% KEY_DRUGS) %>%
  distinct(EMPI, DrugName) %>%
  mutate(exposed = 1L) %>%
  pivot_wider(
    names_from = DrugName,
    values_from = exposed,
    values_fill = 0
  )

expanded_model_data <- PD_risk_all_matched %>%
  select(EMPI, PD_status, Age, Gender, Race_Category) %>%
  left_join(antiHTN_patient_class, by = "EMPI") %>%
  left_join(medication_burden, by = "EMPI") %>%
  left_join(key_drug_indicator, by = "EMPI") %>%
  mutate(
    across(
      any_of(c("ARB", "CCB", "ACEi", "BBL", "Diuretic", "Losartan", "Amlodipine")),
      ~ coalesce(.x, 0L)
    ),
    total_distinct_medications = coalesce(total_distinct_medications, 0L)
  )

###############################################################
## Step S9. Mutual antihypertensive adjustment
###############################################################

losartan_mutual_model <- glm(
  PD_status ~ Losartan + CCB + ACEi + BBL + Diuretic + Age + Gender + factor(Race_Category),
  data = expanded_model_data,
  family = binomial()
)

amlodipine_mutual_model <- glm(
  PD_status ~ Amlodipine + ARB + ACEi + BBL + Diuretic + Age + Gender + factor(Race_Category),
  data = expanded_model_data,
  family = binomial()
)

extract_adjusted_drug <- function(model, drug_name, analysis_name, analysis_group) {
  
  result <- broom::tidy(
    model,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    filter(term == drug_name)
  
  tibble(
    Analysis = analysis_name,
    Analysis_group = analysis_group,
    Lag_years = NA_real_,
    DrugName = drug_name,
    OR = result$estimate,
    CI_lower = result$conf.low,
    CI_upper = result$conf.high,
    p_value = result$p.value,
    Y_case = NA_integer_,
    Y_ctrl = NA_integer_,
    N_case = NA_integer_,
    N_ctrl = NA_integer_,
    case_exposure_percent = NA_real_,
    control_exposure_percent = NA_real_,
    N_PD = sum(expanded_model_data$PD_status == 1),
    N_HC = sum(expanded_model_data$PD_status == 0)
  )
}

sensitivity_results[["Mutual_adjustment"]] <- bind_rows(
  extract_adjusted_drug(
    losartan_mutual_model,
    "Losartan",
    "Adjusted for concurrent antihypertensive classes",
    "Confounding adjustment"
  ),
  extract_adjusted_drug(
    amlodipine_mutual_model,
    "Amlodipine",
    "Adjusted for concurrent antihypertensive classes",
    "Confounding adjustment"
  )
)

###############################################################
## Step S10. Antihypertensive classes + medication burden
###############################################################

losartan_full_model <- glm(
  PD_status ~ Losartan + CCB + ACEi + BBL + Diuretic + total_distinct_medications + Age + Gender + factor(Race_Category),
  data = expanded_model_data,
  family = binomial()
)

amlodipine_full_model <- glm(
  PD_status ~ Amlodipine + ARB + ACEi + BBL + Diuretic + total_distinct_medications + Age + Gender + factor(Race_Category),
  data = expanded_model_data,
  family = binomial()
)

sensitivity_results[["Full_adjustment"]] <- bind_rows(
  extract_adjusted_drug(
    losartan_full_model,
    "Losartan",
    "Antihypertensive classes + medication burden",
    "Confounding adjustment"
  ),
  extract_adjusted_drug(
    amlodipine_full_model,
    "Amlodipine",
    "Antihypertensive classes + medication burden",
    "Confounding adjustment"
  )
)

###############################################################
## Step S11. Exposure >=2 distinct medication dates
###############################################################

drug_exposure_2dates <- drug_exposure_patient %>%
  filter(n_medication_dates >= 2)

sensitivity_results[["Two_med_dates"]] <- fit_key_drugs(
  PD_risk_all_matched,
  drug_exposure_2dates,
  "Exposure defined by >=2 medication dates",
  "Exposure robustness"
)

###############################################################
## Step S12. Exclude >180-day dopaminergic lead
###############################################################

first_dopaminergic_date <- drug_library_separated_MGB %>%
  filter(
    DrugName %in% dopaminergic_drugs,
    !is.na(Medication_Date)
  ) %>%
  group_by(EMPI) %>%
  summarise(
    first_dopaminergic_date = min(Medication_Date),
    .groups = "drop"
  )

PD_delay_180 <- PD_index_primary %>%
  left_join(first_dopaminergic_date, by = "EMPI") %>%
  mutate(
    days_dopaminergic_minus_PD = as.numeric(first_dopaminergic_date - index_date)
  ) %>%
  filter(
    !is.na(days_dopaminergic_minus_PD),
    days_dopaminergic_minus_PD < -180
  ) %>%
  select(EMPI)

PD_no_major_delay <- PD_follow_up_analysis %>%
  anti_join(PD_delay_180, by = "EMPI") %>%
  select(EMPI)

matched_no_major_delay <- rematch_PD_subset(PD_no_major_delay$EMPI)

sensitivity_results[["No_180d_delay"]] <- fit_key_drugs(
  matched_no_major_delay,
  drug_exposure_primary,
  "Exclude dopaminergic treatment >180 d before PD code",
  "PD timing"
)

###############################################################
## Step S13. Combine results
###############################################################

analysis_levels <- c(
  "Primary analysis",
  "Neurologist-supported PD",
  "PD with >=2 distinct diagnosis dates",
  "PD + levodopa/dopamine agonist",
  "Exclude post-index secondary parkinsonism",
  "Exclude dopaminergic treatment >180 d before PD code",
  "Exclude exposure within 1-year pre-index window",
  "Exclude exposure within 2-year pre-index window",
  "Exclude exposure within 5-year pre-index window",
  "Index shifted 1 year earlier",
  "Index shifted 2 years earlier",
  "Index shifted 5 years earlier",
  "Adjusted for concurrent antihypertensive classes",
  "Antihypertensive classes + medication burden",
  "Exposure defined by >=2 medication dates"
)

sensitivity_results_all <- bind_rows(sensitivity_results) %>%
  mutate(
    Analysis = factor(Analysis, levels = analysis_levels)
  ) %>%
  arrange(DrugName, Analysis)

###############################################################
## Step S14. Cohort-size summary
###############################################################

sensitivity_cohort_summary <- bind_rows(
  
  tibble(
    Analysis = c(
      "Primary analysis",
      "Neurologist-supported PD",
      "PD with >=2 distinct diagnosis dates",
      "PD + levodopa/dopamine agonist",
      "Exclude post-index secondary parkinsonism",
      "Exclude dopaminergic treatment >180 d before PD code"
    ),
    N_PD = c(
      sum(PD_risk_all_matched$PD_status == 1),
      sum(matched_neurologist_PD$PD_status == 1),
      sum(matched_repeated_PD$PD_status == 1),
      sum(matched_dopaminergic_PD$PD_status == 1),
      sum(matched_stable_PD$PD_status == 1),
      sum(matched_no_major_delay$PD_status == 1)
    ),
    N_HC = c(
      sum(PD_risk_all_matched$PD_status == 0),
      sum(matched_neurologist_PD$PD_status == 0),
      sum(matched_repeated_PD$PD_status == 0),
      sum(matched_dopaminergic_PD$PD_status == 0),
      sum(matched_stable_PD$PD_status == 0),
      sum(matched_no_major_delay$PD_status == 0)
    )
  ),
  
  sensitivity_results_all %>%
    filter(Analysis_group == "Pre-index exposure exclusion") %>%
    distinct(Analysis, N_PD, N_HC) %>%
    mutate(Analysis = as.character(Analysis)),
  
  shifted_index_cohort_QC %>%
    transmute(
      Analysis = Analysis,
      N_PD = Matched_PD,
      N_HC = Matched_HC
    )
)

###############################################################
## Step S15. Forest plot
###############################################################

sensitivity_plot_data <- sensitivity_results_all %>%
  filter(!is.na(OR), !is.na(CI_lower), !is.na(CI_upper)) %>%
  mutate(
    Analysis = factor(
      Analysis,
      levels = rev(analysis_levels)
    )
  )

p_sensitivity <- ggplot(
  sensitivity_plot_data,
  aes(x = OR, y = Analysis, shape = DrugName)
) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbar(
    aes(xmin = CI_lower, xmax = CI_upper),
    width = 0.18,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.55)
  ) +
  scale_x_log10() +
  labs(
    x = "Odds ratio (95% CI)",
    y = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "top")

print(p_sensitivity)

###############################################################
## Step S16. Save forest plot
###############################################################

ggsave(
  "./data/results/MGB_Losartan_Amlodipine_sensitivity_FINAL.pdf",
  p_sensitivity,
  width = 9,
  height = 8
)

ggsave(
  "./data/results/MGB_Losartan_Amlodipine_sensitivity_FINAL.png",
  p_sensitivity,
  width = 9,
  height = 8,
  dpi = 600
)

###############################################################
## Step S17. Save workbook
###############################################################

write.xlsx(
  list(
    Sensitivity_results = sensitivity_results_all,
    Cohort_sizes = sensitivity_cohort_summary,
    HC_index_QC = HC_index_QC,
    Index_date_QC = index_date_QC,
    Exposure_exclusion_QC = exposure_exclusion_QC,
    Shifted_index_cohort_QC = shifted_index_cohort_QC,
    Shifted_index_exposure_QC = shifted_index_exposure_QC,
    Neurology_clinics = valid_neurology_clinics,
    Repeated_PD = repeated_PD_ids,
    Dopaminergic_PD = dopaminergic_PD_ids,
    Post_index_secondary = post_index_secondary_PD,
    Delay_gt180d_PD = PD_delay_180,
    Medication_burden = medication_burden,
    Antihypertensive_exposure = antiHTN_patient_class
  ),
  SENSITIVITY_OUTPUT,
  overwrite = TRUE
)

###############################################################
## Step S18. Console summary
###############################################################

cat("\n")
cat("====================================================\n")
cat("MGB TARGETED SENSITIVITY ANALYSES\n")
cat("LOSARTAN + AMLODIPINE\n")
cat("====================================================\n")

cat("\nHC INDEX QC:\n")
print(tibble::as_tibble(HC_index_QC))

cat("\nINDEX DATE QC:\n")
print(tibble::as_tibble(index_date_QC))

cat("\n====================================================\n")
cat("PANEL A: PRE-INDEX EXPOSURE EXCLUSION\n")
cat("====================================================\n")
print(
  tibble::as_tibble(exposure_exclusion_QC),
  n = nrow(exposure_exclusion_QC)
)

cat("\n====================================================\n")
cat("PANEL B: SHIFTED INDEX DATE\n")
cat("====================================================\n")

cat("\nShifted-index cohort QC:\n")
print(
  tibble::as_tibble(shifted_index_cohort_QC),
  n = nrow(shifted_index_cohort_QC)
)

cat("\nShifted-index exposure QC:\n")
print(
  tibble::as_tibble(shifted_index_exposure_QC),
  n = nrow(shifted_index_exposure_QC)
)

cat("\n====================================================\n")
cat("ALL SENSITIVITY RESULTS\n")
cat("====================================================\n")

sensitivity_results_print <- sensitivity_results_all %>%
  select(
    Analysis_group,
    Analysis,
    Lag_years,
    DrugName,
    OR,
    CI_lower,
    CI_upper,
    p_value,
    N_PD,
    N_HC,
    case_exposure_percent,
    control_exposure_percent
  ) %>%
  tibble::as_tibble()

print(
  sensitivity_results_print,
  n = nrow(sensitivity_results_print)
)

cat("\n====================================================\n")
cat("PD PHENOTYPE QC\n")
cat("====================================================\n")

cat("\nNeurologist-supported PD patients: ", nrow(neurologist_PD_ids), "\n", sep = "")
cat("PD with >=2 distinct PD dates: ", nrow(repeated_PD_ids), "\n", sep = "")
cat("PD with levodopa/dopamine agonist: ", nrow(dopaminergic_PD_ids), "\n", sep = "")
cat("PD with post-index secondary parkinsonism: ", nrow(post_index_secondary_PD), "\n", sep = "")
cat("PD excluded for >180-day dopaminergic lead: ", nrow(PD_delay_180), "\n", sep = "")

cat("====================================================\n")