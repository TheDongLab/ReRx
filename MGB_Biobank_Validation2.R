###############################################################
## VALIDATION 2
##
## Antihypertensive treatment-intensity trajectories
## during the 5 years before index
##
## PURPOSE:
## Directly evaluate potential de-prescribing during the
## prodromal period preceding PD diagnosis.
##
## Antihypertensive definition is harmonized with TTE:
##
##   ARB
##   CCB
##   ACEi
##   BBL
##   Diuretic
###############################################################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(openxlsx)
library(ggplot2)

setwd("$HOME/PROJECT_FOLDER")

###############################################################
## Step 1. Final matched analytic cohort
###############################################################

trajectory_cohort <- PD_risk_all_matched %>%
  transmute(
    EMPI = as.character(EMPI),
    PD_status,
    Age,
    Gender,
    Race_Category
  ) %>%
  distinct(
    EMPI,
    .keep_all = TRUE
  )


###############################################################
## Step 2. Define index dates
##
## EXACTLY harmonized with manuscript and sensitivity analysis
##
## PD:
##   first qualifying PD diagnosis
##
## HC:
##   last recorded encounter/history in the EHR
##
## For HC, encounter/history endpoint is defined from the
## latest available diagnosis or medication record.
###############################################################

###############################################################
## Step 2A. PD index
###############################################################

PD_index_for_trajectory <- PD_index_primary %>%
  transmute(
    EMPI = as.character(EMPI),
    index_date = as.Date(index_date)
  )


###############################################################
## Step 2B. HC diagnosis-history dates
###############################################################

HC_diagnosis_history_for_index <- health_diagnosis %>%
  transmute(
    EMPI = as.character(EMPI),
    Encounter_Date = as.Date(Date)
  ) %>%
  filter(
    !is.na(Encounter_Date)
  )


###############################################################
## Step 2C. HC medication-history dates
###############################################################

HC_medication_history_for_index <- drug_library_separated_MGB %>%
  transmute(
    EMPI = as.character(EMPI),
    Encounter_Date = as.Date(Medication_Date)
  ) %>%
  filter(
    !is.na(Encounter_Date)
  )


###############################################################
## Step 2D. Combine HC encounter/history records
###############################################################

HC_encounter_history_for_index <- bind_rows(
  HC_diagnosis_history_for_index,
  HC_medication_history_for_index
) %>%
  semi_join(
    trajectory_cohort %>%
      filter(PD_status == 0) %>%
      select(EMPI),
    by = "EMPI"
  ) %>%
  distinct(
    EMPI,
    Encounter_Date
  )


###############################################################
## Step 2E. HC index = last recorded encounter/history
###############################################################

HC_index_for_trajectory <- HC_encounter_history_for_index %>%
  group_by(
    EMPI
  ) %>%
  summarise(
    index_date = max(Encounter_Date),
    .groups = "drop"
  )


###############################################################
## Step 2F. HC index QC
###############################################################

HC_index_QC_validation2 <- trajectory_cohort %>%
  filter(
    PD_status == 0
  ) %>%
  select(
    EMPI
  ) %>%
  left_join(
    HC_index_for_trajectory,
    by = "EMPI"
  ) %>%
  summarise(
    N_HC = n(),
    N_with_index = sum(!is.na(index_date)),
    N_missing_index = sum(is.na(index_date))
  )


###############################################################
## Step 3. Combine PD and HC index dates
###############################################################

trajectory_index <- trajectory_cohort %>%
  left_join(
    PD_index_for_trajectory %>%
      rename(
        PD_index_date = index_date
      ),
    by = "EMPI"
  ) %>%
  left_join(
    HC_index_for_trajectory %>%
      rename(
        HC_index_date = index_date
      ),
    by = "EMPI"
  ) %>%
  mutate(
    index_date = case_when(
      PD_status == 1 ~ PD_index_date,
      PD_status == 0 ~ HC_index_date,
      TRUE ~ as.Date(NA)
    )
  ) %>%
  select(
    -PD_index_date,
    -HC_index_date
  ) %>%
  filter(
    !is.na(index_date)
  )


###############################################################
## Step 3A. Final index QC
###############################################################

trajectory_index_QC <- trajectory_cohort %>%
  select(
    EMPI,
    PD_status
  ) %>%
  left_join(
    trajectory_index %>%
      select(
        EMPI,
        index_date
      ),
    by = "EMPI"
  ) %>%
  group_by(
    PD_status
  ) %>%
  summarise(
    N = n(),
    N_with_index = sum(!is.na(index_date)),
    N_missing_index = sum(is.na(index_date)),
    .groups = "drop"
  )

###############################################################
## Step 4. Define antihypertensive medications
##
## EXACTLY harmonized with TTE
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


###############################################################
## Step 5. Build TTE-compatible antihypertensive dictionary
##
## Use lowercase key to avoid capitalization mismatches
###############################################################

antihypertensive_dictionary <- bind_rows(
  
  tibble(
    DrugName_key = ARB,
    antiHTN_class = "ARB"
  ),
  
  tibble(
    DrugName_key = CCB,
    antiHTN_class = "CCB"
  ),
  
  tibble(
    DrugName_key = ACEi,
    antiHTN_class = "ACEi"
  ),
  
  tibble(
    DrugName_key = BBL,
    antiHTN_class = "BBL"
  ),
  
  tibble(
    DrugName_key = Diuretic,
    antiHTN_class = "Diuretic"
  )
  
) %>%
  distinct()


###############################################################
## Step 6. Extract antihypertensive medication records
###############################################################

antiHTN_history <- drug_library_separated_MGB %>%
  
  transmute(
    
    medication_record_id,
    
    EMPI =
      as.character(EMPI),
    
    Medication_Date =
      as.Date(Medication_Date),
    
    DrugName =
      str_trim(
        as.character(DrugName)
      ),
    
    DrugName_key =
      str_to_lower(
        str_trim(
          as.character(DrugName)
        )
      )
  ) %>%
  
  inner_join(
    antihypertensive_dictionary,
    by = "DrugName_key"
  ) %>%
  
  semi_join(
    trajectory_index %>%
      select(EMPI),
    by = "EMPI"
  ) %>%
  
  filter(
    !is.na(Medication_Date)
  )


###############################################################
## Step 7. Calculate antihypertensive timing relative to index
###############################################################

antiHTN_relative <- antiHTN_history %>%
  
  inner_join(
    trajectory_index %>%
      select(
        EMPI,
        index_date,
        PD_status
      ),
    by = "EMPI"
  ) %>%
  
  mutate(
    
    days_before_index =
      as.numeric(
        index_date -
          Medication_Date
      )
  ) %>%
  
  filter(
    days_before_index >= 1,
    days_before_index <= 1825
  )


###############################################################
## Step 8. Define five 1-year windows
###############################################################

antiHTN_relative <- antiHTN_relative %>%
  
  mutate(
    
    year_before_index =
      case_when(
        
        days_before_index >= 1 &
          days_before_index <= 365 ~
          -1L,
        
        days_before_index >= 366 &
          days_before_index <= 730 ~
          -2L,
        
        days_before_index >= 731 &
          days_before_index <= 1095 ~
          -3L,
        
        days_before_index >= 1096 &
          days_before_index <= 1460 ~
          -4L,
        
        days_before_index >= 1461 &
          days_before_index <= 1825 ~
          -5L
      )
  )


###############################################################
## Step 9. Count distinct antihypertensive classes
##
## Example:
##
## Losartan + valsartan -> 1 ARB class
##
## Losartan + amlodipine -> 2 classes
##
## Combination pills are handled correctly because
## RxNorm library is ingredient-level.
###############################################################

patient_year_class_count <- antiHTN_relative %>%
  
  distinct(
    EMPI,
    year_before_index,
    antiHTN_class
  ) %>%
  
  count(
    EMPI,
    year_before_index,
    name = "n_antihypertensive_classes"
  )


###############################################################
## Step 10. Build EHR observation history
##
## Important methodological update:
##
## A patient-year should be coded as 0 antihypertensive classes
## ONLY if there is evidence that the patient was observed in
## the MGB EHR during that year.
##
## No EHR observation -> NA, not 0.
###############################################################


###############################################################
## Step 10A. Diagnosis-based observation
###############################################################

all_diagnosis_for_observation <- bind_rows(
  
  PD_diagnosis %>%
    transmute(
      EMPI = as.character(EMPI),
      Observation_Date = as.Date(Date)
    ),
  
  health_diagnosis %>%
    transmute(
      EMPI = as.character(EMPI),
      Observation_Date = as.Date(Date)
    )
  
) %>%
  
  filter(
    !is.na(Observation_Date)
  ) %>%
  
  semi_join(
    trajectory_index %>%
      select(EMPI),
    by = "EMPI"
  )


###############################################################
## Step 10B. Medication-based observation
##
## Prefer raw medication history if Med_all still exists.
###############################################################

if (exists("Med_all")) {
  
  all_medication_for_observation <- Med_all %>%
    
    transmute(
      
      EMPI =
        as.character(EMPI),
      
      Observation_Date =
        as.Date(
          Medication_Date,
          format = "%m/%d/%Y"
        )
    ) %>%
    
    filter(
      !is.na(Observation_Date)
    )
  
} else {
  
  all_medication_for_observation <-
    drug_library_separated_MGB %>%
    
    transmute(
      
      EMPI =
        as.character(EMPI),
      
      Observation_Date =
        as.Date(Medication_Date)
    ) %>%
    
    filter(
      !is.na(Observation_Date)
    )
}


all_medication_for_observation <-
  all_medication_for_observation %>%
  
  semi_join(
    trajectory_index %>%
      select(EMPI),
    by = "EMPI"
  )


###############################################################
## Step 10C. Combine evidence of EHR observation
###############################################################

ehr_observation <- bind_rows(
  
  all_diagnosis_for_observation,
  
  all_medication_for_observation
  
) %>%
  
  distinct(
    EMPI,
    Observation_Date
  )


###############################################################
## Step 11. Assign EHR observations to pre-index years
###############################################################

ehr_observation_relative <- ehr_observation %>%
  
  inner_join(
    
    trajectory_index %>%
      select(
        EMPI,
        index_date
      ),
    
    by = "EMPI"
  ) %>%
  
  mutate(
    
    days_before_index =
      as.numeric(
        index_date -
          Observation_Date
      ),
    
    year_before_index =
      case_when(
        
        days_before_index >= 1 &
          days_before_index <= 365 ~
          -1L,
        
        days_before_index >= 366 &
          days_before_index <= 730 ~
          -2L,
        
        days_before_index >= 731 &
          days_before_index <= 1095 ~
          -3L,
        
        days_before_index >= 1096 &
          days_before_index <= 1460 ~
          -4L,
        
        days_before_index >= 1461 &
          days_before_index <= 1825 ~
          -5L,
        
        TRUE ~
          NA_integer_
      )
  ) %>%
  
  filter(
    !is.na(year_before_index)
  )


###############################################################
## Step 12. Define observable patient-years
###############################################################

observable_patient_year <- ehr_observation_relative %>%
  
  distinct(
    EMPI,
    year_before_index
  ) %>%
  
  mutate(
    observed_in_EHR = TRUE
  )


###############################################################
## Step 13. Create complete patient × year grid
###############################################################

patient_year_grid <- trajectory_index %>%
  
  select(
    EMPI,
    PD_status,
    Age,
    Gender,
    Race_Category,
    index_date
  ) %>%
  
  tidyr::crossing(
    year_before_index =
      -5:-1
  )


###############################################################
## Step 14. Merge observation status and treatment intensity
###############################################################

patient_year_trajectory <- patient_year_grid %>%
  
  left_join(
    observable_patient_year,
    by = c(
      "EMPI",
      "year_before_index"
    )
  ) %>%
  
  left_join(
    patient_year_class_count,
    by = c(
      "EMPI",
      "year_before_index"
    )
  ) %>%
  
  mutate(
    
    observed_in_EHR =
      coalesce(
        observed_in_EHR,
        FALSE
      ),
    
    #############################################################
    ## Key:
    ##
    ## observed + no antihypertensive -> 0
    ## not observed                   -> NA
    #############################################################
    
    n_antihypertensive_classes =
      case_when(
        
        observed_in_EHR &
          is.na(n_antihypertensive_classes) ~
          0L,
        
        observed_in_EHR ~
          n_antihypertensive_classes,
        
        TRUE ~
          NA_integer_
      ),
    
    any_antihypertensive =
      case_when(
        
        is.na(n_antihypertensive_classes) ~
          NA_integer_,
        
        n_antihypertensive_classes > 0 ~
          1L,
        
        TRUE ~
          0L
      )
  )


###############################################################
## Step 15. EHR observation QC
###############################################################

observation_QC <- patient_year_trajectory %>%
  
  group_by(
    PD_status,
    year_before_index
  ) %>%
  
  summarise(
    
    total_patients =
      n(),
    
    observed_patients =
      sum(
        observed_in_EHR
      ),
    
    observation_percent =
      round(
        100 *
          mean(
            observed_in_EHR
          ),
        2
      ),
    
    .groups = "drop"
  )


###############################################################
## Step 16. Overall antihypertensive trajectory
##
## Only observable patient-years contribute.
###############################################################

trajectory_summary <- patient_year_trajectory %>%
  
  filter(
    observed_in_EHR
  ) %>%
  
  group_by(
    PD_status,
    year_before_index
  ) %>%
  
  summarise(
    
    N_observed =
      n(),
    
    #############################################################
    ## Number of patients with ANY antihypertensive
    #############################################################
    
    N_treated =
      sum(
        any_antihypertensive == 1,
        na.rm = TRUE
      ),
    
    percent_any_antihypertensive =
      round(
        100 *
          N_treated /
          N_observed,
        2
      ),
    
    #############################################################
    ## Mean number of antihypertensive classes
    #############################################################
    
    mean_classes =
      mean(
        n_antihypertensive_classes,
        na.rm = TRUE
      ),
    
    SD_classes =
      sd(
        n_antihypertensive_classes,
        na.rm = TRUE
      ),
    
    SE_classes =
      SD_classes /
      sqrt(N_observed),
    
    CI_lower =
      mean_classes -
      1.96 * SE_classes,
    
    CI_upper =
      mean_classes +
      1.96 * SE_classes,
    
    median_classes =
      median(
        n_antihypertensive_classes,
        na.rm = TRUE
      ),
    
    IQR_25 =
      quantile(
        n_antihypertensive_classes,
        0.25,
        na.rm = TRUE
      ),
    
    IQR_75 =
      quantile(
        n_antihypertensive_classes,
        0.75,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


trajectory_wide <- trajectory_summary %>%
  
  mutate(
    
    Group =
      if_else(
        PD_status == 1,
        "PD cases",
        "Controls"
      )
  ) %>%
  
  select(
    
    Group,
    
    year_before_index,
    
    N_observed,
    
    N_treated,
    
    percent_any_antihypertensive,
    
    mean_classes,
    
    SE_classes,
    
    CI_lower,
    
    CI_upper,
    
    median_classes,
    
    IQR_25,
    
    IQR_75
  )


###############################################################
## Step 17. CLASS-SPECIFIC antihypertensive trajectories
##
## ARB
## CCB
## ACEi
## BBL
## Diuretic
##
## Main purpose:
## directly show actual treated N and prevalence for each class.
###############################################################


###############################################################
## Step 17A. Identify patient-year-class exposure
###############################################################

patient_year_class_exposure <- antiHTN_relative %>%
  
  distinct(
    EMPI,
    year_before_index,
    antiHTN_class
  ) %>%
  
  mutate(
    class_exposed = 1L
  )


###############################################################
## Step 17B. Create observable patient-year × class grid
##
## IMPORTANT:
## Only patient-years with EHR observation are included.
###############################################################

class_names <- c(
  "ARB",
  "CCB",
  "ACEi",
  "BBL",
  "Diuretic"
)


observable_class_grid <- patient_year_trajectory %>%
  
  filter(
    observed_in_EHR
  ) %>%
  
  select(
    EMPI,
    PD_status,
    year_before_index
  ) %>%
  
  tidyr::crossing(
    antiHTN_class =
      class_names
  )


###############################################################
## Step 17C. Add class-specific exposure
###############################################################

class_patient_year <- observable_class_grid %>%
  
  left_join(
    
    patient_year_class_exposure,
    
    by = c(
      "EMPI",
      "year_before_index",
      "antiHTN_class"
    )
    
  ) %>%
  
  mutate(
    
    class_exposed =
      coalesce(
        class_exposed,
        0L
      )
  )


###############################################################
## Step 17D. Class-specific summary
##
## N_observed = people observable in that year
## N_treated  = people with that antihypertensive class
##
## Percent = N_treated / N_observed
###############################################################

class_trajectory_summary <- class_patient_year %>%
  
  group_by(
    PD_status,
    year_before_index,
    antiHTN_class
  ) %>%
  
  summarise(
    
    N_observed =
      n(),
    
    N_treated =
      sum(
        class_exposed == 1
      ),
    
    Percent_treated =
      round(
        100 *
          N_treated /
          N_observed,
        2
      ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    Group =
      if_else(
        PD_status == 1,
        "PD cases",
        "Controls"
      )
    
  ) %>%
  
  select(
    
    Group,
    
    PD_status,
    
    antiHTN_class,
    
    year_before_index,
    
    N_observed,
    
    N_treated,
    
    Percent_treated
    
  ) %>%
  
  arrange(
    antiHTN_class,
    Group,
    year_before_index
  )


###############################################################
## Step 18. Wide class-specific table
##
## Easier to visually inspect actual N changes.
###############################################################

class_trajectory_wide_N <- class_trajectory_summary %>%
  
  select(
    Group,
    antiHTN_class,
    year_before_index,
    N_treated
  ) %>%
  
  pivot_wider(
    
    names_from =
      year_before_index,
    
    values_from =
      N_treated,
    
    names_prefix =
      "Year_"
  )


class_trajectory_wide_percent <- class_trajectory_summary %>%
  
  select(
    Group,
    antiHTN_class,
    year_before_index,
    Percent_treated
  ) %>%
  
  pivot_wider(
    
    names_from =
      year_before_index,
    
    values_from =
      Percent_treated,
    
    names_prefix =
      "Year_"
  )


###############################################################
## Step 19. Formal overall mixed-effects model
###############################################################

trajectory_model_data <- patient_year_trajectory %>%
  
  filter(
    observed_in_EHR,
    !is.na(
      n_antihypertensive_classes
    )
  )


if (
  requireNamespace(
    "lme4",
    quietly = TRUE
  ) &&
  requireNamespace(
    "broom.mixed",
    quietly = TRUE
  )
) {
  
  trajectory_model <- lme4::lmer(
    
    n_antihypertensive_classes ~
      
      PD_status *
      year_before_index +
      
      Age +
      Gender +
      factor(Race_Category) +
      
      (1 | EMPI),
    
    data =
      trajectory_model_data
  )
  
  
  trajectory_model_summary <-
    broom.mixed::tidy(
      
      trajectory_model,
      
      effects =
        "fixed",
      
      conf.int =
        TRUE
    )
  
} else {
  
  trajectory_model <- NULL
  
  trajectory_model_summary <- tibble(
    
    note =
      "Install lme4 and broom.mixed to run mixed model."
  )
}


###############################################################
## Step 20. Auxiliary model:
## Any antihypertensive treatment
###############################################################

if (
  requireNamespace(
    "lme4",
    quietly = TRUE
  ) &&
  requireNamespace(
    "broom.mixed",
    quietly = TRUE
  )
) {
  
  any_antiHTN_model <- lme4::glmer(
    
    any_antihypertensive ~
      
      PD_status *
      year_before_index +
      
      Age +
      Gender +
      factor(Race_Category) +
      
      (1 | EMPI),
    
    data =
      trajectory_model_data,
    
    family =
      binomial()
  )
  
  
  any_antiHTN_model_summary <-
    broom.mixed::tidy(
      
      any_antiHTN_model,
      
      effects =
        "fixed",
      
      conf.int =
        TRUE,
      
      exponentiate =
        TRUE
    )
  
} else {
  
  any_antiHTN_model <- NULL
  
  any_antiHTN_model_summary <-
    tibble(
      
      note =
        "Install lme4 and broom.mixed to run mixed logistic model."
    )
}


###############################################################
## Step 21. Faster class-specific trajectory models
###############################################################

class_model_results <- list()

for (current_class in class_names) {
  
  class_model_data <- class_patient_year %>%
    filter(
      antiHTN_class == current_class
    ) %>%
    left_join(
      trajectory_index %>%
        select(
          EMPI,
          Age,
          Gender,
          Race_Category
        ),
      by = "EMPI"
    )
  
  model_i <- glm(
    
    class_exposed ~
      
      PD_status *
      year_before_index +
      
      Age +
      Gender +
      factor(Race_Category),
    
    data = class_model_data,
    
    family = binomial()
  )
  
  
  model_table_i <- broom::tidy(
    model_i,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    mutate(
      antiHTN_class = current_class
    )
  
  
  class_model_results[[current_class]] <-
    model_table_i
}


class_model_summary <- bind_rows(
  class_model_results
)


class_interaction_summary <- class_model_summary %>%
  filter(
    term == "PD_status:year_before_index"
  ) %>%
  select(
    antiHTN_class,
    estimate,
    conf.low,
    conf.high,
    p.value
  ) %>%
  rename(
    Interaction_OR = estimate,
    CI_lower = conf.low,
    CI_upper = conf.high
  )

###############################################################
## Step 22. Extract interaction terms only
##
## This gives a compact reviewer-facing table.
###############################################################

if (
  "term" %in%
  names(
    class_model_summary
  )
) {
  
  class_interaction_summary <-
    class_model_summary %>%
    
    filter(
      term ==
        "PD_status:year_before_index"
    ) %>%
    
    select(
      
      antiHTN_class,
      
      estimate,
      
      std.error,
      
      conf.low,
      
      conf.high
      
    ) %>%
    
    rename(
      
      Interaction_OR =
        estimate,
      
      CI_lower =
        conf.low,
      
      CI_upper =
        conf.high
    )
  
} else {
  
  class_interaction_summary <-
    class_model_summary
}


###############################################################
## Step 23. Year -5 to Year -1 overall change
##
## Both years must be observable.
###############################################################

patient_change <- patient_year_trajectory %>%
  
  filter(
    year_before_index %in%
      c(-5, -1),
    
    observed_in_EHR
  ) %>%
  
  mutate(
    
    year_label =
      case_when(
        
        year_before_index == -5 ~
          "year_m5",
        
        year_before_index == -1 ~
          "year_m1"
      )
  ) %>%
  
  select(
    
    EMPI,
    
    PD_status,
    
    year_label,
    
    n_antihypertensive_classes
  ) %>%
  
  pivot_wider(
    
    names_from =
      year_label,
    
    values_from =
      n_antihypertensive_classes
  ) %>%
  
  filter(
    
    !is.na(year_m5),
    
    !is.na(year_m1)
  ) %>%
  
  mutate(
    
    change_minus5_to_minus1 =
      year_m1 -
      year_m5
  )


change_summary <- patient_change %>%
  
  group_by(
    PD_status
  ) %>%
  
  summarise(
    
    N =
      n(),
    
    mean_change =
      mean(
        change_minus5_to_minus1,
        na.rm = TRUE
      ),
    
    median_change =
      median(
        change_minus5_to_minus1,
        na.rm = TRUE
      ),
    
    percent_decreased =
      round(
        100 *
          mean(
            change_minus5_to_minus1 < 0,
            na.rm = TRUE
          ),
        2
      ),
    
    percent_unchanged =
      round(
        100 *
          mean(
            change_minus5_to_minus1 == 0,
            na.rm = TRUE
          ),
        2
      ),
    
    percent_increased =
      round(
        100 *
          mean(
            change_minus5_to_minus1 > 0,
            na.rm = TRUE
          ),
        2
      ),
    
    .groups =
      "drop"
  )


###############################################################
## Step 24. Plot OVERALL treatment intensity
###############################################################

trajectory_plot_data <- trajectory_summary %>%
  
  mutate(
    
    Group =
      if_else(
        PD_status == 1,
        "PD cases",
        "Controls"
      )
  )


p_intensity <- ggplot(
  
  trajectory_plot_data,
  
  aes(
    
    x =
      year_before_index,
    
    y =
      mean_classes,
    
    group =
      Group,
    
    linetype =
      Group,
    
    shape =
      Group
  )
  
) +
  
  geom_ribbon(
    
    aes(
      ymin =
        CI_lower,
      
      ymax =
        CI_upper,
      
      fill =
        Group
    ),
    
    alpha =
      0.12,
    
    color =
      NA
  ) +
  
  geom_line(
    linewidth =
      1
  ) +
  
  geom_point(
    size =
      2.8
  ) +
  
  scale_x_continuous(
    
    breaks =
      -5:-1,
    
    labels =
      c(
        "−5",
        "−4",
        "−3",
        "−2",
        "−1"
      )
  ) +
  
  labs(
    
    x =
      "Years before index date",
    
    y =
      "Mean number of antihypertensive classes",
    
    linetype =
      NULL,
    
    shape =
      NULL,
    
    fill =
      NULL
  ) +
  
  theme_classic(
    base_size =
      13
  ) +
  
  theme(
    legend.position =
      "top"
  )


print(
  p_intensity
)


###############################################################
## Step 25. Plot CLASS-SPECIFIC prevalence
###############################################################

p_class_prevalence <- ggplot(
  
  class_trajectory_summary,
  
  aes(
    
    x =
      year_before_index,
    
    y =
      Percent_treated,
    
    group =
      Group,
    
    linetype =
      Group,
    
    shape =
      Group
  )
  
) +
  
  geom_line(
    linewidth =
      0.9
  ) +
  
  geom_point(
    size =
      2.3
  ) +
  
  facet_wrap(
    ~ antiHTN_class,
    scales =
      "free_y"
  ) +
  
  scale_x_continuous(
    
    breaks =
      -5:-1,
    
    labels =
      c(
        "−5",
        "−4",
        "−3",
        "−2",
        "−1"
      )
  ) +
  
  labs(
    
    x =
      "Years before index date",
    
    y =
      "Patients treated (%)",
    
    linetype =
      NULL,
    
    shape =
      NULL
  ) +
  
  theme_classic(
    base_size =
      12
  ) +
  
  theme(
    
    legend.position =
      "top",
    
    strip.background =
      element_blank(),
    
    strip.text =
      element_text(
        face = "bold"
      )
  )


print(
  p_class_prevalence
)


###############################################################
## Step 26. Save figures
###############################################################

ggsave(
  
  "./data/results/Validation2_antihypertensive_intensity_trajectory.pdf",
  
  p_intensity,
  
  width =
    7,
  
  height =
    5
)


ggsave(
  
  "./data/results/Validation2_antihypertensive_class_trajectory.pdf",
  
  p_class_prevalence,
  
  width =
    9,
  
  height =
    6
)


###############################################################
## Step 27. Save validation workbook
###############################################################

write.xlsx(
  
  list(
    HC_Index_QC = HC_index_QC_validation2,
    Index_QC = trajectory_index_QC,
    
    TTE_drug_definition =
      antihypertensive_dictionary,
    
    Observation_QC =
      observation_QC,
    
    Overall_trajectory =
      trajectory_wide,
    
    Class_trajectory =
      class_trajectory_summary,
    
    Class_N_wide =
      class_trajectory_wide_N,
    
    Class_percent_wide =
      class_trajectory_wide_percent,
    
    Overall_mixed_model =
      trajectory_model_summary,
    
    Any_antiHTN_model =
      any_antiHTN_model_summary,
    
    Class_models =
      class_model_summary,
    
    Class_interactions =
      class_interaction_summary,
    
    Change_5y_to_1y =
      change_summary,
    
    Patient_year =
      patient_year_trajectory
    
  ),
  
  "./data/results/Validation2_antihypertensive_5yr_trajectory_by_class.xlsx",
  
  overwrite =
    TRUE
)


###############################################################
## Step 28. Console output
###############################################################

cat("\n")
cat("====================================================\n")
cat("VALIDATION 2: ANTIHYPERTENSIVE TRAJECTORY\n")
cat("OVERALL + CLASS-SPECIFIC ANALYSIS\n")
cat("====================================================\n")


cat("\nEHR observation coverage:\n")

print(
  observation_QC
)


cat("\nOverall antihypertensive trajectory:\n")

print(
  trajectory_wide
)


cat("\n====================================================\n")
cat("ACTUAL NUMBER OF PATIENTS USING EACH CLASS\n")
cat("====================================================\n")

print(
  class_trajectory_wide_N
)


cat("\n====================================================\n")
cat("PERCENT OF OBSERVABLE PATIENTS USING EACH CLASS\n")
cat("====================================================\n")

print(
  class_trajectory_wide_percent
)


cat("\n====================================================\n")
cat("FULL CLASS-SPECIFIC TRAJECTORY\n")
cat("N_observed + N_treated + Percent\n")
cat("====================================================\n")

print(
  class_trajectory_summary,
  n = Inf
)


cat("\nOverall mixed model:\n")

print(
  trajectory_model_summary
)


cat("\nClass-specific interaction models:\n")

print(
  class_interaction_summary
)


cat("\nYear -5 to Year -1 overall change:\n")

print(
  change_summary
)

cat("\nHC index QC:\n")
print(HC_index_QC_validation2)

cat("\nFinal trajectory index QC:\n")
print(trajectory_index_QC)

cat("====================================================\n")