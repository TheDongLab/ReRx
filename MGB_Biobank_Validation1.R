###############################################################
## VALIDATION 1
##
## Anti-PD medication timing relative to first PD diagnosis
##
## Analysis population:
## FINAL PD cases included in the primary drug-wide analysis
###############################################################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(openxlsx)

setwd("$HOME/PROJECT_FOLDER")

###############################################################
## Step 1. Define FINAL PD analytic cohort
###############################################################

final_PD_ids <- PD_risk_all_matched %>%
  filter(
    PD_status == 1
  ) %>%
  transmute(
    EMPI = as.character(EMPI)
  ) %>%
  distinct()

###############################################################
## Step 2. Final PD timing backbone
##
## Directly use PD_index_primary generated in the
## revised drug-wide screening script
###############################################################

PD_timing_base_final <- PD_index_primary %>%
  mutate(
    EMPI = as.character(EMPI)
  ) %>%
  semi_join(
    final_PD_ids,
    by = "EMPI"
  ) %>%
  transmute(
    EMPI,
    first_PD_code_date = index_date,
    first_PD_code,
    first_PD_code_type
  ) %>%
  distinct()

###############################################################
## Step 3. Use COMPLETE medication history
##
## Important:
## Do NOT use only pre-index medication records here.
###############################################################

PD_medication_history_final <- drug_library_separated_MGB %>%
  transmute(
    medication_record_id,
    EMPI = as.character(EMPI),
    Medication_Date = as.Date(Medication_Date),
    DrugName = str_to_title(
      str_trim(DrugName)
    )
  ) %>%
  semi_join(
    final_PD_ids,
    by = "EMPI"
  ) %>%
  filter(
    !is.na(Medication_Date),
    !is.na(DrugName),
    DrugName != ""
  )


###############################################################
## Step 4. Anti-PD medication dictionary
###############################################################

anti_PD_dictionary <- tibble::tribble(
  
  ~DrugName,       ~anti_PD_class,
  
  ## Levodopa
  "Levodopa",      "N04BA (Dopa and dopa derivatives)",
  
  ## Dopamine agonists
  "Pramipexole",   "N04BC (Dopamine agonist)",
  "Ropinirole",    "N04BC (Dopamine agonist)",
  "Rotigotine",    "N04BC (Dopamine agonist)",
  "Apomorphine",   "N04BC (Dopamine agonist)",
  "Bromocriptine", "N04BC (Dopamine agonist)",
  "Cabergoline",   "N04BC (Dopamine agonist)",
  "Pergolide",     "N04BC (Dopamine agonist)",
  "Piribedil",     "N04BC (Dopamine agonist)",
  
  ## MAO-B inhibitors
  "Rasagiline",    "N04BD (MAO-B inhibitor)",
  "Selegiline",    "N04BD (MAO-B inhibitor)",
  "Safinamide",    "N04BD (MAO-B inhibitor)",
  
  ## COMT inhibitors
  "Entacapone",    "N04BX (COMT inhibitor)",
  "Tolcapone",     "N04BX (COMT inhibitor)",
  "Opicapone",     "N04BX (COMT inhibitor)",
  
  ## Amantadine
  "Amantadine",    "N04BB (Adamantane derivatives)"
)


###############################################################
## Step 5. Extract anti-PD medication history
###############################################################

anti_PD_history_final <- PD_medication_history_final %>%
  inner_join(
    anti_PD_dictionary,
    by = "DrugName"
  ) %>%
  distinct(
    EMPI,
    Medication_Date,
    DrugName,
    anti_PD_class,
    .keep_all = TRUE
  )


###############################################################
## Step 6. Medication coverage
###############################################################

PD_any_medication_final <- PD_medication_history_final %>%
  distinct(EMPI) %>%
  mutate(
    has_any_medication = TRUE
  )


###############################################################
## Step 7. First ANY anti-PD medication
###############################################################

first_any_anti_PD_final <- anti_PD_history_final %>%
  group_by(EMPI) %>%
  summarise(
    
    first_anti_PD_date =
      min(
        Medication_Date,
        na.rm = TRUE
      ),
    
    first_anti_PD_drugs =
      paste(
        sort(
          unique(
            DrugName[
              Medication_Date ==
                min(Medication_Date, na.rm = TRUE)
            ]
          )
        ),
        collapse = " / "
      ),
    
    first_anti_PD_classes =
      paste(
        sort(
          unique(
            anti_PD_class[
              Medication_Date ==
                min(Medication_Date, na.rm = TRUE)
            ]
          )
        ),
        collapse = " / "
      ),
    
    .groups = "drop"
  )


###############################################################
## Step 8. First medication date by anti-PD class
###############################################################

first_anti_PD_by_class_final <- anti_PD_history_final %>%
  group_by(
    EMPI,
    anti_PD_class
  ) %>%
  summarise(
    
    first_class_date =
      min(
        Medication_Date,
        na.rm = TRUE
      ),
    
    first_class_drug =
      paste(
        sort(
          unique(
            DrugName[
              Medication_Date ==
                min(Medication_Date, na.rm = TRUE)
            ]
          )
        ),
        collapse = " / "
      ),
    
    .groups = "drop"
  )


###############################################################
## Step 9. Reviewer PRIMARY definition:
## Levodopa OR dopamine agonist
###############################################################

reviewer_primary_history_final <- anti_PD_history_final %>%
  filter(
    anti_PD_class %in%
      c(
        "N04BA (Dopa and dopa derivatives)",
        "N04BC (Dopamine agonist)"
      )
  )


first_reviewer_dopaminergic_final <-
  reviewer_primary_history_final %>%
  group_by(EMPI) %>%
  summarise(
    
    first_dopaminergic_date =
      min(
        Medication_Date,
        na.rm = TRUE
      ),
    
    first_dopaminergic_drug =
      paste(
        sort(
          unique(
            DrugName[
              Medication_Date ==
                min(Medication_Date, na.rm = TRUE)
            ]
          )
        ),
        collapse = " / "
      ),
    
    .groups = "drop"
  )


###############################################################
## Step 10. Cohort medication coverage
###############################################################

PD_medication_coverage_final <- PD_timing_base_final %>%
  
  left_join(
    PD_any_medication_final,
    by = "EMPI"
  ) %>%
  
  left_join(
    first_any_anti_PD_final %>%
      select(
        EMPI,
        first_anti_PD_date
      ),
    by = "EMPI"
  ) %>%
  
  left_join(
    first_reviewer_dopaminergic_final %>%
      select(
        EMPI,
        first_dopaminergic_date
      ),
    by = "EMPI"
  ) %>%
  
  mutate(
    
    has_any_medication =
      coalesce(
        has_any_medication,
        FALSE
      ),
    
    has_any_anti_PD =
      !is.na(first_anti_PD_date),
    
    has_reviewer_dopaminergic =
      !is.na(first_dopaminergic_date)
  )


N_total_PD_final <-
  nrow(
    PD_medication_coverage_final
  )

N_any_medication_final <-
  sum(
    PD_medication_coverage_final$
      has_any_medication
  )

N_any_anti_PD_final <-
  sum(
    PD_medication_coverage_final$
      has_any_anti_PD
  )

N_dopaminergic_final <-
  sum(
    PD_medication_coverage_final$
      has_reviewer_dopaminergic
  )


medication_coverage_summary_final <- tibble(
  
  Population = c(
    "Final primary PD cases",
    "Any medication history",
    "No medication history",
    "Any anti-PD medication",
    "Medication history but no anti-PD medication",
    "Levodopa or dopamine agonist"
  ),
  
  N = c(
    
    N_total_PD_final,
    
    N_any_medication_final,
    
    N_total_PD_final -
      N_any_medication_final,
    
    N_any_anti_PD_final,
    
    N_any_medication_final -
      N_any_anti_PD_final,
    
    N_dopaminergic_final
  )
  
) %>%
  
  mutate(
    
    Percent_of_PD =
      round(
        100 *
          N /
          N_total_PD_final,
        2
      )
  )


###############################################################
## Step 11. Anti-PD class prevalence
###############################################################

anti_PD_class_prevalence_final <- anti_PD_history_final %>%
  
  distinct(
    EMPI,
    anti_PD_class
  ) %>%
  
  count(
    anti_PD_class,
    name = "N_patients"
  ) %>%
  
  mutate(
    
    Percent_of_all_PD =
      round(
        100 *
          N_patients /
          N_total_PD_final,
        2
      ),
    
    Percent_of_medication_history =
      round(
        100 *
          N_patients /
          N_any_medication_final,
        2
      )
  ) %>%
  
  arrange(
    desc(N_patients)
  )


###############################################################
## Step 12. ANY anti-PD timing
###############################################################

any_anti_PD_timing_final <- PD_timing_base_final %>%
  
  left_join(
    first_any_anti_PD_final,
    by = "EMPI"
  ) %>%
  
  mutate(
    
    days_anti_PD_minus_PD =
      as.numeric(
        first_anti_PD_date -
          first_PD_code_date
      ),
    
    timing_group = case_when(
      
      is.na(first_anti_PD_date) ~
        "No anti-PD medication",
      
      days_anti_PD_minus_PD < 0 ~
        "Before first PD code",
      
      days_anti_PD_minus_PD == 0 ~
        "Same day",
      
      days_anti_PD_minus_PD > 0 ~
        "After first PD code"
    )
  )


any_anti_PD_timing_summary_final <-
  any_anti_PD_timing_final %>%
  
  filter(
    !is.na(first_anti_PD_date)
  ) %>%
  
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
        2
      ),
    
    Percent_of_all_PD =
      round(
        100 *
          N /
          N_total_PD_final,
        2
      )
  )


###############################################################
## Step 13. Reviewer-primary timing
###############################################################

reviewer_primary_timing_final <- PD_timing_base_final %>%
  
  left_join(
    first_reviewer_dopaminergic_final,
    by = "EMPI"
  ) %>%
  
  mutate(
    
    days_dopaminergic_minus_PD =
      as.numeric(
        first_dopaminergic_date -
          first_PD_code_date
      ),
    
    timing_group = case_when(
      
      is.na(first_dopaminergic_date) ~
        "No levodopa/dopamine agonist",
      
      days_dopaminergic_minus_PD < 0 ~
        "Before first PD code",
      
      days_dopaminergic_minus_PD == 0 ~
        "Same day",
      
      days_dopaminergic_minus_PD > 0 ~
        "After first PD code"
    )
  )


reviewer_primary_timing_summary_final <-
  reviewer_primary_timing_final %>%
  
  filter(
    !is.na(first_dopaminergic_date)
  ) %>%
  
  count(
    timing_group,
    name = "N"
  ) %>%
  
  mutate(
    
    Percent_among_treated =
      round(
        100 *
          N /
          sum(N),
        2
      ),
    
    Percent_of_all_PD =
      round(
        100 *
          N /
          N_total_PD_final,
        2
      )
  )


###############################################################
## Step 14. Reviewer-primary lead time
###############################################################

reviewer_primary_lead_time_final <-
  reviewer_primary_timing_final %>%
  
  filter(
    days_dopaminergic_minus_PD < 0
  ) %>%
  
  mutate(
    days_before_PD =
      -days_dopaminergic_minus_PD
  ) %>%
  
  summarise(
    
    N_before =
      n(),
    
    median_days_before =
      median(
        days_before_PD,
        na.rm = TRUE
      ),
    
    IQR_25 =
      quantile(
        days_before_PD,
        0.25,
        na.rm = TRUE
      ),
    
    IQR_75 =
      quantile(
        days_before_PD,
        0.75,
        na.rm = TRUE
      ),
    
    Percent_gt_30_days =
      round(
        100 *
          mean(days_before_PD > 30),
        2
      ),
    
    Percent_gt_90_days =
      round(
        100 *
          mean(days_before_PD > 90),
        2
      ),
    
    Percent_gt_180_days =
      round(
        100 *
          mean(days_before_PD > 180),
        2
      ),
    
    Percent_gt_365_days =
      round(
        100 *
          mean(days_before_PD > 365),
        2
      ),
    
    Percent_gt_730_days =
      round(
        100 *
          mean(days_before_PD > 730),
        2
      )
  )


###############################################################
## Step 15. Timing by EACH anti-PD class
###############################################################

class_timing_final <- first_anti_PD_by_class_final %>%
  
  left_join(
    PD_timing_base_final %>%
      select(
        EMPI,
        first_PD_code_date
      ),
    by = "EMPI"
  ) %>%
  
  mutate(
    
    days_class_minus_PD =
      as.numeric(
        first_class_date -
          first_PD_code_date
      ),
    
    timing_group = case_when(
      
      days_class_minus_PD < 0 ~
        "Before first PD code",
      
      days_class_minus_PD == 0 ~
        "Same day",
      
      days_class_minus_PD > 0 ~
        "After first PD code"
    )
  )


class_timing_summary_final <- class_timing_final %>%
  
  count(
    anti_PD_class,
    timing_group,
    name = "N"
  ) %>%
  
  group_by(
    anti_PD_class
  ) %>%
  
  mutate(
    
    Total_class_patients =
      sum(N),
    
    Percent =
      round(
        100 *
          N /
          Total_class_patients,
        2
      )
  ) %>%
  
  ungroup()


###############################################################
## Step 16. Pre-code lead time by class
###############################################################

class_pre_PD_lead_time_final <- class_timing_final %>%
  
  filter(
    days_class_minus_PD < 0
  ) %>%
  
  mutate(
    days_before_PD =
      -days_class_minus_PD
  ) %>%
  
  group_by(
    anti_PD_class
  ) %>%
  
  summarise(
    
    N_before =
      n(),
    
    median_days_before =
      median(
        days_before_PD,
        na.rm = TRUE
      ),
    
    IQR_25 =
      quantile(
        days_before_PD,
        0.25,
        na.rm = TRUE
      ),
    
    IQR_75 =
      quantile(
        days_before_PD,
        0.75,
        na.rm = TRUE
      ),
    
    Percent_gt_90_days =
      round(
        100 *
          mean(days_before_PD > 90),
        2
      ),
    
    Percent_gt_180_days =
      round(
        100 *
          mean(days_before_PD > 180),
        2
      ),
    
    Percent_gt_365_days =
      round(
        100 *
          mean(days_before_PD > 365),
        2
      ),
    
    Percent_gt_730_days =
      round(
        100 *
          mean(days_before_PD > 730),
        2
      ),
    
    .groups = "drop"
  )


###############################################################
## Step 17. Extreme-date QC
###############################################################

extreme_pre_PD_dates_final <- class_timing_final %>%
  
  filter(
    days_class_minus_PD < -3650
  ) %>%
  
  select(
    EMPI,
    anti_PD_class,
    first_class_drug,
    first_class_date,
    first_PD_code_date,
    days_class_minus_PD
  ) %>%
  
  arrange(
    days_class_minus_PD
  )


###############################################################
## Step 18. Save Validation 1
###############################################################

write.xlsx(
  
  list(
    
    Medication_coverage =
      medication_coverage_summary_final,
    
    Anti_PD_class_prevalence =
      anti_PD_class_prevalence_final,
    
    Any_anti_PD_timing =
      any_anti_PD_timing_summary_final,
    
    Reviewer_primary =
      reviewer_primary_timing_summary_final,
    
    Reviewer_primary_lead =
      reviewer_primary_lead_time_final,
    
    Timing_by_class =
      class_timing_summary_final,
    
    Lead_time_by_class =
      class_pre_PD_lead_time_final,
    
    Extreme_date_QC =
      extreme_pre_PD_dates_final,
    
    Patient_level =
      reviewer_primary_timing_final
    
  ),
  
  "./data/results/Validation1_PD_diagnosis_delay_final_cohort.xlsx",
  
  overwrite = TRUE
)


###############################################################
## Step 19. Console summary
###############################################################

cat("\n")
cat("====================================================\n")
cat("VALIDATION 1: PD DIAGNOSIS DELAY\n")
cat("FINAL PRIMARY ANALYTIC COHORT\n")
cat("====================================================\n")

print(
  medication_coverage_summary_final
)

print(
  reviewer_primary_timing_summary_final
)

print(
  reviewer_primary_lead_time_final
)

print(
  class_timing_summary_final
)

cat("====================================================\n")