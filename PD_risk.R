###############################################################
## MGB Biobank drug-wide screening for PD
##
## Revised analysis
##
## Main updates:
## 1. Broad PD ascertainment
## 2. Exclude PRE-INDEX dementia only for PD
## 3. Do NOT exclude secondary/atypical parkinsonism here
##    -> handled later as sensitivity analyses
## 4. Use RxNorm ingredient-level medication library
## 5. Preserve original medication_record_id
## 6. PD medication exposure must precede PD index date
## 7. Controls retain original study framework
###############################################################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(MatchIt)
library(broom)
library(openxlsx)

setwd("$HOME/PROJECT_FOLDER")


###############################################################
## Step 1. Load demographic data
###############################################################

PD_follow_up_1 <- read.table(
  "./data/MGB_Biobank/PD30K_2005P001191_20240621_170059-1/XD010_20240621_170059-1_Dem.txt",
  sep = "|",
  header = TRUE,
  fill = TRUE,
  na.strings = c("", "NA")
)

PD_follow_up_2 <- read.table(
  "./data/MGB_Biobank/PD30K_2005P001191_20240621_170059-2/XD010_20240621_170059-2_Dem.txt",
  sep = "|",
  header = TRUE,
  fill = TRUE,
  na.strings = c("", "NA")
)

PD_follow_up <- bind_rows(
  PD_follow_up_1,
  PD_follow_up_2
) %>%
  mutate(
    EMPI = as.character(EMPI)
  ) %>%
  filter(
    Age >= 30
  ) %>%
  select(
    EMPI,
    Date_of_Birth,
    Age,
    Gender_Legal_Sex,
    Race_Group
  ) %>%
  distinct(
    EMPI,
    .keep_all = TRUE
  ) %>%
  mutate(
    
    PD_status = 1,
    
    Age_Category = case_when(
      Age >= 30 & Age < 40 ~ 1,
      Age >= 40 & Age < 50 ~ 2,
      Age >= 50 & Age < 60 ~ 3,
      Age >= 60 & Age < 70 ~ 4,
      Age >= 70 & Age < 80 ~ 5,
      Age >= 80 & Age < 90 ~ 6,
      Age >= 90 ~ 7,
      TRUE ~ NA_real_
    ),
    
    Gender = case_when(
      Gender_Legal_Sex == "Male" ~ 1,
      Gender_Legal_Sex == "Female" ~ 0,
      TRUE ~ NA_real_
    ),
    
    Race_Category = case_when(
      Race_Group == "White" ~ 1,
      Race_Group == "Black" ~ 2,
      Race_Group == "Asian" ~ 3,
      Race_Group %in% c(
        "Unknown/Missing",
        "Other",
        "Declined",
        "Two or More",
        "American Indian or Alaska Native",
        "Native Hawaiian or Other Pacific Islander"
      ) ~ 4,
      TRUE ~ NA_real_
    )
  )


###############################################################
## Step 2. Load healthy-control demographic data
###############################################################

health_follow_up_1 <- read.table(
  "./data/MGB_Biobank/MGB_biobank_matched_health_control/1/XD010_20240630_131521-1_Dem.txt",
  sep = "|",
  header = TRUE,
  fill = TRUE,
  na.strings = c("", "NA")
)

health_follow_up_2 <- read.table(
  "./data/MGB_Biobank/MGB_biobank_matched_health_control/2/XD010_20240630_131521-2_Dem.txt",
  sep = "|",
  header = TRUE,
  fill = TRUE,
  na.strings = c("", "NA")
)

health_follow_up_3 <- read.table(
  "./data/MGB_Biobank/MGB_biobank_matched_health_control/3/XD010_20240630_131521-3_Dem.txt",
  sep = "|",
  header = TRUE,
  fill = TRUE,
  na.strings = c("", "NA")
)

health_follow_up <- bind_rows(
  health_follow_up_1,
  health_follow_up_2,
  health_follow_up_3
) %>%
  mutate(
    EMPI = as.character(EMPI)
  ) %>%
  filter(
    Age >= 30
  ) %>%
  select(
    EMPI,
    Date_of_Birth,
    Age,
    Gender_Legal_Sex,
    Race_Group
  ) %>%
  distinct(
    EMPI,
    .keep_all = TRUE
  ) %>%
  mutate(
    
    PD_status = 0,
    
    Age_Category = case_when(
      Age >= 30 & Age < 40 ~ 1,
      Age >= 40 & Age < 50 ~ 2,
      Age >= 50 & Age < 60 ~ 3,
      Age >= 60 & Age < 70 ~ 4,
      Age >= 70 & Age < 80 ~ 5,
      Age >= 80 & Age < 90 ~ 6,
      Age >= 90 ~ 7,
      TRUE ~ NA_real_
    ),
    
    Gender = case_when(
      Gender_Legal_Sex == "Male" ~ 1,
      Gender_Legal_Sex == "Female" ~ 0,
      TRUE ~ NA_real_
    ),
    
    Race_Category = case_when(
      Race_Group == "White" ~ 1,
      Race_Group == "Black" ~ 2,
      Race_Group == "Asian" ~ 3,
      Race_Group %in% c(
        "Unknown/Missing",
        "Other",
        "Declined",
        "Two or More",
        "American Indian or Alaska Native",
        "Native Hawaiian or Other Pacific Islander"
      ) ~ 4,
      TRUE ~ NA_real_
    )
  )


###############################################################
## Step 3. Load diagnosis data
###############################################################

PD_diagnosis_1 <- read.csv(
  "./data/MGB_Biobank/PD30K_2005P001191_20240621_170059-1/XD010_20240621_170059-1_Dia.txt",
  sep = "|",
  header = TRUE,
  fill = TRUE,
  stringsAsFactors = FALSE
)

PD_diagnosis_2 <- read.csv(
  "./data/MGB_Biobank/PD30K_2005P001191_20240621_170059-2/XD010_20240621_170059-2_Dia.txt",
  sep = "|",
  header = TRUE,
  fill = TRUE,
  stringsAsFactors = FALSE
)

PD_diagnosis <- bind_rows(
  PD_diagnosis_1,
  PD_diagnosis_2
) %>%
  mutate(
    EMPI = as.character(EMPI),
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Code = toupper(trimws(as.character(Code))),
    Code_Type = toupper(trimws(as.character(Code_Type)))
  )


health_diagnosis_1 <- read.csv(
  "./data/MGB_Biobank/MGB_biobank_matched_health_control/1/XD010_20240630_131521-1_Dia.txt",
  sep = "|",
  header = TRUE,
  fill = TRUE,
  stringsAsFactors = FALSE
)

health_diagnosis_2 <- read.csv(
  "./data/MGB_Biobank/MGB_biobank_matched_health_control/2/XD010_20240630_131521-2_Dia.txt",
  sep = "|",
  header = TRUE,
  fill = TRUE,
  stringsAsFactors = FALSE
)

health_diagnosis_3 <- read.csv(
  "./data/MGB_Biobank/MGB_biobank_matched_health_control/3/XD010_20240630_131521-3_Dia.txt",
  sep = "|",
  header = TRUE,
  fill = TRUE,
  stringsAsFactors = FALSE
)

health_diagnosis <- bind_rows(
  health_diagnosis_1,
  health_diagnosis_2,
  health_diagnosis_3
) %>%
  mutate(
    EMPI = as.character(EMPI),
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Code = toupper(trimws(as.character(Code))),
    Code_Type = toupper(trimws(as.character(Code_Type)))
  )


###############################################################
## Step 4. Broad primary PD definition
###############################################################

PD_codes <- PD_diagnosis %>%
  filter(
    
    (Code_Type == "ICD9" &
       Code %in% c("332", "332.0")) |
      
      (Code_Type == "ICD10" &
         grepl("^G20", Code)) |
      
      (Code_Type == "LMR" &
         Code %in% c("LPA312", "LPA883")) |
      
      (Code_Type == "ONCALL" &
         Code == "WHJM1") |
      
      (Code_Type == "DSM4" &
         Code == "332..3")
    
  ) %>%
  filter(
    !is.na(Date)
  ) %>%
  distinct()


###############################################################
## Step 5. Define patient-level PD index date
###############################################################

PD_index <- PD_codes %>%
  group_by(EMPI) %>%
  arrange(Date, .by_group = TRUE) %>%
  summarise(
    
    index_date =
      first(Date),
    
    first_PD_code =
      first(Code),
    
    first_PD_code_type =
      first(Code_Type),
    
    .groups = "drop"
  )


###############################################################
## Step 6. Define dementia codes
##
## IMPORTANT:
## dementia only; do NOT include G21 / 332.1 here.
###############################################################

PD_dementia_records <- PD_diagnosis %>%
  filter(
    
    (
      Code_Type == "ICD9" &
        Code %in% c(
          "290.0", "290.10", "290.11", "290.12",
          "290.20", "290.21", "290.3", "290.4",
          "290.40", "290.41", "290.42", "290.43",
          "294.1", "294.10", "294.11",
          "294.20", "294.21",
          "331.19", "331.82"
        )
    ) |
      
      (
        Code_Type == "ICD10" &
          (
            grepl("^F01", Code) |
              grepl("^F02", Code) |
              grepl("^F03", Code) |
              Code %in% c(
                "G31.09",
                "G31.83",
                "F10.27",
                "F10.97",
                "F13.27"
              )
          )
      ) |
      
      (
        Code_Type == "LMR" &
          Code == "LPA99"
      ) |
      
      (
        Code_Type == "ONCALL" &
          Code == "YHAL6"
      ) |
      
      (
        Code_Type == "DSM4" &
          Code %in% c(
            "294.10.1",
            "290.10.1",
            "290.0.1",
            "290.42.1",
            "290.40.1",
            "290.43.1"
          )
      )
    
  ) %>%
  filter(
    !is.na(Date)
  )


###############################################################
## Step 7. Identify PRE-INDEX dementia
###############################################################

PD_pre_index_dementia <- PD_dementia_records %>%
  inner_join(
    PD_index %>%
      select(
        EMPI,
        index_date
      ),
    by = "EMPI"
  ) %>%
  filter(
    Date <= index_date
  ) %>%
  distinct(
    EMPI
  )


###############################################################
## Step 8. Final primary PD cohort
###############################################################

PD_index_primary <- PD_index %>%
  anti_join(
    PD_pre_index_dementia,
    by = "EMPI"
  )


###############################################################
## Step 9. Healthy-control neurological exclusion
##
## Keep your original broad healthy-control exclusion here.
##
## Because your primary screening retains the original HC
## framework, we do not assign a new matched pseudo-index date.
###############################################################

health_diagnosis_removed <- health_diagnosis %>%
  filter(
    
    ###########################################################
    ## Paste your EXISTING healthy-control exclusion filter
    ## here unchanged.
    ##
    ## Use the exact block from your previous script.
    ###########################################################
    
    (Code_Type == "ICD9" &
       Code %in% c(
         "806.00", "806.04", "806.05", "806.09",
         "806.1", "806.10", "806.15", "806.20",
         "806.24", "806.25", "806.29", "806.30",
         "806.35", "806.4", "806.5", "806.60",
         "806.69", "806.70", "806.79", "806.8",
         "806.9", "907.2",
         "952.0", "952.00", "952.04", "952.05",
         "952.09", "952.10", "952.15", "952.19",
         "952.2", "952.3", "952.4", "952.8",
         "952.9", "V15.52",
         "649.40", "649.41", "649.43",
         "331.0", "332", "332.0", "332.1",
         "335.20", "340",
         "300.4", "299.0", "299.00", "299.01",
         "314.01", "314.00",
         "290.0", "290.10", "290.11", "290.12",
         "290.20", "290.21", "290.3", "290.4",
         "290.40", "290.41", "290.42", "290.43",
         "294.1", "294.10", "294.11",
         "294.20", "294.21", "331.19", "331.82"
       )
    ) |
      
      (Code_Type == "ICD9" & grepl("^345", Code)) |
      (Code_Type == "ICD9" & grepl("^295", Code)) |
      (Code_Type == "ICD9" & grepl("^296", Code)) |
      
      (Code_Type == "ICD10" &
         Code %in% c(
           "G46.3", "Z13.850", "Z87.820",
           "G12.21", "G35",
           "F32.A", "F53.0", "F25.0", "F19.94",
           "F06.30", "F06.34", "F06.32", "F06.31",
           "F10.94", "F10.24",
           "F90.2", "F90.8", "F90.1", "F90.0", "F90.9",
           "G31.09", "G31.83"
         )
      ) |
      
      (Code_Type == "ICD10" & grepl("^F01", Code)) |
      (Code_Type == "ICD10" & grepl("^F02", Code)) |
      (Code_Type == "ICD10" & grepl("^F03", Code)) |
      
      (Code_Type == "ICD10" & grepl("^S06.2X", Code)) |
      (Code_Type == "ICD10" & grepl("^S06.30", Code)) |
      (Code_Type == "ICD10" & grepl("^G40", Code)) |
      (Code_Type == "ICD10" & grepl("^G30", Code)) |
      (Code_Type == "ICD10" & grepl("^G20", Code)) |
      (Code_Type == "ICD10" & grepl("^G21", Code)) |
      (Code_Type == "ICD10" & grepl("^F20", Code)) |
      (Code_Type == "ICD10" & grepl("^F31", Code)) |
      
      (Code_Type == "APDRG" &
         Code %in% c(
           "014", "013",
           "043", "750", "753", "754"
         )
      ) |
      
      (Code_Type == "LMR" &
         Code %in% c(
           "LPA1429", "LPA894", "LPA1009", "LPA867",
           "LPA312", "LPA883", "LPA884", "LPA585",
           "LPA268", "LPA101", "LPA487", "LPA44",
           "LPA999", "LPA1234", "LPA604",
           "LPA1513", "LPA99"
         )
      ) |
      
      (Code_Type == "ONCALL" &
         Code %in% c(
           "NLGD2", "NLPR2", "WHCE1", "WHCQ9",
           "WHMT3", "WHJM1", "MHJA5", "WHEQ1",
           "YJSN1", "YLAB2", "YJSD3", "YJBG1",
           "YJCC3", "YJCM1", "YJHP1", "YLCE9",
           "YKKT3", "YGAA1", "YHAL6"
         )
      ) |
      
      (Code_Type == "DSM4" &
         Code %in% c(
           "345.4.3", "345.9.3",
           "331.0.3", "332..3",
           "295.9.3", "295.3.3",
           "296.90.1", "292.84.1",
           "294.10.1",
           "290.10.1", "290.0.1",
           "290.42.1", "290.40.1", "290.43.1"
         )
      ) |
      
      (Code_Type == "DSM4" &
         grepl("^296", Code))
    
  ) %>%
  distinct(
    EMPI
  )


health_follow_up_primary <- health_follow_up %>%
  anti_join(
    health_diagnosis_removed,
    by = "EMPI"
  )


###############################################################
## Step 10. Restrict PD demographic table to eligible PD cases
###############################################################

PD_follow_up_primary <- PD_follow_up %>%
  inner_join(
    PD_index_primary,
    by = "EMPI"
  )


###############################################################
## Step 11. Load NEW RxNorm ingredient-level medication library
###############################################################

drug_library_separated_MGB <- readRDS(
  "./data/EBioMedicine_revision/processed_data/MGB_Biobank/RxNorm/drug_library_separated_MGB_RxNorm.rds"
)

drug_library_separated_MGB <- drug_library_separated_MGB %>%
  mutate(
    EMPI =
      as.character(EMPI),
    
    Medication_Date =
      as.Date(Medication_Date),
    
    DrugName =
      str_to_title(
        str_trim(DrugName)
      )
  )


###############################################################
## Step 12. Apply exposure timing
##
## PD:
## medication must precede PD index date
##
## Controls:
## retain medication history under original screening framework
###############################################################

PD_medications_pre_index <- drug_library_separated_MGB %>%
  inner_join(
    PD_index_primary %>%
      select(
        EMPI,
        index_date
      ),
    by = "EMPI"
  ) %>%
  filter(
    !is.na(Medication_Date),
    Medication_Date < index_date
  )


HC_medications <- drug_library_separated_MGB %>%
  semi_join(
    health_follow_up_primary %>%
      select(EMPI),
    by = "EMPI"
  ) %>%
  filter(
    !is.na(Medication_Date)
  )


###############################################################
## Step 13. Combine medication exposure histories
###############################################################

Medication_history_primary <- bind_rows(
  
  PD_medications_pre_index %>%
    select(
      medication_record_id,
      EMPI,
      Medication_Date,
      DrugName
    ),
  
  HC_medications %>%
    select(
      medication_record_id,
      EMPI,
      Medication_Date,
      DrugName
    )
)


###############################################################
## Step 14. Define medication exposure
##
## IMPORTANT:
## Count distinct ORIGINAL medication records.
##
## This prevents combination-drug splitting from inflating
## prescription counts.
###############################################################

drug_exposure_patient <- Medication_history_primary %>%
  group_by(
    EMPI,
    DrugName
  ) %>%
  summarise(
    
    n_medication_records =
      n_distinct(
        medication_record_id
      ),
    
    n_medication_dates =
      n_distinct(
        Medication_Date
      ),
    
    .groups = "drop"
  )


###############################################################
## Step 15. Primary exposure rule
##
## Keep >=1 record to reproduce original screening.
##
## Later sensitivity:
## >=2 distinct dates
###############################################################

drug_exposure_primary <- drug_exposure_patient %>%
  filter(
    n_medication_records >= 1
  )


###############################################################
## Step 16. Restrict to patients with usable medication history
##
## PD:
## at least one mapped medication record BEFORE PD index
##
## HC:
## at least one mapped medication record
###############################################################

PD_with_medication <- PD_medications_pre_index %>%
  distinct(EMPI)

HC_with_medication <- HC_medications %>%
  distinct(EMPI)


###############################################################
## Step 17. Restrict demographic cohorts BEFORE matching
###############################################################

PD_follow_up_analysis <- PD_follow_up_primary %>%
  semi_join(
    PD_with_medication,
    by = "EMPI"
  )

health_follow_up_analysis <- health_follow_up_primary %>%
  semi_join(
    HC_with_medication,
    by = "EMPI"
  )


###############################################################
## Step 18. Medication-coverage QC before matching
###############################################################

medication_coverage_before_matching <- tibble(
  
  Group = c(
    "PD eligible before medication requirement",
    "PD with pre-index medication history",
    "HC eligible before medication requirement",
    "HC with medication history"
  ),
  
  N = c(
    n_distinct(PD_follow_up_primary$EMPI),
    n_distinct(PD_follow_up_analysis$EMPI),
    n_distinct(health_follow_up_primary$EMPI),
    n_distinct(health_follow_up_analysis$EMPI)
  )
)

print(
  medication_coverage_before_matching
)


###############################################################
## Step 19. Build source cohort for matching
###############################################################

PD_risk_primary <- bind_rows(
  
  PD_follow_up_analysis %>%
    select(
      EMPI,
      Date_of_Birth,
      Age,
      Gender,
      Race_Category,
      Age_Category,
      PD_status
    ),
  
  health_follow_up_analysis %>%
    select(
      EMPI,
      Date_of_Birth,
      Age,
      Gender,
      Race_Category,
      Age_Category,
      PD_status
    )
  
) %>%
  
  distinct(
    EMPI,
    .keep_all = TRUE
  ) %>%
  
  filter(
    !is.na(Age),
    !is.na(Gender),
    !is.na(Race_Category)
  )


###############################################################
## Step 20. Demographic matching
##
## Preserve original 1:2 framework.
###############################################################

set.seed(42)

match_model <- matchit(
  
  PD_status ~
    Age +
    Gender +
    Race_Category,
  
  data =
    PD_risk_primary,
  
  method =
    "nearest",
  
  ratio =
    2,
  
  caliper =
    0.1
)

PD_risk_all_matched <- match.data(
  match_model
) %>%
  mutate(
    EMPI = as.character(EMPI)
  )


###############################################################
## Step 21. Matched cohort summary
###############################################################

cohort_summary <- PD_risk_all_matched %>%
  count(
    PD_status,
    name = "N"
  )

print(
  cohort_summary
)


###############################################################
## Step 22. Keep medication exposure only in matched cohort
###############################################################

Medication_history_PD_risk <- drug_exposure_primary %>%
  semi_join(
    PD_risk_all_matched %>%
      select(EMPI),
    by = "EMPI"
  )


###############################################################
## Step 23. Drugs eligible for screening
##
## Reproduce original threshold:
## at least 10 exposed patients overall.
###############################################################

drug_patients <- Medication_history_PD_risk %>%
  group_by(
    DrugName
  ) %>%
  summarise(
    
    PatientCount =
      n_distinct(EMPI),
    
    .groups = "drop"
  ) %>%
  filter(
    PatientCount >= 10
  )


###############################################################
## Step 24. Drug-wide logistic regression
###############################################################

result_list <- vector(
  "list",
  nrow(drug_patients)
)

for (
  i in seq_len(
    nrow(drug_patients)
  )
) {
  
  drug <-
    drug_patients$DrugName[i]
  
  
  exposed_ids <-
    Medication_history_PD_risk %>%
    filter(
      DrugName == drug
    ) %>%
    pull(EMPI) %>%
    unique()
  
  
  merged_data <- PD_risk_all_matched %>%
    mutate(
      
      drug_used =
        as.integer(
          EMPI %in%
            exposed_ids
        )
    )
  
  
  #############################################################
  ## Exposure counts
  #############################################################
  
  Y_case <-
    sum(
      merged_data$drug_used == 1 &
        merged_data$PD_status == 1
    )
  
  Y_ctrl <-
    sum(
      merged_data$drug_used == 1 &
        merged_data$PD_status == 0
    )
  
  N_case <-
    sum(
      merged_data$drug_used == 0 &
        merged_data$PD_status == 1
    )
  
  N_ctrl <-
    sum(
      merged_data$drug_used == 0 &
        merged_data$PD_status == 0
    )
  
  
  #############################################################
  ## Skip unstable drugs
  #############################################################
  
  if (
    Y_case == 0 ||
    Y_ctrl == 0
  ) {
    next
  }
  
  
  #############################################################
  ## Logistic regression
  #############################################################
  
  model <- tryCatch(
    
    glm(
      
      PD_status ~
        drug_used +
        Age +
        Gender +
        factor(Race_Category),
      
      data =
        merged_data,
      
      family =
        binomial()
    ),
    
    error =
      function(e) NULL
  )
  
  
  if (is.null(model)) {
    next
  }
  
  
  coef_table <-
    summary(model)$coefficients
  
  
  if (
    !"drug_used" %in%
    rownames(coef_table)
  ) {
    next
  }
  
  
  beta <-
    coef_table[
      "drug_used",
      "Estimate"
    ]
  
  SE <-
    coef_table[
      "drug_used",
      "Std. Error"
    ]
  
  raw_p_value <-
    coef_table[
      "drug_used",
      "Pr(>|z|)"
    ]
  
  
  #############################################################
  ## Wald CI
  #############################################################
  
  OR <-
    exp(beta)
  
  CI_lower <-
    exp(
      beta -
        1.96 * SE
    )
  
  CI_upper <-
    exp(
      beta +
        1.96 * SE
    )
  
  
  result_list[[i]] <-
    data.frame(
      
      DrugName =
        drug,
      
      OR =
        OR,
      
      CI_lower =
        CI_lower,
      
      CI_upper =
        CI_upper,
      
      p_value =
        raw_p_value,
      
      Y_case =
        Y_case,
      
      Y_ctrl =
        Y_ctrl,
      
      N_case =
        N_case,
      
      N_ctrl =
        N_ctrl
    )
}


###############################################################
## Step 25. Combine results and calculate FDR
###############################################################

result_df <- bind_rows(
  result_list
) %>%
  mutate(
    
    fdr_value =
      p.adjust(
        p_value,
        method = "fdr"
      )
    
  ) %>%
  arrange(
    fdr_value,
    p_value
  )


###############################################################
## Step 26. Add exposure prevalence
###############################################################

result_df <- result_df %>%
  mutate(
    
    case_exposure_percent =
      round(
        100 *
          Y_case /
          (
            Y_case +
              N_case
          ),
        2
      ),
    
    control_exposure_percent =
      round(
        100 *
          Y_ctrl /
          (
            Y_ctrl +
              N_ctrl
          ),
        2
      )
  )


###############################################################
## Step 27. Save full drug-wide results
###############################################################

write.xlsx(result_df, "./data/results/PD_risk_results_MGB_RxNorm_revised_medication_eligible.xlsx", rowNames = FALSE, overwrite = TRUE)


###############################################################
## Step 28. Extract Losartan and Amlodipine
###############################################################

key_drug_results <- result_df %>%
  filter(
    DrugName %in%
      c(
        "Losartan",
        "Amlodipine"
      )
  )

print(
  key_drug_results
)


###############################################################
## Step 29. Final summary
###############################################################

cat("\n")
cat("====================================================\n")
cat("MGB revised drug-wide screening complete\n")
cat("====================================================\n")

cat("\nMedication coverage before matching:\n")
print(
  medication_coverage_before_matching
)

cat("\nMatched cohort:\n")
print(
  cohort_summary
)

cat(
  "\nNumber of drugs screened:",
  nrow(result_df),
  "\n"
)

cat("\nLosartan / Amlodipine:\n")
print(
  key_drug_results
)

cat("====================================================\n")



###############################################################
## Updated MGB demographic characteristics table
## Based on FINAL matched cohort: PD_risk_all_matched
###############################################################

library(dplyr)
library(tidyr)

demo_table_data <- PD_risk_all_matched %>%
  mutate(
    Group = if_else(PD_status == 1, "Cases", "Controls"),
    
    Age_group = case_when(
      Age >= 30 & Age < 60 ~ "30–59",
      Age >= 60 ~ "≥60",
      TRUE ~ NA_character_
    ),
    
    Sex = case_when(
      Gender == 1 ~ "Male",
      Gender == 0 ~ "Female",
      TRUE ~ NA_character_
    ),
    
    Race_group = case_when(
      Race_Category == 1 ~ "White",
      Race_Category %in% c(2, 3, 4) ~ "Non-white",
      TRUE ~ NA_character_
    )
  )


###############################################################
## Cohort sizes
###############################################################

N_case <- sum(demo_table_data$PD_status == 1)
N_ctrl <- sum(demo_table_data$PD_status == 0)
N_total <- nrow(demo_table_data)


###############################################################
## Helper function: n (%)
###############################################################

format_n_pct <- function(n, denom) {
  sprintf("%s (%.1f)", format(n, big.mark = ","), 100 * n / denom)
}


###############################################################
## Age
###############################################################

age_counts <- demo_table_data %>%
  count(Group, Age_group) %>%
  pivot_wider(
    names_from = Group,
    values_from = n,
    values_fill = 0
  )

age_total <- demo_table_data %>%
  count(Age_group, name = "Total")

age_counts <- age_counts %>%
  left_join(age_total, by = "Age_group")

age_p <- chisq.test(
  table(demo_table_data$PD_status,
        demo_table_data$Age_group)
)$p.value


###############################################################
## Sex
###############################################################

sex_counts <- demo_table_data %>%
  count(Group, Sex) %>%
  pivot_wider(
    names_from = Group,
    values_from = n,
    values_fill = 0
  )

sex_total <- demo_table_data %>%
  count(Sex, name = "Total")

sex_counts <- sex_counts %>%
  left_join(sex_total, by = "Sex")

sex_p <- chisq.test(
  table(demo_table_data$PD_status,
        demo_table_data$Sex)
)$p.value


###############################################################
## Race
###############################################################

race_counts <- demo_table_data %>%
  count(Group, Race_group) %>%
  pivot_wider(
    names_from = Group,
    values_from = n,
    values_fill = 0
  )

race_total <- demo_table_data %>%
  count(Race_group, name = "Total")

race_counts <- race_counts %>%
  left_join(race_total, by = "Race_group")

race_p <- chisq.test(
  table(demo_table_data$PD_status,
        demo_table_data$Race_group)
)$p.value


###############################################################
## Format output
###############################################################

age_final <- age_counts %>%
  mutate(
    Cases = mapply(format_n_pct, Cases, N_case),
    Controls = mapply(format_n_pct, Controls, N_ctrl),
    Total = mapply(format_n_pct, Total, N_total)
  )

sex_final <- sex_counts %>%
  mutate(
    Cases = mapply(format_n_pct, Cases, N_case),
    Controls = mapply(format_n_pct, Controls, N_ctrl),
    Total = mapply(format_n_pct, Total, N_total)
  )

race_final <- race_counts %>%
  mutate(
    Cases = mapply(format_n_pct, Cases, N_case),
    Controls = mapply(format_n_pct, Controls, N_ctrl),
    Total = mapply(format_n_pct, Total, N_total)
  )


###############################################################
## Print manuscript-ready values
###############################################################

cat("\nCases: n =", format(N_case, big.mark = ","), "\n")
cat("Controls: n =", format(N_ctrl, big.mark = ","), "\n")
cat("Total: N =", format(N_total, big.mark = ","), "\n\n")

cat("AGE\n")
print(age_final)
cat("P =", signif(age_p, 3), "\n\n")

cat("SEX\n")
print(sex_final)
cat("P =", signif(sex_p, 3), "\n\n")

cat("RACE\n")
print(race_final)
cat("P =", signif(race_p, 3), "\n")

###############################################################
## SMD for demographic characteristics in matched MGB cohort
###############################################################

library(dplyr)

demo_smd_data <- PD_risk_all_matched %>%
  mutate(
    Age_60plus = ifelse(Age >= 60, 1, 0),
    Female = ifelse(Gender == 0, 1, 0),
    Nonwhite = ifelse(Race_Category != 1, 1, 0)
  )

binary_smd <- function(x, group) {
  
  p_case <- mean(x[group == 1], na.rm = TRUE)
  p_ctrl <- mean(x[group == 0], na.rm = TRUE)
  
  (p_case - p_ctrl) /
    sqrt((p_case * (1 - p_case) +
            p_ctrl * (1 - p_ctrl)) / 2)
}

smd_results <- tibble(
  Variable = c(
    "Age ≥60 years",
    "Female sex",
    "Non-white race and ethnicity"
  ),
  
  SMD = c(
    binary_smd(
      demo_smd_data$Age_60plus,
      demo_smd_data$PD_status
    ),
    
    binary_smd(
      demo_smd_data$Female,
      demo_smd_data$PD_status
    ),
    
    binary_smd(
      demo_smd_data$Nonwhite,
      demo_smd_data$PD_status
    )
  )
) %>%
  mutate(
    Absolute_SMD = abs(SMD)
  )

print(smd_results)

###############################################################
## Updated MGB flowchart counts
## Run AFTER the completed revised MGB script
###############################################################

library(dplyr)

cat("\n==================================================\n")
cat("UPDATED MGB FLOWCHART COUNTS\n")
cat("==================================================\n\n")


###############################################################
## 1. ORIGINAL DEMOGRAPHIC FILES
## Before age >=30 restriction and deduplication
###############################################################

PD_raw <- bind_rows(PD_follow_up_1, PD_follow_up_2) %>%
  mutate(EMPI = as.character(EMPI))

HC_raw <- bind_rows(
  health_follow_up_1,
  health_follow_up_2,
  health_follow_up_3
) %>%
  mutate(EMPI = as.character(EMPI))


cat("1. ORIGINAL DATA\n")

cat("PD raw unique participants:",
    format(n_distinct(PD_raw$EMPI), big.mark = ","), "\n")

cat("HC raw unique participants:",
    format(n_distinct(HC_raw$EMPI), big.mark = ","), "\n\n")


###############################################################
## 2. AGE <30
###############################################################

PD_under30 <- PD_raw %>%
  filter(!is.na(Age), Age < 30) %>%
  summarise(N = n_distinct(EMPI)) %>%
  pull(N)

HC_under30 <- HC_raw %>%
  filter(!is.na(Age), Age < 30) %>%
  summarise(N = n_distinct(EMPI)) %>%
  pull(N)

cat("2. AGE <30\n")
cat("PD removed:", format(PD_under30, big.mark = ","), "\n")
cat("HC removed:", format(HC_under30, big.mark = ","), "\n")
cat("Total removed:",
    format(PD_under30 + HC_under30, big.mark = ","), "\n\n")


###############################################################
## 3. DUPLICATED EMPI RECORDS
##
## Number of extra demographic rows removed by distinct(EMPI)
###############################################################

PD_age30_rows <- PD_raw %>%
  filter(Age >= 30)

HC_age30_rows <- HC_raw %>%
  filter(Age >= 30)

PD_duplicate_rows <-
  nrow(PD_age30_rows) - n_distinct(PD_age30_rows$EMPI)

HC_duplicate_rows <-
  nrow(HC_age30_rows) - n_distinct(HC_age30_rows$EMPI)

cat("3. DUPLICATE DEMOGRAPHIC ROWS REMOVED\n")
cat("PD:", format(PD_duplicate_rows, big.mark = ","), "\n")
cat("HC:", format(HC_duplicate_rows, big.mark = ","), "\n")
cat("Total:",
    format(PD_duplicate_rows + HC_duplicate_rows,
           big.mark = ","), "\n\n")


###############################################################
## 4. AFTER AGE >=30 + DEDUPLICATION
###############################################################

N_PD_demo <- n_distinct(PD_follow_up$EMPI)
N_HC_demo <- n_distinct(health_follow_up$EMPI)

cat("4. AFTER AGE >=30 + DEDUPLICATION\n")
cat("PD:", format(N_PD_demo, big.mark = ","), "\n")
cat("HC:", format(N_HC_demo, big.mark = ","), "\n")
cat("Total:",
    format(N_PD_demo + N_HC_demo, big.mark = ","), "\n\n")


###############################################################
## 5. PD PHENOTYPE
###############################################################

N_PD_broad <- n_distinct(PD_index$EMPI)

N_PD_dementia <- n_distinct(PD_pre_index_dementia$EMPI)

N_PD_primary <- n_distinct(PD_follow_up_primary$EMPI)

cat("5. PD PHENOTYPE\n")
cat("Broad PD cases with qualifying PD code:",
    format(N_PD_broad, big.mark = ","), "\n")

cat("PD removed for pre-index dementia:",
    format(N_PD_dementia, big.mark = ","), "\n")

cat("Eligible PD after dementia exclusion + demographics:",
    format(N_PD_primary, big.mark = ","), "\n\n")


###############################################################
## 6. HEALTHY CONTROL EXCLUSION
###############################################################

N_HC_removed_dx <- health_follow_up %>%
  semi_join(health_diagnosis_removed, by = "EMPI") %>%
  summarise(N = n_distinct(EMPI)) %>%
  pull(N)

N_HC_primary <- n_distinct(health_follow_up_primary$EMPI)

cat("6. CONTROL DIAGNOSIS EXCLUSIONS\n")
cat("Controls removed:",
    format(N_HC_removed_dx, big.mark = ","), "\n")

cat("Eligible controls after exclusions:",
    format(N_HC_primary, big.mark = ","), "\n\n")


###############################################################
## 7. MEDICATION HISTORY REQUIREMENT
###############################################################

N_PD_med <- n_distinct(PD_follow_up_analysis$EMPI)
N_HC_med <- n_distinct(health_follow_up_analysis$EMPI)

PD_no_med <- N_PD_primary - N_PD_med
HC_no_med <- N_HC_primary - N_HC_med

cat("7. MEDICATION HISTORY REQUIREMENT\n")

cat("PD without usable pre-index medication history:",
    format(PD_no_med, big.mark = ","), "\n")

cat("HC without usable medication history:",
    format(HC_no_med, big.mark = ","), "\n")

cat("Total removed for medication-history requirement:",
    format(PD_no_med + HC_no_med, big.mark = ","), "\n\n")

cat("After medication-history requirement:\n")
cat("PD:", format(N_PD_med, big.mark = ","), "\n")
cat("HC:", format(N_HC_med, big.mark = ","), "\n")
cat("Total:",
    format(N_PD_med + N_HC_med, big.mark = ","), "\n\n")


###############################################################
## 8. COMPLETE MATCHING COVARIATES
###############################################################

N_PD_matchsource <- sum(PD_risk_primary$PD_status == 1)
N_HC_matchsource <- sum(PD_risk_primary$PD_status == 0)

PD_missing_demo <- N_PD_med - N_PD_matchsource
HC_missing_demo <- N_HC_med - N_HC_matchsource

cat("8. COMPLETE MATCHING COVARIATES\n")

cat("PD removed for missing matching covariates:",
    format(PD_missing_demo, big.mark = ","), "\n")

cat("HC removed for missing matching covariates:",
    format(HC_missing_demo, big.mark = ","), "\n")

cat("Matching source cohort:\n")
cat("PD:", format(N_PD_matchsource, big.mark = ","), "\n")
cat("HC:", format(N_HC_matchsource, big.mark = ","), "\n")
cat("Total:",
    format(N_PD_matchsource + N_HC_matchsource,
           big.mark = ","), "\n\n")


###############################################################
## 9. FINAL MATCHED COHORT
###############################################################

N_PD_matched <- sum(PD_risk_all_matched$PD_status == 1)
N_HC_matched <- sum(PD_risk_all_matched$PD_status == 0)

cat("9. FINAL 1:2 MATCHED COHORT\n")

cat("PD cases:",
    format(N_PD_matched, big.mark = ","), "\n")

cat("Controls:",
    format(N_HC_matched, big.mark = ","), "\n")

cat("Total:",
    format(N_PD_matched + N_HC_matched,
           big.mark = ","), "\n\n")


###############################################################
## 10. REMOVED BY MATCHING
###############################################################

cat("10. NOT RETAINED AFTER MATCHING\n")

cat("PD:",
    format(N_PD_matchsource - N_PD_matched,
           big.mark = ","), "\n")

cat("HC:",
    format(N_HC_matchsource - N_HC_matched,
           big.mark = ","), "\n")

cat("Total:",
    format(
      (N_PD_matchsource + N_HC_matchsource) -
        (N_PD_matched + N_HC_matched),
      big.mark = ","
    ),
    "\n"
)

cat("\n==================================================\n")

##### Data analysis of associations between use of drugs and PD risk in AMP-PD ######

### ================================================
## PPMI (Parkinson’s Progression Markers Initiative)
### ================================================

# Demographic filtration and definition
PD_risk_status_follow_up_PPMI <- read.csv("./data/raw_data/PPMI/PPMI_Curated_Data_Cut_Public_20230612.csv")
PD_risk_primary_PPMI <- PD_risk_status_follow_up_PPMI %>%
  filter(EVENT_ID == "BL") %>%
  select(PATNO, COHORT, CONCOHORT, subgroup, EVENT_ID, YEAR, age, age_at_visit, SEX, educ, race, moca, updrs3_score) %>%
  filter(age_at_visit >= 30) %>%
  filter(!is.na(race)) %>%
  filter(race != ".") %>%
  filter(!is.na(SEX)) %>%
  filter(SEX != ".") %>%
  mutate(age_at_visit_category = case_when(
    age_at_visit >= 30 & age_at_visit < 40 ~ 1,
    age_at_visit >= 40 & age_at_visit < 50 ~ 2,
    age_at_visit >= 50 & age_at_visit < 60 ~ 3,
    age_at_visit >= 60 & age_at_visit < 70 ~ 4,
    age_at_visit >= 70 & age_at_visit < 80 ~ 5,
    age_at_visit >= 80 & age_at_visit < 90 ~ 6,
    age_at_visit >= 90 ~ 7,
    TRUE ~ NA_real_
  )) %>%
  mutate(
    SEX = as.numeric(SEX),
    race = as.numeric(race),
    CONCOHORT = ifelse(is.na(CONCOHORT), COHORT, CONCOHORT),
    PD_status = ifelse(CONCOHORT %in% c(2, 4), 0, ifelse(CONCOHORT == 1, 1, NA))
  )

# Date of enrollment
Date_birth <- read.csv("./data/raw_data/PPMI/Demographics_27Sep2023.csv") %>%
  select(BIRTHDT, PATNO)

PD_risk_primary_update_PPMI <- merge(PD_risk_primary_PPMI, Date_birth, by = "PATNO", all.x = TRUE) %>%
  mutate(
    birth_date = as.Date(paste0("01/", BIRTHDT), format = "%d/%m/%Y"),
    enrollment_date = as.Date(birth_date + dyears(age))
  )

# Prepare the tables of PPMI
PPMI_drug_library_separated_update <- merge(PPMI_drug_library_separated, PD_risk_primary_update_PPMI, by = "PATNO", all.x = TRUE) %>%
  filter(medication_date <= enrollment_date)

common_PATNOs_PPMI <- intersect(PPMI_drug_library_separated_update$PATNO, PD_risk_primary_update_PPMI$PATNO)

PD_risk_all_PPMI <- PD_risk_primary_update_PPMI[PD_risk_primary_update_PPMI$PATNO %in% common_PATNOs_PPMI, ] %>%
  mutate(source = 1) %>%
  select(PATNO, PD_status, age_at_visit_category, SEX, race, source)

Medication_history_PD_risk_PPMI <- PPMI_drug_library_separated_update[PPMI_drug_library_separated_update$PATNO %in% common_PATNOs_PPMI, ] %>%
  select(PATNO, DrugName)

### ============================================
## PDBP (Parkinson's Disease Biomarkers Program)
### ============================================

# Demographic filtration and definition
PD_risk_status_follow_up_PDBP <- read_excel("./project/data/raw_data/PDBP/PDBP_datasets/PD_risk_status_follow_up.xlsx")
colnames(PD_risk_status_follow_up_PDBP)[colnames(PD_risk_status_follow_up_PDBP) == "NeurologicalExam.Required Fields.GUID"] <- "PATNO"
colnames(PD_risk_status_follow_up_PDBP)[colnames(PD_risk_status_follow_up_PDBP) == "NeurologicalExam.Neurological Examination.InclusnXclusnCntrlInd"] <- "PD_status_primary"
colnames(PD_risk_status_follow_up_PDBP)[colnames(PD_risk_status_follow_up_PDBP) == "NeurologicalExam.Required Fields.VisitTypPDBP"] <- "EVENT_ID"
colnames(PD_risk_status_follow_up_PDBP)[colnames(PD_risk_status_follow_up_PDBP) == "NeurologicalExam.Neurological Examination.NeuroExamPrimaryDiagnos"] <- "Subgroup"
colnames(PD_risk_status_follow_up_PDBP)[colnames(PD_risk_status_follow_up_PDBP) == "Study ID"] <- "Study_ID"
PD_risk_primary_PDBP <- PD_risk_status_follow_up_PDBP %>%
  select(Study_ID, PATNO, PD_status_primary, EVENT_ID, Subgroup) %>%
  filter(EVENT_ID == "Baseline") %>%
  mutate(PD_status = ifelse(PD_status_primary == "Case", 1, ifelse(PD_status_primary == "Control", 0, NA)))

Demographics <- read_excel("./project/data/raw_data/PDBP/PDBP_datasets/Demographics.xlsx")
colnames(Demographics)[colnames(Demographics) == "Study ID"] <- "Study_ID"
colnames(Demographics)[colnames(Demographics) == "Demographics.Required Fields.VisitTypPDBP"] <- "EVENT_ID"
colnames(Demographics)[colnames(Demographics) == "Demographics.Required Fields.GUID"] <- "PATNO"
colnames(Demographics)[colnames(Demographics) == "Demographics.Required Fields.AgeYrs"] <- "age_at_visit"
colnames(Demographics)[colnames(Demographics) == "Demographics.Demographics.GenderTypPDBP"] <- "Sex"
colnames(Demographics)[colnames(Demographics) == "Demographics.Demographics.RaceExpndCatPDBP"] <- "Race"
colnames(Demographics)[colnames(Demographics) == "Demographics.Demographics.EduLvlUSATypPDBP"] <- "Education_level"
colnames(Demographics)[colnames(Demographics) == "Demographics.Demographics.EmplmtStatus"] <- "Employment_status"
PD_Demographics <- Demographics %>%
  select(Study_ID, PATNO, Sex, age_at_visit, Race, Education_level, Employment_status, EVENT_ID) %>%
  filter(EVENT_ID == "Baseline")

PD_definition <- c("Parkinson's Disease", "Parkinson's Disease Dementia", "Parkinson's Disease Dementia - Mild Cognitive Impairment")
PD_risk <- merge(PD_risk_primary_PDBP, PD_Demographics, by = c("Study_ID", "PATNO")) %>%
  filter(PD_status_primary == "Control" | (PD_status_primary == "Case" & Subgroup %in% PD_definition)) %>%
  filter(age_at_visit != "20 - 29") %>%
  mutate(SEX = ifelse(Sex == "Male", 1, ifelse(Sex == "Female", 0, NA))) %>%
  mutate(age_at_visit_category = case_when(
    age_at_visit == "30 - 39" ~ 1,
    age_at_visit == "40 - 49" ~ 2,
    age_at_visit == "50 - 59" ~ 3,
    age_at_visit == "60 - 69" ~ 4,
    age_at_visit == "70 - 79" ~ 5,
    age_at_visit == "80 - 89" ~ 6,
    age_at_visit == "90+" ~ 7,
    TRUE ~ NA_real_
  )) %>%
  mutate(race = case_when(
    grepl("^White$|Caucasian", Race, ignore.case = TRUE) ~ 1,
    grepl("Black|African", Race, ignore.case = TRUE) ~ 2,
    grepl("Asian", Race, ignore.case = TRUE) ~ 3,
    grepl("Native Hawaiian|Pacific Islander", Race, ignore.case = TRUE) ~ 4,
    grepl("American Indian|Alaska Native", Race, ignore.case = TRUE) ~ 4,
    grepl("Hispanic|Latino", Race, ignore.case = TRUE) ~ 4,
    grepl("Other|Unknown", Race, ignore.case = TRUE) ~ 4,
    TRUE ~ 4
  )) %>%
  filter(!is.na(age_at_visit_category)) %>%
  select(Study_ID, PATNO, SEX, age_at_visit_category, race, Subgroup, PD_status, PD_status_primary) %>%
  distinct(PATNO, .keep_all = TRUE)

# Prepare the tables of PDBP
common_PATNOs_PDBP <- intersect(PDBP_drug_library_separated$PATNO, PD_risk$PATNO)
Medication_history_PD_risk_PDBP <- PDBP_drug_library_separated[PDBP_drug_library_separated$PATNO %in% common_PATNOs_PDBP, ] %>%
  select(PATNO, DrugName)
PD_risk_all_PDBP <- PD_risk[PD_risk$PATNO %in% common_PATNOs_PDBP, ] %>%
  mutate(source = 2) %>%
  select(PATNO, PD_status, age_at_visit_category, SEX, race, source)

# Combine all the tables for analysis
Medication_history_PD_risk_all <- rbind(Medication_history_PD_risk_PPMI, Medication_history_PD_risk_PDBP)
PD_risk_all_all <- rbind(PD_risk_all_PPMI, PD_risk_all_PDBP)
drug_patients_all <- Medication_history_PD_risk_all %>%
  group_by(DrugName) %>%
  summarise(PatientCount = n_distinct(PATNO)) %>%
  ungroup() %>%
  filter(PatientCount >= 10)

### ==============================================================
## A comprehensive drug-wide analysis based on logistic regression
### ==============================================================

tableA <- drug_patients_all
tableB <- Medication_history_PD_risk_all
tableC <- PD_risk_all_all

result_df <- data.frame(Drug = character(), OR = numeric(), CI_lower = numeric(), CI_upper = numeric(), p_value = numeric(), FDR_value = numeric(), Y_case = numeric(), Y_ctrl = numeric(), N_case = numeric(), N_ctrl = numeric())

for (drug in tableA$DrugName) {
  
  patients_with_drug <- tableB %>%
    filter(DrugName == drug) %>%
    select(PATNO)
  
  merged_data <- tableC %>%
    mutate(drug_used = ifelse(PATNO %in% patients_with_drug$PATNO, 1, 0))
  
  contingency_table <- table(merged_data$drug_used, merged_data$PD_status)
  
  Y_case <- contingency_table[2, 2]
  Y_ctrl <- contingency_table[2, 1]
  N_case <- contingency_table[1, 2]
  N_ctrl <- contingency_table[1, 1]
  
  model <- glm(PD_status ~ drug_used + age_at_visit_category + SEX + race + source, data = merged_data, family = "binomial")
  
  model_summary <- tidy(model)
  
  or_value <- exp(coef(model)["drug_used"])
  
  ci <- tryCatch(exp(confint(model)[2, ]))
  
  raw_p_value <- summary(model)$coefficients[2, "Pr(>|z|)"]
  
  result_df <- rbind(result_df, data.frame(DrugName = drug, OR = or_value, CI_lower = ci[1], CI_upper = ci[2], p_value = raw_p_value, Y_case = Y_case, Y_ctrl = Y_ctrl, N_case = N_case, N_ctrl = N_ctrl))
}

write.xlsx(result_df, "./data/results/PD_risk_results_AMPPD.xlsx", rowNames = FALSE)