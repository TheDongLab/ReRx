library(broom)
library(broom.mixed)
library(cobalt)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(lme4)
library(lmerTest)
library(lubridate)
library(MatchIt)
library(Matrix)
library(meta)
library(openxlsx)
library(patchwork)
library(RColorBrewer)
library(readxl)
library(reshape2)
library(stringr)
library(tidyr)
library(tidyverse)
library(data.table)
library(survival)

# --- User input section ---
setwd("/data/MarketScan_data/cohort")

CCAE_medication_case_path <- "./CCAE_ASL_D.csv"
CCAE_medication_control_path <- "./CCAE_ASL_controls_D.csv"
Medication_Dictionary_path <- "/data/MarketScan_data/dictionary/REDBOOK.csv"
ALS_O_path <- "./CCAE_ASL_O.csv"
ALS_S_path <- "./CCAE_ASL_S.csv"
Medication_Dictionary_update_path <- "/home/sk3486/Medication/CCAE/CCAE_Medication_dictionary_update.csv"
demographic_ALS_path <- "./CCAE_ASL_T.csv"
demographic_control_path <- "./CCAE_ASL_controls_T.csv"
result_output_path <- "/home/sk3486/Medication/result_df.csv"
# --- End of user input section ---

# Read medication data
CCAE_medication_case <- read.csv(CCAE_medication_case_path)
CCAE_medication_control <- read.csv(CCAE_medication_control_path)
CCAE_medication_combined <- bind_rows(CCAE_medication_case, CCAE_medication_control)

Medication_Dictionary <- read.csv(Medication_Dictionary_path) %>%
  select(GENERID, GENNME) %>%
  distinct(GENERID, .keep_all = TRUE)

# ALS ICD identification
ALS_ICD <- c("G1221", "33520")
ALS_O <- fread(ALS_O_path, select = c("ENROLID", "SVCDATE", "DX1", "DX2", "DX3", "DX4")) %>%
  pivot_longer(cols = starts_with("DX"), names_to = "DX_type", values_to = "Diagnosis_Code") %>%
  filter(Diagnosis_Code %in% ALS_ICD) %>%
  distinct(ENROLID, SVCDATE)
ALS_S <- fread(ALS_S_path, select = c("ENROLID", "SVCDATE", "DX1", "DX2", "DX3", "DX4")) %>%
  pivot_longer(cols = starts_with("DX"), names_to = "DX_type", values_to = "Diagnosis_Code") %>%
  filter(Diagnosis_Code %in% ALS_ICD) %>%
  distinct(ENROLID, SVCDATE)
ALS_index_date <- bind_rows(ALS_O, ALS_S) %>%
  mutate(SVCDATE = as.Date(SVCDATE)) %>%
  group_by(ENROLID) %>%
  summarise(ALS_index_date = min(SVCDATE), .groups = "drop")

# Medication before diagnosis
CCAE_medication_case_1 <- fread(CCAE_medication_case_path, select = c("ENROLID", "SVCDATE", "GENERID"))
CCAE_medication_control_1 <- fread(CCAE_medication_control_path, select = c("ENROLID", "SVCDATE", "GENERID"))
CCAE_medication_combined_1 <- bind_rows(CCAE_medication_case_1, CCAE_medication_control_1) %>%
  filter(!is.na(GENERID)) %>%
  distinct()
rm(CCAE_medication_case_1, CCAE_medication_control_1)

Medication_Dictionary_update <- read.csv(Medication_Dictionary_update_path, header = TRUE)

CCAE_medication_combine_update <- merge(CCAE_medication_combined_1, Medication_Dictionary, by = "GENERID", all.x = TRUE) %>%
  distinct()

CCAE_medication_combine_update_final <- merge(CCAE_medication_combine_update, Medication_Dictionary_update, by = "GENNME", all.x = TRUE) %>%
  filter(!is.na(Active_Ingredients)) %>%
  separate_rows(Active_Ingredients, sep = ";") %>%
  distinct()

# Demographics
demographic_ALS <- fread(demographic_ALS_path, select = c("ENROLID", "DOBYR", "SEX", "DTSTART", "DTEND")) %>%
  distinct() %>%
  mutate(DTSTART = as.Date(DTSTART), DTEND = as.Date(DTEND))

demographic_control <- fread(demographic_control_path, select = c("ENROLID", "DOBYR", "SEX", "DTSTART", "DTEND")) %>%
  distinct() %>%
  mutate(DTSTART = as.Date(DTSTART), DTEND = as.Date(DTEND))

Demographic_ALS <- demographic_ALS %>%
  group_by(ENROLID) %>%
  summarise(
    DOBYR = first(DOBYR),
    SEX = first(SEX),
    DTSTART = min(DTSTART, na.rm = TRUE),
    DTEND = max(DTEND, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(ALS_status = 1) %>%
  left_join(ALS_index_date, by = "ENROLID")

Demographic_control <- demographic_control %>%
  group_by(ENROLID) %>%
  summarise(
    DOBYR = first(DOBYR),
    SEX = first(SEX),
    DTSTART = min(DTSTART, na.rm = TRUE),
    DTEND = max(DTEND, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(ALS_status = 0) %>%
  left_join(ALS_index_date, by = "ENROLID")

Demographic_all <- bind_rows(Demographic_ALS, Demographic_control)

# Entry/Exit ages
Demographic_all <- Demographic_all %>%
  mutate(
    entry_date = DTSTART,
    exit_date = if_else(ALS_status == 1, pmin(DTEND, ALS_index_date, na.rm = TRUE), DTEND),
    entry_age = year(entry_date) - DOBYR,
    exit_age = year(exit_date) - DOBYR
  )


CCAE_medication_combine_update_final_with_date_filtered <- CCAE_medication_combine_update_final %>%
  group_by(ENROLID, Active_Ingredients) %>%
  slice_min(order_by = SVCDATE, n = 1, with_ties = FALSE) %>% # revise this row if you want to implement dose-response analysis
  ungroup()

common_ENROLID <- intersect(CCAE_medication_combine_update_final_with_date_filtered$ENROLID, Demographic_all$ENROLID)
Demographic_all_ALS_risk <- Demographic_all %>% filter(ENROLID %in% common_ENROLID)
Medication_history_ALS_risk <- CCAE_medication_combine_update_final_with_date_filtered %>% filter(ENROLID %in% common_ENROLID)

# Incidence density sampling (strict matching)
case_data <- Demographic_all_ALS_risk %>% filter(ALS_status == 1)
control_pool <- Demographic_all_ALS_risk %>% filter(ALS_status == 0)

matched_sets <- list()
strata_id <- 1

for (i in seq_len(nrow(case_data))) {
  case_i <- case_data[i, ]
  case_date <- case_i$ALS_index_date
  case_sex <- case_i$SEX
  case_age <- case_i$entry_age
  case_entry_year <- year(case_i$entry_date)
  
  risk_set <- control_pool %>%
    filter(
      SEX == case_sex,
      entry_age == case_age,                       
      year(entry_date) == case_entry_year,        
      entry_date <= case_date,                    
      exit_date >= case_date                      
    )
  
  if (nrow(risk_set) >= 5) {
    sampled_controls <- sample_n(risk_set, 5)
    sampled_controls$ALS_index_date <- case_date
    
    matched_set <- bind_rows(
      mutate(case_i, strata = strata_id),
      mutate(sampled_controls, strata = strata_id)
    )
    
    matched_sets[[length(matched_sets) + 1]] <- matched_set
    strata_id <- strata_id + 1
  }
}

matched_data <- bind_rows(matched_sets)

# Update ALS_index_date for all members in the strata
matched_data <- matched_data %>%
  group_by(strata) %>%
  mutate(
    ALS_index_date = ALS_index_date[ALS_status == 1][1]
  ) %>%
  ungroup()


Medication_matched <- Medication_history_ALS_risk %>%
  inner_join(matched_data, by = "ENROLID")

Medication_matched <- Medication_matched %>%
  mutate(
    start_5yr = ALS_index_date - years(5),
    start_2yr = ALS_index_date - years(2)
  )

# 1. All years before diagnosis
Medication_matched_filtered <- Medication_matched %>%
  filter(SVCDATE < ALS_index_date)

# # 2. Between 5 and 2 years before diagnosis
# Medication_matched_filtered <- Medication_matched %>%
#   filter(SVCDATE >= start_5yr & SVCDATE < start_2yr)

# # 3. More than 5 years before diagnosis
# Medication_matched_filtered <- Medication_matched %>%
#   filter(SVCDATE < start_5yr)



drug_patients <- Medication_matched_filtered %>%
  group_by(Active_Ingredients) %>%
  summarise(PatientCount = n_distinct(ENROLID)) %>%
  ungroup() %>%
  filter(PatientCount >= 10)


library(comorbidity)
library(survival)
library(dplyr)

# Example: reload outpatient claims with all diagnosis codes
ALS_O_full <- fread(ALS_O_path, select = c("ENROLID", "SVCDATE", "DX1", "DX2", "DX3", "DX4")) %>%
  pivot_longer(cols = starts_with("DX"), names_to = "DX_type", values_to = "DX") %>%
  filter(!is.na(DX)) %>%
  distinct(ENROLID, DX)

ALS_S_full <- fread(ALS_S_path, select = c("ENROLID", "SVCDATE", "DX1", "DX2", "DX3", "DX4")) %>%
  pivot_longer(cols = starts_with("DX"), names_to = "DX_type", values_to = "DX") %>%
  filter(!is.na(DX)) %>%
  distinct(ENROLID, DX)


all_dx <- bind_rows(ALS_O_full, ALS_S_full) %>%
  distinct(ENROLID, DX)


elix <- comorbidity(
  x = all_dx,
  id = "ENROLID",
  code = "DX",
  map = "elixhauser_icd10_quan",  # ICD-10 Elixhauser mapping
  assign0 = TRUE
)


matched_data <- matched_data %>%
  left_join(elix, by = "ENROLID")

comorb_cols <- colnames(elix)[-1]  # all comorbidity columns except ENROLID

result_df <- data.frame(
  Active_Ingredients = character(),
  OR = numeric(),
  CI_lower = numeric(),
  CI_upper = numeric(),
  p_value = numeric(),
  fdr_value = numeric(),
  Y_case = numeric(),
  Y_ctrl = numeric(),
  N_case = numeric(),
  N_ctrl = numeric()
)

for (drug in drug_patients$Active_Ingredients) {
  # Identify drug exposure
  drug_exposure <- Medication_matched_filtered %>%
    filter(Active_Ingredients == drug) %>%
    pull(ENROLID)
  
  analysis_data <- matched_data %>%
    mutate(drug_used = if_else(ENROLID %in% drug_exposure, 1, 0))
  
  # Calculate counts for summary
  contingency_table <- table(analysis_data$drug_used, analysis_data$ALS_status)
  Y_case <- ifelse("1" %in% rownames(contingency_table) && "1" %in% colnames(contingency_table),
                   contingency_table["1", "1"], 0)
  Y_ctrl <- ifelse("1" %in% rownames(contingency_table) && "0" %in% colnames(contingency_table),
                   contingency_table["1", "0"], 0)
  N_case <- ifelse("0" %in% rownames(contingency_table) && "1" %in% colnames(contingency_table),
                   contingency_table["0", "1"], 0)
  N_ctrl <- ifelse("0" %in% rownames(contingency_table) && "0" %in% colnames(contingency_table),
                   contingency_table["0", "0"], 0)
  
  # Fit model only if drug_used has both 0 and 1
  if (length(unique(analysis_data$drug_used)) == 2) {
    formula_str <- paste("ALS_status ~ drug_used + SEX + exit_age +",
                         paste(comorb_cols, collapse = " + "),
                         "+ strata(strata)")
    
    model <- tryCatch(
      clogit(as.formula(formula_str), data = analysis_data),
      error = function(e) NULL
    )
    
    if (!is.null(model)) {
      or_value <- exp(coef(model)["drug_used"])
      ci <- tryCatch(exp(confint(model)["drug_used", ]), error = function(e) c(NA, NA))
      raw_p_value <- summary(model)$coefficients["drug_used", "Pr(>|z|)"]
    } else {sdf
      or_value <- NA
      ci <- c(NA, NA)
      raw_p_value <- NA
    }
  } else {
    or_value <- NA
    ci <- c(NA, NA)
    raw_p_value <- NA
  }
  
  # Append result
  result_df <- rbind(result_df, data.frame(
    Active_Ingredients = drug,
    OR = or_value,
    CI_lower = ci[1],
    CI_upper = ci[2],
    p_value = raw_p_value,
    Y_case = Y_case,
    Y_ctrl = Y_ctrl,
    N_case = N_case,
    N_ctrl = N_ctrl
  ))
}

# FDR correction
result_df$fdr_value <- p.adjust(result_df$p_value, method = "fdr")

# Save results
write.csv(result_df, result_output_path, row.names = FALSE)