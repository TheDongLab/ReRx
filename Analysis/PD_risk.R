##### Data analysis of associations between use of drugs and PD risk in MGB Biobank

### ===============
## Data preparation
### ===============

# Demographic filtration and definition
setwd("~/project/ReRx/")
PD_follow_up_1 <- read.table("./data/raw_data/MGB_Biobank/PD_patients/1/XD010_20240621_170059-1_Dem.txt", sep="|", header=TRUE, fill=TRUE, na.strings=c("", "NA"))
PD_follow_up_2 <- read.table("./data/raw_data/MGB_Biobank/PD_patients/2/XD010_20240621_170059-2_Dem.txt", sep="|", header=TRUE, fill=TRUE, na.strings=c("", "NA"))
PD_follow_up <- bind_rows(PD_follow_up_1, PD_follow_up_2) %>%
  filter(Age >= 30) %>%
  select(EMPI, Date_of_Birth, Age, Gender_Legal_Sex, Race_Group) %>%
  mutate(PD_status = 1) %>%
  mutate(Age_Category = case_when(
    Age >= 30 & Age < 40 ~ 1,
    Age >= 40 & Age < 50 ~ 2,
    Age >= 50 & Age < 60 ~ 3,
    Age >= 60 & Age < 70 ~ 4,
    Age >= 70 & Age < 80 ~ 5,
    Age >= 80 & Age < 90 ~ 6,
    Age >= 90 ~ 7,
    TRUE ~ NA_real_
  )) %>%
  mutate(Gender = ifelse(Gender_Legal_Sex == "Male", 1, 0)) %>%
  mutate(Race_Category = case_when(
    Race_Group == "White" ~ 1,
    Race_Group == "Black" ~ 2,
    Race_Group == "Asian" ~ 3,
    Race_Group %in% c("Unknown/Missing", "Other", "Declined", "Two or More", "American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander") ~ 4,
    TRUE ~ NA_real_
  ))

health_follow_up_1 <- read.table("./data/raw_data/MGB_Biobank/health_control/1/XD010_20240630_131521-1_Dem.txt", sep="|", header=TRUE, fill=TRUE, na.strings=c("", "NA"))
health_follow_up_2 <- read.table("./data/raw_data/MGB_Biobank/health_control/2/XD010_20240630_131521-2_Dem.txt", sep="|", header=TRUE, fill=TRUE, na.strings=c("", "NA"))
health_follow_up_3 <- read.table("./data/raw_data/MGB_Biobank/health_control/3/XD010_20240630_131521-3_Dem.txt", sep="|", header=TRUE, fill=TRUE, na.strings=c("", "NA"))
health_follow_up <- bind_rows(health_follow_up_1, health_follow_up_2, health_follow_up_3) %>%
  filter(Age >= 30) %>%
  select(EMPI, Date_of_Birth, Age, Gender_Legal_Sex, Race_Group) %>%
  mutate(PD_status = 0) %>%
  mutate(Age_Category = case_when(
    Age >= 30 & Age < 40 ~ 1,
    Age >= 40 & Age < 50 ~ 2,
    Age >= 50 & Age < 60 ~ 3,
    Age >= 60 & Age < 70 ~ 4,
    Age >= 70 & Age < 80 ~ 5,
    Age >= 80 & Age < 90 ~ 6,
    Age >= 90 ~ 7,
    TRUE ~ NA_real_
  )) %>%
  mutate(Gender = ifelse(Gender_Legal_Sex == "Male", 1, 0)) %>%
  mutate(Race_Category = case_when(
    Race_Group == "White" ~ 1,
    Race_Group == "Black" ~ 2,
    Race_Group == "Asian" ~ 3,
    Race_Group %in% c("Unknown/Missing", "Other", "Declined", "Two or More", "American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander") ~ 4,
    TRUE ~ NA_real_
  ))

# Diagnosis filtration
PD_diagnosis_1 <- read.csv("./data/raw_data/MGB_Biobank/PD_patients/1/XD010_20240621_170059-1_Dia.txt", sep="|", header=TRUE, fill=TRUE)
PD_diagnosis_2 <- read.csv("./data/raw_data/MGB_Biobank/PD_patients/2/XD010_20240621_170059-2_Dia.txt", sep="|", header=TRUE, fill=TRUE)
PD_diagnosis <- bind_rows(PD_diagnosis_1, PD_diagnosis_2) %>%
  arrange(EMPI)

health_diagnosis_1 <- read.csv("./data/raw_data/MGB_Biobank/health_control/1/XD010_20240630_131521-1_Dia.txt", sep="|", header=TRUE, fill=TRUE)
health_diagnosis_2 <- read.csv("./data/raw_data/MGB_Biobank/health_control/2/XD010_20240630_131521-2_Dia.txt", sep="|", header=TRUE, fill=TRUE)
health_diagnosis_3 <- read.csv("./data/raw_data/MGB_Biobank/health_control/3/XD010_20240630_131521-3_Dia.txt", sep="|", header=TRUE, fill=TRUE)
health_diagnosis <- bind_rows(health_diagnosis_1, health_diagnosis_2, health_diagnosis_3) %>%
  arrange(EMPI)

PD_diagnosis_selected_PD <- PD_diagnosis %>%
  filter(
    (Code_Type == "ICD9" & Code %in% c("332", "332.0")) |
      (Code_Type == "ICD10" & grepl("^G20", Code)) |
      (Code_Type == "LMR" & Code %in% c("LPA213", "LPA883", "LPA884")) |
      (Code_Type == "Oncall" & Code == "WHJM1") |
      (Code_Type == "DSM4" & Code == "332..3")
  ) %>%
  group_by(EMPI) %>%
  filter(Date == min(Date)) %>%
  ungroup() %>%
  distinct(EMPI, Date, .keep_all = TRUE)

PD_diagnosis_removed <- PD_diagnosis[PD_diagnosis$EMPI %in% PD_diagnosis_selected_PD$EMPI, ] %>%
  filter(
    (Code_Type == "ICD9" & Code %in% c("290.0", "290.10", "290.11", "290.12", "290.20", "290.21", 
                                       "290.3", "290.4", "290.40", "290.41", "290.42", "290.43", 
                                       "294.1", "294.10", "294.11", "294.20", "294.21", "331.19", 
                                       "331.82", "332.1")) |
      (Code_Type == "ICD10" & Code %in% c("F01.50", "F01.51", "F01.518", "F01.52", "F01.53", "F01.54", 
                                          "F01.A0", "F01.A11", "F01.A18", "F01.A3", "F01.A4", "F01.B0", 
                                          "F01.B11", "F01.B18", "F01.B2", "F01.B3", "F01.B4", "F01.C0", 
                                          "F01.C11", "F01.C18", "F01.C3", "F02.80", "F02.81", "F02.811", 
                                          "F02.818", "F02.82", "F02.83", "F02.84", "F02.A0", "F02.A18", 
                                          "F02.A3", "F02.A4", "F02.B0", "F02.B11", "F02.B18", "F02.B2", 
                                          "F02.B3", "F02.B4", "F02.C0", "F02.C11", "F02.C18", "F02.C3", 
                                          "F03.9", "F03.90", "F03.91", "F03.911", "F03.918", "F03.92", 
                                          "F03.93", "F03.94", "F03.A0", "F03.A11", "F03.A18", "F03.A2", 
                                          "F03.A3", "F03.A4", "F03.B0", "F03.B11", "F03.B18", "F03.B2", 
                                          "F03.B3", "F03.B4", "F03.C0", "F03.C11", "F03.C18", "F03.C2", 
                                          "F03.C3", "F03.C4", "F10.27", "F10.97", "F13.27", "G31.09", 
                                          "G31.83")) |
      (Code_Type == "ICD10" & grepl("^G21", Code)) |
      (Code_Type == "LMR" & Code == "LPA99") |
      (Code_Type == "Oncall" & Code == "YHAL6") |
      (Code_Type == "DSM4" & Code %in% c("294.10.1", "290.10.1", "290.0.1", "290.42.1", "290.40.1", "290.43.1"))
  ) %>%
  distinct(EMPI, .keep_all = TRUE)

PD_diagnosis_final <- anti_join(PD_diagnosis, PD_diagnosis_removed, by = "EMPI")

health_diagnosis_removed <- health_diagnosis %>%
  filter(
    (Code_Type == "ICD9" & Code %in% c("806.00", "806.04", "806.05", "806.09", "806.1", "806.10", "806.15", "806.20", "806.24", "806.25", "806.29", 
                                       "806.30", "806.35", "806.4", "806.5", "806.60", "806.69", "806.70", "806.79", "806.8", "806.9", "907.2", 
                                       "952.0", "952.00", "952.04", "952.05", "952.09", "952.10", "952.15", "952.19", "952.2", "952.3", "952.4", 
                                       "952.8", "952.9", "V15.52", "649.40", "649.41", "649.43", "331.0", "332", "332.0", "332.1", "335.20", 
                                       "340", "300.4", "299.0", "299.00", "299.01", "314.01", "314.00", "290.0", "290.10", "290.11", "290.12", 
                                       "290.20", "290.21", "290.3", "290.4", "290.40", "290.41", "290.42", "290.43", "294.1", "294.10", 
                                       "294.11", "294.20", "294.21", "331.19", "331.82")) |
      (Code_Type == "ICD9" & grepl("^345", Code)) |
      (Code_Type == "ICD9" & grepl("^295", Code)) |
      (Code_Type == "ICD9" & grepl("^296", Code)) |
      (Code_Type == "ICD10" & Code %in% c("G46.3", "Z13.850", "Z87.820", "G12.21", "G35", "F32.A", "F53.0", "F25.0", "F19.94", "F06.30", 
                                          "F06.34", "F06.32", "F06.31", "F10.94", "F10.24", "F90.2", "F90.8", "F90.1", "F90.0", "F90.9", 
                                          "F01.50", "F01.51", "F01.518", "F01.52", "F01.53", "F01.54", "F01.A0", "F01.A11", "F01.A18", 
                                          "F01.A3", "F01.A4", "F01.B0", "F01.B11", "F01.B18", "F01.B2", "F01.B3", "F01.B4", "F01.C0", 
                                          "F01.C11", "F01.C18", "F01.C3", "F02.80", "F02.81", "F02.811", "F02.818", "F02.82", "F02.83", 
                                          "F02.84", "F02.A0", "F02.A18", "F02.A3", "F02.A4", "F02.B0", "F02.B11", "F02.B18", "F02.B2", 
                                          "F02.B3", "F02.B4", "F02.C0", "F02.C11", "F02.C18", "F02.C3", "F03.9", "F03.90", "F03.91", 
                                          "F03.911", "F03.918", "F03.92", "F03.93", "F03.94", "F03.A0", "F03.A11", "F03.A18", "F03.A2", 
                                          "F03.A3", "F03.A4", "F03.B0", "F03.B11", "F03.B18", "F03.B2", "F03.B3", "F03.B4", "F03.C0", 
                                          "F03.C11", "F03.C18", "F03.C2", "F03.C3", "F03.C4", "F10.27", "F10.97", "F13.27", "G31.09", "G31.83")) |
      (Code_Type == "ICD10" & grepl("^S06.2X", Code)) |
      (Code_Type == "ICD10" & grepl("^S06.30", Code)) |
      (Code_Type == "ICD10" & grepl("^G40", Code)) |
      (Code_Type == "ICD10" & grepl("^G30", Code)) |
      (Code_Type == "ICD10" & grepl("^G20", Code)) |
      (Code_Type == "ICD10" & grepl("^G21", Code)) |
      (Code_Type == "ICD10" & grepl("^F20", Code)) |
      (Code_Type == "ICD10" & grepl("^F31", Code)) |
      (Code_Type == "APDRG" & Code %in% c("014", "013")) |
      (Code_Type == "LMR" & Code %in% c("LPA1429", "LPA894", "LPA1009", "LPA867", "LPA312", "LPA883", "LPA884", "LPA585", "LPA268", "LPA101", 
                                        "LPA487", "LPA44", "LPA999", "LPA1234", "LPA604", "LPA1513", "LPA99")) |
      (Code_Type == "APDRG" & Code %in% c("043", "750", "753", "754")) |
      (Code_Type == "Oncall" & Code %in% c("NLGD2", "NLPR2","WHCE1", "WHCQ9", "WHMT3","WHJM1","MHJA5", "WHEQ1", "YJSN1", "YLAB2", "YJSD3", 
                                           "YJBG1", "YJCC3", "YJCM1", "YJHP1", "YLCE9", "YKKT3", "YGAA1", "YHAL6")) |
      (Code_Type == "DSM4" & Code %in% c("345.4.3", "345.9.3", "331.0.3", "332..3", "295.9.3", "295.3.3", "296.90.1", "292.84.1", "294.10.1", 
                                         "290.10.1", "290.0.1", "290.42.1", "290.40.1", "290.43.1")) | 
      (Code_Type == "DSM4" & grepl("^296", Code))
  ) %>%
  distinct(EMPI, .keep_all = TRUE)

health_diagnosis_final <- anti_join(health_diagnosis, health_diagnosis_removed, by = "EMPI")

All_diagnosis <- bind_rows(PD_diagnosis_final, health_diagnosis_final) %>%
  group_by(EMPI, Diagnosis_Name) %>%
  filter(Date == min(Date)) %>%
  ungroup()

# Drug exposure criteria (based on the index date and the number of prescriptions)
drug_library_separated_index_date <- left_join(drug_library_separated_MGB, PD_diagnosis_selected_PD, by = "EMPI")
drug_library_separated_index_date$Medication_Date <- as.Date(drug_library_separated_index_date$Medication_Date, format = "%m/%d/%Y")
drug_library_separated_index_date$Date <- as.Date(drug_library_separated_index_date$Date, format = "%m/%d/%Y")
drug_library_separated_1 <- drug_library_separated_index_date %>%
  filter(is.na(Date) | Medication_Date < Date)
drug_count <- drug_library_separated_1 %>%
  group_by(EMPI, DrugName) %>%
  summarise(count = n(), .groups = 'drop')
drug_exposure <- drug_count %>%
  filter(count >= 1)
drug_library_separated_final <- left_join(drug_library_separated_1, drug_exposure, by = c("EMPI", "DrugName")) %>%
  filter(!is.na(count))

### ==============================
## Propensity score matching (PSM)
### ==============================

common_PD_EMPI <- intersect(PD_follow_up$EMPI, PD_diagnosis_selected_PD$EMPI)
PD_diagnosis_selected_PD_update <- PD_diagnosis_selected_PD[PD_diagnosis_selected_PD$EMPI %in% common_PD_EMPI, ]
PD_follow_up_update <- PD_follow_up[PD_follow_up$EMPI %in% common_PD_EMPI, ]
PD_risk_primary <- bind_rows(PD_follow_up_update, health_follow_up) %>%
  distinct(EMPI, .keep_all = TRUE)  %>%
  mutate(Birth_Year = as.numeric(str_sub(Date_of_Birth, start = -4)))
common_EMPI <- intersect(intersect(drug_library_separated_final$EMPI, PD_risk_primary$EMPI), All_diagnosis$EMPI)
PD_risk_all <- PD_risk_primary[PD_risk_primary$EMPI %in% common_EMPI, ]

match_model <- matchit(
  PD_status ~ Birth_Year + Age + Gender + Race_Category,
  data = PD_risk_all,
  method = "nearest",
  ratio = 2,
  caliper = 0.1
)
PD_risk_all_matched <- match.data(match_model)

# plot of PSM
love.plot(match_model, abs = FALSE, threshold = 0.1,
          var.order = "unadjusted",
          var.names = c("Birth_Year" = "Birth Year", "Age" = "Age", "Gender" = "Gender", "Race_Category" = "Race"),
          colors = c("#E74C3C", "#1ABC9C"),
          shapes = c(16, 17),
          title = "Covariate Balance",
          sample.names = c("Unmatched", "Matched"),
          xlab = "Mean Differences",
          ylim = c(-0.15, 0.15))

PD_risk_all_matched <- merge(PD_risk_all_matched, PD_diagnosis_selected_PD_update, by = "EMPI", all.x = TRUE) %>%
  filter(
    (PD_status == 0 & (is.na(Date) | Date == "")) |
      (PD_status == 1 & !(is.na(Date) | Date == ""))
  )

# prepare all the tables for analysis
Medication_history_PD_risk <- drug_library_separated_final[drug_library_separated_final$EMPI %in% PD_risk_all_matched$EMPI, ]
drug_patients <- Medication_history_PD_risk %>%
  group_by(DrugName) %>%
  summarise(PatientCount = n_distinct(EMPI)) %>%
  ungroup() %>%
  filter(PatientCount >= 10)

### ==============================================================
## A comprehensive drug-wide analysis based on logistic regression
### ==============================================================

tableA <- drug_patients
tableB <- Medication_history_PD_risk
tableC <- PD_risk_all_matched

result_df <- data.frame(Drug = character(), OR = numeric(), CI_lower = numeric(), CI_upper = numeric(), p_value = numeric(), FDR_value = numeric(), Y_case = numeric(), Y_ctrl = numeric(), N_case = numeric(), N_ctrl = numeric())

for (drug in tableA$DrugName) {
  
  patients_with_drug <- tableB %>%
    filter(DrugName == drug) %>%
    select(EMPI)
  
  merged_data <- tableC %>%
    mutate(drug_used = ifelse(EMPI %in% patients_with_drug$EMPI, 1, 0))
  
  contingency_table <- table(merged_data$drug_used, merged_data$PD_status)
  
  Y_case <- contingency_table[2, 2]
  Y_ctrl <- contingency_table[2, 1]
  N_case <- contingency_table[1, 2]
  N_ctrl <- contingency_table[1, 1]
  
  model <- glm(PD_status ~ drug_used + Age_Category + Gender + Race_Category, data = merged_data, family = "binomial")
  
  model_summary <- tidy(model)
  
  or_value <- exp(coef(model)["drug_used"])
  
  ci <- tryCatch(exp(confint(model)[2, ]))
  
  raw_p_value <- summary(model)$coefficients[2, "Pr(>|z|)"]
  
  result_df <- rbind(result_df, data.frame(DrugName = drug, OR = or_value, CI_lower = ci[1], CI_upper = ci[2], p_value = raw_p_value, Y_case = Y_case, Y_ctrl = Y_ctrl, N_case = N_case, N_ctrl = N_ctrl))
}
result_df$fdr_value <- p.adjust(result_df$p_value, method = "fdr")

write.xlsx(result_df, "./data/results/PD_risk_results_MGB.xlsx", rowNames = FALSE)



##### Data analysis of associations between use of drugs and PD risk in AMP-PD

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