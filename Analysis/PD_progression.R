##### Data analysis of associations between use of drugs and PD progression in AMP-PD

### =======================================================================================================
## cognition/motor impairment (you can change tiny codes to implement motor impairment as annotation below)
### =======================================================================================================

### PPMI ###
# PD risk status
PD_risk_status_follow_up_PPMI <- read.csv("./data/raw_data/PPMI/PPMI_Curated_Data_Cut_Public_20230612.csv")
PD_risk_primary_PPMI <- PD_risk_status_follow_up_PPMI %>%
  select(PATNO, COHORT, CONCOHORT, subgroup, EVENT_ID, YEAR, age, age_at_visit, SEX, educ, race, moca, updrs3_score) %>%
  filter(age_at_visit >= 30) %>%
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
    educ = as.numeric(educ),
    CONCOHORT = ifelse(is.na(CONCOHORT), COHORT, CONCOHORT),
    PD_status = ifelse(CONCOHORT %in% c(2, 4), 0, ifelse(CONCOHORT == 1, 1, NA))
  ) %>%
  filter(SEX != ".") %>%
  filter(race != ".") %>%
  filter(age_at_visit_category != ".") %>%
  filter(PD_status == 1)

PD_risk_primary_PPMI_update <- PD_risk_primary_PPMI %>%
  filter(moca != ".") %>%
  mutate(moca = as.numeric(moca)) %>%
  group_by(PATNO) %>%
  mutate(
    Age = first(age_at_visit_category),
    Sex = first(SEX),
    Race = first(race),
    Educ = first(educ)
  ) %>%
  ungroup() %>%
  group_by(PATNO) %>%
  filter(any(EVENT_ID == "BL")) %>%
  filter(!any(EVENT_ID == "BL" & (is.na(moca) | moca < 26))) %>% # for motor - filter(!any(EVENT_ID == "BL" & (is.na(updrs3_score) | updrs3_score > 32))) %>%
  ungroup() %>%
  filter(PATNO %in% PATNO[YEAR > 0]) %>%
  group_by(PATNO) %>%
  filter(max(YEAR) >= 3) %>%
  ungroup() %>%
  select(PATNO, YEAR, age, Age, Sex, Race, Educ, moca) # for motor - select(PATNO, YEAR, age, Age, Sex, Race, Educ, updrs3_score)

# Date of enrollment
Date_birth <- read.csv("./project/data/raw_data/PPMI/Demographics_27Sep2023.csv") %>%
  select(BIRTHDT, PATNO)

PD_risk_primary_update_PPMI <- merge(PD_risk_primary_PPMI_update, Date_birth, by = "PATNO", all.x = TRUE) %>%
  mutate(
    birth_date = as.Date(paste0("01/", BIRTHDT), format = "%d/%m/%Y"),
    enrollment_date = as.Date(birth_date + dyears(age))
  )

# Prepare all the tables
PPMI_drug_library_separated_update <- merge(PPMI_drug_library_separated, PD_risk_primary_update_PPMI, by = "PATNO", all.x = TRUE) %>%
  filter(medication_date <= enrollment_date)

common_PATNOs_PPMI <- intersect(PPMI_drug_library_separated_update$PATNO, PD_risk_primary_update_PPMI$PATNO)

PD_risk_all_PPMI <- PD_risk_primary_update_PPMI[PD_risk_primary_update_PPMI$PATNO %in% common_PATNOs_PPMI, ] %>%
  mutate(source = 1) %>%
  select(PATNO, YEAR, Age, Sex, Race, Educ, moca, source) # for motor - select(PATNO, YEAR, Age, Sex, Race, Educ, updrs3_score, source)

Medication_history_PD_risk_PPMI <- PPMI_drug_library_separated_update[PPMI_drug_library_separated_update$PATNO %in% common_PATNOs_PPMI, ] %>%
  select(PATNO, DrugName)

drug_list = c("Amlodipine", "Losartan", "Potassium", "Salbutamol", "Propranolol", "Mirtazapine", "Solifenacin", "Quetiapine", "Sildenafil")

### PDBP ###
### data cleaning of PD risk
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
  mutate(Educ = case_when(
      Education_level %in% c("High school graduate", "12th Grade, no diploma", 
                  "GED or equivalent", "8th Grade", "High School Graduate", 
                  "10th Grade", "9th Grade", "6th Grade", "4th Grade", "7th Grade", "Unknown") ~ 1,
      Education_level %in% c("Some college, no degree", "Associate degree: academic program", 
                  "Associate degree: occupational/technical/vocational program", 
                  "Bachelor's degree (e.g., BA, AB, BS, BBA)") ~ 2,
      Education_level %in% c("Master's degree (e.g., MA, MS, MEng, MEd, MBA)", 
                  "Professional school degree (e.g., MD, DDS, DVM, JD)", 
                  "Doctoral degree (e.g., PhD, EdD)") ~ 3,
      TRUE ~ NA_integer_
    )) %>%
  filter(!is.na(age_at_visit_category)) %>%
  filter(!is.na(Educ)) %>%
  select(PATNO, SEX, age_at_visit_category, race, Subgroup, PD_status, Educ) %>%
  distinct(PATNO, .keep_all = TRUE) %>%
  filter(PD_status == 1)

# MoCA score
PD_progression_MoCA_primary <- read_excel("./project/data/raw_data/PDBP/PDBP_datasets/PD_progression_MoCA.xlsx")
colnames(PD_progression_MoCA_primary)[colnames(PD_progression_MoCA_primary) == "Study ID"] <- "Study_ID"
colnames(PD_progression_MoCA_primary)[colnames(PD_progression_MoCA_primary) == "MoCA.Required Fields.VisitTypPDBP"] <- "Date"
colnames(PD_progression_MoCA_primary)[colnames(PD_progression_MoCA_primary) == "MoCA.MoCA.MOCA_Total"] <- "moca"
colnames(PD_progression_MoCA_primary)[colnames(PD_progression_MoCA_primary) == "MoCA.Required Fields.GUID"] <- "PATNO"
PD_progression_MoCA_primary_filtered <- PD_progression_MoCA_primary %>%
  group_by(PATNO) %>%
  mutate(has_baseline_record = any(Date == "Baseline")) %>%
  mutate(has_valid_baseline = any(Date == "Baseline" & !is.na(moca) & moca >= 26)) %>%
  ungroup() %>%
  filter(has_baseline_record & has_valid_baseline) %>%
  filter(!is.na(moca)) %>%
  mutate(Event_date = ifelse(Date == "Baseline", 0, ifelse(Date == "6 months", 0.5, ifelse(Date == "12 months", 1, ifelse(Date == "18 months", 1.5, ifelse(Date == "24 months", 2, ifelse(Date == "30 months", 2.5, ifelse(Date == "36 months", 3, ifelse(Date == "42 months", 3.5, ifelse(Date == "48 months", 4, ifelse(Date == "60 months", 5, ifelse(Date == "72 months", 6, NA)))))))))))) %>%
  group_by(PATNO) %>%
  filter(!any(Event_date == 0 & (is.na(moca) | moca < 26))) %>%
  ungroup() %>%
  filter(PATNO %in% PATNO[Event_date > 0]) %>%
  group_by(PATNO) %>%
  filter(max(Event_date) >= 3) %>%
  ungroup() %>%
  group_by(PATNO, Event_date) %>%
  summarize(moca = mean(moca, na.rm = TRUE), .groups = 'drop') %>%
  ungroup()

PD_risk_primary_update_PDBP <- merge(PD_progression_MoCA_primary_filtered, PD_risk, by = "PATNO", all.x = TRUE) %>%
  filter(PD_status == 1) %>%
  rename(YEAR = Event_date) %>%
  group_by(PATNO) %>%
  mutate(
    Age = first(age_at_visit_category),
    Sex = first(SEX),
    Race = first(race),
    Educ = first(Educ)
  ) %>%
  ungroup()

common_PATNOs_PDBP <- intersect(PDBP_drug_library_separated$PATNO, PD_risk_primary_update_PDBP$PATNO)
Medication_history_PD_risk_PDBP <- PDBP_drug_library_separated[PDBP_drug_library_separated$PATNO %in% common_PATNOs_PDBP, ] %>%
  select(PATNO, DrugName)
PD_risk_all_PDBP <- PD_risk_primary_update_PDBP[PD_risk_primary_update_PDBP$PATNO %in% common_PATNOs_PDBP, ] %>%
  mutate(source = 2) %>%
  select(PATNO, YEAR, Age, Sex, Race, Educ, moca, source)

# updrs3 score
PD_progression_motor_primary <- read_excel("./project/data/raw_data/PDBP/PDBP_datasets/PD_progression_MDS_UPDRS.xlsx")
colnames(PD_progression_motor_primary)[colnames(PD_progression_motor_primary) == "Study ID"] <- "Study_ID"
colnames(PD_progression_motor_primary)[colnames(PD_progression_motor_primary) == "MDS_UPDRS.Required Fields.VisitTypPDBP"] <- "Date"
colnames(PD_progression_motor_primary)[colnames(PD_progression_motor_primary) == "MDS_UPDRS.Part III: Motor Examination.MDSUPDRS_PartIIIScore"] <- "updrs3_score"
colnames(PD_progression_motor_primary)[colnames(PD_progression_motor_primary) == "MDS_UPDRS.Required Fields.GUID"] <- "PATNO"
PD_progression_motor_primary_filtered <- PD_progression_motor_primary %>%
  group_by(PATNO) %>%
  mutate(has_baseline_record = any(Date == "Baseline")) %>%
  mutate(has_valid_baseline = any(Date == "Baseline" & !is.na(updrs3_score) & updrs3_score <= 32)) %>%
  ungroup() %>%
  filter(has_baseline_record & has_valid_baseline) %>%
  filter(!is.na(updrs3_score)) %>%
  mutate(Event_date = ifelse(Date == "Baseline", 0, ifelse(Date == "6 months", 0.5, ifelse(Date == "12 months", 1, ifelse(Date == "18 months", 1.5, ifelse(Date == "24 months", 2, ifelse(Date == "30 months", 2.5, ifelse(Date == "36 months", 3, ifelse(Date == "42 months", 3.5, ifelse(Date == "48 months", 4, ifelse(Date == "60 months", 5, ifelse(Date == "72 months", 6, NA)))))))))))) %>%
  group_by(PATNO) %>%
  filter(any(Event_date == 0)) %>%
  filter(!any(Event_date == 0 & (is.na(updrs3_score) | updrs3_score > 32))) %>%
  ungroup() %>%
  filter(PATNO %in% PATNO[Event_date > 0]) %>%
  group_by(PATNO) %>%
  filter(max(Event_date) >= 3) %>%
  ungroup() %>%
  group_by(PATNO, Event_date) %>%
  summarize(updrs3_score = mean(updrs3_score, na.rm = TRUE), .groups = 'drop') %>%
  ungroup()

PD_risk_primary_update_PDBP <- merge(PD_progression_motor_primary_filtered, PD_risk, by = "PATNO", all.x = TRUE) %>%
  filter(PD_status == 1) %>%
  rename(YEAR = Event_date) %>%
  group_by(PATNO) %>%
  mutate(
    Age = first(age_at_visit_category),
    Sex = first(SEX),
    Race = first(race),
    Educ = first(Educ)
  ) %>%
  ungroup()

common_PATNOs_PDBP <- intersect(PDBP_drug_library_separated$PATNO, PD_risk_primary_update_PDBP$PATNO)
Medication_history_PD_risk_PDBP <- PDBP_drug_library_separated[PDBP_drug_library_separated$PATNO %in% common_PATNOs_PDBP, ] %>%
  select(PATNO, DrugName)
PD_risk_all_PDBP <- PD_risk_primary_update_PDBP[PD_risk_primary_update_PDBP$PATNO %in% common_PATNOs_PDBP, ] %>%
  mutate(source = 2) %>%
  select(PATNO, YEAR, Age, Sex, Race, Educ, updrs3_score, source)

# combine PPMI and PDBP
Medication_history_PD_risk_all <- rbind(Medication_history_PD_risk_PPMI, Medication_history_PD_risk_PDBP)
PD_risk_all_all <- rbind(PD_risk_all_PPMI, PD_risk_all_PDBP)

### ====================================================================
## A comprehensive drug-wide analysis based on linear mixed-effects model
### ====================================================================

results <- list()
summary_table <- data.frame(Drug = character(), Estimate = numeric(), Std_Error = numeric(), p_value = numeric(), CI_Lower = numeric(), CI_Upper = numeric())

for (drug in drug_list) {
  
  drug_use <- Medication_history_PD_risk_all %>%
    filter(DrugName == drug) %>%
    select(PATNO) %>%
    distinct() %>%
    mutate(Drug_Used = 1)
  
  analysis_data <- PD_risk_all_all %>%
    left_join(drug_use, by = "PATNO") %>%
    mutate(Drug_Used = ifelse(is.na(Drug_Used), 0, 1),
           Drug_Used = factor(Drug_Used, levels = c("0", "1")))
  
  model <- lmerTest::lmer(moca ~ Drug_Used * YEAR + Age + Sex + Race + Educ + source + (1 | PATNO), # for motor - updrs3_score ~ 
                          data = analysis_data)
  
  results[[drug]] <- summary(model)
  
  model_tidy <- broom.mixed::tidy(model, conf.int = TRUE, conf.level = 0.95)
  interaction_term <- model_tidy %>% filter(term == "Drug_Used1:YEAR")
  
  if (nrow(interaction_term) > 0) {
    summary_table <- summary_table %>%
      add_row(Drug = drug, 
              Estimate = interaction_term$estimate, 
              Std_Error = interaction_term$std.error, 
              p_value = interaction_term$p.value,
              CI_Lower = interaction_term$conf.low,
              CI_Upper = interaction_term$conf.high)
  }
}

summary_table <- summary_table %>%
  mutate(FDR = p.adjust(p_value, method = "fdr")) %>%
  arrange(p_value)

print(summary_table)
write.xlsx(summary_table, "./data/results/PD_progression_cognition.xlsx") # for motor - PD_progression_motor.xlsx

### =======================================================
## Draw pictures for the changes of MoCA or UPDRS III score
### =======================================================

# moca change
baseline_moca <- PD_risk_all_all %>%
  group_by(PATNO) %>%
  summarise(Baseline_MOCA = first(moca[order(YEAR)]), .groups = "drop")

plot_data <- PD_risk_all_all %>%
  left_join(baseline_moca, by = "PATNO") %>%
  mutate(MOCA_Change = moca - Baseline_MOCA) %>%
  left_join(Medication_history_PD_risk_all %>%
              filter(DrugName == "Amlodipine") %>% # you can change the drug name to what you want
              select(PATNO) %>%
              distinct() %>%
              mutate(Drug_Used = 1), by = "PATNO") %>%
  mutate(Drug_Used = ifelse(is.na(Drug_Used), 0, 1))

n_never <- length(unique(plot_data$PATNO[plot_data$Drug_Used == 0]))
n_ever <- length(unique(plot_data$PATNO[plot_data$Drug_Used == 1]))

p_value_text <- paste("p =", formatC(summary_table$p_value[summary_table$Drug == "Amlodipine"], format = "e", digits = 2))
fdr_value_text <- paste("FDR =", formatC(summary_table$FDR[summary_table$Drug == "Amlodipine"], format = "e", digits = 2))

ggplot(plot_data, aes(x = YEAR, y = MOCA_Change, 
                      color = as.factor(Drug_Used), 
                      fill = as.factor(Drug_Used))) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.5, alpha = 0.3) +  # trend + shadow
  scale_color_manual(values = c("black", "red"), labels = c(paste("never-user (N=", n_never, ")", sep=""), paste("ever-user (N=", n_ever, ")", sep=""))) +  scale_fill_manual(values = c("black", "red")) +  # color of shadow
  scale_x_continuous(breaks = seq(0, max(plot_data$YEAR), 1), expand = c(0, 0)) +  # X
  scale_y_continuous(expand = c(0, 0)) +  # Y
  labs(x = "Time (years)", y = "Change in MoCA score",
       color = "Drug Usage (Amlodipine)") +  
  theme_classic(base_size = 14) +  # background
  theme(axis.line = element_line(color = "black", size = 0.8),  # X/Y
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black", size = 0.8),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.position = c(0.01, 1.05),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", color = "white")) +
        guides(fill = "none") +
      annotate("text", x = 0.1, y = -1.5, label = p_value_text, size = 5, hjust = 0) +
      annotate("text", x = 2.7, y = -1.5, label = fdr_value_text, size = 5, hjust = 0)

# motor change
baseline_updrs3_score <- PD_risk_all_all %>%
  group_by(PATNO) %>%
  summarise(Baseline_updrs3_score = first(updrs3_score[order(YEAR)]), .groups = "drop")

plot_data <- PD_risk_all_all %>%
  left_join(baseline_updrs3_score, by = "PATNO") %>%
  mutate(updrs3_score_Change = updrs3_score - Baseline_updrs3_score) %>%
  left_join(Medication_history_PD_risk_all %>%
              filter(DrugName == "Sildenafil") %>% # you can change the drug name to what you want
              select(PATNO) %>%
              distinct() %>%
              mutate(Drug_Used = 1), by = "PATNO") %>%
  mutate(Drug_Used = ifelse(is.na(Drug_Used), 0, 1))

n_never <- length(unique(plot_data$PATNO[plot_data$Drug_Used == 0]))
n_ever <- length(unique(plot_data$PATNO[plot_data$Drug_Used == 1]))

p_value_text <- paste("p =", formatC(summary_table$p_value[summary_table$Drug == "Sildenafil"], format = "e", digits = 2))
fdr_value_text <- paste("FDR =", formatC(summary_table$FDR[summary_table$Drug == "Sildenafil"], format = "e", digits = 2))

ggplot(plot_data, aes(x = YEAR, y = updrs3_score_Change, 
                      color = as.factor(Drug_Used), 
                      fill = as.factor(Drug_Used))) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.5, alpha = 0.3) +  # trend + shadow
  scale_color_manual(values = c("black", "red"), labels = c(paste("never-user (N=", n_never, ")", sep=""), paste("ever-user (N=", n_ever, ")", sep=""))) +  scale_fill_manual(values = c("black", "red")) +  # color of shadow
  scale_x_continuous(breaks = seq(0, max(plot_data$YEAR), 1), expand = c(0, 0)) +  # X
  scale_y_continuous(expand = c(0, 0)) +  # Y
  labs(x = "Time (years)", y = "Change in UPDRS3 score",
       color = "Drug Usage (Sildenafil)") +  
  theme_classic(base_size = 14) +  # background
  theme(axis.line = element_line(color = "black", size = 0.8),  # X/Y
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black", size = 0.8),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.position = c(0.015, 1.05),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", color = "white")) +
  guides(fill = "none") +
  annotate("text", x = 9, y = 0.5, label = p_value_text, size = 5, hjust = 0) +
  annotate("text", x = 5.5, y = 0.5, label = fdr_value_text, size = 5, hjust = 0)