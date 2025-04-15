##### Sensitivity analysis

### ======================================================================
## Drug exposure definition and time window prior to the index date (year)
### ======================================================================

# time window prior to the index date
drug_library_separated_1 <- drug_library_separated_index_date %>%
  filter(is.na(Date) | Medication_Date < (Date - years(5)))

# number of prescriptions
drug_exposure <- drug_count %>%
  filter(count >= 2)

# prepare the required tables for sensitivity analysis
drug_library_separated_final <- left_join(drug_library_separated_1, drug_exposure, by = c("EMPI", "DrugName")) %>%
  filter(!is.na(count))

Medication_history_PD_risk <- drug_library_separated_final[drug_library_separated_final$EMPI %in% PD_risk_all_matched$EMPI, ]
drug_patients <- Medication_history_PD_risk %>%
  group_by(DrugName) %>%
  summarise(PatientCount = n_distinct(EMPI)) %>%
  ungroup() %>%
  filter(PatientCount >= 10)

### =========================================
## Draw forest plots for sensitivity analyses
### =========================================

# drugs that are significantly associated with PD risk
Drug_list = c("Amiodarone", "Amlodipine", "Losartan", "Potassium", "Salbutamol", 
              "Fludrocortisone", "Propranolol", "Donepezil", "Mirabegron", 
              "Mirtazapine", "Solifenacin", "Quetiapine", "Gabapentin", 
              "Vitamin B12", "Diazepam", "Sildenafil", "Lorazepam", "Omega-3")

# number of prescriptions
PD_risk_results_MGB <- read_excel("~/PD_risk_results_MGB.xlsx") %>%
  filter(DrugName %in% Drug_list) %>%
  mutate(Group = "≥1") %>%
  mutate(color = "orange")

PD_risk_results_MGB_sensitivity_1 <- read_excel("~/PD_risk_results_MGB_sensitivity1.xlsx") %>%
  filter(DrugName %in% Drug_list) %>%
  mutate(Group = "≥2") %>%
  mutate(color = "skyblue")

data1 <- rbind(PD_risk_results_MGB_sensitivity_1, PD_risk_results_MGB)

for (drug in Drug_list) {
  drug_data <- data1 %>% 
    filter(DrugName == drug) %>%
    mutate(Group = factor(Group, levels = rev(unique(Group))))
  
  plot <- ggplot(drug_data, aes(x = Group, y = OR, ymin = CI_lower, ymax = CI_upper, color = color)) +
    geom_pointrange(size = 1, linewidth = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1.5) +
    scale_color_identity() +
    theme_minimal() +
    labs(x = "No. of prescriptions",
         y = "OR (95% CI)",
         title = drug) +
    theme(axis.text = element_text(size = 14),
          title = element_text(size = 16))
  
  ggsave(filename = paste0(drug, "_forest_plot.pdf"), plot = plot, width = 2, height = 4)
}

# time window prior to the index date
PD_risk_results_MGB <- read_excel("~/PD_risk_results_MGB.xlsx") %>%
  filter(DrugName %in% Drug_list) %>%
  mutate(Group = "0") %>%
  mutate(color = "blue")

PD_risk_results_MGB_sensitivity_2 <- read_excel("~/PD_risk_results_MGB_sensitivity2.xlsx") %>%
  filter(DrugName %in% Drug_list) %>%
  mutate(Group = "1") %>%
  mutate(color = "red")

PD_risk_results_MGB_sensitivity_3 <- read_excel("PD_risk_results_MGB_sensitivity3.xlsx") %>%
  filter(DrugName %in% Drug_list) %>%
  mutate(Group = "2") %>%
  mutate(color = "green")

PD_risk_results_MGB_sensitivity_4 <- read_excel("PD_risk_results_MGB_sensitivity4.xlsx") %>%
  filter(DrugName %in% Drug_list) %>%
  mutate(Group = "5") %>%
  mutate(color = "cyan")

data2 <- rbind(PD_risk_results_MGB_sensitivity_4, PD_risk_results_MGB_sensitivity_3, PD_risk_results_MGB_sensitivity_2, PD_risk_results_MGB)

for (drug in Drug_list) {
  drug_data <- data2 %>% 
    filter(DrugName == drug) %>%
    mutate(Group = factor(Group, levels = rev(unique(Group))))
  
  plot <- ggplot(drug_data, aes(x = Group, y = OR, ymin = CI_lower, ymax = CI_upper, color = color)) +
    geom_pointrange(size = 1, linewidth = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1.5) +
    scale_color_identity() +
    theme_minimal() +
    labs(x = "time window prior to the index date (year)",
         y = "OR (95% CI)",
         title = drug) +
    theme(axis.text = element_text(size = 14),
          title = element_text(size = 16))
  
  ggsave(filename = paste0(drug, "_forest_plot.pdf"), plot = plot, width = 2, height = 4)
}