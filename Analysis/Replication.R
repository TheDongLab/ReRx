##### Identify drugs that are both replicated in MGB Biobank (discovery) and AMP-PD (replication)

### =====================
## Replicate by drug name
### =====================

setwd("~/project/ReRx/data/results")
PD_risk_results_MGB <- read_excel("~/PD_risk/PD_risk_results_MGB.xlsx")
PD_risk_results_AMPPD <- read_excel("~/PD_risk/PD_risk_results_AMPPD.xlsx")

MGB_increase <- PD_risk_results_MGB %>%
  filter(fdr_value <= 0.05, OR >= 1) %>%
  select(DrugName)

MGB_decrease <- PD_risk_results_MGB %>%
  filter(fdr_value <= 0.05, OR <= 1) %>%
  select(DrugName)

AMPPD_increase <- PD_risk_results_AMPPD %>%
  filter(p_value <= 0.05, OR >= 1) %>%
  select(DrugName)

AMPPD_decrease <- PD_risk_results_AMPPD %>%
  filter(p_value <= 0.05, OR <= 1) %>%
  select(DrugName)

Replicated_increase <- MGB_increase %>%
  filter(DrugName %in% AMPPD_increase$DrugName)

Replicated_decrease <- MGB_decrease %>%
  filter(DrugName %in% AMPPD_decrease$DrugName)

Replicated_results_increase <- PD_risk_results_MGB %>%
  filter(DrugName %in% Replicated_increase$DrugName) %>%
  rename(MGB_OR = OR, MGB_CI_lower = CI_lower, MGB_CI_upper = CI_upper) %>%
  left_join(
    PD_risk_results_AMPPD %>%
      filter(DrugName %in% Replicated_increase$DrugName) %>%
      select(DrugName, OR, CI_lower, CI_upper, p_value) %>%
      rename(AMPPD_OR = OR, AMPPD_CI_lower = CI_lower, AMPPD_CI_upper = CI_upper, AMPPD_p_value = p_value),
    by = "DrugName"
  )

Replicated_results_decrease <- PD_risk_results_MGB %>%
  filter(DrugName %in% Replicated_decrease$DrugName) %>%
  rename(MGB_OR = OR, MGB_CI_lower = CI_lower, MGB_CI_upper = CI_upper) %>%
  left_join(
    PD_risk_results_AMPPD %>%
      filter(DrugName %in% Replicated_decrease$DrugName) %>%
      select(DrugName, OR, CI_lower, CI_upper, p_value) %>%
      rename(AMPPD_OR = OR, AMPPD_CI_lower = CI_lower, AMPPD_CI_upper = CI_upper, AMPPD_p_value = p_value),
    by = "DrugName"
  ) 

PD_risk_results_MGB_increase <- PD_risk_results_MGB[PD_risk_results_MGB$DrugName %in% Replicated_increase$DrugName,]
PD_risk_results_AMPPD_increase <- PD_risk_results_AMPPD[PD_risk_results_AMPPD$DrugName %in% Replicated_increase$DrugName,]
PD_risk_results_MGB_decrease <- PD_risk_results_MGB[PD_risk_results_MGB$DrugName %in% Replicated_decrease$DrugName,]
PD_risk_results_AMPPD_decrease <- PD_risk_results_AMPPD[PD_risk_results_AMPPD$DrugName %in% Replicated_decrease$DrugName,]

### ====================
## Replicate by ATC code
### ====================

atc_codes <- read.csv("~/data/raw_data/atccodes.csv") %>%
  mutate(DrugName = str_to_sentence(tolower(Name)))
MGB_with_ATC <- left_join(MGB_decrease, atc_codes, by = c("DrugName" = "DrugName")) %>%
  distinct(DrugName, ATC.code, .keep_all = TRUE)
# using the code below to save the primary dataset and then update it manually
# write.xlsx(MGB_with_ATC, "MGB_with_ATC.xlsx")

AMPPD_with_ATC <- left_join(AMPPD_decrease, atc_codes, by = c("DrugName" = "DrugName")) %>%
  distinct(DrugName, ATC.code, .keep_all = TRUE)
# using the code below to save the primary dataset and then update it manually
# write.xlsx(AMPPD_with_ATC, "AMPPD_with_ATC.xlsx")

MGB_with_ATC_update <- read_excel("~/MGB_with_ATC_update.xlsx") %>%
  filter(!is.na(ATC.code)) %>%
  mutate(ATC_code_5 = substr(ATC.code, 1, 5)) %>%
  distinct(DrugName, ATC_code_5, .keep_all = TRUE)

AMPPD_with_ATC_update <- read_excel("~/AMPPD_with_ATC_update.xlsx") %>%
  filter(!is.na(ATC.code)) %>%
  mutate(ATC_code_5 = substr(ATC.code, 1, 5)) %>%
  distinct(DrugName, ATC_code_5, .keep_all = TRUE)

ATC_replicated <- intersect(MGB_with_ATC_update$ATC_code_5, AMPPD_with_ATC_update$ATC_code_5)

MGB_replicated <- MGB_with_ATC_update %>%
  filter(ATC_code_5 %in% ATC_replicated)

AMPPD_replicated <- AMPPD_with_ATC_update %>%
  filter(ATC_code_5 %in% ATC_replicated)