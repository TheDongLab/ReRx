##### Demographic characteristics of MGB Biobank and AMP-PD

### =====
## AMP-PD
### =====

# characteristic
PD_risk_all_all <- PD_risk_all_all %>%
  mutate(
    Age_Category_update = case_when(
      age_at_visit_category %in% c(1, 2, 3) ~ "30-59",
      age_at_visit_category %in% c(4, 5, 6, 7) ~ "≥ 60",
      TRUE ~ "Unknown"
    ),
    Gender_update = ifelse(SEX == 1, "Male", "Female"),
    Race_Category_update = case_when(
      race == 1 ~ "White",
      race %in% 2:4 ~ "Non-white",
      TRUE ~ "Unknown"
    )
  )

calculate_p_value <- function(variable) {
  tbl <- table(PD_risk_all_all[[variable]], PD_risk_all_all$PD_status)
  if (min(tbl) < 5) {
    fisher.test(tbl)$p.value
  } else {
    chisq.test(tbl)$p.value
  }
}

p_values <- data.frame(
  Variable = c("Age", "Gender", "Race"),
  P_value = c(
    calculate_p_value("Age_Category_update"),
    calculate_p_value("Gender_update"),
    calculate_p_value("Race_Category_update")
  )
)

age_summary <- PD_risk_all_all %>%
  group_by(PD_status, Age_Category_update) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(PD_status) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

gender_summary <- PD_risk_all_all %>%
  group_by(PD_status, Gender_update) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(PD_status) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

race_summary <- PD_risk_all_all %>%
  group_by(PD_status, Race_Category_update) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(PD_status) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

final_table <- list(
  Age = age_summary %>%
    mutate(Category = case_when(
      Age_Category_update == "30-59" ~ "30-59",
      Age_Category_update == "≥ 60" ~ "≥ 60",
      TRUE ~ "Unknown"
    )),
  Gender = gender_summary %>%
    mutate(Category = ifelse(Gender_update == "Male", "Male", "Female")),
  Race = race_summary %>%
    mutate(Category = ifelse(Race_Category_update == "White", "White", "Non-white"))
) %>%
  bind_rows(.id = "Variable") %>%
  select(Variable, Category, PD_status, count, percent) %>%
  pivot_wider(names_from = PD_status, values_from = c(count, percent)) %>%
  left_join(p_values, by = "Variable")

print(final_table)

### ==========
## MGB Biobank
### ==========

# characteristic
PD_risk_all_matched <- PD_risk_all_matched %>%
  mutate(
    Age_Category_update = case_when(
      Age_Category %in% c(1, 2, 3) ~ "30-59",
      Age_Category %in% c(4, 5, 6, 7) ~ "≥ 60",
      TRUE ~ "Unknown"
    ),
    Gender_update = ifelse(Gender == 1, "Male", "Female"),
    Race_Category_update = case_when(
      Race_Category == 1 ~ "White",
      Race_Category %in% 2:4 ~ "Non-white",
      TRUE ~ "Unknown"
    )
  )

calculate_p_value <- function(variable) {
  tbl <- table(PD_risk_all_matched[[variable]], PD_risk_all_matched$PD_status)
  if (min(tbl) < 5) {
    fisher.test(tbl)$p.value
  } else {
    chisq.test(tbl)$p.value
  }
}

p_values <- data.frame(
  Variable = c("Age", "Gender", "Race"),
  P_value = c(
    calculate_p_value("Age_Category_update"),
    calculate_p_value("Gender_update"),
    calculate_p_value("Race_Category_update")
  )
)

age_summary <- PD_risk_all_matched %>%
  group_by(PD_status, Age_Category_update) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(PD_status) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

gender_summary <- PD_risk_all_matched %>%
  group_by(PD_status, Gender_update) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(PD_status) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

race_summary <- PD_risk_all_matched %>%
  group_by(PD_status, Race_Category_update) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(PD_status) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

final_table <- list(
  Age = age_summary %>%
    mutate(Category = case_when(
      Age_Category_update == "30-59" ~ "30-59",
      Age_Category_update == "≥ 60" ~ "≥ 60",
      TRUE ~ "Unknown"
    )),
  Gender = gender_summary %>%
    mutate(Category = ifelse(Gender_update == "Male", "Male", "Female")),
  Race = race_summary %>%
    mutate(Category = ifelse(Race_Category_update == "White", "White", "Non-white"))
) %>%
  bind_rows(.id = "Variable") %>%
  select(Variable, Category, PD_status, count, percent) %>%
  pivot_wider(names_from = PD_status, values_from = c(count, percent)) %>%
  left_join(p_values, by = "Variable")

print(final_table)
