library(dplyr)
library(readr)
library(metafor)

# Read datasets
CCAEAll_df <- read_csv("/home/sk3486/Medication/FinalResults/CCAE5.csv")
MDCRAll_df <- read_csv("/home/sk3486/Medication/FinalResults/MDCR5.csv")
MedicaidAll_df <- read_csv("/home/sk3486/Medication/FinalResults/Medicaid5.csv")

# Combine datasets
AllDatasets_combined <- bind_rows(
  CCAEAll_df %>% mutate(dataset = "CCAE"),
  MDCRAll_df %>% mutate(dataset = "MDCR"),
  MedicaidAll_df %>% mutate(dataset = "Medicaid")
)

# Convert OR and CI to logOR and SE
AllDatasets_combined <- AllDatasets_combined %>%
  mutate(
    logOR = log(OR),
    SE = (log(CI_upper) - log(CI_lower)) / (2 * 1.96),
    N_total = N_case + N_ctrl
  )

# Prepare separate results dataframes
headers <- c("drug", "nb_total", "odd_ratio", "low_odd_ratio", "high_odd_ratio", "p_value")
result_DL <- data.frame(matrix(ncol = length(headers), nrow = 0))
colnames(result_DL) <- headers

result_HK <- data.frame(matrix(ncol = length(headers), nrow = 0))
colnames(result_HK) <- headers

drug_list <- unique(AllDatasets_combined$Active_Ingredients)

for (d in drug_list) {
  drug_data <- AllDatasets_combined %>% filter(Active_Ingredients == d)

  if (nrow(drug_data) < 2) next  # skip single-dataset drugs

  nb_total <- sum(drug_data$N_total, na.rm = TRUE)

  # DL meta-analysis
  res_DL <- tryCatch({
    rma(yi = logOR, sei = SE, data = drug_data, method = "DL", measure = "OR")
  }, error = function(e) NULL)

  if (!is.null(res_DL)) {
    result_DL <- rbind(result_DL,
                       data.frame(
                         drug = d,
                         nb_total = nb_total,
                         odd_ratio = exp(res_DL$b),
                         low_odd_ratio = exp(res_DL$ci.lb),
                         high_odd_ratio = exp(res_DL$ci.ub),
                         p_value = res_DL$pval
                       ))
  }
}

# Adjust p-values (FDR)
result_DL$fdr <- p.adjust(result_DL$p_value, method = "fdr")

# Save results
write.csv(result_DL, "/home/sk3486/Medication/Meta_Analysis_5.csv", row.names = FALSE)