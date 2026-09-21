# ============================================================
# Rebuild discovery + replication combined results
# Replicated-drug forest plot + supplementary antihypertensive table/figure
# ============================================================

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 0. Paths: ONLY change these
# ------------------------------------------------------------
mgb_file   <- "$HOME/PROJECT_FOLDER/data/results/PD_risk_results_MGB_RxNorm_revised_medication_eligible.xlsx"
amppd_file <- "$HOME/PROJECT_FOLDER/data/results/PD_risk_results_AMPPD.xlsx"
out_dir    <- "$HOME/PROJECT_FOLDER/data/results/"

# ------------------------------------------------------------
# 1. Read xlsx/csv automatically
# ------------------------------------------------------------
read_result_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    read_excel(path)
  } else if (ext == "csv") {
    read_csv(path, show_col_types = FALSE)
  } else if (ext %in% c("tsv", "txt")) {
    read_tsv(path, show_col_types = FALSE)
  } else {
    stop("Unsupported file type: ", ext)
  }
}

MGB_raw   <- read_result_file(mgb_file)
AMPPD_raw <- read_result_file(amppd_file)

# ------------------------------------------------------------
# 2. Check the columns needed downstream
#    Expected from your previous MGB/AMP-PD result pipeline:
#    DrugName, OR, CI_lower, CI_upper,
#    Y_case, N_case, Y_ctrl, N_ctrl, p_value
#    MGB should additionally contain fdr_value
# ------------------------------------------------------------
required_common <- c(
  "DrugName", "OR", "CI_lower", "CI_upper",
  "Y_case", "N_case", "Y_ctrl", "N_ctrl", "p_value"
)

missing_mgb   <- setdiff(required_common, names(MGB_raw))
missing_amppd <- setdiff(required_common, names(AMPPD_raw))

if (length(missing_mgb) > 0) {
  stop("MGB is missing columns: ", paste(missing_mgb, collapse = ", "))
}
if (length(missing_amppd) > 0) {
  stop("AMP-PD is missing columns: ", paste(missing_amppd, collapse = ", "))
}
if (!"fdr_value" %in% names(MGB_raw)) {
  stop("MGB is missing fdr_value.")
}

# If AMP-PD does not contain fdr_value, add an NA column so bind_rows is clean.
if (!"fdr_value" %in% names(AMPPD_raw)) {
  AMPPD_raw$fdr_value <- NA_real_
}

# ------------------------------------------------------------
# 3. Rebuild the combined file
# ------------------------------------------------------------
MGB <- MGB_raw %>%
  mutate(Cohort = "MGB (discovery)")

AMPPD <- AMPPD_raw %>%
  mutate(Cohort = "AMPPD (replication)")

Med <- bind_rows(MGB, AMPPD) %>%
  mutate(
    DrugName = str_squish(as.character(DrugName)),
    DrugName_clean = str_to_lower(DrugName)
  )

write.xlsx(
  Med,
  file = file.path(out_dir, "PD_risk_results.combined.xlsx"),
  overwrite = TRUE
)

# ============================================================
# PART A. Five replicated drugs
# Losartan -> Amlodipine -> Salbutamol -> Melatonin -> Amiodarone
# Albuterol in MGB is harmonized to Salbutamol for matching/display.
# ============================================================

replicated_ref <- tribble(
  ~Drug_display, ~Drug_match,   ~drug_order,
  "Losartan",    "losartan",    1,
  "Amlodipine",  "amlodipine",  2,
  "Salbutamol",  "salbutamol",  3,
  "Melatonin",   "melatonin",   4,
  "Amiodarone",  "amiodarone",  5
)

Med_rep <- Med %>%
  mutate(
    Drug_match = case_when(
      DrugName_clean %in% c("albuterol", "salbutamol") ~ "salbutamol",
      TRUE ~ DrugName_clean
    ),
    cohort_order = case_when(
      Cohort == "MGB (discovery)" ~ 1L,
      Cohort == "AMPPD (replication)" ~ 2L,
      TRUE ~ 99L
    )
  ) %>%
  inner_join(replicated_ref, by = "Drug_match") %>%
  filter(Cohort %in% c("MGB (discovery)", "AMPPD (replication)")) %>%
  arrange(drug_order, cohort_order)

# Safety check: exactly one discovery + one replication row for every drug
rep_check <- Med_rep %>%
  count(Drug_display, Cohort, name = "n")

if (nrow(rep_check) != 10 || any(rep_check$n != 1)) {
  print(rep_check)
  stop(
    "The five-drug table does not contain exactly one row per drug per cohort. ",
    "Check duplicate drug rows or drug-name spelling."
  )
}

# ------------------------------------------------------------
# 4. Export the five-drug discovery/replication result table
# ------------------------------------------------------------
replicated_table <- Med_rep %>%
  mutate(
    `OR (95% CI)` = sprintf("%.2f (%.2f to %.2f)", OR, CI_lower, CI_upper),
    `PD cases Ever-user / Never-user` =
      paste0(format(Y_case, big.mark = ","), "/", format(N_case, big.mark = ",")),
    `Controls Ever-user / Never-user` =
      paste0(format(Y_ctrl, big.mark = ","), "/", format(N_ctrl, big.mark = ",")),
    `P value shown in analysis` = ifelse(
      Cohort == "MGB (discovery)", fdr_value, p_value
    )
  ) %>%
  select(
    Medication = Drug_display,
    Cohort,
    OR,
    CI_lower,
    CI_upper,
    `OR (95% CI)`,
    `PD cases Ever-user / Never-user`,
    `Controls Ever-user / Never-user`,
    p_value,
    fdr_value,
    `P value shown in analysis`
  )

write.xlsx(
  replicated_table,
  file = file.path(out_dir, "five_replicated_drugs_discovery_replication.xlsx"),
  overwrite = TRUE
)

# ------------------------------------------------------------
# 5. Forest plot styled like your previous Figure 1
# ------------------------------------------------------------
plot_rows <- Med_rep %>%
  mutate(
    cohort_label = ifelse(
      Cohort == "MGB (discovery)", "discovery", "replication"
    ),
    case_text = paste0(
      format(Y_case, big.mark = ",", scientific = FALSE),
      "/",
      format(N_case, big.mark = ",", scientific = FALSE)
    ),
    ctrl_text = paste0(
      format(Y_ctrl, big.mark = ",", scientific = FALSE),
      "/",
      format(N_ctrl, big.mark = ",", scientific = FALSE)
    ),
    or_text = sprintf("%.2f   (%.2f  to  %.2f)", OR, CI_lower, CI_upper),
    
    # y positions:
    # each medication uses 3 rows: drug header + discovery + replication
    y_header = 16 - (drug_order - 1) * 3,
    y = ifelse(cohort_order == 1, y_header - 1, y_header - 2)
  )

drug_headers <- replicated_ref %>%
  mutate(y_header = 16 - (drug_order - 1) * 3)

# Layout positions in an artificial x-coordinate system.
med_x       <- 0.15
case_x      <- 2.10
ctrl_x      <- 3.75
forest_left <- 5.20
forest_right<- 8.45
or_text_x   <- 9.05
plot_right  <- 11.25

# Convert OR in [0,1] to figure x coordinates.
or_to_x <- function(z) {
  forest_left + z * (forest_right - forest_left)
}

plot_rows <- plot_rows %>%
  mutate(
    x_or = or_to_x(OR),
    x_lo = or_to_x(pmax(CI_lower, 0)),
    x_hi = or_to_x(pmin(CI_upper, 1))
  )

tick_vals <- c(0, 0.25, 0.50, 0.75, 1.00)
tick_df <- tibble(
  val = tick_vals,
  x = or_to_x(tick_vals),
  lab = sprintf("%.2f", tick_vals)
)

p_rep <- ggplot() +
  # Light blue medication header rows
  geom_rect(
    data = drug_headers,
    aes(
      xmin = 0,
      xmax = plot_right,
      ymin = y_header - 0.45,
      ymax = y_header + 0.45
    ),
    inherit.aes = FALSE,
    fill = "#E3F4FA",
    color = NA
  ) +
  
  # Medication names
  geom_text(
    data = drug_headers,
    aes(x = med_x + 0.15, y = y_header, label = Drug_display),
    inherit.aes = FALSE,
    hjust = 0,
    fontface = "bold.italic",
    size = 4.2
  ) +
  
  # discovery / replication labels
  geom_text(
    data = plot_rows,
    aes(x = med_x + 0.25, y = y, label = cohort_label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.8
  ) +
  
  # Case counts
  geom_text(
    data = plot_rows,
    aes(x = case_x, y = y, label = case_text),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.8
  ) +
  
  # Control counts
  geom_text(
    data = plot_rows,
    aes(x = ctrl_x, y = y, label = ctrl_text),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.8
  ) +
  
  # Reference line at OR = 1
  geom_segment(
    aes(
      x = or_to_x(1),
      xend = or_to_x(1),
      y = 0.65,
      yend = 16.55
    ),
    linetype = "dashed",
    linewidth = 0.55
  ) +
  
  # CI
  geom_segment(
    data = plot_rows,
    aes(x = x_lo, xend = x_hi, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.8
  ) +
  
  # CI end caps
  geom_segment(
    data = plot_rows,
    aes(x = x_lo, xend = x_lo, y = y - 0.18, yend = y + 0.18),
    inherit.aes = FALSE,
    linewidth = 0.8
  ) +
  geom_segment(
    data = plot_rows,
    aes(x = x_hi, xend = x_hi, y = y - 0.18, yend = y + 0.18),
    inherit.aes = FALSE,
    linewidth = 0.8
  ) +
  
  # OR point
  geom_point(
    data = plot_rows,
    aes(x = x_or, y = y),
    inherit.aes = FALSE,
    size = 3.0
  ) +
  
  # Right-side OR text
  geom_text(
    data = plot_rows,
    aes(x = or_text_x, y = y, label = or_text),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.8
  ) +
  
  # Column headers
  annotate(
    "text",
    x = med_x,
    y = 17.1,
    label = "Medication",
    hjust = 0,
    fontface = "bold",
    size = 4.1
  ) +
  annotate(
    "text",
    x = case_x,
    y = 17.1,
    label = "PD cases",
    hjust = 0,
    fontface = "bold",
    size = 4.1
  ) +
  annotate(
    "text",
    x = ctrl_x,
    y = 17.1,
    label = "Controls",
    hjust = 0,
    fontface = "bold",
    size = 4.1
  ) +
  annotate(
    "text",
    x = or_text_x,
    y = 17.1,
    label = "OR (95% CI)",
    hjust = 0,
    fontface = "bold",
    size = 4.1
  ) +
  annotate(
    "text",
    x = case_x,
    y = 16.35,
    label = "Ever-user / Never-user",
    hjust = 0,
    size = 3.8
  ) +
  
  # Custom forest x-axis
  geom_segment(
    aes(
      x = forest_left,
      xend = forest_right,
      y = 0.55,
      yend = 0.55
    ),
    linewidth = 0.75
  ) +
  geom_segment(
    data = tick_df,
    aes(x = x, xend = x, y = 0.55, yend = 0.35),
    inherit.aes = FALSE,
    linewidth = 0.65
  ) +
  geom_text(
    data = tick_df,
    aes(x = x, y = 0.05, label = lab),
    inherit.aes = FALSE,
    size = 3.6
  ) +
  annotate(
    "text",
    x = (forest_left + forest_right) / 2,
    y = -0.60,
    label = "Decreased PD risk (OR)",
    size = 4.0
  ) +
  
  coord_cartesian(
    xlim = c(0, plot_right),
    ylim = c(-1.0, 17.5),
    clip = "off"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.margin = margin(10, 12, 8, 10)
  )

p_rep

ggsave(
  file.path(out_dir, "five_replicated_drugs_forest_plot.pdf"),
  p_rep,
  width = 11.5,
  height = 8.0,
  units = "in"
)

ggsave(
  file.path(out_dir, "five_replicated_drugs_forest_plot.png"),
  p_rep,
  width = 11.5,
  height = 8.0,
  units = "in",
  dpi = 600
)

# ============================================================
# PART B. Supplementary antihypertensive medication table
# Same logic as your current code, now built from the new Med object.
# ============================================================

drug_ref <- tribble(
  ~Category, ~DrugName,
  "Angiotensin II receptor blockers (ARBs), plain – C09CA", "Losartan",
  "Angiotensin II receptor blockers (ARBs), plain – C09CA", "Eprosartan",
  "Angiotensin II receptor blockers (ARBs), plain – C09CA", "Valsartan",
  "Angiotensin II receptor blockers (ARBs), plain – C09CA", "Irbesartan",
  "Angiotensin II receptor blockers (ARBs), plain – C09CA", "Tasosartan",
  "Angiotensin II receptor blockers (ARBs), plain – C09CA", "Candesartan",
  "Angiotensin II receptor blockers (ARBs), plain – C09CA", "Telmisartan",
  "Angiotensin II receptor blockers (ARBs), plain – C09CA", "Olmesartan medoxomil",
  "Angiotensin II receptor blockers (ARBs), plain – C09CA", "Azilsartan medoxomil",
  "Angiotensin II receptor blockers (ARBs), plain – C09CA", "Fimasartan",
  
  "Calcium channel blockers (CCBs) – C08", "Amlodipine",
  "Calcium channel blockers (CCBs) – C08", "Felodipine",
  "Calcium channel blockers (CCBs) – C08", "Isradipine",
  "Calcium channel blockers (CCBs) – C08", "Nicardipine",
  "Calcium channel blockers (CCBs) – C08", "Nifedipine",
  "Calcium channel blockers (CCBs) – C08", "Nimodipine",
  "Calcium channel blockers (CCBs) – C08", "Nisoldipine",
  "Calcium channel blockers (CCBs) – C08", "Nitrendipine",
  "Calcium channel blockers (CCBs) – C08", "Lacidipine",
  "Calcium channel blockers (CCBs) – C08", "Nilvadipine",
  "Calcium channel blockers (CCBs) – C08", "Manidipine",
  "Calcium channel blockers (CCBs) – C08", "Barnidipine",
  "Calcium channel blockers (CCBs) – C08", "Lercanidipine",
  "Calcium channel blockers (CCBs) – C08", "Cilnidipine",
  "Calcium channel blockers (CCBs) – C08", "Benidipine",
  "Calcium channel blockers (CCBs) – C08", "Clevidipine",
  "Calcium channel blockers (CCBs) – C08", "Levamlodipine",
  "Calcium channel blockers (CCBs) – C08", "Mibefradil",
  "Calcium channel blockers (CCBs) – C08", "Verapamil",
  "Calcium channel blockers (CCBs) – C08", "Gallopamil",
  "Calcium channel blockers (CCBs) – C08", "Etripamil",
  "Calcium channel blockers (CCBs) – C08", "Diltiazem",
  "Calcium channel blockers (CCBs) – C08", "Fendiline",
  "Calcium channel blockers (CCBs) – C08", "Bepridil",
  "Calcium channel blockers (CCBs) – C08", "Lidoflazine",
  "Calcium channel blockers (CCBs) – C08", "Perhexiline",
  
  "ACE inhibitors, plain – C09AA", "Captopril",
  "ACE inhibitors, plain – C09AA", "Enalapril",
  "ACE inhibitors, plain – C09AA", "Lisinopril",
  "ACE inhibitors, plain – C09AA", "Perindopril",
  "ACE inhibitors, plain – C09AA", "Ramipril",
  "ACE inhibitors, plain – C09AA", "Quinapril",
  "ACE inhibitors, plain – C09AA", "Benazepril",
  "ACE inhibitors, plain – C09AA", "Cilazapril",
  "ACE inhibitors, plain – C09AA", "Fosinopril",
  "ACE inhibitors, plain – C09AA", "Trandolapril",
  "ACE inhibitors, plain – C09AA", "Spirapril",
  "ACE inhibitors, plain – C09AA", "Delapril",
  "ACE inhibitors, plain – C09AA", "Moexipril",
  "ACE inhibitors, plain – C09AA", "Temocapril",
  "ACE inhibitors, plain – C09AA", "Zofenopril",
  "ACE inhibitors, plain – C09AA", "Imidapril",
  
  "Beta blockers (BBs) – C07", "Alprenolol",
  "Beta blockers (BBs) – C07", "Oxprenolol",
  "Beta blockers (BBs) – C07", "Pindolol",
  "Beta blockers (BBs) – C07", "Propranolol",
  "Beta blockers (BBs) – C07", "Timolol",
  "Beta blockers (BBs) – C07", "Sotalol",
  "Beta blockers (BBs) – C07", "Nadolol",
  "Beta blockers (BBs) – C07", "Mepindolol",
  "Beta blockers (BBs) – C07", "Carteolol",
  "Beta blockers (BBs) – C07", "Tertatolol",
  "Beta blockers (BBs) – C07", "Bopindolol",
  "Beta blockers (BBs) – C07", "Bupranolol",
  "Beta blockers (BBs) – C07", "Penbutolol",
  "Beta blockers (BBs) – C07", "Cloranolol",
  "Beta blockers (BBs) – C07", "Practolol",
  "Beta blockers (BBs) – C07", "Metoprolol",
  "Beta blockers (BBs) – C07", "Atenolol",
  "Beta blockers (BBs) – C07", "Acebutolol",
  "Beta blockers (BBs) – C07", "Betaxolol",
  "Beta blockers (BBs) – C07", "Bevantolol",
  "Beta blockers (BBs) – C07", "Bisoprolol",
  "Beta blockers (BBs) – C07", "Celiprolol",
  "Beta blockers (BBs) – C07", "Esmolol",
  "Beta blockers (BBs) – C07", "Epanolol",
  "Beta blockers (BBs) – C07", "S-atenolol",
  "Beta blockers (BBs) – C07", "Nebivolol",
  "Beta blockers (BBs) – C07", "Talinolol",
  "Beta blockers (BBs) – C07", "Landiolol",
  "Beta blockers (BBs) – C07", "Labetalol",
  "Beta blockers (BBs) – C07", "Carvedilol",
  
  "Thiazides, plain – C03AA", "Bendroflumethiazide",
  "Thiazides, plain – C03AA", "Hydroflumethiazide",
  "Thiazides, plain – C03AA", "Hydrochlorothiazide",
  "Thiazides, plain – C03AA", "Chlorothiazide",
  "Thiazides, plain – C03AA", "Polythiazide",
  "Thiazides, plain – C03AA", "Trichlormethiazide",
  "Thiazides, plain – C03AA", "Cyclopenthiazide",
  "Thiazides, plain – C03AA", "Methyclothiazide",
  "Thiazides, plain – C03AA", "Cyclothiazide",
  "Thiazides, plain – C03AA", "Mebutizide",
  
  "C09X other agents acting on the renin-angiotensin system", "Remikiren",
  "C09X other agents acting on the renin-angiotensin system", "Aliskiren",
  "C09X other agents acting on the renin-angiotensin system", "Sparsentan",
  
  "C03D aldosterone antagonists and other potassium-sparing agents", "Spironolactone",
  "C03D aldosterone antagonists and other potassium-sparing agents", "Potassium canrenoate",
  "C03D aldosterone antagonists and other potassium-sparing agents", "Canrenone",
  "C03D aldosterone antagonists and other potassium-sparing agents", "Eplerenone",
  "C03D aldosterone antagonists and other potassium-sparing agents", "Finerenone",
  "C03D aldosterone antagonists and other potassium-sparing agents", "Amiloride",
  "C03D aldosterone antagonists and other potassium-sparing agents", "Triamterene"
) %>%
  mutate(
    DrugName_clean = str_to_lower(str_trim(DrugName)),
    order_id = row_number()
  )

# ------------------------------------------------------------
# 6. Clean combined results for supplementary table
# ------------------------------------------------------------
Med_clean <- Med %>%
  mutate(
    DrugName_clean = str_to_lower(str_trim(DrugName)),
    Cohort = str_trim(Cohort),
    OR_CI = ifelse(
      !is.na(OR) & !is.na(CI_lower) & !is.na(CI_upper),
      sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
      NA_character_
    ),
    Case = ifelse(
      !is.na(Y_case) & !is.na(N_case),
      paste0(Y_case, "/", N_case),
      NA_character_
    ),
    Control = ifelse(
      !is.na(Y_ctrl) & !is.na(N_ctrl),
      paste0(Y_ctrl, "/", N_ctrl),
      NA_character_
    ),
    P_value_use = case_when(
      Cohort == "MGB (discovery)" ~ fdr_value,
      Cohort == "AMPPD (replication)" ~ p_value,
      TRUE ~ p_value
    )
  ) %>%
  filter(
    Cohort %in% c("MGB (discovery)", "AMPPD (replication)")
  )

MGB_data <- Med_clean %>%
  filter(Cohort == "MGB (discovery)") %>%
  select(
    DrugName_clean,
    `MGB Biobank (discovery) OR (95% CI)` = OR_CI,
    `MGB Cases Ever-user / Never-user` = Case,
    `MGB Controls Ever-user / Never-user` = Control,
    `MGB P value (FDR)` = P_value_use
  )

AMPPD_data <- Med_clean %>%
  filter(Cohort == "AMPPD (replication)") %>%
  select(
    DrugName_clean,
    `AMP-PD (replication) OR (95% CI)` = OR_CI,
    `AMP-PD Cases Ever-user / Never-user` = Case,
    `AMP-PD Controls Ever-user / Never-user` = Control,
    `AMP-PD P value` = P_value_use
  )

final_table <- drug_ref %>%
  left_join(MGB_data, by = "DrugName_clean") %>%
  left_join(AMPPD_data, by = "DrugName_clean") %>%
  arrange(order_id) %>%
  select(
    Category,
    `Antihypertensive medications` = DrugName,
    `MGB Biobank (discovery) OR (95% CI)`,
    `MGB Cases Ever-user / Never-user`,
    `MGB Controls Ever-user / Never-user`,
    `MGB P value (FDR)`,
    `AMP-PD (replication) OR (95% CI)`,
    `AMP-PD Cases Ever-user / Never-user`,
    `AMP-PD Controls Ever-user / Never-user`,
    `AMP-PD P value`
  )

write.xlsx(
  final_table,
  file = file.path(out_dir, "antihypertensive_medications_integrated_results.xlsx"),
  overwrite = TRUE
)

# ============================================================
# PART C. Supplementary MGB antihypertensive forest plot
# Current logic retained: MGB p_value < 0.05
# If you want FDR < 0.05 instead, replace p_value with fdr_value below.
# ============================================================

plot_data_mgb <- Med_clean %>%
  filter(Cohort == "MGB (discovery)") %>%
  inner_join(
    drug_ref %>%
      select(
        Category,
        DrugName_clean,
        DrugName_ref = DrugName,
        order_id
      ),
    by = "DrugName_clean"
  ) %>%
  mutate(
    DrugName = DrugName_ref,
    
    Class_abbr = case_when(
      str_detect(Category, "ACE inhibitors") ~ "ACEi",
      str_detect(Category, "Angiotensin II receptor blockers") ~ "ARB",
      str_detect(Category, regex("aldosterone", ignore_case = TRUE)) ~
        "Aldosterone antagonist",
      str_detect(Category, "Calcium channel blockers") ~ "CCB",
      str_detect(Category, "Beta blockers") ~ "BB",
      str_detect(Category, "Thiazides") ~ "Thiazide",
      str_detect(Category, "renin-angiotensin") ~ "Other RAAS",
      TRUE ~ "Other"
    ),
    
    Group_order = case_when(
      Class_abbr == "ACEi" ~ 1,
      Class_abbr == "ARB" ~ 2,
      Class_abbr == "Aldosterone antagonist" ~ 3,
      TRUE ~ 4
    ),
    
    Direction = case_when(
      OR < 1 ~ "OR < 1",
      OR > 1 ~ "OR > 1",
      TRUE ~ "OR = 1"
    )
  ) %>%
  filter(
    fdr_value < 0.05,
    !is.na(OR),
    !is.na(CI_lower),
    !is.na(CI_upper)
  )

if (nrow(plot_data_mgb) > 0) {
  
  drug_order_df <- plot_data_mgb %>%
    distinct(
      DrugName,
      DrugName_clean,
      Class_abbr,
      Group_order,
      order_id
    ) %>%
    arrange(Group_order, order_id)
  
  plot_data_mgb <- plot_data_mgb %>%
    mutate(
      DrugName = factor(
        DrugName,
        levels = rev(drug_order_df$DrugName)
      )
    )
  
  right_labels <- plot_data_mgb %>%
    distinct(DrugName, Class_abbr)
  
  x_min <- min(plot_data_mgb$CI_lower, na.rm = TRUE)
  x_max <- max(plot_data_mgb$CI_upper, na.rm = TRUE)
  right_label_x <- x_max * 2.2
  
  p_supp <- ggplot(
    plot_data_mgb,
    aes(
      x = OR,
      y = DrugName,
      xmin = CI_lower,
      xmax = CI_upper,
      color = Direction
    )
  ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.5,
      color = "grey45"
    ) +
    geom_errorbarh(
      height = 0.18,
      linewidth = 0.65
    ) +
    geom_point(size = 2.5) +
    geom_text(
      data = right_labels,
      aes(
        x = right_label_x,
        y = DrugName,
        label = Class_abbr
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 3.2
    ) +
    annotate(
      "text",
      x = right_label_x,
      y = length(unique(plot_data_mgb$DrugName)) + 1,
      label = "Drug class",
      hjust = 0,
      fontface = "bold",
      size = 3.5
    ) +
    scale_x_log10(
      limits = c(x_min * 0.7, right_label_x * 1.8),
      breaks = c(0.3, 0.5, 1, 2, 5, 10),
      labels = c("0.3", "0.5", "1.0", "2.0", "5.0", "10.0")
    ) +
    scale_color_manual(
      values = c(
        "OR < 1" = "#D62728",
        "OR > 1" = "#1F77B4",
        "OR = 1" = "grey40"
      )
    ) +
    labs(
      x = "Odds ratio (log scale)",
      y = "Medications",
      color = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "top",
      axis.text.y = element_text(size = 9),
      axis.title.y = element_text(face = "bold"),
      axis.title.x = element_text(size = 11),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.4),
      plot.margin = margin(10, 110, 10, 10)
    )
  
  p_supp
  
  plot_height <- max(
    6,
    0.35 * length(unique(plot_data_mgb$DrugName)) + 1.5
  )
  
  ggsave(
    file.path(out_dir, "significant_antihypertensive_forest_plot_revised.pdf"),
    p_supp,
    width = 10,
    height = plot_height,
    units = "in"
  )
  
  ggsave(
    file.path(out_dir, "significant_antihypertensive_forest_plot_revised.png"),
    p_supp,
    width = 10,
    height = plot_height,
    units = "in",
    dpi = 600
  )
  
} else {
  warning("No MGB antihypertensive drugs met fdr_value < 0.05; supplementary plot not created.")
}

# ============================================================
# Finished.
# Main outputs:
# 1) PD_risk_results.combined.xlsx
# 2) five_replicated_drugs_discovery_replication.xlsx
# 3) five_replicated_drugs_forest_plot.pdf/png
# 4) antihypertensive_medications_integrated_results.xlsx
# 5) significant_antihypertensive_forest_plot_revised.pdf/png
# ============================================================
