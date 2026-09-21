library(tidyverse)
library(patchwork)

# =========================
# 1. Data
# =========================

df <- tribble(
  ~analysis, ~group, ~HR, ~LCL, ~UCL, ~type,
  "Primary analysis", "ARB", 0.70, 0.59, 0.83, "Primary",
  "SA1. Unweighted Cox regression model", "ARB", 0.76, 0.64, 0.90, "Secondary",
  "SA2. Follow-up initiated at the index date", "ARB", 0.72, 0.61, 0.84, "Secondary",
  "SA3. Alternative outcome definition", "ARB", 0.66, 0.55, 0.80, "Secondary",
  "SA4. Alternative PD diagnosis ICD-10-CM code", "ARB", 0.71, 0.60, 0.84, "Secondary",
  "SA5. Intention-to-treat (ITT) analysis", "ARB", 0.84, 0.74, 0.96, "Secondary",
  "SA6. 1:1 Propensity score matching", "ARB", 0.75, 0.62, 0.92, "Secondary",
  "SA7. Exclusion of β-blockers", "ARB", 0.78, 0.66, 0.93, "Secondary",
  "SA8. Additional censoring (discontinuation)", "ARB", 0.69, 0.56, 0.85, "Secondary",
  "SA9. Exposure window restricted to 180 days", "ARB", 0.70, 0.58, 0.83, "Secondary",
  "SA10. 1-year PD outcome lag analysis", "ARB", 0.73, 0.60, 0.88, "Secondary",
  "SA11. 2-year PD outcome lag analysis", "ARB", 0.75, 0.61, 0.93, "Secondary",
  "SA12. 5-year PD outcome lag analysis", "ARB", 0.95, 0.69, 1.31, "Secondary",
  "SA13. Expanded covariates in PS model", "ARB", 0.73, 0.61, 0.86, "Secondary",
  "SA14. ARB + additional covariate adjustment", "ARB", 0.74, 0.62, 0.88, "Secondary",
  
  "Primary analysis", "CCB", 0.88, 0.77, 0.99, "Primary",
  "SA1. Unweighted Cox regression model", "CCB", 0.90, 0.80, 1.02, "Secondary",
  "SA2. Follow-up initiated at the index date", "CCB", 0.87, 0.78, 0.99, "Secondary",
  "SA3. Alternative outcome definition", "CCB", 0.90, 0.78, 1.03, "Secondary",
  "SA4. Alternative PD diagnosis ICD-10-CM code", "CCB", 0.88, 0.78, 1.00, "Secondary",
  "SA5. Intention-to-treat (ITT) analysis", "CCB", 1.01, 0.92, 1.11, "Secondary",
  "SA6. 1:1 Propensity score matching", "CCB", 0.85, 0.73, 0.99, "Secondary",
  "SA7. Exclusion of β-blockers", "CCB", 0.94, 0.82, 1.08, "Secondary",
  "SA8. Additional censoring (discontinuation)", "CCB", 0.90, 0.77, 1.05, "Secondary",
  "SA9. Exposure window restricted to 180 days", "CCB", 0.86, 0.75, 0.98, "Secondary",
  "SA10. 1-year PD outcome lag analysis", "CCB", 0.80, 0.69, 0.92, "Secondary",
  "SA11. 2-year PD outcome lag analysis", "CCB", 0.77, 0.65, 0.92, "Secondary",
  "SA12. 5-year PD outcome lag analysis", "CCB", 0.84, 0.63, 1.11, "Secondary",
  "SA13. Expanded covariates in PS model", "CCB", 0.89, 0.78, 1.01, "Secondary"
)

# =========================
# 2. Order
# =========================

order_levels <- c(
  "Primary analysis",
  "SA1. Unweighted Cox regression model",
  "SA2. Follow-up initiated at the index date",
  "SA3. Alternative outcome definition",
  "SA4. Alternative PD diagnosis ICD-10-CM code",
  "SA5. Intention-to-treat (ITT) analysis",
  "SA6. 1:1 Propensity score matching",
  "SA7. Exclusion of β-blockers",
  "SA8. Additional censoring (discontinuation)",
  "SA9. Exposure window restricted to 180 days",
  "SA10. 1-year PD outcome lag analysis",
  "SA11. 2-year PD outcome lag analysis",
  "SA12. 5-year PD outcome lag analysis",
  "SA13. Expanded covariates in PS model",
  "SA14. ARB + additional covariate adjustment"
)

df <- df %>%
  mutate(
    analysis = factor(analysis,
                      levels = rev(order_levels)),
    type = factor(type,
                  levels = c("Primary", "Secondary"))
  )

# =========================
# 3. Common x-axis settings
# =========================

x_limits <- c(0.50, 1.35)
x_breaks <- c(0.50, 0.75, 1.00, 1.20)


# =========================
# 4. ARB forest plot
# =========================

p_arb <- df %>%
  filter(group == "ARB") %>%
  ggplot(aes(x = HR, y = analysis)) +
  
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "gray45"
  ) +
  
  geom_errorbarh(
    aes(xmin = LCL, xmax = UCL),
    height = 0.22,
    linewidth = 0.8
  ) +
  
  geom_point(
    aes(shape = type),
    size = 3.5,
    color = "#C00000"
  ) +
  
  scale_shape_manual(
    values = c(
      Primary = 17,
      Secondary = 16
    )
  ) +
  
  scale_x_continuous(
    limits = x_limits,
    breaks = x_breaks,
    labels = c("0.50", "0.75", "1.00", "1.20"),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  labs(
    title = "ARB vs Other first-line antihypertensives",
    x = "Adjusted Hazard Ratio (95% CI)",
    y = NULL
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 12,
      hjust = 0.5
    ),
    
    axis.text.x = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.title.x = element_text(
      size = 11,
      margin = margin(t = 8)
    )
  )

# =========================
# 5. CCB forest plot
# =========================

p_ccb <- df %>%
  filter(group == "CCB") %>%
  ggplot(aes(x = HR, y = analysis)) +
  
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "gray45"
  ) +
  
  geom_errorbarh(
    aes(xmin = LCL, xmax = UCL),
    height = 0.22,
    linewidth = 0.8
  ) +
  
  geom_point(
    aes(shape = type),
    size = 3.5,
    color = "#0086B8"
  ) +
  
  scale_shape_manual(
    values = c(
      Primary = 17,
      Secondary = 16
    )
  ) +
  
  scale_x_continuous(
    limits = x_limits,
    breaks = x_breaks,
    labels = c("0.50", "0.75", "1.00", "1.20"),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  labs(
    title = "CCB vs Other first-line antihypertensives",
    x = "Adjusted Hazard Ratio (95% CI)",
    y = NULL
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 12,
      hjust = 0.5
    ),
    
    axis.text.x = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.title.x = element_text(
      size = 11,
      margin = margin(t = 8)
    )
  )

# =========================
# 6. Left text panel
# =========================

label_df <- tibble(
  analysis = factor(
    rev(order_levels),
    levels = rev(order_levels)
  )
)

p_labels <- ggplot(label_df, aes(y = analysis)) +
  
  geom_text(
    data = label_df %>%
      filter(as.character(analysis) == "Primary analysis"),
    aes(x = 0, label = "Primary Analysis"),
    hjust = 0,
    fontface = "bold",
    size = 4
  ) +
  
  geom_text(
    data = label_df %>%
      filter(as.character(analysis) != "Primary analysis"),
    aes(
      x = 0,
      label = paste0(
        "•   ",
        str_remove(
          as.character(analysis),
          "^SA[0-9]+\\. "
        )
      )
    ),
    hjust = 0,
    size = 3.6
  ) +
  
  xlim(0, 1) +
  
  theme_void()

display_labels <- c(
  "Primary analysis" = "Primary Analysis",
  "SA1. Unweighted Cox regression model" =
    "•   Unweighted Cox regression model",
  "SA2. Follow-up initiated at the index date" =
    "•   Follow-up initiated at the index date",
  "SA3. Alternative outcome definition" =
    "•   Alternative outcome definition",
  "SA4. Alternative PD diagnosis ICD-10-CM code" =
    "•   Alternative PD diagnosis ICD-10-CM code",
  "SA5. Intention-to-treat (ITT) analysis" =
    "•   Intention-to-treat (ITT) analysis",
  "SA6. 1:1 Propensity score matching" =
    "•   1:1 propensity score matching",
  "SA7. Exclusion of β-blockers" =
    "•   Exclusion of β-blockers",
  "SA8. Additional censoring (discontinuation)" =
    "•   Additional censoring (discontinuation)",
  "SA9. Exposure window restricted to 180 days" =
    "•   Exposure window restricted to 180 days",
  "SA10. 1-year PD outcome lag analysis" =
    "•   1-year PD outcome lag analysis",
  "SA11. 2-year PD outcome lag analysis" =
    "•   2-year PD outcome lag analysis",
  "SA12. 5-year PD outcome lag analysis" =
    "•   5-year PD outcome lag analysis",
  "SA13. Expanded covariates in PS model" =
    "•   Expanded covariates in PS model",
  "SA14. ARB + additional covariate adjustment" =
    "•   Additional adjustment for residual imbalance"
)

label_df <- tibble(
  analysis = factor(
    order_levels,
    levels = rev(order_levels)
  ),
  label = display_labels[order_levels]
)

p_labels <- ggplot(label_df, aes(y = analysis)) +
  
  geom_text(
    aes(x = 0, label = label),
    hjust = 0,
    size = 3.6,
    fontface = ifelse(
      label_df$analysis == "Primary analysis",
      "bold",
      "plain"
    )
  ) +
  
  xlim(0, 1) +
  theme_void()

p_final <-
  p_labels +
  p_arb +
  p_ccb +
  plot_layout(
    widths = c(1.35, 1, 1)
  )

p_final

ggsave(
  "$HOME/TTE/HTN_final/MDCR_results/target_trial_sensitivity_forest_plot_updated_1.pdf",
  p_final,
  width = 12,
  height = 4,
)