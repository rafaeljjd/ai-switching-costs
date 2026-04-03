# ==============================================================================
# 04b_mechanisms.R — Cross-sectional mechanism tests
# ==============================================================================

source(file.path(here::here(), "source", "ai_usage", "00_setup.R"))
df <- read_rds(file.path(DIR_CLEAN, "ai_usage_clean.rds"))

N <- nrow(df)

# =============================================================================
# 1. Demand suppression × usage frequency
# =============================================================================

# OLS: demand_suppression ~ frequency + demographics
df_reg <- df |>
  filter(!is.na(demand_suppression)) |>
  mutate(
    freq_num = as.numeric(frequency_ordered)  # 1=Less than weekly ... 4=Daily
  )

reg_suppression <- lm(
  demand_suppression ~ freq_num + age_group + gender + education,
  data = df_reg
)

reg_summary <- summary(reg_suppression)
cat("=== Regression: Demand Suppression on Usage Frequency ===\n")
print(reg_summary)

# Save coefficient table
reg_coef <- broom::tidy(reg_suppression) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
save_tab("tab_reg_suppression", reg_coef)

# Fig 13: Conditional means by frequency bin
fig_supp_freq <- df_reg |>
  group_by(frequency_ordered) |>
  summarise(
    mean_supp = mean(demand_suppression, na.rm = TRUE),
    se_supp   = sd(demand_suppression, na.rm = TRUE) / sqrt(n()),
    n         = n(),
    .groups   = "drop"
  ) |>
  ggplot(aes(x = frequency_ordered, y = mean_supp)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_errorbar(aes(ymin = mean_supp - 1.96 * se_supp,
                    ymax = mean_supp + 1.96 * se_supp),
                width = 0.2) +
  geom_text(aes(label = paste0("$", round(mean_supp, 1), "\n(n=", n, ")")),
            vjust = -0.7, size = 3.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = label_dollar(),
                     expand = expansion(mult = c(0.05, 0.2))) +
  labs(x = "Usage frequency", y = "Mean demand suppression (Q13 - Q14)",
       title = "Demand Suppression by Usage Frequency",
       subtitle = "95% CI bars | Does heavier use deepen lock-in?")

save_fig("fig_suppression_by_frequency", fig_supp_freq)

# =============================================================================
# 2. WTA in 2×2 grid: memory awareness × perceived improvement
# =============================================================================

improvement_median <- median(df$perceived_improvement_num, na.rm = TRUE)

df_2x2 <- df |>
  filter(!is.na(wta_num), !is.na(perceived_improvement_num)) |>
  mutate(
    aware = if_else(ai_remembers == "Yes", "Aware", "Unaware / Unsure"),
    improvement_group = if_else(perceived_improvement_num >= improvement_median,
                                "High improvement", "Low improvement"),
    improvement_group = factor(improvement_group,
                               levels = c("Low improvement", "High improvement"))
  )

fig_wta_2x2_data <- df_2x2 |>
  group_by(aware, improvement_group) |>
  summarise(
    mean_wta = mean(wta_num),
    se_wta   = sd(wta_num) / sqrt(n()),
    n        = n(),
    .groups  = "drop"
  )

fig_wta_2x2 <- ggplot(fig_wta_2x2_data,
                       aes(x = improvement_group, y = mean_wta, fill = aware)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_wta - se_wta, ymax = mean_wta + se_wta),
                position = position_dodge(width = 0.7), width = 0.2) +
  geom_text(aes(label = paste0("$", round(mean_wta), "\n(n=", n, ")"),
                group = aware),
            position = position_dodge(width = 0.7), vjust = -0.8, size = 3) +
  scale_fill_manual(values = c("Aware" = "#2C7BB6", "Unaware / Unsure" = "#D7191C")) +
  scale_y_continuous(labels = label_dollar(),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(x = NULL, y = "Mean WTA for memory erasure",
       fill = "Memory awareness",
       title = "WTA by Memory Awareness and Perceived Improvement",
       subtitle = paste0("Median split at perceived improvement = ", improvement_median)) +
  theme(legend.position = "top")

save_fig("fig_wta_awareness_improvement", fig_wta_2x2)

# =============================================================================
# 3. Concentration beliefs vs actual multihoming
# =============================================================================

conc_levels <- c(
  "Each would be much less personalized than if I used only one",
  "Each would be somewhat less personalized",
  "It wouldn't make much difference",
  "Each might actually be better, since they'd specialize in different tasks"
)

conc_short <- c(
  "Each would be much less personalized than if I used only one" = "Much less\npersonalized",
  "Each would be somewhat less personalized" = "Somewhat less\npersonalized",
  "It wouldn't make much difference" = "No difference",
  "Each might actually be better, since they'd specialize in different tasks" = "Better\n(specialize)"
)

fig_conc_data <- df |>
  filter(!is.na(memory_concentration_effect), memory_concentration_effect != "") |>
  mutate(
    belief = factor(memory_concentration_effect, levels = conc_levels),
    belief_short = conc_short[memory_concentration_effect],
    belief_short = factor(belief_short,
                          levels = c("Much less\npersonalized",
                                     "Somewhat less\npersonalized",
                                     "No difference",
                                     "Better\n(specialize)"))
  ) |>
  group_by(belief_short) |>
  summarise(
    pct_multihome = mean(is_multihomer) * 100,
    n             = n(),
    .groups       = "drop"
  )

fig_concentration <- ggplot(fig_conc_data,
                            aes(x = belief_short, y = pct_multihome)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_text(aes(label = paste0(round(pct_multihome), "%\n(n=", n, ")")),
            vjust = -0.5, size = 3.2) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Belief about concentration effect",
       y = "% who actually multihome (use 2+ platforms)",
       title = "Concentration Beliefs vs. Actual Multihoming",
       subtitle = "Do people who believe concentration helps still multihome?")

save_fig("fig_concentration_vs_multihoming", fig_concentration, width = 9, height = 5.5)
