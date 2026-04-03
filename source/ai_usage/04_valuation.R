# ==============================================================================
# 04_valuation.R — WTA/WTP analysis, demand curves, model objects
# ==============================================================================

source(file.path(here::here(), "source", "ai_usage", "00_setup.R"))
df <- read_rds(file.path(DIR_CLEAN, "ai_usage_clean.rds"))

N <- nrow(df)

# =============================================================================
# Mapping table: Survey → Model notation
# =============================================================================

tab_model_mapping <- tribble(
  ~survey_question, ~variable,                      ~model_symbol, ~description,
  "Q12",           "wta_memory_erasure",             "W1",          "WTA to erase ChatGPT memory",
  "Q13",           "wtp_claude_no_strings",           "W2",          "WTP for Claude (memory intact)",
  "Q14",           "wtp_claude_with_erasure",         "W4",          "WTP for Claude (memory erased)",
  "Q15",           "wtp_claude_with_transfer",        "W3",          "WTP for Claude (memory transferred)"
)

tab_model_mapping <- tab_model_mapping |>
  add_row(survey_question = "Q13 - Q14", variable = "demand_suppression",
          model_symbol = "W2 - W4", description = "Demand suppression from memory") |>
  add_row(survey_question = "Q15 - Q14", variable = "portability_recovery",
          model_symbol = "W3 - W4", description = "Demand recovered by portability") |>
  add_row(survey_question = "1-(Q13-Q15)/(Q13-Q14)", variable = "phi_eff",
          model_symbol = "phi_eff", description = "Effective portability share")

save_tab("tab_model_mapping", tab_model_mapping)

# =============================================================================
# Table 4: Valuation summary
# =============================================================================

val_summary <- function(x, label) {
  x <- x[!is.na(x)]
  tibble(
    measure = label,
    n       = length(x),
    pct_zero = mean(x == 0) * 100,
    mean    = mean(x),
    median  = median(x),
    sd      = sd(x),
    iqr     = IQR(x)
  )
}

tab_valuation <- bind_rows(
  val_summary(df$wta_num,              "WTA memory erasure (W1)"),
  val_summary(df$wtp_no_strings_num,   "WTP Claude no strings (W2)"),
  val_summary(df$wtp_with_erasure_num, "WTP Claude with erasure (W4)"),
  val_summary(df$wtp_with_transfer_num,"WTP Claude with transfer (W3)")
)

save_tab("tab_valuation_summary", tab_valuation)

# =============================================================================
# Fig 9: WTA distribution
# =============================================================================

wta_levels <- c("$0", "$5", "$10", "$25", "$50", "$100", "$200", "$500",
                "$1,000", "No amount would be enough")

fig_wta <- df |>
  mutate(wta = factor(wta_memory_erasure, levels = wta_levels)) |>
  count(wta) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(x = wta, y = pct)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 3.2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "% of respondents",
       title = "WTA for Permanent Memory Erasure",
       subtitle = paste0(
         "Mean = $", round(mean(df$wta_num, na.rm = TRUE)),
         " | Median = $", median(df$wta_num, na.rm = TRUE),
         " (excl. 'No amount')"))

save_fig("fig_wta_distribution", fig_wta)

# =============================================================================
# Fig 10: WTP comparison across 3 scenarios (grouped bars)
# =============================================================================

wtp_levels <- c("$0", "$5", "$10", "$15", "$20", "$30", "$50", "More than $50")

fig10_data <- bind_rows(
  df |> transmute(scenario = "No strings (W2)",     wtp = wtp_claude_no_strings),
  df |> transmute(scenario = "With erasure (W4)",    wtp = wtp_claude_with_erasure),
  df |> transmute(scenario = "With transfer (W3)",   wtp = wtp_claude_with_transfer)
) |>
  mutate(
    wtp      = factor(wtp, levels = wtp_levels),
    scenario = factor(scenario,
                      levels = c("No strings (W2)", "With transfer (W3)",
                                 "With erasure (W4)"))
  ) |>
  count(scenario, wtp) |>
  group_by(scenario) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

fig_wtp_comparison <- ggplot(fig10_data, aes(x = wtp, y = pct, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = c("No strings (W2)" = "#2C7BB6",
                                "With transfer (W3)" = "#74AA9C",
                                "With erasure (W4)" = "#D7191C")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Monthly WTP", y = "% of respondents", fill = NULL,
       title = "WTP for Claude Pro Across Scenarios",
       subtitle = "Within-subject comparison") +
  theme(legend.position = "top")

save_fig("fig_wtp_comparison", fig_wtp_comparison, width = 10, height = 6)

# =============================================================================
# Fig 11: Demand curves (2 panels)
# =============================================================================

# Panel A: Complementary CDF (demand curve)
price_grid <- c(0, 5, 10, 15, 20, 30, 50)

ccdf_data <- expand_grid(
  scenario = c("No strings (W2)", "With transfer (W3)", "With erasure (W4)"),
  price = price_grid
) |>
  mutate(
    share = pmap_dbl(list(scenario, price), function(s, p) {
      vals <- case_when(
        s == "No strings (W2)"    ~ df$wtp_no_strings_num,
        s == "With transfer (W3)" ~ df$wtp_with_transfer_num,
        s == "With erasure (W4)"  ~ df$wtp_with_erasure_num
      )
      mean(vals >= p, na.rm = TRUE)
    }),
    scenario = factor(scenario,
                      levels = c("No strings (W2)", "With transfer (W3)",
                                 "With erasure (W4)"))
  )

p11a <- ggplot(ccdf_data, aes(x = price, y = share, color = scenario)) +
  geom_step(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("No strings (W2)" = "#2C7BB6",
                                 "With transfer (W3)" = "#74AA9C",
                                 "With erasure (W4)" = "#D7191C")) +
  scale_x_continuous(labels = label_dollar(), breaks = price_grid) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  labs(x = "Monthly price", y = "Share willing to pay at least p",
       color = NULL, title = "A. Demand Curves") +
  theme(legend.position = "top")

# Panel B: Extensive vs intensive margin
margin_data <- tribble(
  ~scenario, ~extensive, ~intensive,
  "No strings\n(W2)",
    mean(df$wtp_no_strings_num > 0, na.rm = TRUE),
    mean(df$wtp_no_strings_num[df$wtp_no_strings_num > 0], na.rm = TRUE),
  "With transfer\n(W3)",
    mean(df$wtp_with_transfer_num > 0, na.rm = TRUE),
    mean(df$wtp_with_transfer_num[df$wtp_with_transfer_num > 0], na.rm = TRUE),
  "With erasure\n(W4)",
    mean(df$wtp_with_erasure_num > 0, na.rm = TRUE),
    mean(df$wtp_with_erasure_num[df$wtp_with_erasure_num > 0], na.rm = TRUE)
) |>
  mutate(scenario = factor(scenario,
                           levels = c("No strings\n(W2)", "With transfer\n(W3)",
                                      "With erasure\n(W4)")))

p11b_ext <- ggplot(margin_data, aes(x = scenario, y = extensive)) +
  geom_col(fill = "#2C7BB6", width = 0.5) +
  geom_text(aes(label = paste0(round(extensive * 100), "%")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Share with WTP > $0",
       title = "B. Extensive Margin")

p11b_int <- ggplot(margin_data, aes(x = scenario, y = intensive)) +
  geom_col(fill = "#74AA9C", width = 0.5) +
  geom_text(aes(label = paste0("$", round(intensive, 1))),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = label_dollar(),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Mean WTP | WTP > $0",
       title = "C. Intensive Margin")

fig_demand_curves <- p11a / (p11b_ext | p11b_int) + plot_layout(heights = c(2, 1))
save_fig("fig_demand_curves", fig_demand_curves, width = 10, height = 9)

# =============================================================================
# Fig 12: Mean WTA by perceived improvement level (bar chart)
# =============================================================================

fig_wta_by_imp <- df |>
  filter(!is.na(perceived_improvement_num), !is.na(wta_num)) |>
  group_by(perceived_improvement_num) |>
  summarise(
    mean_wta = mean(wta_num),
    se_wta   = sd(wta_num) / sqrt(n()),
    n        = n(),
    .groups  = "drop"
  ) |>
  ggplot(aes(x = factor(perceived_improvement_num), y = mean_wta)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_errorbar(aes(ymin = mean_wta - se_wta, ymax = mean_wta + se_wta),
                width = 0.2) +
  geom_text(aes(label = paste0("$", round(mean_wta))),
            vjust = -1, size = 3.2) +
  scale_y_continuous(labels = label_dollar(),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Perceived improvement from memory (1-7)",
       y = "Mean WTA for memory erasure",
       title = "WTA by Perceived Improvement",
       subtitle = "Error bars = SE of mean")

save_fig("fig_wta_by_improvement", fig_wta_by_imp)

# =============================================================================
# Within-subject t-tests
# =============================================================================

# Demand suppression: Q13 - Q14 against zero
tt_suppression <- t.test(df$demand_suppression, mu = 0)
suppression_stats <- list(
  mean = mean(df$demand_suppression, na.rm = TRUE),
  sd   = sd(df$demand_suppression, na.rm = TRUE),
  p    = tt_suppression$p.value
)

# Portability recovery: Q15 - Q14 against zero
tt_recovery <- t.test(df$portability_recovery, mu = 0)
recovery_stats <- list(
  mean = mean(df$portability_recovery, na.rm = TRUE),
  sd   = sd(df$portability_recovery, na.rm = TRUE),
  p    = tt_recovery$p.value
)

# Effective portability
phi_stats <- list(
  median = median(df$phi_eff, na.rm = TRUE),
  mean   = mean(df$phi_eff, na.rm = TRUE),
  n      = sum(!is.na(df$phi_eff))
)

cat("\n=== Demand Suppression (Q13 - Q14) ===\n")
cat(sprintf("Mean = $%.2f, SD = $%.2f, p = %.4f\n",
            suppression_stats$mean, suppression_stats$sd, suppression_stats$p))

cat("\n=== Portability Recovery (Q15 - Q14) ===\n")
cat(sprintf("Mean = $%.2f, SD = $%.2f, p = %.4f\n",
            recovery_stats$mean, recovery_stats$sd, recovery_stats$p))

cat("\n=== Effective Portability (phi_eff) ===\n")
cat(sprintf("Median = %.2f, Mean = %.2f, N = %d\n",
            phi_stats$median, phi_stats$mean, phi_stats$n))

# =============================================================================
# Consistency check
# =============================================================================

consistency <- df |>
  filter(!is.na(wtp_no_strings_num) & !is.na(wtp_with_transfer_num) &
         !is.na(wtp_with_erasure_num)) |>
  summarise(
    pct_consistent = mean(wtp_no_strings_num >= wtp_with_transfer_num &
                          wtp_with_transfer_num >= wtp_with_erasure_num) * 100,
    pct_w2_ge_w4   = mean(wtp_no_strings_num >= wtp_with_erasure_num) * 100,
    n              = n()
  )

cat("\n=== Consistency Check ===\n")
cat(sprintf("W2 >= W3 >= W4: %.1f%%\n", consistency$pct_consistent))
cat(sprintf("W2 >= W4: %.1f%%\n", consistency$pct_w2_ge_w4))

# =============================================================================
# Sensitivity: censored values
# =============================================================================

sensitivity <- tribble(
  ~scenario, ~wta_mean, ~wtp_ns_mean, ~wtp_er_mean, ~wtp_tr_mean,
  "Baseline (excl. censored)",
    mean(df$wta_num, na.rm = TRUE),
    mean(df$wtp_no_strings_num, na.rm = TRUE),
    mean(df$wtp_with_erasure_num, na.rm = TRUE),
    mean(df$wtp_with_transfer_num, na.rm = TRUE),
  "Censor 'No amount' at $1,000",
    mean(df$wta_cens_1000, na.rm = TRUE),
    mean(df$wtp_no_strings_cens, na.rm = TRUE),
    mean(df$wtp_with_erasure_cens, na.rm = TRUE),
    mean(df$wtp_with_transfer_cens, na.rm = TRUE),
  "Censor 'No amount' at $2,000",
    mean(df$wta_cens_2000, na.rm = TRUE),
    mean(df$wtp_no_strings_cens, na.rm = TRUE),
    mean(df$wtp_with_erasure_cens, na.rm = TRUE),
    mean(df$wtp_with_transfer_cens, na.rm = TRUE)
)

save_tab("tab_sensitivity", sensitivity)
cat("\nSensitivity analysis saved.\n")
