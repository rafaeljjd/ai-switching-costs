# ==============================================================================
# 05_subsample_chatgpt_no_claude.R — ChatGPT users without Claude subscription
# ==============================================================================

source(file.path(here::here(), "source", "ai_usage", "00_setup.R"))
df <- read_rds(file.path(DIR_CLEAN, "ai_usage_clean.rds"))

# --- Restrict sample ---------------------------------------------------------
sub <- df |>
  filter(personal_chat_gpt_open_ai == TRUE, paid_claude_pro == FALSE)

cat("=============================================================\n")
cat("Subsample: ChatGPT users WITHOUT Claude Pro subscription\n")
cat("=============================================================\n")
cat(sprintf("N = %d  (full sample: %d)\n\n", nrow(sub), nrow(df)))

# --- 1. Multihoming ----------------------------------------------------------
cat("--- Multihoming ---\n")
cat(sprintf("Multihomers (2+ platforms): %d / %d (%.1f%%)\n",
            sum(sub$is_multihomer), nrow(sub),
            mean(sub$is_multihomer) * 100))
cat(sprintf("Mean platforms used: %.1f\n\n", mean(sub$n_personal)))

cat("Platform count distribution:\n")
print(sub |> count(n_personal) |> mutate(pct = round(n / sum(n) * 100, 1)))
cat("\n")

# --- 2. WTA for memory erasure -----------------------------------------------
cat("--- WTA for Memory Erasure ---\n")
wta <- sub$wta_num
cat(sprintf("N with valid WTA: %d\n", sum(!is.na(wta))))
cat(sprintf("Mean:   $%.1f\n", mean(wta, na.rm = TRUE)))
cat(sprintf("Median: $%.0f\n", median(wta, na.rm = TRUE)))
cat(sprintf("SD:     $%.1f\n", sd(wta, na.rm = TRUE)))
cat(sprintf("%% at $0:          %.1f%%\n", mean(wta == 0, na.rm = TRUE) * 100))
cat(sprintf("%% 'No amount':    %.1f%%\n", mean(sub$wta_infinite, na.rm = TRUE) * 100))
cat(sprintf("Mean (cens $1k):  $%.1f\n", mean(sub$wta_cens_1000, na.rm = TRUE)))
cat(sprintf("Mean (cens $2k):  $%.1f\n\n", mean(sub$wta_cens_2000, na.rm = TRUE)))

cat("WTA distribution:\n")
print(sub |>
        count(wta_memory_erasure) |>
        mutate(pct = round(n / sum(n) * 100, 1)) |>
        arrange(match(wta_memory_erasure,
                      c("$0","$5","$10","$25","$50","$100","$200","$500",
                        "$1,000","No amount would be enough"))))
cat("\n")

# --- 3. WTP for Claude (3 scenarios) -----------------------------------------
cat("--- WTP for Claude Pro (3 scenarios) ---\n\n")

val_summary <- function(x, label) {
  x <- x[!is.na(x)]
  cat(sprintf("%s  (N=%d)\n", label, length(x)))
  cat(sprintf("  Mean:   $%.1f\n", mean(x)))
  cat(sprintf("  Median: $%.0f\n", median(x)))
  cat(sprintf("  SD:     $%.1f\n", sd(x)))
  cat(sprintf("  %% at $0: %.1f%%\n\n", mean(x == 0) * 100))
}

val_summary(sub$wtp_no_strings_num,    "WTP No Strings (W2)")
val_summary(sub$wtp_with_erasure_num,  "WTP With Erasure (W4)")
val_summary(sub$wtp_with_transfer_num, "WTP With Transfer (W3)")

cat("--- Within-subject gaps ---\n")
cat(sprintf("Demand suppression (W2-W4): mean = $%.2f, SD = $%.2f\n",
            mean(sub$demand_suppression, na.rm = TRUE),
            sd(sub$demand_suppression, na.rm = TRUE)))
cat(sprintf("Portability recovery (W3-W4): mean = $%.2f, SD = $%.2f\n",
            mean(sub$portability_recovery, na.rm = TRUE),
            sd(sub$portability_recovery, na.rm = TRUE)))
cat(sprintf("Effective portability (phi_eff): median = %.2f, mean = %.2f\n",
            median(sub$phi_eff, na.rm = TRUE),
            mean(sub$phi_eff, na.rm = TRUE)))
