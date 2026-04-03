# ==============================================================================
# 01_clean.R — Data ingestion and cleaning
# ==============================================================================

source(file.path(here::here(), "source", "ai_usage", "00_setup.R"))

# --- Read Qualtrics CSV (3 header rows) --------------------------------------
raw <- read_csv(file.path(DIR_RAW, "ai_usage.csv"),
                col_names = TRUE, show_col_types = FALSE)

# Rows 1-2 of the data are Qualtrics question text and import IDs — drop them
raw <- raw |> slice(-(1:2))

# --- Filter invalid responses ------------------------------------------------
df <- raw |>
  filter(Status != "Survey Preview") |>
  filter(`consent-agree` == "I agree")

cat("N after cleaning:", nrow(df), "\n")

# --- Type conversions --------------------------------------------------------
df <- df |>
  mutate(
    Progress          = as.numeric(Progress),
    duration_sec      = as.numeric(`Duration (in seconds)`),
    duration_min      = duration_sec / 60,
    StartDate         = ymd_hms(StartDate),
    EndDate           = ymd_hms(EndDate),
    RecordedDate      = ymd_hms(RecordedDate)
  )

# --- WTA / WTP to numeric ---------------------------------------------------

# WTA for memory erasure
wta_map <- c(
  "$0" = 0, "$5" = 5, "$10" = 10, "$25" = 25, "$50" = 50,
  "$100" = 100, "$200" = 200, "$500" = 500, "$1,000" = 1000
)

df <- df |>
  mutate(
    wta_infinite  = wta_memory_erasure == "No amount would be enough",
    wta_num       = wta_map[wta_memory_erasure],
    wta_cens_1000 = if_else(wta_infinite, 1000, wta_num),
    wta_cens_2000 = if_else(wta_infinite, 2000, wta_num)
  )

# WTP for Claude (3 scenarios)
wtp_map <- c(
  "$0" = 0, "$5" = 5, "$10" = 10, "$15" = 15,
  "$20" = 20, "$30" = 30, "$50" = 50
)

for (var in c("wtp_claude_no_strings", "wtp_claude_with_erasure",
              "wtp_claude_with_transfer")) {
  suffix <- gsub("wtp_claude_", "", var)
  df[[paste0("wtp_", suffix, "_gt50")]] <- df[[var]] == "More than $50"
  df[[paste0("wtp_", suffix, "_num")]]  <- wtp_map[df[[var]]]
  df[[paste0("wtp_", suffix, "_cens")]] <- if_else(
    df[[var]] == "More than $50", 75, wtp_map[df[[var]]]
  )
}

# WTP ad-free
wtp_adfree_map <- c(
  "$0 - I'd rather see ads than pay" = 0, "$5/month" = 5,
  "$10/month" = 10, "$15/month" = 15, "$20/month" = 20
)

df <- df |>
  mutate(
    wtp_adfree_gt20 = wtp_adfree == "More than $20/month",
    wtp_adfree_num  = wtp_adfree_map[wtp_adfree],
    wtp_adfree_cens = if_else(wtp_adfree_gt20, 30, wtp_adfree_num)
  )

# --- Likert / ordinal to numeric ---------------------------------------------
extract_leading <- function(x) {
  as.numeric(str_extract(x, "^\\d+"))
}

df <- df |>
  mutate(
    perceived_improvement_num = extract_leading(perceived_improvement_from_memory),
    forecast_quality_num      = extract_leading(forecast_quality_loss),
    ad_sentiment_num          = extract_leading(ad_sentiment),
    forecast_fidelity_num     = as.numeric(str_extract(forecast_transfer_fidelity, "\\d+"))
  )

# --- Parse multi-select columns ---------------------------------------------
platforms_all <- c(
  "ChatGPT (OpenAI)", "Google Gemini", "Grok (xAI)", "Microsoft Copilot",
  "Claude (Anthropic)", "Perplexity", "DeepSeek",
  "Meta AI (Facebook / Instagram / WhatsApp)", "Character.AI"
)

switching_loss_items <- c(
  "It wouldn't know my preferences or style",
  "I'd lose my conversation history",
  "I'd have to re-explain my ongoing projects and context",
  "The alternative is just lower quality, regardless of personalization",
  "I'd have to learn a new interface",
  "I'd lose integration with other tools I use",
  "Nothing significant"
)

# Helper: create indicator columns from a comma-separated multi-select
explode_multiselect <- function(df, col, items, prefix) {
  for (item in items) {
    safe_name <- paste0(prefix, "_", janitor::make_clean_names(item))
    df[[safe_name]] <- str_detect(df[[col]], fixed(item)) & !is.na(df[[col]])
  }
  df[[paste0("n_", prefix)]] <- str_count(df[[col]], fixed(",")) + 1L
  df[[paste0("n_", prefix)]][is.na(df[[col]]) | df[[col]] == ""] <- 0L
  df
}

df <- df |>
  explode_multiselect("ai_used_personal",    platforms_all,        "personal") |>
  explode_multiselect("ai_used_work",         platforms_all,        "work") |>
  explode_multiselect("switching_losses",     switching_loss_items, "loss")

# Paid subscriptions (different item names)
paid_items <- c(
  "ChatGPT Plus or Pro", "Claude Pro",
  "Google Gemini Advanced / Google One AI Premium",
  "Grok Premium (X Premium+)", "Perplexity Pro", "Microsoft Copilot Pro"
)

for (item in paid_items) {
  safe <- paste0("paid_", janitor::make_clean_names(item))
  df[[safe]] <- str_detect(df$ai_paid_subscriptions, fixed(item)) &
                !is.na(df$ai_paid_subscriptions)
}
df$n_paid <- str_count(df$ai_paid_subscriptions, fixed(",")) + 1L
df$n_paid[is.na(df$ai_paid_subscriptions) |
          df$ai_paid_subscriptions == "None" |
          df$ai_paid_subscriptions == ""] <- 0L

# --- Derived variables -------------------------------------------------------
df <- df |>
  mutate(
    is_multihomer = n_personal >= 2,
    is_paying     = n_paid > 0,

    # Ordered factor for conversations to personalize
    conversations_ordered = factor(
      conversations_to_personalize,
      levels  = c("1-5", "6-20", "21-50", "50+",
                  "It would never fully catch up"),
      ordered = TRUE
    ),

    # Frequency as ordered factor
    frequency_ordered = factor(
      ai_frequency_personal,
      levels  = c("Less than weekly", "Weekly",
                  "Several times a week", "Daily"),
      ordered = TRUE
    )
  )

# --- Within-subject valuation gaps -------------------------------------------
df <- df |>
  mutate(
    # Demand suppression: Q13 - Q14 (no-strings minus with-erasure)
    demand_suppression   = wtp_no_strings_num - wtp_with_erasure_num,
    # Portability recovery: Q15 - Q14 (with-transfer minus with-erasure)
    portability_recovery = wtp_with_transfer_num - wtp_with_erasure_num,
    # Effective portability: phi_eff = 1 - (Q13 - Q15) / (Q13 - Q14)
    phi_eff = case_when(
      demand_suppression == 0 ~ NA_real_,
      TRUE ~ 1 - (wtp_no_strings_num - wtp_with_transfer_num) / demand_suppression
    )
  )

# --- Save cleaned data -------------------------------------------------------
write_rds(df, file.path(DIR_CLEAN, "ai_usage_clean.rds"))
cat("Cleaned data saved. Rows:", nrow(df), " Cols:", ncol(df), "\n")
