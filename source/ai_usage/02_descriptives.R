# ==============================================================================
# 02_descriptives.R — Sample description, usage patterns, and ads
# ==============================================================================

source(file.path(here::here(), "source", "ai_usage", "00_setup.R"))
df <- read_rds(file.path(DIR_CLEAN, "ai_usage_clean.rds"))

N <- nrow(df)

# =============================================================================
# Table 1: Demographics
# =============================================================================

demo_vars <- c("age_group", "gender", "education", "occupation", "industry")

tab_demographics <- map_dfr(demo_vars, function(v) {
  df |>
    count(category = .data[[v]]) |>
    mutate(variable = v, pct = n / sum(n) * 100) |>
    select(variable, category, n, pct)
})

save_tab("tab_demographics", tab_demographics)

# =============================================================================
# Table 2: Platform usage (personal, work, paid)
# =============================================================================

platforms_all <- c(
  "ChatGPT (OpenAI)", "Google Gemini", "Grok (xAI)", "Microsoft Copilot",
  "Claude (Anthropic)", "Perplexity", "DeepSeek",
  "Meta AI (Facebook / Instagram / WhatsApp)", "Character.AI"
)

tab_platform <- tibble(platform = platforms_all) |>
  mutate(
    n_personal = map_dbl(platform, ~ sum(str_detect(df$ai_used_personal, fixed(.x)),
                                         na.rm = TRUE)),
    n_work     = map_dbl(platform, ~ sum(str_detect(df$ai_used_work, fixed(.x)),
                                         na.rm = TRUE)),
    n_paid     = map_dbl(platform, ~ {
      # Map platform name to subscription name
      sub_name <- case_when(
        .x == "ChatGPT (OpenAI)"  ~ "ChatGPT Plus or Pro",
        .x == "Claude (Anthropic)" ~ "Claude Pro",
        .x == "Google Gemini"      ~ "Google Gemini Advanced",
        .x == "Microsoft Copilot"  ~ "Microsoft Copilot Pro",
        .x == "Perplexity"         ~ "Perplexity Pro",
        .x == "Grok (xAI)"        ~ "Grok Premium",
        TRUE ~ NA_character_
      )
      if (is.na(sub_name)) return(0)
      sum(str_detect(df$ai_paid_subscriptions, fixed(sub_name)), na.rm = TRUE)
    }),
    pct_personal = n_personal / N * 100,
    pct_work     = n_work / N * 100,
    pct_paid     = n_paid / N * 100
  ) |>
  arrange(desc(n_personal))

save_tab("tab_platform_usage", tab_platform)

# =============================================================================
# Inline stats
# =============================================================================

stats_desc <- list(
  N              = N,
  median_dur_min = median(df$duration_min, na.rm = TRUE),
  pct_chatgpt    = sum(str_detect(df$ai_used_personal,
                                   fixed("ChatGPT (OpenAI)")), na.rm = TRUE) / N * 100,
  pct_claude     = sum(str_detect(df$ai_used_personal,
                                   fixed("Claude (Anthropic)")), na.rm = TRUE) / N * 100,
  pct_paying     = mean(df$is_paying) * 100,
  pct_multihomer = mean(df$is_multihomer) * 100,
  mean_platforms  = mean(df$n_personal)
)

# =============================================================================
# Fig 1: Platform market shares (personal vs work)
# =============================================================================

fig1_data <- tab_platform |>
  select(platform, Personal = n_personal, Work = n_work) |>
  pivot_longer(-platform, names_to = "context", values_to = "n") |>
  mutate(
    pct = n / N * 100,
    platform = fct_reorder(platform, pct, .fun = max)
  )

fig_platform_shares <- ggplot(fig1_data,
       aes(y = platform, x = pct, fill = context)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c(Personal = "#2C7BB6", Work = "#D7191C")) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(x = "% of respondents", y = NULL, fill = NULL,
       title = "AI Platform Usage",
       subtitle = "Personal vs. work use (multi-select)") +
  theme(legend.position = "top")

save_fig("fig_platform_shares", fig_platform_shares)

# =============================================================================
# Fig 2: Usage frequency
# =============================================================================

fig_usage_freq <- df |>
  mutate(freq = factor(ai_frequency_personal,
                       levels = c("Daily", "Several times a week",
                                  "Weekly", "Less than weekly"))) |>
  count(freq) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(x = freq, y = pct)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "% of respondents",
       title = "AI Usage Frequency (Personal)")

save_fig("fig_usage_frequency", fig_usage_freq)

# =============================================================================
# Fig 3: Number of platforms (single panel)
# =============================================================================

fig_multihoming <- df |>
  mutate(n_plat = pmin(n_personal, 5),
         n_plat_lab = if_else(n_plat == 5, "5+", as.character(n_plat))) |>
  count(n_plat, n_plat_lab) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(x = factor(n_plat), y = pct)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 3.5) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5+")) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Number of AI platforms used (personal)", y = "% of respondents",
       title = "Platform Multihoming")

save_fig("fig_multihoming", fig_multihoming)

# Inline: multihoming frequency distribution
multihoming_freq_tab <- df |> count(multihoming_frequency) |> mutate(pct = n / sum(n) * 100)

# =============================================================================
# Fig 4: WTP for ad-free
# =============================================================================

adfree_levels <- c("$0 - I'd rather see ads than pay", "$5/month", "$10/month",
                   "$15/month", "$20/month", "More than $20/month")

fig_wtp_adfree <- df |>
  mutate(wtp_adfree = factor(wtp_adfree, levels = adfree_levels)) |>
  count(wtp_adfree) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(x = wtp_adfree, y = pct)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 3.2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "% of respondents",
       title = "WTP for Ad-Free AI",
       subtitle = paste0("Mean ad sentiment: ",
                         round(mean(df$ad_sentiment_num, na.rm = TRUE), 1),
                         " / 7 (1 = strongly dislike)"))

save_fig("fig_wtp_adfree", fig_wtp_adfree)

# =============================================================================
# Fig 5: Ad sentiment
# =============================================================================

fig_ad_sentiment <- df |>
  count(ad_sentiment_num) |>
  filter(!is.na(ad_sentiment_num)) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(x = factor(ad_sentiment_num), y = pct)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Ad sentiment (1 = strongly dislike, 7 = strongly enjoy)",
       y = "% of respondents",
       title = "Sentiment Toward Ads in AI Responses")

save_fig("fig_ad_sentiment", fig_ad_sentiment)
