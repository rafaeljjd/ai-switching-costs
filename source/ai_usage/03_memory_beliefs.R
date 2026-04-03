# ==============================================================================
# 03_memory_beliefs.R — Memory awareness and switching cost perceptions
# ==============================================================================

source(file.path(here::here(), "source", "ai_usage", "00_setup.R"))
df <- read_rds(file.path(DIR_CLEAN, "ai_usage_clean.rds"))

N <- nrow(df)

# =============================================================================
# Table 3: Memory awareness crosstab
# =============================================================================

tab_memory_awareness <- df |>
  count(ai_remembers, reviewed_memories) |>
  mutate(pct = n / sum(n) * 100)

save_tab("tab_memory_awareness", tab_memory_awareness)

# Inline stats
stats_memory <- list(
  pct_remembers       = mean(df$ai_remembers == "Yes", na.rm = TRUE) * 100,
  pct_reviewed        = mean(df$reviewed_memories == "Yes", na.rm = TRUE) * 100,
  pct_didnt_know      = mean(df$reviewed_memories == "I didn't know I could", na.rm = TRUE) * 100,
  improvement_mean    = mean(df$perceived_improvement_num, na.rm = TRUE),
  improvement_median  = median(df$perceived_improvement_num, na.rm = TRUE),
  improvement_sd      = sd(df$perceived_improvement_num, na.rm = TRUE)
)

# =============================================================================
# Fig 6: Memory beliefs overview (3 panels)
# =============================================================================

# Panel A: Does AI remember?
p6a <- df |>
  count(ai_remembers) |>
  mutate(pct = n / sum(n) * 100,
         ai_remembers = factor(ai_remembers, levels = c("Yes", "No", "I'm not sure"))) |>
  ggplot(aes(x = ai_remembers, y = pct)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "% of respondents", title = "A. Does your AI remember you?")

# Panel B: Reviewed memories?
p6b <- df |>
  mutate(reviewed_memories = factor(reviewed_memories,
                                    levels = c("Yes", "No", "I didn't know I could"))) |>
  count(reviewed_memories) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(x = reviewed_memories, y = pct)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 3.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "% of respondents", title = "B. Reviewed or edited memories?")

# Panel C: Perceived improvement (1-7)
p6c <- df |>
  filter(!is.na(perceived_improvement_num)) |>
  count(perceived_improvement_num) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(x = factor(perceived_improvement_num), y = pct)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_vline(xintercept = stats_memory$improvement_mean, linetype = "dashed",
             color = "red", linewidth = 0.7) +
  annotate("text", x = stats_memory$improvement_mean + 0.3,
           y = Inf, vjust = 1.5, label = paste0("Mean = ", round(stats_memory$improvement_mean, 1)),
           color = "red", size = 3.5) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = "1 = Not at all ... 7 = Dramatically improved",
       y = "% of respondents",
       title = "C. Perceived improvement from memory")

fig_memory_beliefs <- p6a + p6b + p6c + plot_layout(ncol = 3)
save_fig("fig_memory_beliefs", fig_memory_beliefs, width = 14, height = 5)

# =============================================================================
# Fig 7: Switching losses — what would you lose?
# =============================================================================

loss_items <- c(
  "It wouldn't know my preferences or style",
  "I'd lose my conversation history",
  "I'd have to re-explain my ongoing projects and context",
  "The alternative is just lower quality, regardless of personalization",
  "I'd have to learn a new interface",
  "I'd lose integration with other tools I use",
  "Nothing significant"
)

loss_counts <- tibble(item = loss_items) |>
  mutate(
    n   = map_dbl(item, ~ sum(str_detect(df$switching_losses, fixed(.x)), na.rm = TRUE)),
    pct = n / N * 100,
    item = fct_reorder(item, pct),
    highlight = item == "Nothing significant"
  )

fig_switching_losses <- ggplot(loss_counts, aes(y = item, x = pct, fill = highlight)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")), hjust = -0.15, size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "grey60", "FALSE" = "#2C7BB6"), guide = "none") +
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 35)) +
  labs(x = "% of respondents (multi-select)", y = NULL,
       title = "Perceived Switching Losses",
       subtitle = "\"What would you lose if you switched to a different AI assistant?\"")

save_fig("fig_switching_losses", fig_switching_losses, width = 9, height = 5)

# =============================================================================
# Fig 8: Conversations to personalize
# =============================================================================

conv_levels <- c("1-5", "6-20", "21-50", "50+", "It would never fully catch up")

fig_conversations <- df |>
  mutate(conv = factor(conversations_to_personalize, levels = conv_levels)) |>
  count(conv) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(x = conv, y = pct)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 3.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "% of respondents",
       title = "Conversations Needed to Re-Personalize",
       subtitle = "\"How many conversations before a new AI felt as personalized?\"")

save_fig("fig_conversations_to_personalize", fig_conversations)

stats_memory$pct_never_catch_up <- mean(
  df$conversations_to_personalize == "It would never fully catch up", na.rm = TRUE
) * 100

stats_memory$pct_nothing_sig <- mean(
  str_detect(df$switching_losses, fixed("Nothing significant")), na.rm = TRUE
) * 100
