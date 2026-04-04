# ==============================================================================
# 03b_openended.R — Open-ended response classification and figures
# ==============================================================================

source(file.path(here::here(), "source", "ai_usage", "00_setup.R"))
df <- read_rds(file.path(DIR_CLEAN, "ai_usage_clean.rds"))

# Helper: case-insensitive regex check across a vector of patterns
has_any <- function(text, patterns) {
  str_detect(text, regex(paste(patterns, collapse = "|"), ignore_case = TRUE))
}

# ==============================================================================
# 1. WHY MULTIHOME — classify N=133
# ==============================================================================

wm <- df |>
  filter(!is.na(why_multihome), nchar(trimws(why_multihome)) > 0) |>
  select(ResponseId, text = why_multihome)

wm <- wm |>
  mutate(
    `Cross-validate / fact-check` = has_any(text, c(
      "check", "verif", "confirm", "double.check", "cross.ref", "accurate",
      "accuracy", "same answer", "same result", "same info", "consistent",
      "make sure", "right answer", "correct", "trust", "hallucin",
      "both have the same", "compare.*answer", "fact.check"
    )),
    `Task specialization` = has_any(text, c(
      "better at", "better for", "stronger at", "good at",
      "best for", "best at", "depends on.*task", "certain task",
      "different task", "specific task", "speciali", "strength",
      "one.*for.*other.*for", "code|coding", "image|photo|picture",
      "writing|brainstorm", "research"
    )),
    `Compare / explore models` = has_any(text, c(
      "see how", "see if", "figuring out", "testing", "curious",
      "compare", "contrast", "try.*out", "experiment",
      "see.*different", "see.*result", "difference"
    )),
    `Quality dissatisfaction` = has_any(text, c(
      "not.*satisf", "don.t get.*answer", "doesn.t give",
      "struggle", "poor response", "wrong", "not right",
      "false info", "not what I", "biased", "limit",
      "shy away", "refuse", "won.t answer"
    )),
    `Different perspectives` = has_any(text, c(
      "perspective", "different.*idea", "different.*opinion",
      "second opinion", "varied", "variety", "more option",
      "additional", "supplement", "more info"
    )),
    `Convenience / platform default` = has_any(text, c(
      "convenient", "built.in", "baked in", "already.*on",
      "device", "browser", "phone", "work.*pays",
      "embedded", "integration", "microsoft suite"
    ))
  )

N_wm <- nrow(wm)

wm_summary <- wm |>
  summarise(across(-c(ResponseId, text), ~ sum(.x))) |>
  pivot_longer(everything(), names_to = "category", values_to = "n") |>
  mutate(
    pct = n / N_wm * 100,
    category = fct_reorder(category, pct)
  )

fig_wm <- ggplot(wm_summary, aes(y = category, x = pct)) +
  geom_col(fill = "#2C7BB6", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "% (", n, ")")),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(x = "% of multihome responses (non-exclusive)", y = NULL,
       title = "Why Do You Use More Than One AI?",
       subtitle = paste0("N = ", N_wm, " open-ended responses, keyword-classified"))

save_fig("fig_openended_why_multihome", fig_wm, width = 9, height = 5)

# ==============================================================================
# 2. WHY SINGLE-HOME — classify N=73
# ==============================================================================

ws <- df |>
  filter(!is.na(why_singlehome), nchar(trimws(why_singlehome)) > 0) |>
  select(ResponseId, text = why_singlehome)

ws <- ws |>
  mutate(
    `Familiarity / habit` = has_any(text, c(
      "familiar", "comfortable", "used to", "habit",
      "know.*best", "know.*work", "what I know",
      "started with", "never switch", "stuck with",
      "gotten used", "always used"
    )),
    `Good enough / meets needs` = has_any(text, c(
      "good enough", "sufficient", "does.*job",
      "does what I need", "meets my need", "works.*fine",
      "serves my need", "reliable", "no need",
      "no reason", "don.t.*need", "adequate",
      "works for me", "never.*fail", "success"
    )),
    `Low usage / don't care` = has_any(text, c(
      "don.t use.*much", "don.t use.*often",
      "barely", "rarely", "not.*often",
      "don.t.*care", "pointless", "never.*start",
      "haven.t tried", "never.*tried",
      "don.t.*particularly"
    )),
    `Sunk investment / history` = has_any(text, c(
      "history", "knows me", "knows.*about me",
      "remember", "understand.*me",
      "personali", "relationship",
      "how I talk", "my.*voice", "long time"
    )),
    `Already paying` = has_any(text, c(
      "pay", "paid", "pro version", "subscription",
      "premium", "company.*pays", "work.*pays"
    )),
    `Alternatives are similar` = has_any(text, c(
      "same", "similar", "no.*difference",
      "act the same", "not.*different",
      "all.*same", "nearly the same"
    )),
    `Convenience / default` = has_any(text, c(
      "convenient", "easy", "easier",
      "already.*on", "phone", "browser",
      "built.in", "most.*used"
    ))
  )

N_ws <- nrow(ws)

ws_summary <- ws |>
  summarise(across(-c(ResponseId, text), ~ sum(.x))) |>
  pivot_longer(everything(), names_to = "category", values_to = "n") |>
  mutate(
    pct = n / N_ws * 100,
    category = fct_reorder(category, pct)
  )

fig_ws <- ggplot(ws_summary, aes(y = category, x = pct)) +
  geom_col(fill = "#D97757", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "% (", n, ")")),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(x = "% of single-home responses (non-exclusive)", y = NULL,
       title = "Why Do You Stick With One AI?",
       subtitle = paste0("N = ", N_ws, " open-ended responses, keyword-classified"))

save_fig("fig_openended_why_singlehome", fig_ws, width = 9, height = 5)

# ==============================================================================
# 3. WHAT DOES YOUR AI REMEMBER? — classify N=181
# ==============================================================================

mr <- df |>
  filter(!is.na(memory_beliefs_openended), nchar(trimws(memory_beliefs_openended)) > 0) |>
  select(ResponseId, text = memory_beliefs_openended)

mr <- mr |>
  mutate(
    `Past conversations / chat history` = has_any(text, c(
      "past conv", "previous conv", "prior conv",
      "chat history", "older chat", "past question",
      "previous question", "prior question",
      "past.*chat", "history.*chat", "older.*question",
      "what.*asked", "things.*asked", "questions.*asked"
    )),
    `Preferences / response style` = has_any(text, c(
      "prefer", "style", "tone", "voice",
      "how I.*like", "how I.*want", "format",
      "the way I", "manner", "casual", "professional",
      "concise", "summariz"
    )),
    `Personal facts (name, age, location)` = has_any(text, c(
      "my name", "my age", "my location", "where I live",
      "my state", "my zip", "my job", "my work",
      "my career", "my occupation", "my family",
      "my kid", "my cat", "my dog", "my car",
      "my major", "my height", "my weight",
      "demographic", "gender"
    )),
    `Interests / hobbies` = has_any(text, c(
      "interest", "hobbi", "fitness", "music",
      "art", "cooking", "travel", "gaming",
      "game", "sport", "astrology", "invest",
      "free time"
    )),
    `Projects / ongoing work` = has_any(text, c(
      "project", "work.*on", "working on",
      "business", "novel", "writing",
      "job hunt", "interview", "code|coding",
      "task", "assignment"
    )),
    `Health / medical info` = has_any(text, c(
      "health", "medical", "diagnos", "anxiety",
      "mental health", "disabilit", "illness",
      "vitamin", "supplement", "diet",
      "fitness", "prescription", "autistic"
    )),
    `Everything / a lot` = has_any(text, c(
      "everything", "pretty much everything",
      "a lot", "scary amount", "too much",
      "all.*about me", "all.*tell"
    )),
    `Nothing / unsure` = has_any(text, c(
      "nothing", "not sure", "don.t think",
      "don.t know", "no idea", "not.*remember",
      "haven.t.*seen evidence", "I don.t use"
    ))
  )

N_mr <- nrow(mr)

mr_summary <- mr |>
  summarise(across(-c(ResponseId, text), ~ sum(.x))) |>
  pivot_longer(everything(), names_to = "category", values_to = "n") |>
  mutate(
    pct = n / N_mr * 100,
    category = fct_reorder(category, pct)
  )

fig_mr <- ggplot(mr_summary, aes(y = category, x = pct)) +
  geom_col(fill = "#74AA9C", width = 0.6) +
  geom_text(aes(label = paste0(round(pct), "% (", n, ")")),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(x = "% of memory belief responses (non-exclusive)", y = NULL,
       title = "What Do You Think Your AI Remembers About You?",
       subtitle = paste0("N = ", N_mr, " open-ended responses, keyword-classified"))

save_fig("fig_openended_memory_beliefs", fig_mr, width = 10, height = 5.5)

# ==============================================================================
# 4. MEMORY MENTIONS across all three questions
# ==============================================================================

memory_patterns <- c(
  "memory", "memories", "remember", "personali",
  "knows me", "knows.*about me", "history.*with",
  "familiar with me", "understands? me",
  "built.*relationship", "knows my",
  "how I talk", "my.*voice", "my style",
  "adapted", "tailored", "customiz"
)

pct_memory_multihome <- wm |>
  summarise(pct = mean(has_any(text, memory_patterns)) * 100) |>
  pull(pct)

pct_memory_singlehome <- ws |>
  summarise(pct = mean(has_any(text, memory_patterns)) * 100) |>
  pull(pct)

pct_memory_beliefs <- mr |>
  summarise(pct = mean(has_any(text, memory_patterns)) * 100) |>
  pull(pct)

fig_memory_mentions <- tibble(
  question = c(
    "Why multihome?\n(N=133)",
    "Why single-home?\n(N=73)",
    "What does AI remember?\n(N=181)"
  ),
  pct = c(pct_memory_multihome, pct_memory_singlehome, pct_memory_beliefs)
) |>
  mutate(question = factor(question, levels = question)) |>
  ggplot(aes(x = question, y = pct)) +
  geom_col(fill = c("#2C7BB6", "#D97757", "#74AA9C"), width = 0.55) +
  geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 4.5) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.15)),
                     limits = c(0, 100)) +
  labs(x = NULL, y = "% of responses mentioning memory / personalization",
       title = "How Often Does Memory Come Up Unprompted?",
       subtitle = "Share of open-ended responses referencing memory, personalization, or AI \"knowing\" the user")

save_fig("fig_memory_mentions_across_questions", fig_memory_mentions, width = 8, height = 5.5)

cat("Open-ended figures saved.\n")
