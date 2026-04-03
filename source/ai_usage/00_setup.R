# ==============================================================================
# 00_setup.R — Environment configuration
# ==============================================================================

library(tidyverse)
library(janitor)
library(scales)
library(knitr)
library(kableExtra)
library(patchwork)

# --- Paths -------------------------------------------------------------------
PROJECT_ROOT <- here::here()
DIR_RAW      <- file.path(PROJECT_ROOT, "data", "raw")
DIR_CLEAN    <- file.path(PROJECT_ROOT, "data", "clean")
DIR_FIG      <- file.path(PROJECT_ROOT, "output", "figures")
DIR_TAB      <- file.path(PROJECT_ROOT, "output", "tables")
DIR_REPORT   <- file.path(PROJECT_ROOT, "output", "reports")

for (d in c(DIR_CLEAN, DIR_FIG, DIR_TAB, DIR_REPORT)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- ggplot theme ------------------------------------------------------------
theme_pilot <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title         = element_text(face = "bold"),
      legend.position    = "bottom",
      plot.title         = element_text(face = "bold", size = rel(1.1)),
      plot.subtitle      = element_text(color = "grey40"),
      strip.text         = element_text(face = "bold")
    )
}
theme_set(theme_pilot())

# --- Platform colour palette -------------------------------------------------
platform_colors <- c(
  "ChatGPT (OpenAI)"                          = "#74AA9C",
  "Claude (Anthropic)"                         = "#D97757",
  "Google Gemini"                              = "#4285F4",
  "Microsoft Copilot"                          = "#0078D4",
  "Grok (xAI)"                                = "#1D1D1F",
  "Perplexity"                                 = "#1FB8CD",
  "DeepSeek"                                   = "#5B6AE0",
  "Meta AI (Facebook / Instagram / WhatsApp)"  = "#0668E1",
  "Character.AI"                               = "#9B59B6",
  "Other"                                      = "grey60",
  "None"                                       = "grey80"
)

# --- Helpers -----------------------------------------------------------------
save_fig <- function(name, plot = last_plot(), width = 8, height = 5) {
  ggsave(file.path(DIR_FIG, paste0(name, ".pdf")), plot,
         width = width, height = height, device = cairo_pdf)
  ggsave(file.path(DIR_FIG, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300)
  invisible(plot)
}

save_tab <- function(name, df) {
  write_csv(df, file.path(DIR_TAB, paste0(name, ".csv")))
  invisible(df)
}
