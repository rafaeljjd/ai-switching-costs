df <- readRDS("data/clean/ai_usage_clean.rds")

sink("output/tables/openended_responses.txt")

cat("=== WHY MULTIHOME ===\n\n")
x <- df[["why_multihome"]]
x <- x[!is.na(x) & nchar(trimws(x)) > 0]
cat("N =", length(x), "\n\n")
for (i in seq_along(x)) cat(i, ": ", x[i], "\n\n", sep = "")

cat("\n\n=== WHY SINGLE-HOME ===\n\n")
x <- df[["why_singlehome"]]
x <- x[!is.na(x) & nchar(trimws(x)) > 0]
cat("N =", length(x), "\n\n")
for (i in seq_along(x)) cat(i, ": ", x[i], "\n\n", sep = "")

cat("\n\n=== WHAT DO YOU THINK YOUR AI REMEMBERS ABOUT YOU? ===\n\n")
x <- df[["memory_beliefs_openended"]]
x <- x[!is.na(x) & nchar(trimws(x)) > 0]
cat("N =", length(x), "\n\n")
for (i in seq_along(x)) cat(i, ": ", x[i], "\n\n", sep = "")

sink()
cat("Done\n")
