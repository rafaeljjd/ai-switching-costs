Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
rmarkdown::render(
  input      = "source/ai_usage/06_report.Rmd",
  output_dir = "output/reports",
  output_file = "ai_usage_pilot_report.html"
)
