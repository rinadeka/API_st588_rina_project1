# author: Rina Deka
# date: 06/25/2023
# purpose: To render foodVignette.Rmd as a markdown (.md) file named README.md for project repo.

rmarkdown::render(
  input="foodVignette.Rmd",
  output_format = "github_document",
  output_file = "README.md",
  runtime = "static",
  clean = TRUE,
  params = NULL,
  knit_meta = NULL,
  envir = parent.frame(),
  run_pandoc = TRUE,
  quiet = FALSE,
  encoding = "UTF-8"
)

