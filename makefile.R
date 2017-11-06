unlink("data", recursive = TRUE) # where data is stored after clean
unlink("olivia_results",recursive=TRUE)
unlink("graph", recursive = TRUE) # where graphs are stores

dir.create(file.path("data"), showWarnings = FALSE)
dir.create(file.path("olivia_results"), showWarnings = FALSE)
dir.create(file.path("graph"), showWarnings = FALSE)

## run all scripts
source("get_data.R")
source("clean_data.R")
source("olivia_analysis.R")
source("olivia_vis.R")
rmarkdown::render("olivia_paper.Rmd", output_format = "html_document")
