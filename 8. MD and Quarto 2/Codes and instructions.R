# Quarto #2
# YAML, rendering and parameters
# Reproducible Research 2024
# Wojciech Hardy

library(quarto)

setwd("")

# Converting from Rmd to Qmd

## Step 1) 
knitr::convert_chunk_header(input = "RMD_class_1_cut.Rmd", 
                            output = "QMD_class_1_cut.qmd")

## Step 2)
readLines("QMD_class_1_cut.qmd")[1:5]

readLines("QMD_class_1_cut.qmd") %>%
  stringr::str_replace(
    pattern = "output: html_document", 
    replace = "format: html") %>%
  writeLines(con = "QMD_class_1_cut.qmd")

readLines("QMD_class_1_cut.qmd")[1:5]

# Launching a preview mode
sys::exec_wait("quarto preview QMD_class_2.qmd")

# To create a PDF file you need a TeX installation:
sys::exec_wait("quarto install tinytex")

# Rendering
library(quarto)

quarto_render("QMD_class_1_cut.qmd", output_format = "docx")
