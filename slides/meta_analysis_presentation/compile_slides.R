# Compile Slides Script ---------------------------------------------------

file_name <- list.files(pattern = ".Rmd")

# This script compiles RMarkdown slides both in HMTL and PDF format

pagedown::chrome_print(rmarkdown::render(file_name, params = list(pdf_button = FALSE))) # pdf version

rmarkdown::render(file_name, params = list(pdf_button = TRUE)) # html version