# Compile Slides Script ---------------------------------------------------

file_name <- list.files(pattern = ".Rmd")

# This script compiles RMarkdown slides both in HMTL and PDF format

rmarkdown::render(file_name) # html version

pagedown::chrome_print(rmarkdown::render(file_name, params = list(pdf_button = FALSE))) # pdf version