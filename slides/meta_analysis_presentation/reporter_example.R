# Metafor Reporting Example

# Packages

library(tidyverse)
library(metafor)

# Data

massage <- read.table("data_massage.txt", header = T)

# Model

fit.re <- rma(yi, vi, data = massage, method = "REML")

# Reporting

reporter(fit.re)
