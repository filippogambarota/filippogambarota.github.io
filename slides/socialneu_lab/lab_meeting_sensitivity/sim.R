# Packages ----------------------------------------------------------------

library(tidyverse)
library(pwr)
library(furrr) # for parallel

# Setup -------------------------------------------------------------------

set.seed(2021)
nsim <- 1000 # number of simulation for each condition
furrr_options(seed = T)

# We can simulate directly on the effect size scale using cohen's d as the
# mean and 1 as variance

m1 <- 0.3 # mean of the effect - the null effect has mean = 0

sim <- expand_grid(
  sample_size = seq(10, 300, 5),
  d = seq(0, 1, 0.1),
  nsim = 1:nsim
)

# This function takes sample size and d, simulate data and extract the p-value

sample_size = 30
d <- 0.2

get_p <- function(sample_size, d){
  g1 <- rnorm(sample_size, d, 1)
  g0 <- rnorm(sample_size, 0, 1)
  t.test(g1, g0)$p.value
}

plan(multisession(workers = 4))

sim <- sim %>% 
  mutate(p_value = future_map2_dbl(sample_size, d, get_p, .options = furrr_options(seed = TRUE)))

# Saving ------------------------------------------------------------------

saveRDS(sim, "sim.rds")