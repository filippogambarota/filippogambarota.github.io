library(tidyverse)

get_info <- function(x){
    d <- data.frame(m = x$estimate[1] - x$estimate[2],
                    se = x$stderr,
                    t = x$statistic,
                    p = x$p.value)
    rownames(d) <- NULL
    d
}


nsim <- 1e4

n0 <- 30
n1 <- 20
m0 <- 0
m1 <- 0
sratio <- 3

equal_t <- vector(mode = "list", length = nsim)
unequal_t <- vector(mode = "list", length = nsim)

for(i in 1:nsim){
  g0 <- rnorm(n0, m0, 1)  
  g1 <- rnorm(n1, m0, sratio)
  equal_t[[i]] <- t.test(g0, g1, var.equal = TRUE)
  unequal_t[[i]] <- t.test(g0, g1, var.equal = FALSE)
}

sim <- tibble(ttest = c(equal_t, unequal_t),
              type = rep(c("standard", "welch"), each = nsim))

sim$tidy <- lapply(sim$ttest, get_info)

saveRDS(sim, "objects/sim-welch.rds")

se_plot <- sim |> 
    ggplot(aes(x = se, fill = type)) +
    geom_density(color = "black",
                 alpha = 0.5) +
    ggthemes::theme_par(base_size = 15) +
    theme(legend.position = c(0.8,0.8),
          legend.title = element_blank()) +
    xlab("Standard Error")

pvalue_plot <- sim |> 
    ggplot(aes(x = p, fill = type)) +
    geom_density(color = "black",
                 alpha = 0.5) +
    ggthemes::theme_par(base_size = 15) +
    theme(legend.position = c(0.8,0.8),
          legend.title = element_blank()) +
    xlab("P-value")

equal <- ggplot() +
    xlim(c(-5, 6)) +
    stat_function(geom = "area", fun = dnorm, args = list(mean = 0, sd = 1),
                  aes(fill = "Treated"),
                  alpha = 0.5) +
    stat_function(geom = "area", fun = dnorm, args = list(mean = 1, sd = 1),
                  aes(fill = "Control"),
                  alpha = 0.5) +
    theme_minimal(base_size = 15) +
    theme(legend.title = element_blank(),
          legend.position = "none")


unequal <- ggplot() +
    xlim(c(-6, 8)) +
    stat_function(geom = "area", fun = dnorm, args = list(mean = 0, sd = 1),
                  aes(fill = "Treated"),
                  alpha = 0.5) +
    stat_function(geom = "area", fun = dnorm, args = list(mean = 1, sd = 2),
                  aes(fill = "Control"),
                  alpha = 0.5) +
    theme_minimal(base_size = 15) +
    theme(legend.title = element_blank(),
          legend.position = "none")

(equal / unequal)

library(patchwork)




