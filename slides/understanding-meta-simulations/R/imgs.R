# Packages ----------------------------------------------------------------

library(tidyverse)
library(distributional)
library(ggdist)
library(here)
library(latex2exp)

# Functions ---------------------------------------------------------------

theme_rfig <- function(size = 15){
    theme_minimal(base_size = size)
}

# Fixed-vs-Random Effect --------------------------------------------------

fixed_effect <- data.frame(
    id = 1:4,
    yi = rep(0.5, 4),
    vi = c(0.01, 0.02, 0.1, 0.05),
    model = "Fixed-Effects",
    di = 0.5
)

fixed_effect$note <- sprintf("$d_i = \\theta_f + \\epsilon_i$")

random_effect <- data.frame(
    id = 1:4,
    yi = c(-0.1, 0.4, 0.7, 0.25),
    vi = c(0.01, 0.02, 0.08, 0.1),
    model = "Random-Effects"
)

random_effect$note <- sprintf("$d_i = \\theta_r + theta_i + \\epsilon_i$")

random_effect$di <- mean(random_effect$yi)

models <- rbind(fixed_effect, random_effect)
models$dist <- map2(models$yi, sqrt(models$vi), distributional::dist_normal)
models$note <- latex2exp::TeX(models$note, output = "character")
models$delta <- models$yi - models$di

fixed_vs_random <- ggplot(models, aes(y = factor(id))) +
    geom_vline(aes(xintercept = di), linetype = "dashed", alpha = 0.5) +
    stat_dist_halfeye(aes(dist = dist), .width = 0.95) +
    facet_wrap(~model, scales = "free_x") +
    ylab("Study") +
    xlab(TeX("$d_i$")) +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank()) +
    theme_rfig() +
    geom_label(aes(x = di, y = 0.6, label = note), parse = TRUE, size = 7, label.size = NA) +
    geom_segment(aes(x = di + delta, xend = di, y = id-0.08, yend = id-0.08),
                 position = position_dodge2(width = 1, padding = 1)) +
    geom_point(aes(x = yi, y = id-0.08, alpha = model), show.legend = FALSE) +
    scale_alpha_manual(values = c(0, 1))

fixed <- ggplot(filter(models, model == "Fixed-Effects"), aes(y = factor(id))) +
    geom_vline(aes(xintercept = di), linetype = "dashed", alpha = 0.5) +
    stat_dist_halfeye(aes(dist = dist), .width = 0.95) +
    #facet_wrap(~model, scales = "free_x") +
    ylab("Study") +
    xlab(TeX("$d_i$")) +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank()) +
    theme_rfig() +
    geom_label(aes(x = di, y = 0.6, label = note), parse = TRUE, size = 7, label.size = NA) +
    geom_segment(aes(x = di + delta, xend = di, y = id-0.08, yend = id-0.08),
                 position = position_dodge2(width = 1, padding = 1)) +
    geom_point(aes(x = yi, y = id-0.08, alpha = model), show.legend = FALSE) +
    scale_alpha_manual(values = c(0, 1))

random <- ggplot(filter(models, model == "Random-Effects"), aes(y = factor(id))) +
    geom_vline(aes(xintercept = di), linetype = "dashed", alpha = 0.5) +
    stat_dist_halfeye(aes(dist = dist), .width = 0.95) +
    #facet_wrap(~model, scales = "free_x") +
    ylab("Study") +
    xlab(TeX("$d_i$")) +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank()) +
    theme_rfig() +
    geom_label(aes(x = di, y = 0.6, label = note), parse = TRUE, size = 7, label.size = NA) +
    geom_segment(aes(x = di + delta, xend = di, y = id-0.08, yend = id-0.08),
                 position = position_dodge2(width = 1, padding = 1)) +
    geom_point(aes(x = yi, y = id-0.08, alpha = model), show.legend = FALSE) +
    scale_alpha_manual(values = c(0, 1))

# Metaregression with binary predictor ------------------------------------

binary_metareg <- data.frame(
    id = 1:6,
    yi = c(0.1, -0.1, -0.1, 0.5, 0.7, 0.8),
    vi = c(0.01, 0.02, 0.01, 0.01, 0.02, 0.03),
    cond = rep(c("a", "b"), each = 3)
)

binary_metareg$dist <- map2(binary_metareg$yi, 
                            sqrt(binary_metareg$vi), 
                            dist_normal)

binary_metareg$note <- sprintf("Condition %s", toupper(binary_metareg$cond))

mm <- c(mean(binary_metareg$yi[binary_metareg$cond == "a"]),
        mean(binary_metareg$yi[binary_metareg$cond == "b"]))

binary_metareg$cond_mean <- rep(mm, each = 3)
binary_metareg$res <- binary_metareg$yi - binary_metareg$cond_mean
binary_metareg$gm <- mean(binary_metareg$yi)

xlim <- c(mean(mm) - 1, mean(mm) + 1)

plot_metareg_bin <- ggplot(binary_metareg,
                           aes(y = id)) +
    geom_segment(aes(x = gm, xend = yi, y = id-0.2, yend = id-0.2),
                 color = "darkgreen", size = 1) +
    geom_segment(aes(x = cond_mean, xend = yi, y = id-0.2, yend = id-0.2),
                 color = "firebrick", size = 1) +
    geom_point(aes(x = yi, y = id-0.2), size = 3) +
    geom_vline(xintercept = mm) +
    geom_vline(xintercept = mean(binary_metareg$cond_mean), linetype = "dashed") +
    stat_dist_halfeye(aes(dist = dist),
                      show.legend = FALSE,
                      .width = 0.95) +
    theme(axis.ticks.x = element_blank()) +
    ylab("Study") +
    geom_label(aes(x = cond_mean, y = 7, label = note), 
               size = 6, label.size = NA) +
    theme(axis.text.x = element_blank()) +
    theme_rfig() +
    xlab(latex2exp::TeX("$d_i$")) +
    ggtitle("Categorical Predictor") +
    xlim(xlim) +
    annotate("text", x = 1, y = 1.5, 
             label = TeX("\\textbf{Residual $\\tau^2$}"), 
             parse = TRUE, color = "firebrick", 
             family = "bold", size = 8) +
    annotate("text", x = 1, y = 2, 
             label = TeX("\\textbf{Explained $\\tau^2$}"), 
             parse = TRUE, color = "darkgreen", 
             family = "bold", size = 8)

# Metaregression with numerical predictor ---------------------------------

x <- seq(1, 6, 1)

cont_metareg <- data.frame(
    id = 1:6,
    #yi = 0.2 + x*0.1 + rnorm(6, 0, 0.15),
    yi = c(0.2656016, 0.5375599, 0.6202332, 0.3823055, 0.5343109, 0.9233668),
    vi = c(0.001, 0.002, 0.001, 0.001, 0.002, 0.003),
    x
)

cont_metareg$dist <- map2(cont_metareg$yi, 
                          sqrt(cont_metareg$vi), 
                          dist_normal)

# cont_metareg$res <- cont_metareg$yi - cont_metareg$pi
# cont_metareg$yi <- cont_metareg$yi + 0.1*sign(cont_metareg$res)

fit <- lm(yi ~ x, data = cont_metareg)
cont_metareg$pi <- predict(fit, newdata = data.frame(x = cont_metareg$x-0.1))
cont_metareg$gm <- mean(cont_metareg$yi)
cont_metareg$res <- cont_metareg$yi - cont_metareg$pi

plot_metareg_cont <- ggplot(cont_metareg,
                            aes(x = x, y = yi)) +
    geom_segment(aes(x = x-0.1, xend = x-0.1, y = pi, yend = yi), color = "firebrick", linewidth = 1) +
    geom_segment(aes(x = x-0.1, xend = x-0.1, y = gm, yend = pi), color = "darkgreen", linewidth = 1) +
    geom_point(aes(x = x-0.1, y = yi), size = 3, color = "black") +
    geom_hline(yintercept = mean(cont_metareg$yi), linetype = "dashed") +
    stat_halfeye(aes(dist = dist),
                 show.legend = FALSE,
                 color = "black",
                 .width = 0.95) +
    geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2]) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    theme_rfig() +
    ylab(latex2exp::TeX("$d_i$")) +
    ggtitle("Numerical Predictor") +
    annotate("text", x = 6.5, y = 0.25,
             label = TeX("\\textbf{Residual $\\tau^2$}"), 
             parse = TRUE, color = "firebrick", size = 8,
             family = "lmroman") +
    annotate("text", x = 6.5, y = 0.3, 
             label = TeX("\\textbf{Explained $\\tau^2$}"), 
             parse = TRUE, color = "darkgreen", size = 8,
             family = "lmroman")

# Saving ------------------------------------------------------------------

r_imgs <- list(fixed_vs_random = fixed_vs_random, 
               fixed = fixed,
               random = random,
               plot_metareg_bin = plot_metareg_bin,
               plot_metareg_cont = plot_metareg_cont)

saveRDS(r_imgs, here("objects", "r-imgs.rds"))
