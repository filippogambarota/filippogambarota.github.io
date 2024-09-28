# checking the impact of estimating a model with or without
# covariates and how to intepret parameters from emmeans and
# summary()


library(ggplot2)
library(emmeans)
library(car)

N <- 1e3 # total sample size

dat <- data.frame(
    group = rep(c("a", "b"), each = N/2),
    age = sample(4:12, N, TRUE)
)

b0 <- 0   # intercept
b1 <- 0.5 # group effect
b2 <- 0.7 # age effect (covariate)
b3 <- 0   # interaction
B <- c(b0, b1, b2, b3)

dat$group <- factor(dat$group)
contrasts(dat$group) <- contr.sum(2)/2

X <- model.matrix(~group * age, data = dat)
dat$lp <- c(X %*% B)
dat$y <- dat$lp + rnorm(N, 0, 1)

# # plotting
# ggplot(dat, aes(x = age, y = y, color = group)) +
#     geom_point() +
#     geom_smooth(method = "lm", se = FALSE, formula = y ~ x)

# interaction model
fit_int <- lm(y ~ group * age, data = dat)

# ancova
fit_ancova <- lm(y ~ group + age, data = dat)

# group only (t-test)
fit_group <- lm(y ~ group, data = dat)

# let's compare the coefficients

car::compareCoefs(fit_int, fit_ancova, fit_group)

# we can see that the ancova model (the true model) is the most
# precise (less variance) in estimating the group effect

# the reason is that, as long as age has an effect on y, the residual
# variance is lower including age, reducing the standard error of
# the coefficients.

# the standard error of the interaction model is the lowest. The reason
# is that, when including the interaction, the model is testing the group
# difference when age is 0 (or another value if centered) but fixing a 
# single value for age.

# using emmeans, we can estimate the group effect, averaging over the
# age effect. This is very different to the group effect from the summary
# object because we are not fixing a single value

summary(fit_int)
emmeans(fit_int, pairwise ~ group, at = list(age = unique(dat$age)))

# the result from emmeans is also very close to the ancova model

summary(fit_ancova)






