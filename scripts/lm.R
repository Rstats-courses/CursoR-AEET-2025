
library("knitr")



trees <- read.csv("data/trees.csv")
head(trees)

include_graphics("images/anscombe.png")

plot(trees$height)

include_graphics("images/reg_outliers.png")

hist(trees$height)

hist(trees$dbh)

plot(height ~ dbh, data = trees, las = 1)

library(ggplot2)
theme_set(theme_minimal(base_size = 18))
ggplot(trees) +
  geom_point(aes(dbh, height))

m1 <- lm(height ~ dbh, data = trees)

library("equatiomatic")
m1 <- lm(height ~ dbh, data = trees)
equatiomatic::extract_eq(m1)

equatiomatic::extract_eq(m1, use_coefs = TRUE)

summary(m1)

include_graphics("images/gaussian.png")

library(arm)
library(ggplot2)

coefs <- as.data.frame(coef(sim(m1)))
names(coefs) <- c("intercept", "slope")

ggplot(coefs) +
  geom_density(aes(intercept), fill = "grey80") +
  xlim(-1, 21) +
  geom_vline(xintercept = 0)

ggplot(coefs) +
  geom_density(aes(slope), fill = "grey80") +
  xlim(-0.1, 0.7) +
  geom_vline(xintercept = 0)

res <- data.frame(residual = residuals(m1))
ggplot(res) +
  geom_density(aes(residual), fill = "grey80") +
  geom_vline(xintercept = 0) +
  annotate("text", x = 3, y = 0.015, label = "SD = 4", size = 5)

coef(m1)

confint(m1)

library("broom")
tidy(m1)

glance(m1)

library("parameters")
parameters(m1)

library("effects")
summary(allEffects(m1))

include_graphics("images/nature_significance.PNG")

library("report")
report(m1)

library("xtable")
xtable(m1, digits = 2)

library("texreg")
texreg(m1, single.row = TRUE)

library("modelsummary")
modelsummary(m1, output = "markdown")

library("gtsummary")
tbl_regression(m1, intercept = TRUE)

library("effects")
plot(allEffects(m1))

library("visreg")
visreg(m1)

visreg(m1, gg = TRUE) + theme_bw()

library("sjPlot")
plot_model(m1, type = "eff")

library("see")
plot(parameters(m1), show_intercept = TRUE) +
  labs(title = "Height ~ Diameter")   # ggplot2

plot(simulate_parameters(m1)) +
  labs(title = "Density of the slope parameter")

include_graphics("images/lm_resid_assump.png")

hist(residuals(m1))

def.par <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow=2))
plot(m1)
par(def.par)

library("performance")
check_model(m1)

## library("easystats")
## model_dashboard(m1)

trees$height.pred <- fitted(m1)
trees$resid <- residuals(m1)
head(trees)

plot(height ~ height.pred, data = trees,
     xlab = "Tree height (predicted)", ylab = "Tree height (observed)",
     las = 1, xlim = c(10,60), ylim = c(10,60))
abline(a = 0, b = 1)

new.dbh <- data.frame(dbh = c(39))
predict(m1, new.dbh, se.fit = TRUE)

predict(m1, new.dbh, interval = "confidence")

predict(m1, new.dbh, interval = "prediction")

library("modelbased")
conf <- estimate_expectation(m1, data = NULL)
conf$obs <- trees$height

ggplot(conf) +
  aes(x = dbh, y = obs) +
  geom_point(size = 0.5, alpha = 0.3) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), fill = "blue", alpha = 0.2) +
  geom_line(aes(x = dbh, y = Predicted), colour = "blue", size = 0.5) +
  labs(x = "diameter", y = "height", title = "Confidence interval") +
  theme_minimal(base_size = 18)


pred <- estimate_prediction(m1)
pred$obs <- trees$height

ggplot(pred) +
  aes(x = dbh, y = obs) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), fill = "blue", alpha = 0.2) +
  geom_line(aes(x = dbh, y = Predicted), colour = "blue", size = 2) +
  labs(x = "diameter", y = "height", title = "Prediction interval") +
  theme_minimal(base_size = 18)

plot(height ~ as.factor(sex), data = trees)

m2 <- lm(height ~ sex, data = trees)
summary(m2)

## m2 <- lm(height ~ sex, data = trees)

m2 <- lm(height ~ sex, data = trees)
summary(m2)

report(m2)

coefs <- as.data.frame(coef(sim(m2)))
names(coefs) <- c("intercept", "slope")

ggplot(coefs) +
  geom_density(aes(intercept), fill = "grey80") +
  xlim(-1, 40) +
  geom_vline(xintercept = 0)

ggplot(coefs) +
  geom_density(aes(slope), fill = "grey80") +
  xlim(-3, 2) +
  geom_vline(xintercept = 0)

library("modelbased")
estimate_means(m2)

estimate_contrasts(m2)

plot(allEffects(m2))

visreg(m2)

plot_model(m2, type = "eff")

hist(resid(m2))

def.par <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow=2))
plot(m2)
par(def.par)

library("performance")
check_model(m2)

## model_dashboard(m2)

plot(height ~ site, data = trees)

m3 <- lm(height ~ site, data = trees)

m3 <- lm(height ~ site, data = trees)
summary(m3)

extract_eq(m3)

trees$site <- as.factor(trees$site)

m3 <- lm(height ~ site, data = trees)
extract_eq(m3)

m3 <- lm(height ~ site, data = trees)
summary(m3)

plot(simulate_parameters(m3), stack = FALSE)

library("modelbased")
estimate_means(m3)

estimate_contrasts(m3)

kable(xtable::xtable(m3), digits = 2)

summary(allEffects(m3))

plot(allEffects(m3))

visreg(m3)

plot_model(m3, type = "eff")

def.par <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow = 2))
plot(m3)
par(def.par)

check_model(m3)

## lm(height ~ site + dbh, data = trees)

m4 <- lm(height ~ site + dbh, data = trees)
summary(m4)

parameters(m4)

summary(allEffects(m4))

plot(allEffects(m4))

par(mfcol = c(1, 2))
visreg(m4)
dev.off()

plot_model(m4, type = "eff")


plot_model(m4, type = "est")

plot(parameters(m4))

## plot(height ~ dbh, data = trees, las = 1)
## abline(a = coef(m4)[1], b = coef(m4)[11])
## for (i in 2:10) {
##   abline(a = coef(m4)[1] + coef(m4)[i], b = coef(m4)[11])
## }

## ggplot with different colour for each site
ggplot(trees) +
  aes(x = dbh, y = height, colour = site) +
  geom_point() +
  geom_abline(intercept = coef(m4)[1], slope = coef(m4)[11]) +
  geom_abline(intercept = coef(m4)[1] + coef(m4)[2], slope = coef(m4)[11]) +
  geom_abline(intercept = coef(m4)[1] + coef(m4)[3], slope = coef(m4)[11]) +
  geom_abline(intercept = coef(m4)[1] + coef(m4)[8], slope = coef(m4)[11])

estimate_slopes(m4)

def.par <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow=2))
plot(m4)
par(def.par)

check_model(m4)

trees$height.pred <- fitted(m4)
plot(trees$height.pred, trees$height, xlab = "Tree height (predicted)", ylab = "Tree height (observed)", las = 1, xlim = c(10,60), ylim = c(10,60))
abline(a = 0, b = 1)

performance::check_predictions(m4)

## library(bayesplot)
## sims <- simulate(m4, nsim = 100)
## ppc_dens_overlay(trees$height, yrep = t(as.matrix(sims)))

trees.10cm <- data.frame(site = as.factor(1:10),
                        dbh = 10)
trees.10cm

predict(m4, newdata = trees.10cm, interval = "confidence")

predict(m4, newdata = trees.10cm, interval = "prediction")

predict(m4, newdata = trees.10cm, interval = "prediction",
        level = 0.99)

m5 <- lm(height ~ site*dbh, data = trees)
summary(m5)

visreg(m5, xvar = "dbh", by = "site")

## library("modelStudio")
## m5.explain <- DALEX::explain(
##   m5,
##   data = trees,
##   y = trees$height)
## modelStudio(m5.explain)
