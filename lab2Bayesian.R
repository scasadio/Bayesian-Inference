library(ggplot2)
library(bayesplot)
library(rstanarm)
library(rstan)

# Example 1 - Generalized linear model for a binary outcome
data(wells)
wells$dist100 <- wells$dist / 100
ggplot(wells, aes(x = dist100, y = ..density.., fill = switch == 1)) +  geom_histogram(bins = 50) +  scale_fill_manual(values = c("gray30", "skyblue"))

bin1 <- glm(switch ~ dist100, data = wells, family = binomial(link = "logit"))

t_prior <- student_t(df = 7, location = 0, scale = 2.5)
bin2 <- stan_glm(switch ~ dist100, data = wells, family = binomial(link = "logit"), 
                 prior = t_prior, prior_intercept = t_prior, seed = 12345)
#stan.glm function allows to fit generalized linear models with optional prior distribution for the coefficient

pr_switch <- function(x, ests) plogis(ests[1] + ests[2] * x)
ggplot(wells, aes(x = dist100, y = switch, color = switch)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +  geom_point(aes_string(x="dist100"), position = position_jitter(height = 0.05, width = 0.1),
                                                          size = 2, shape = 21, stroke = 0.2)+
  stat_function(fun = pr_switch, args = list(ests = coef(bin2)),
                size = 2, color = "gray35")

bin3 <- update(bin2, formula = switch ~ dist100 + arsenic)
coef_bin3 <- round(coef(bin3), 3)

pr_switch2 <- function(x, y, ests) plogis(ests[1] + ests[2] * x + ests[3] * y)
grid <- expand.grid(dist100 = seq(0, 4, length.out = 100), arsenic = seq(0, 10, length.out = 100))
grid$prob <- with(grid, pr_switch2(dist100, arsenic, coef(bin3)))
ggplot(grid, aes(x = dist100, y = arsenic)) +
  geom_tile(aes(fill = prob)) +
  geom_point(data = wells, aes(color = factor(switch)), size = 2, alpha = 0.85) +
  scale_fill_gradient() +
  scale_color_manual("switch", values = c("white", "black"), labels = c("No", "Yes"))

waic(bin3)
summary(bin3)

posterior_interval(bin3, prob=0.5)
y_tilde<-posterior_predict(bin2)
pp_check(y_tilde, plotfun="dens_overlay")

# Example 2 - Generalized linear model for count data
data(roaches)
roaches$roach1 <- roaches$roach1 / 100
count1 <- glm(y ~ roach1 + treatment + senior, offset = log(exposure2), data = roaches, family = poisson)
count2 <- stan_glm(y ~ roach1 + treatment + senior, offset = log(exposure2), data = roaches, family = poisson,
                   prior = normal(0, 2.5), prior_intercept = normal(0, 5), seed = 12345)

summary(count2)

#Example 3 - Generalized linear model: linear regression
data(kidiq)

lin1 <- stan_glm(kid_score ~ mom_hs, data = kidiq, family = gaussian(link = "identity"),seed = 12345)

base <- ggplot(kidiq, aes(x = mom_hs, y = kid_score)) +
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) +
  scale_x_continuous(breaks = c(0,1), labels = c("No HS", "HS"))

base + geom_abline(intercept = coef(lin1)[1], slope = coef(lin1)[2], color = "skyblue4", size = 1)

draws <- as.data.frame(lin1)
colnames(draws)[1:2] <- c("a", "b")

base +
  geom_abline(data = draws, aes(intercept = a, slope = b),
              color = "skyblue", size = 0.2, alpha = 0.25) +
  geom_abline(intercept = coef(lin1)[1], slope = coef(lin1)[2],
              color = "skyblue4", size = 1)