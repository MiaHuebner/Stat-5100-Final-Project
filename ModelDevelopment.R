
# 2024-2025 season data
library(corrplot)
library(tidyverse)
library(MASS)
library(car)
library(ggplot2)
library(dplyr)

rm(list = ls())
options(digits = 4, scipen = 999)
set.seed(123)

train_season <- read.csv("2024-25-player-season-totals.csv")

train_season <- train_season |>
  mutate(
    tpg = turnovers / games_played,
    mpg = minutes / games_played,
    fga_pg = field_goal_attempts / games_played,
    tpa_rate = ifelse(field_goal_attempts > 0,
                      three_point_attempts / field_goal_attempts,
                      NA_real_),
    fta_pg = free_throw_attempts / games_played
  )


# Data exploration
str(train_season)
nrow(train_season)
summary(train_season)
colSums(is.na(train_season))
train_season$position


# Subset and clean data
train_subset <- train_season |>
  dplyr::select(mpg, fga_pg, points_pg, age,
                position, assists_pg, rebounds_pg,
                tpg, tpa_rate, fta_pg) |>
  filter(!is.na(position), position != "") |>
  mutate(position = factor(position)) |>
  filter(is.finite(tpa_rate)) |>
  drop_na()

nrow(train_subset)
levels(train_subset$position)
table(train_subset$position)
summary(train_subset)


# Distribution of NBA player PPG
ggplot(train_subset, aes(x = points_pg)) +
  geom_histogram(bins = 25) +
  labs(
    title = "Distribution of NBA Player Points Per Game",
    x = "Points Per Game",
    y = "Number of Players"
  )


# Correlation plot
corr_matrix <- cor(
  train_subset |> dplyr::select(where(is.numeric)),
  use = "complete.obs"
)

corrplot(corr_matrix, method = "circle")
round(corr_matrix, 2)


# Correlations between predictors only
predictor_corr <- cor(
  train_subset |> dplyr::select(-points_pg, -position)
)

corrplot(predictor_corr,
         method = "color",
         type = "upper",
         addCoef.col = "black")

# Strongest correlations between predictors
corr_pairs <- as.data.frame(as.table(predictor_corr))
corr_pairs$Var1 <- as.character(corr_pairs$Var1)
corr_pairs$Var2 <- as.character(corr_pairs$Var2)

corr_pairs <- corr_pairs[corr_pairs$Var1 < corr_pairs$Var2, ]

corr_pairs <- corr_pairs[
  order(abs(corr_pairs$Freq), decreasing = TRUE), ]

head(corr_pairs, 10)

# Identify strongly correlated predictors
corr_pairs[abs(corr_pairs$Freq) >= 0.7, ]

# Potential groups of correlated predictors:
# Playing time: mpg, fga_pg, assists_pg, tpg
# Offensive volume: fga_pg, fta_pg, tpg
# Ball handling: assists_pg, tpg
# Player style: rebounds_pg, tpa_rate, position
# Investigate these groups using VIF


# PPG by position
boxplot(points_pg ~ position,
        data = train_subset,
        xlab = "Position",
        ylab = "Points Per Game",
        main = "PPG by Player Position")

# Mean PPG by position
aggregate(points_pg ~ position, data = train_subset, mean)

# Standard deviation of PPG by position
aggregate(points_pg ~ position, data = train_subset, sd)


# Scatterplots of PPG vs predictors
plot(train_subset$mpg, train_subset$points_pg)
plot(train_subset$fga_pg, train_subset$points_pg)
plot(train_subset$tpg, train_subset$points_pg)
plot(train_subset$assists_pg, train_subset$points_pg)
plot(train_subset$rebounds_pg, train_subset$points_pg)
plot(train_subset$age, train_subset$points_pg)
plot(train_subset$fta_pg, train_subset$points_pg)
plot(train_subset$tpa_rate, train_subset$points_pg)


# Simple linear regression
model1a <- lm(points_pg ~ fga_pg, data = train_subset)
summary(model1a)

model1b <- lm(points_pg ~ mpg, data = train_subset)
summary(model1b)

model1c <- lm(points_pg ~ tpg, data = train_subset)
summary(model1c)

model1d <- lm(points_pg ~ assists_pg, data = train_subset)
summary(model1d)


# Multiple linear regression

# Add free throws
model2 <- lm(points_pg ~ fga_pg + fta_pg,
             data = train_subset)
summary(model2)

# Add playing time
model3 <- lm(points_pg ~ fga_pg + mpg,
             data = train_subset)
summary(model3)

# Add other potential predictors
model4 <- lm(points_pg ~ fga_pg + mpg + assists_pg + tpg,
             data = train_subset)
summary(model4)

# Add free throws
model5 <- lm(points_pg ~ fga_pg + fta_pg + assists_pg + tpg,
             data = train_subset)
summary(model5)

fit <- lm(points_pg ~ fga_pg + fta_pg + tpg +
            assists_pg + mpg,
          data = train_subset)
summary(fit)

# Position model
model_position <- lm(points_pg ~ position,
                     data = train_subset)
summary(model_position)

# Add position
model_position2 <- lm(
  points_pg ~ fga_pg + fta_pg + tpg +
    assists_pg + mpg + position,
  data = train_subset
)
summary(model_position2)

# Candidate model
model6 <- lm(points_pg ~ fga_pg + fta_pg + position + mpg,
             data = train_subset)
summary(model6)

# Compare nested models
anova(model1a, model2)
anova(model1a, model3)
anova(model5, fit)
anova(fit, model_position2)
anova(model6, model_position2)


# Full regression model
full_model <- lm(
  points_pg ~ mpg + fga_pg + age + position +
    assists_pg + rebounds_pg + tpg + tpa_rate + fta_pg,
  data = train_subset
)

summary(full_model)

# Partial F-tests for removing individual predictors
drop1(full_model, test = "F")


# Check multicollinearity
vif(model4)
vif(model5)
vif(fit)
vif(model6)
vif(full_model)

# Square adjusted GVIF values to compare them
# with the conventional VIF scale
vif(full_model)[, 3]^2

# VIF below 5 is generally acceptable
# VIF above 5 indicates potential multicollinearity
# VIF above 10 indicates serious multicollinearity


# Investigate removing correlated predictors

# Remove minutes per game
reduced1 <- update(full_model, . ~ . - mpg)
summary(reduced1)
vif(reduced1)[, 3]^2
anova(reduced1, full_model)

# Remove assists from the full model
reduced2 <- update(full_model, . ~ . - assists_pg)
summary(reduced2)
vif(reduced2)[, 3]^2
anova(reduced2, full_model)

# Investigate removing both minutes and assists
reduced3 <- update(full_model, . ~ . - mpg - assists_pg)
summary(reduced3)
vif(reduced3)[, 3]^2
anova(reduced3, full_model)

# Investigate removing turnovers instead
reduced4 <- update(full_model, . ~ . - tpg)
summary(reduced4)
vif(reduced4)[, 3]^2
anova(reduced4, full_model)

# Check remaining predictors in reduced models
drop1(reduced1, test = "F")
drop1(reduced2, test = "F")
drop1(reduced3, test = "F")
drop1(reduced4, test = "F")


# Interaction models
fit2 <- lm(points_pg ~ fga_pg * fta_pg,
           data = train_subset)

fit2b <- lm(points_pg ~ fga_pg * position + fta_pg + mpg,
            data = train_subset)

summary(fit2)
summary(fit2b)

anova(model2, fit2)
anova(model6, fit2b)


# Investigate nonlinear relationships
plot(points_pg ~ fga_pg, data = train_subset,
     main = "PPG vs Field Goal Attempts",
     xlab = "FGA per Game",
     ylab = "Points per Game")

plot(points_pg ~ mpg, data = train_subset,
     main = "PPG vs Minutes Per Game",
     xlab = "Minutes Per Game",
     ylab = "Points per Game")

plot(points_pg ~ age, data = train_subset,
     main = "PPG vs Age",
     xlab = "Age",
     ylab = "Points per Game")


# Nonlinear models and comparisons

# Age
model_age_linear <- lm(points_pg ~ age,
                       data = train_subset)

model_age_quad <- lm(points_pg ~ age + I(age^2),
                     data = train_subset)

summary(model_age_linear)
summary(model_age_quad)
anova(model_age_linear, model_age_quad)

# Minutes per game
model_min_linear <- lm(points_pg ~ mpg,
                       data = train_subset)

model_min_quad <- lm(points_pg ~ mpg + I(mpg^2),
                     data = train_subset)

summary(model_min_linear)
summary(model_min_quad)
anova(model_min_linear, model_min_quad)

# Field goal attempts
model_fga_linear <- lm(points_pg ~ fga_pg,
                       data = train_subset)

model_fga_quad <- lm(points_pg ~ fga_pg + I(fga_pg^2),
                     data = train_subset)

summary(model_fga_linear)
summary(model_fga_quad)
anova(model_fga_linear, model_fga_quad)


# Final model candidates

model5a <- lm(
  points_pg ~ fga_pg + fta_pg + assists_pg + tpg + position,
  data = train_subset
)

model5b <- lm(
  points_pg ~ fga_pg + fta_pg + tpg,
  data = train_subset
)

model5c <- lm(
  points_pg ~ fga_pg + fta_pg + tpg + position,
  data = train_subset
)

model5d <- model5c

model5e <- lm(
  points_pg ~ fga_pg + fta_pg + position,
  data = train_subset
)

# Does adding position improve model5?
anova(model5, model5a)

# Does adding assists improve model5c?
anova(model5c, model5a)

# Does adding position improve model5b?
anova(model5b, model5c)

# Does adding turnovers improve model5e?
anova(model5e, model5d)


# Investigate adding three point attempt rate
model5f <- update(model5d, . ~ . + tpa_rate)

summary(model5f)
anova(model5d, model5f)

# Investigate adding a quadratic FGA term
model5g <- update(model5d, . ~ . + I(fga_pg^2))

summary(model5g)
anova(model5d, model5g)


# Check multicollinearity of final candidates
vif(model5)
vif(model5a)[, 3]^2
vif(model5b)
vif(model5c)[, 3]^2
vif(model5e)[, 3]^2
vif(model5f)[, 3]^2


# Compare candidate models
models <- list(
  Model2 = model2,
  Model3 = model3,
  Model4 = model4,
  Model5 = model5,
  Model6 = model6,
  Full = full_model,
  Reduced1 = reduced1,
  Reduced2 = reduced2,
  Reduced3 = reduced3,
  Reduced4 = reduced4,
  Model5a = model5a,
  Model5b = model5b,
  Model5d = model5d,
  Model5e = model5e,
  Model5f = model5f,
  Model5g = model5g,
  Interaction1 = fit2,
  Interaction2 = fit2b
)

results <- data.frame(
  Model = names(models),
  R2 = sapply(models, function(m) summary(m)$r.squared),
  Adjusted_R2 = sapply(models, function(m) summary(m)$adj.r.squared),
  RSE = sapply(models, function(m) summary(m)$sigma),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC)
)

# Compare adjusted R-squared
results[order(results$Adjusted_R2, decreasing = TRUE), ]

# Compare AIC
results[order(results$AIC), ]

# Compare BIC
results[order(results$BIC), ]


# Provisional final model
# Change this after reviewing model comparisons and VIF
final_model <- model5d

summary(final_model)
vif(final_model)
drop1(final_model, test = "F")
confint(final_model)


# Check regression assumptions

# Residuals vs fitted: linearity
# Normal Q-Q: normality
# Scale-location: constant variance
# Residuals vs leverage: influential observations

par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))

# Residual distribution
resids <- residuals(final_model)

hist(resids,
     breaks = 25,
     main = "Distribution of Residuals",
     xlab = "Residuals")

# Normality test
shapiro.test(resids)

# Constant variance test
ncvTest(final_model)


# Independence of observations
# Each observation should represent a different player.
# Players on the same team may have correlated errors
# because they share coaches and offensive systems.
# Independence should be considered based on how
# the data was collected.

names(train_season)


# Check leverage
lev <- hatvalues(final_model)
n <- nobs(final_model)
p <- final_model$rank

levpoints <- which(lev > 2 * p / n)
length(levpoints)
train_subset[levpoints, ]


# Check influential observations
cooks <- cooks.distance(final_model)

sum(cooks > 4 / n)

plot(cooks,
     type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance")

abline(h = 4 / n, col = "red")

most_influential <- which.max(cooks)

most_influential
train_subset[most_influential, ]


# Refit model without most influential observation
model_without_influential <- lm(
  formula(final_model),
  data = train_subset[-most_influential, ]
)

# Compare coefficients
coef(final_model)
coef(model_without_influential)

summary(final_model)
summary(model_without_influential)


# Predicted vs actual points per game
plot(train_subset$points_pg,
     fitted(final_model),
     main = "Predicted vs Actual Points Per Game",
     xlab = "Actual Points Per Game",
     ylab = "Predicted Points Per Game")

abline(0, 1, col = "red")


# Final model summary
summary(final_model)
coef(final_model)
formula(final_model)

# Additional reduced model selection

# Remove assists
candidate1 <- update(full_model, . ~ . - assists_pg)

# Remove three point attempt rate
candidate2 <- update(candidate1, . ~ . - tpa_rate)

# Investigate removing rebounds
candidate3 <- update(candidate2, . ~ . - rebounds_pg)

# Investigate removing minutes
candidate4 <- update(candidate3, . ~ . - mpg)

# Investigate removing turnovers
candidate5 <- update(candidate4, . ~ . - tpg)

# Compare nested models
anova(candidate1, full_model)
anova(candidate2, candidate1)
anova(candidate3, candidate2)
anova(candidate4, candidate3)
anova(candidate5, candidate4)

# Check VIF
vif(candidate1)[, 3]^2
vif(candidate2)[, 3]^2
vif(candidate3)[, 3]^2
vif(candidate4)[, 3]^2
vif(candidate5)[, 3]^2

# Check significance of remaining predictors
drop1(candidate2, test = "F")
drop1(candidate3, test = "F")
drop1(candidate4, test = "F")
drop1(candidate5, test = "F")

# Compare model performance
AIC(candidate1, candidate2, candidate3,
    candidate4, candidate5, model5d, model5e)

BIC(candidate1, candidate2, candidate3,
    candidate4, candidate5, model5d, model5e)

summary(candidate4)
summary(candidate5)
