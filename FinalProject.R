#2024-2025 season data
library(corrplot)
library(tidyverse)
library(MASS)
library(car)
library(ggplot2)
library(dplyr)

rm(list = ls()) # keep the environment clean and predictable between runs
options(digits = 4, scipen = 999) # make printed numbers and plots a bit nicer
set.seed(123) # set the seed for reproducibility

train_season <- read.csv("2024-25-player-season-totals.csv")
train_season <- train_season |>
  mutate(tpg = turnovers/games_played, mpg = minutes/games_played, 
         fga_pg = field_goal_attempts/games_played, 
         tpa_rate = three_point_attempts/field_goal_attempts,
         fta_pg = free_throw_attempts/games_played,
         )

#data exploration
str(train_season)
nrow(train_season) # 569
train_season$position

#Subset data 
train_subset <- train_season |> 
  dplyr::select(minutes, fga_pg, points_pg, age, 
                position, assists_pg, rebounds_pg, tpg, tpa_rate, fta_pg)

# distribution of nba player ppg
ggplot(train_subset, aes(x = points_pg)) +
  geom_histogram(bins = 25) +
  labs(
    title = "Distribution of NBA Player Points Per Game",
    x = "Points Per Game",
    y = "Number of Players"
  )

#Correlation Plot
corr_matrix <- cor(train_subset %>% select_if(is.numeric), use = "complete.obs") 
corrplot(corr_matrix, method = "circle")
corr_matrix
# strongest correlations
corr_pairs <- as.data.frame(as.table(corr_matrix))
corr_pairs$Var1 <- as.character(corr_pairs$Var1)
corr_pairs$Var2 <- as.character(corr_pairs$Var2)
corr_pairs <- corr_pairs[corr_pairs$Var1 < corr_pairs$Var2, ]
corr_pairs <- corr_pairs[order(abs(corr_pairs$Freq), decreasing = TRUE), ]
head(corr_pairs, 10)
# notes: negative correlation between rebounds and % of fg 3pters, strong correlation between
# fga and tpg, assist and tpg which could lead to corrs of ppg and tpg, ppg and assists

# assign numerical values and split dataset by position
train_subset <- train_subset[train_subset$position != "", ]
train_subset$position <- droplevels(train_subset$position)
train_subset$position <- factor(train_subset$position)
levels(train_subset$position)
# ppg by position
boxplot(points_pg ~ position, data = train_subset,
        xlab = "Position",
        ylab = "Points Per Game",
        main = "PPG by Player Position")
# scatter plots of ppg vs predictors of interest
plot(train_subset$minutes, train_subset$points_pg)
plot(train_subset$fga_pg, train_subset$points_pg) # strongest appeared relationship by far
plot(train_subset$tpg, train_subset$points_pg) # plotted bc of correlation between tpg and fga_pg but not super strong appeared relationship
plot(train_subset$assists_pg, train_subset$points_pg)
plot(train_subset$rebounds_pg, train_subset$points_pg)
plot(train_subset$age, train_subset$points_pg) 

# mean ppg by position
aggregate(points_pg ~ position, data = train_subset, mean)
# sd by position
aggregate(points_pg ~ position, data = train_subset, sd)

# simple linear regression - strongest correlations (ranked by Rs)
model1a <- lm(points_pg ~ fga_pg, data = train_subset) # 1
summary(model1a)
model1b <- lm(points_pg ~ minutes, data = train_subset) # 4
summary(model1b)
model1c <- lm(points_pg ~ tpg, data = train_subset) # 2
summary(model1c)
model1d <- lm(points_pg ~ assists_pg, data = train_subset) # 3
summary(model1d)

# add ft
model2 <- lm(points_pg ~ fga_pg + fta_pg, data = train_subset) # higher R than og
summary(model2)
# add playing time
model3 <- lm(points_pg ~ fga_pg + minutes, data = train_subset) # not much better than og
summary(model3)
# add other potentials
model4 <- lm(points_pg ~ fga_pg + minutes + assists_pg + tpg,
             data = train_subset)
summary(model4)
# add free throws
model5 <- lm(points_pg ~ fga_pg + fta_pg + assists_pg + tpg,
             data = train_subset)
summary(model5)
fit <- lm(points_pg ~ fga_pg + fta_pg + tpg + assists_pg + minutes, data = train_subset)
summary(fit) # highest R2 so far

model_position <- lm(
  points_pg ~ position,
  data = train_subset 
)
summary(model_position) # really bad ?
model_position2 <- lm(points_pg ~ fga_pg + fta_pg + tpg + assists_pg + minutes + position, data = train_subset)
summary(model_position2)

final<- lm(
  points_pg ~ fga_pg + fta_pg + position + minutes,
  data = train_subset 
)
summary(final)# highest R2 found
anova(final, fit)
anova(final, model_position2)

# check multicollinearity
vif(model4)
vif(model5)
vif(fit)
vif(final)

# residuals
resids <- residuals(fit)
plot(fit)
lev <- hatvalues(fit)
n <- nobs(fit)
p <- length(coef(fit))
levpoints<-which(lev > 2 * p / n)
cook <- cooks.distance(fit)
sum(cook > 0.5) # 0
# residuals
resids <- residuals(final)
plot(final)
lev <- hatvalues(final)
n <- nobs(final)
p <- length(coef(final))
levpoints<-which(lev > 2 * p / n)
cook <- cooks.distance(final)
sum(cook > 0.5) # 0

# interaction models
fit2 <- lm(points_pg ~ fga_pg*fta_pg, data = train_subset) #2
fit2b <-  lm(points_pg ~ fga_pg*position + fta_pg + minutes, data = train_subset) #1
summary(fit2b) 
summary(fit2) # both didn't add more than og

# investigate for nonlinear relationships
plot(points_pg ~ fga_pg, data = train_subset,
     main = "PPG vs Field Goal Attempts",
     xlab = "FGA per Game",
     ylab = "Points per Game")
plot(points_pg ~ minutes, data = train_subset,
     main = "PPG vs Minutes",
     xlab = "Minutes",
     ylab = "Points per Game")
plot(points_pg ~ age, data = train_subset,
     main = "PPG vs Age",
     xlab = "Age",
     ylab = "Points per Game")

# nonlinear models & their comparisons
model_age_linear <- lm(
  points_pg ~ age,
  data = train_subset
)
model_age_quad <- lm(
  points_pg ~ age + I(age^2),
  data = train_subset
)
summary(model_age_linear)
summary(model_age_quad)
anova(model_age_linear, model_age_quad)

model_min_linear <- lm(
  points_pg ~ minutes,
  data = train_subset
)
model_min_quad <- lm(
  points_pg ~ minutes + I(minutes^2),
  data = train_subset
)
summary(model_min_linear)
summary(model_min_quad)
anova(model_min_linear, model_min_quad)

model_fga_linear <- lm(
  points_pg ~ fga_pg,
  data = train_subset
)
model_fga_quad <- lm(
  points_pg ~ fga_pg + I(fga_pg^2),
  data = train_subset
)
summary(model_fga_quad)
anova(model_fga_linear, model_fga_quad)

# try to predict new set
test_season <- read.csv("2025-26-player-season-totals.csv")
test_season <- test_season |>
  mutate(tpg = turnovers/games_played, mpg = minutes/games_played, 
         fga_pg = field_goal_attempts/games_played, 
         tpa_rate = three_point_attempts/field_goal_attempts,
         fta_pg = free_throw_attempts/games_played,
  )
test_subset <- test_season |> 
  dplyr::select(minutes, fga_pg, points_pg, age, 
                position, assists_pg, rebounds_pg, tpg, tpa_rate, fta_pg, position)
test_subset <- test_subset[test_subset$position != "", ]
test_subset$position <- droplevels(test_subset$position)
test_subset$position <- factor(test_subset$position)
levels(test_subset$position)
predictions <- predict(model5, newdata = test_subset)

# compare predicted and actual ppg
models <- list(
  Model1 = model1a,
  Model2 = model2,
  Model3 = model3,
  Model4 = model4,
  Model5 = model5,
  Model6 = final,
  Model7 = model_position
)
required_vars <- c(
  "points_pg", "fga_pg", "fta_pg",
  "minutes", "assists_pg", "tpg", "position"
)
test_eval <- test_subset[
  complete.cases(test_subset[, required_vars]),
]
comparison <- data.frame(
  Actual = test_eval$points_pg
)
for (name in names(models)) {
  print(name)
  comparison[[name]] <- predict(
    models[[name]],
    newdata = test_eval
  )
}
head(comparison)
actual <- test_eval$points_pg
evaluate_model <- function(actual, predicted) {
  c(
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted)),
    R2 = 1 - sum((actual - predicted)^2) /
      sum((actual - mean(actual))^2)
  )
}

results <- data.frame(
  Model = names(models),
  t(sapply(names(models), function(name) {
    evaluate_model(actual, comparison[[name]])
  }))
)

results[order(results$RMSE), ]

# final model determination after evaluating predictions
model5 <- lm(points_pg ~ fga_pg + fta_pg + assists_pg + tpg,
             data = train_subset)
model5a <- lm(points_pg ~ fga_pg + fta_pg + assists_pg + tpg + position,
             data = train_subset)
model5b <- lm(points_pg ~ fga_pg + fta_pg + tpg,
             data = train_subset)
model5c <- lm(points_pg ~ fga_pg + fta_pg + tpg + position,
             data = train_subset)
# adding position improves model 5
anova(model5, model5a)
# adding assists doesn't improve model 5c
anova(model5c, model5a)
# adding position improves the model without assists
anova(model5b, model5c)
model5d <- lm(points_pg ~ fga_pg + fta_pg + tpg + position,
                         data = train_subset)
model5e <- lm(points_pg ~ fga_pg + fta_pg + position,
              data = train_subset)
# removing turnovers affects the model
anova(model5d, model5e)


# FINAL MODEL EVALUATIONS
# do other tests on this model before solidifying
vif(model5d)
predictions5d <- predict(model5d, newdata = test_eval)
evaluate_model(
  test_eval$points_pg,
  predictions5d
)
# check resids
par(mfrow = c(2, 2))
plot(model5d)
par(mfrow = c(1, 1))
# check influential obsv
cooks <- cooks.distance(model5d)
sum(cooks > 4 / nrow(train_subset))
train_subset[551, ]
most_influential <- which.max(cooks)
most_influential
train_subset[most_influential, ]
model5d_no551 <- lm(
  points_pg ~ fga_pg + fta_pg + tpg + position,
  data = train_subset[-most_influential, ]
)
coef(model5d)
coef(model5d_no551)

final_model <- model5d
summary(final_model)
