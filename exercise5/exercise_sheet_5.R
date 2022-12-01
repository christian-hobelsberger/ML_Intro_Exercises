#' ### Data Import
library(ggplot2)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlbench)
#' ### Exercise 1
#' 
#' #### c)
#'  
#' Generate data
set.seed(123)
x_train <- seq(10, 15, length.out = 50)
y_train <- 10 + 3 * sin(0.15 * pi * x_train) + rnorm(length(x_train), sd = 0.5)
data_train <- data.frame(x = x_train, y = y_train)
set.seed(321)
x_test <- seq(10, 15, length.out = 10)
y_test <- 10 + 3 * sin(0.15 * pi * x_test) + rnorm(length(x_test), sd = 0.5)
data_test <- data.frame(x = x_test, y = y_test)
#' Solution with lm:
linear_model <- lm(formula = y ~ x, data = data_train)
polynomial_model <- lm(formula = y ~ poly(x, 21), data = data_train)

#' Make predictions
y_linear <- predict(linear_model, data_test)
y_polynomial <- predict(polynomial_model, data_test)

#' Compute and return MSE and MAE
lapply(X = list(y_linear, y_polynomial), FUN = function (i) {
  abs_diff = abs(data_test$y - i)
  list(MSE = mean(abs_diff^2), MAE = mean(abs_diff))
})

#' -> Evaulation on a single train-test-split is dangerous  
#' 
#' ### Exercise 2  
#' 
data(BostonHousing)
data_pollution <- data.frame(dis = BostonHousing$dis, nox = BostonHousing$nox)
data_pollution <- data_pollution[order(data_pollution$dis), ]
head(data_pollution)
ggplot2::ggplot(data_pollution, ggplot2::aes(x = dis, y = nox)) +
  ggplot2::geom_point() +
  ggplot2::theme_classic()
#' #### a)  
#' Create task
(task_pollution = TaskRegr$new(id = "pollution", backend = data_pollution, target = "nox"))
train_rows <- 1:10
test_rows <- setdiff(x = seq_len(task_pollution$nrow), y = train_rows)
#' Learner: Find needed learner
mlr_learners_table = as.data.table(mlr_learners)
mlr_learners_table[, c("key", "packages", "predict_types")]

#' Create learner
learner_pollution = mlr3::lrn("regr.lm")
learner_pollution$train(task_pollution, row_ids = train_rows)

#' Prediction
pred_lm <- learner_pollution$predict(task_pollution, row_ids = test_rows)
pred_lm$score() # MSE
#' #### b)  
#' The proportion of train and test split in a) might be disadvantageous because our data set is of size 506 and we 
#' only use 10 observations to train our complete model. Usually we aim for 2:1 train-test-split, so here we would use
#' at least 300 observations for training. When using only a few observations for training our model it becomes 
#' unstable because we randomly pick 10 points from our data set which covers only a particular area of our 
#' feature space. This also leads to poor generalization
#' 
#'  #### c)
#' Create task
(task_pollution = TaskRegr$new(id = "pollution", backend = data_pollution, target = "nox"))
#'  Create train and test indicies
repetitions <- 1:10
train_proportion_list <- as.list(seq(0.1, 0.9, 0.1))
#' create resampling objects with holdout strategy, using lapply for efficient computation
split_strategies <- lapply(train_proportion_list, function(i) mlr3::rsmp("holdout", ratio = i))

#' Train linear learners and predict in one step
set.seed(123)
learner_pollution = mlr3::lrn("regr.lm")
result_list <- list()
for (i in repetitions) {
result_list[[i]] <- lapply(split_strategies, function(i) mlr3::resample(task_pollution, learner_pollution, i))
}

# Compute MSE with nested loop
MSE_list <- lapply(repetitions, function(i) {
  sapply(result_list[[i]], function(j) j$score()$regr.mse)
})

#' Create proper data frame for viz
MSE_df <- as.data.frame(do.call(cbind, MSE_list))
MSE_df$split_ratios <- unlist(train_proportion_list)
MSE_df_long <- reshape2::melt(MSE_df, id.vars = "split_ratios")
names(MSE_df_long)[2:3] <- c("repetition", "MSE")

#' Plot MSE vs. split ratio
ggplot(MSE_df_long, aes(x = as.factor(split_ratios), y = MSE)) +
  geom_boxplot() +
  labs(x = "Proportion of test samples", y = "Avg. MSE")

#' #### d)
#' Conclusions from c):  
#' 1) A smaller training set tends to have an higher estimated generalization error.  
#' 2) A larger training set will have a higher variance in the individual gerneralization error estimates.