#' ### Imports
library("mlr3")
library("ggplot2")
library("mlr3learners")
library("mlr3viz")

#' ### Task 1 
#' 
#' #### b)
#' Have a look at mlr3::tsk("iris"). What attributes does this task object store?

(task = mlr3::tsk("iris"))
#' The task object stores: 
#' * 150x5 data set of iris flowers
#' * Target variable: species
#' * Propertiers: multiclass
#' * 4 Features (dbl):
#'     - Petal.Length
#'     - Petal.Width
#'     - Sepal.Length
#'     - Sepal.Width

#' #### c)
#' Pick an mlr3 learner of your choice. What are the different settings for this learner?

learner = lrn("classif.rpart")

# different settings:
learner$param_set



#' ### Task 3
#' 
#' Simulate data
set.seed(7L)
x <- seq(from = -2.5, to = 2.5, by =0.25)
y <- -3 + 5 * sin(0.4 * pi * x) + rnorm(n = length(x), mean = 0, sd = 1)
data <- data.frame(x, y)

#' Generate feature matrix for x^0, x^1, x^2, x^4

X <- as.matrix(sapply(seq(0, 3), function(i) x^i))
head(X)

#' Create 3 different feature-combinations
feature_vec <- matrix(
  rbind(
    c(1, 1, 1, 1),
    c(0, -1, 0.2, 0.25),
    c(1, 0.025, 2, 0)
  ),
  nrow = 3
)
feature_vec

#' Compute model outputs for each feature-combination
f_est <- sapply(1:3, function(i) X %*% feature_vec[i, ])
data_est_models <- data.frame(x, f_est)
names(data_est_models) <- c("x", "f1", "f2", "f3")
head(data_est_models)

ggplot(data_est_models) +
  geom_line(mapping = aes(x = x, y = f1, col = "blue")) +
  geom_line(mapping = aes(x = x, y = f2, col = "purple")) +
  geom_line(mapping = aes(x = x, y = f3, col = "lightgreen")) +
  geom_point(data = data, mapping = aes(x = x, y = y))

#' ### Task 4

#' We want to predict the age of an abalone using its longest shell measurement and its weight
#' See https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/ for more details.
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
abalone <- read.table(url, sep = ",", row.names = NULL)
colnames(abalone) <- c(
  "sex", "longest_shell", "diameter", "height", "whole_weight",
  "shucked_weight", "visceral_weight", "shell_weight", "rings")
abalone <- abalone[, c("longest_shell", "whole_weight", "rings")]

#' #### a)
#' Plot LongestShell and WholeWeight on the x- and y-axis, respectively, and color points according to Rings.
ggplot(data = abalone, mapping = aes(x = longest_shell,
                                     y = whole_weight,
                                     color = rings)) +
  geom_point()

#' #### b)
#' Create an mlr3 task for the abalone data.

task_abalone = TaskRegr$new("abalone", abalone, "rings")
task_abalone

#' #### c)
#' Define a linear regression learner (for this you will need to load the mlr3learners extension package frst)
#' and use it to train a linear model on the abalone data.
mlr_learners_table = as.data.table(mlr_learners)
mlr_learners_table[16:nrow(mlr_learners_table), c("key", "packages", "predict_types")]

#' Learner
learner_ablaone = lrn("regr.lm")
learner_ablaone$train(task_abalone)

#' Prediction
pred_lm <- learner_ablaone$predict(task_abalone)
head(data.frame(
  id = 1:length(pred_lm$truth), 
  truth = pred_lm$truth,
  response = pred_lm$response))

#' #### d)
#' Compare the fitted and observed targets visually.


mlr3viz::autoplot(pred_lm)
#' Scatterplot of true vs. response (predicted) values. 
#' The relationship doesn't look strictly linear.


#' Coefficients
print(learner_ablaone$model$coefficients)

#' #### e)
#' Assess the model's training loss in terms of MAE.

#' MAE metric
mae <- mlr3::msr("regr.mae")

#' performance scores
pred_lm$score() # MSE
pred_lm$score(mae)
