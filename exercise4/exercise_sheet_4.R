#' ### Data Import
library(mlbench)
library(ggplot2)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(e1071)

#' ### Exercise 3
#'  
#' Generate data
set.seed(123L)
data_raw <- mlbench::mlbench.cassini(n = 1000)
df <- as.data.frame(data_raw)
df$x.2 <- df$x.2 + rnorm(n = nrow(df), mean = 0, sd = 0.5)

#' Plot: Visualize
ggplot(data = df, mapping = aes(x = x.1, y = x.2, color = classes)) +
  geom_point()

#' Create task
(task_cassini = TaskClassif$new(id = "cassini", backend = df, target = "classes"))

#' Learner: Find needed learners
mlr_learners_table = as.data.table(mlr_learners)
mlr_learners_table[, c("key", "packages", "predict_types")]

#' Create learners
learners_cassini = list(
  mlr3::lrn("classif.lda"),
  mlr3::lrn("classif.qda"),
  mlr3::lrn("classif.naive_bayes"))

#' Train and plot learners' decision boundaries
plots_cassini = lapply(X = learners_cassini, 
                       FUN = function(i) mlr3viz::plot_learner_prediction(learner = i,
                                                                          task = task_cassini))
print(plots_cassini)
#' -> Because of its linear decision boundaries LDA is not really able to classify the data well.
#' QDA and NB look quite similar: They are able to shape the boundaries properly.