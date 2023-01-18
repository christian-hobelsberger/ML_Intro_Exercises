#' ### Data Import
library(ggplot2)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlbench)
library(rpart.plot)
library(mlr3filters)
#' ### Exercise 2
#' 
#' #### a)
#'  
#'  Inspect dataset
?mlr3::mlr_tasks_spam
#' mlr_tasks_spam is a binary classification task
#' Get data set
(task_spam <- mlr_tasks$get("spam"))

#' 
#' #### b)
#' 
#' Learner: Find needed learner (forest)
mlr_learners_table = as.data.table(mlr_learners)
mlr_learners_table[, c("key", "packages", "predict_types")]

#' Create learner
learner_spam = mlr3::lrn("classif.rpart")

#' Train
learner_spam$train(task_spam)

#' Visualize
set.seed(123)
rpart.plot(learner_spam$model, roundint = FALSE)


#' Create 2 subsets:
set.seed(456)
index_subset1 <- sample.int(x = seq_len(4601), size = 4601 * 0.6)
index_subset2 <- sample.int(x = seq_len(4601), size = 4601 * 0.6)

for (i in list(index_subset1, index_subset2)) {
  learner_spam$train(task_spam, row_ids = i)
  rpart.plot(learner_spam$model, roundint = FALSE)
}
#' Observation: The trees with different samples differ quite strong regarding their split-criteria and structure.

#' 
#' #### c)
#' 
#' ii)
#' Create learner
learner_forest = lrn("classif.ranger", "oob.error" = TRUE)
#' Train model
learner_forest$train(task_spam)
#' OOB error
learner$model$prediction.error

#' 
#' #### d)
learner_forest2 <- lrn("classif.ranger", importance = "permutation", "oob.error" = TRUE)
filter_spam <- flt("importance", learner = learner_forest2)
filter_spam$calculate(task_spam)
head(as.data.table(filter_spam), 5)
