#' ### Data Import
library(ggplot2)
library(mlr3)
library(mlr3verse)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3viz)
library(mlbench)
library(rpart.plot)
library(mlr3filters)
library(mlr3tuning)
library(kknn)
#' ### Exercise 2
#' 
#' #### a)
#'  
#' Define task
(task <- tsk("pima"))

#' #### b)
#'  
#' Specify learners
learners_list <- list(
  po(kknn <- lrn("classif.kknn", id = "kknn")),
  po(rdm_forest <- lrn("classif.ranger", id = "ranger"))
)

#' #### c)
#'  
#' Pre-prozessing
ppl_pre_processing <- ppl("robustify", factors_to_numeric = TRUE)

#' #### d)
#'  
#' Branch between models
ppl_branch <- ppl("branch", learners_list)

#' #### e)
#'  
#' Chain pipelines to graph
graph <- ppl_pre_processing %>>% ppl_branch
plot(graph)
graph_learner <- as_learner(graph)

#' #### f)
#'  
#' Tunable hyperparameters
tail(as.data.table(graph_learner$param_set), 10)

#' Specify tuning objects
graph_learner$param_set$values$branch.selection <- to_tune(p_int(1, 2))
graph_learner$param_set$values$kknn.k <- to_tune(p_int(3, 10, depends = branch.selection == 1))
graph_learner$param_set$values$ranger.mtry <- to_tune(p_int(1, 5, depends = branch.selection == 2))

#' Rename learner
graph_learner$id <- "graph_learner"

#' #### g)
#'  
#' Evaluate tuned graph learner
rr <- tune_nested(method = "random_search", task = task, learner = graph_learner, 
            inner_resampling = rsmp("cv", folds = 3),
            outer_resampling = rsmp("cv", folds = 3), measure = msr("classif.ce"),
            term_evals = 3)

#' #### h)
#'  
#' Scores
rr$score()
rr$aggregate()
#' -> Tuned learner with MCE around 0.24