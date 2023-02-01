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
#' #### b)
#'  
#' Define task
(task <- tsk("pima"))

#' Specify learners
learners_list <- list(
  po(kknn <- lrn("classif.kknn", id = "kknn")),
  po(rdm_forest <- lrn("classif.ranger", id = "ranger"))
)

#' Pre-prozessing
ppl_pre_processing <- ppl("robustify", factors_to_numeric = TRUE)

#' Branch between models
ppl_branch <- ppl("branch", learners_list)

#' Chain pipelines to graph
graph <- ppl_pre_processing %>>% ppl_branch
plot(graph)
graph_learner <- as_learner(graph)
