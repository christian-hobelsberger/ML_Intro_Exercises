#' ### Data Import
library(ggplot2)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlbench)
library(rpart.plot)
library(mlr3filters)
library(data.table)
#' ### Exercise 1
#' 
data_baseball <- fread("lab_sessions/data/baseball.csv")
data_baseball$team <- as.factor(data_baseball$team)
data_baseball$league <- as.factor(data_baseball$league)

#' #### 1)
#' Think about what random search actually means with regard to the search space and a suitable stopping
#' criterion.
#' -> We have an one dimensional search space 