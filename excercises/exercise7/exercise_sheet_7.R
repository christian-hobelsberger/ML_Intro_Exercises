#' ### Data Import
library(pROC)
#' ### Exercise 1
#' 
#' #### c)
labels <- c(0,0,0,1,1,1,1,1,0,0)
predictions <- c(0.33,0.27,0.11,0.38,0.17,0.63,0.62,0.33,0.15,0.57)
pred_labels <- ifelse(predictions > 0.5, 1, 0)
roc_score=roc(labels, pred_labels) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")