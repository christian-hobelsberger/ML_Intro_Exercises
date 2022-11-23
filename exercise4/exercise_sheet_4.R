#' ### Data Import
library(mlbench)
library(ggplot2)
library(mlr3)

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
#