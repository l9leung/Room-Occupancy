source("./data/load.R")

split <- function(df, column) {
  # Split data by target variable indicated by index and features
  X  <- df[, -column]
  y <- df[, column, drop = FALSE]
  
  return(list(X, y))
}
