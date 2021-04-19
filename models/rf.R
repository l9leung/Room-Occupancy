# Random Forest

library(randomForest)

set.seed(123)


rf_fit <- function(X_train, y_train, ntree) {
  fit <- randomForest(formula(paste(names(y_train[1]), "~.")),
                      data = cbind(X_train, y_train),
                      importance = TRUE,
                      ntree = ntree)
  
  return(fit)
}


rf_score <- function(fit, X_test, y_test) {
  y_hat <- predict(fit, newdata = cbind(X_test, y_test), type = "response")
  y_hat <- factor(as.vector(y_hat), labels = c("occupied", "vacant"))
  y_hat <- ifelse(y_hat == y_test[,1], TRUE, FALSE)
  accuracy <- length(y_hat[y_hat == TRUE])/length(y_hat)

  return(accuracy)  
}
