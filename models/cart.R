# Regression Tree

library(rpart)
library(rpart.plot)

cart_fit <- function(X_train, y_train, plot = TRUE) {
  fit <- rpart(formula(paste(names(y_train[1]), "~.")),
               data = cbind(X_train, y_train),
               method = "class",
               control = rpart.control(cp = 0.003))
  
  if (plot == TRUE) {
    rpart.plot(fit, type = 2, extra = 102, box.palette = c("deepskyblue", "brown1"), cex = 1)
  }
  
  return(fit)
}


cart_score <- function(tree_fit, X_test, y_test) {
  y_hat <- predict(tree_fit, cbind(X_test, y_test), type = "vector")
  y_hat <- factor(y_hat, labels = c("vacant", "occupied"))
  y_hat <- ifelse(y_hat == y_test[,1], TRUE, FALSE)
  accuracy <- length(y_hat[y_hat == TRUE])/length(y_hat)
  
  return(accuracy)
}
