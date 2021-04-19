## Linear Discriminant Analysis for binary classification

lda <- function(X_train, y_train, X_test, y_test) {
  train <- cbind(X_train, y_train)
  test <- cbind(X_test, y_test)
  
  # Choose the priors based on relative sample size in the training set
  n_occupied <- length(y_train[y_train == "occupied"])
  n_vacant <- length(y_train[y_train == "vacant"])
  prob_occupied <- n_1/length(y_train)
  prob_vacant <- n_0/length(y_train)
  
  # Calculate sample mean vectors
  mean_occupied <- colMeans(train[train$Occupancy == "occupied", -6])
  mean_vacant <- colMeans(train[train$Occupancy == "vacant", -6])
  
  # Calculate pooled sample covariance matrix
  S_occupied <- cov(train[train$Occupancy == "occupied", -6])
  S_vacant <- cov(train[train$Occupancy == "vacant", -6])
  S_pooled <- ((n_occupied-1) * S_occupied + (n_vacant-1) * S_vacant) / (n_occupied+n_vacant-2)
  
  # Calculate coefficients
  S_inv=solve(S_pooled)
  alpha_occupied = -0.5*t(mean_occupied) %*% S_inv %*% mean_occupied + prob_occupied
  alpha_vacant = -0.5*t(mean_vacant) %*% S_inv %*% mean_vacant + prob_vacant
  beta_occupied = S_inv %*% mean_occupied
  beta_vacant = S_inv %*% mean_vacant
  
  # Calculate d and classify
  prediction = c()
  d_occupied_vec = c()
  d_vacant_vec = c()
  label = c("occupied", "vacant")
  for(i in 1:nrow(test)){
    x = t(test[i, -6])
    d_occupied = alpha_occupied + t(beta_occupied) %*% x
    d_vacant = alpha_vacant + t(beta_vacant) %*% x
    d_vec = c(d_occupied, d_vacant)
    prediction = append(prediction, label[which.max(d_vec)])
    d_occupied_vec = append(d_occupied_vec, d_occupied)
    d_vacant_vec = append(d_vacant_vec, d_vacant)
  }

  prediction <- ifelse(prediction == y_test[,1], TRUE, FALSE)
  accuracy <- length(prediction[prediction == TRUE])/length(prediction)
  
  return(accuracy)
}
