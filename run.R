source("./data/load.R")
source("./data/split.R")
source("./models/lda.R")
source("./models/cart.R")
source("./models/rf.R")

X_train <- split(training, 6)[[1]]
y_train <- split(training, 6)[[2]]
X_test <- split(test, 6)[[1]]
y_test <- split(test, 6)[[2]]

# Linear Discriminant Analysis
accuracy_lda <- lda(X_train, y_train, X_test, y_test)
print(accuracy_lda)

# Regression Tree
cart <- cart_fit(X_train, y_train)
cart_accuracy <- cart_score(cart, X_test, y_test)
print(cart_accuracy)

# Random Forest
ntrees = c(1, seq(100, 1000, by = 100))
accuracy_rf = vector(length = length(ntrees))
for (i in 1:length(ntrees)) {
  rf <- rf_fit(X_train, y_train, ntrees[i])
  accuracy_rf[i] <- rf_score(rf, X_test, y_test)
}
print(accuracy_rf)
