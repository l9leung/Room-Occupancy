training <- read.table("./data/training.csv", sep = ",", header = TRUE, row.names = 1)[ ,-1]
training[, 6] <- factor(training[, 6], labels = c("vacant", "occupied"))

test <- read.table("./data/test.csv", sep = ",", header = TRUE, row.names = 1)[, -1]
test[, 6] <- factor(test[, 6], labels = c("vacant", "occupied"))
