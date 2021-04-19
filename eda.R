## Exploratory analysis

library(Hmisc)

source("./data/load.R")

# Pairwise scatter plot
pairs(~Temperature + Humidity + Light + CO2 + HumidityRatio,
      data = training,
      labels = c("Temperature", "Humidity", "Light", "CO2", "Humidity Ratio"), 
      col = c("deepskyblue", "brown1")[training$Occupancy])


# Peek at the data
head(training)
dim(training)
nrow(training[training$Occupancy == "occupied",])/nrow(training)
nrow(training[training$Occupancy == "vacant",])/nrow(training)

# Compare variable means for occupied and vacant
cbind(colMeans(training[training$Occupancy == "occupied", -6]),
      colMeans(training[training$Occupancy == "vacant", -6]))

# Test difference in means for statistical significance
ttests <- function(df) {
  results <- numeric(ncol(df) - 1)
  for (i in 1:(ncol(df)-1)) {
    results[i] <- t.test(df[df$Occupancy == "occupied", i],
                         df[df$Occupancy == "vacant", i],
                         alternative = "two.sided")$p.value
  }
  results
}

ttests(training)

# Correlation matrix
cor_matrix <- rcorr(data.matrix(training[, -1]), type = "pearson")
cor_matrix$r
