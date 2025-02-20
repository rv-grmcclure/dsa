#5.3 examples from APM
install.packages("caret")
library(caret)
# Use the 'c' function to combine numbers into a vector
observed <- c(0.22, 0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4, 
              0.62, 0.99, -0.18, 0.32, 0.34, -0.30, 0.04, -0.87, 0.55, -1.30, -1.15, 0.20)
predicted <- c(0.24, 0.78, -0.66, 0.53, 0.70, -0.75, -0.41, -0.43, 0.49, 0.79, -1.19, 0.06, 0.75, -0.07, 0.43, -0.42,
               -0.25, -0.64, -1.26, -0.07)

residualValues <- observed - predicted
summary(residualValues)

# Observed values versus predicted values
# It is a good idea to plot the values on a common scale.
axisRange <- extendrange(c(observed, predicted))
plot(observed, predicted,ylim = axisRange,xlim = axisRange)
# Add a 45 degree reference line
abline(0, 1, col = "darkgrey", lty = 2)
# Predicted values versus residuals
plot(predicted, residualValues, ylab = "residual")
abline(h = 0, col = "darkgrey", lty = 2)

R2(predicted, observed)
RMSE(predicted, observed)

# Simple correlation
cor(predicted, observed)
# Rank correlation
cor(predicted, observed, method = "spearman")