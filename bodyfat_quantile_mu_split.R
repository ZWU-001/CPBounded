# Clear the current R environment
rm(list=ls())

# Load necessary libraries
library(ggplot2) # For plotting
library(betareg) # For beta regression

# Define the logit transformation function
logit <- function(p) {
  out <-  qlogis(p) # Equivalent to log(p / (1 - p))
  return(out)
}

# Define the inverse logit (expit) function
expit <- function(p) {
  out <- plogis(p) # Equivalent to 1 / (1 + exp(-p))
  return(out)
}

# Load the dataset with 183 observations of body measurements and body fat
bodyfat <- read.csv("/Users/jameswu/Downloads/BodyFat.csv")
dataset <- data.frame(X1 = bodyfat$BMI, X2 = bodyfat$Neck, X3 = bodyfat$Chest,
                      X4 = bodyfat$Hips, X5 = bodyfat$Waist, X6 = bodyfat$Forearm,
                      X7 = bodyfat$PThigh, X8 = bodyfat$Wrist, y = bodyfat$Fat)

# Set seed for reproducibility
set.seed(123)

# Split dataset into training, calibration, and test sets
n <- nrow(dataset)
split_indices <- sample(1:n)

n3 <- 20
n1 <- floor((n - n3) / 2)
n2 <- n - n3 - n1 

idx1 <- split_indices[1:n1]               # Training indices
idx2 <- split_indices[(n1 + 1):(n1 + n2)] # Calibration indices
idx3 <- split_indices[(n1 + n2 + 1):n]    # Testing indices

training_set <- dataset[idx1, ]
calibration_set <- dataset[idx2, ]
testing_set <- dataset[idx3, ]

# Set significance level
alpha <- 0.1

# Fit beta regression to the training data with only mu (mean) modeled
beta_model <- betareg(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = training_set, link = "logit")

# Extract the estimated precision parameter
phi <- beta_model$coefficients$precision
if (is.null(phi)) phi <- beta_model$coefficients$phi    # For older betareg versions

# Predict the mean response (mu) on both calibration and test sets
calibration_set$mu_hat <- predict(beta_model, newdata = calibration_set, type = "response")
testing_set$mu_hat <- predict(beta_model, newdata = testing_set, type = "response")

# Compute quantile residuals on the calibration set
calibration_set$quantile_resid <- abs(qnorm(pbeta(
  calibration_set$y,
  shape1 = calibration_set$mu_hat * phi,
  shape2 = (1 - calibration_set$mu_hat) * phi
)))

# Compute the split conformal quantile of residuals
level_split <- ceiling((1 - alpha) * (nrow(calibration_set) + 1))
q_alpha <- sort(calibration_set$quantile_resid)[level_split]

# Construct the prediction intervals and truncate with the unit interval
testing_set$lower <- mapply(function(mu) {
  qbeta(pnorm(-q_alpha), shape1 = mu * phi, shape2 = (1 - mu) * phi)
}, testing_set$mu_hat)

testing_set$upper <- mapply(function(mu) {
  qbeta(pnorm(q_alpha), shape1 = mu * phi, shape2 = (1 - mu) * phi)
}, testing_set$mu_hat)

# Compute coverage and average width of intervals
coverage_split <- mean((testing_set$y >= testing_set$lower) & (testing_set$y <= testing_set$upper))
width_split <- mean(testing_set$upper - testing_set$lower)

# Create dataframe for plotting results
plot_data_split <- data.frame(
  Index = 1:nrow(testing_set),
  True_y = testing_set$y,
  Predicted = testing_set$mu_hat,
  Lower = testing_set$lower,
  Upper = testing_set$upper
)

# Sort observations by true values for better visualization
plot_data_split <- plot_data_split[order(plot_data_split$True_y), ]
plot_data_split$Index <- 1:nrow(plot_data_split)

# Plot split CP intervals
ggplot(plot_data_split, aes(x = Index)) +
  geom_point(aes(y = True_y), color = "black", size = 2, alpha = 0.8) +
  geom_point(aes(y = Predicted), color = "green", shape = 17, size = 2) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.3, color = "red", alpha = 0.5) +
  labs(title = "Beta Method (quantile with mu) Split CP",
       y = "Response (y)",
       x = "Test Observation (sorted by True y)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

# Output summary statistics
cat("Coverage rate (split CP):", coverage_split, "\n")
cat("Average interval width (split CP):", width_split, "\n")