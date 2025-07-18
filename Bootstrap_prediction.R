# Clear the current R environment
rm(list=ls())

# Load necessary libraries
library(ggplot2) # For plotting
library(betareg) # For beta regression
library(VGAM) # For digamma and trigamma functions

# Load the dataset with 183 observations of body measurements and body fat
bodyfat <- read.csv("/Users/jameswu/Downloads/BodyFat_data.csv")
dataset <- data.frame(X1 = bodyfat$BMI, X2 = bodyfat$Neck, X3 = bodyfat$Chest,
                      X4 = bodyfat$Hips, X5 = bodyfat$Waist, X6 = bodyfat$Forearm,
                      X7 = bodyfat$PThigh, X8 = bodyfat$Wrist, y = bodyfat$Fat)

# Split the data randomly into training, calibration, and testing sets
set.seed(123)
n <- nrow(dataset)
split_indices <- sample(1:n)
n3 <- 20
n1 <- floor((n - n3) / 2)
n2 <- n - n3 - n1 

idx1 <- split_indices[1:n1]
idx2 <- split_indices[(n1 + 1):(n1 + n2)]
idx3 <- split_indices[(n1 + n2 + 1):n]

training_set <- dataset[idx1, ]
calibration_set <- dataset[idx2, ]
testing_set <- dataset[idx3, ]

# Combine the training and the calibration sets for the implementation of bootstrap
data_full <- rbind(training_set, calibration_set)

# Set bootstrap parameters
B <- 1000
alpha <- 0.1
n_full <- nrow(data_full)
n_test <- nrow(testing_set)
residual_matrix <- matrix(NA, nrow = B, ncol = n_test) # rows = bootstrap samples, cols = testing points

# Fit beta regression on combined training and calibration sets
beta_model <- betareg(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 |  X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, 
                      data = data_full, link = "logit")

# Compute estimated parameters and calculate transformed residual components
mu_hat <- fitted(beta_model)
phi_hat <- predict(beta_model, type = "precision")
mu_star <- digamma(mu_hat * phi_hat) - digamma((1 - mu_hat) * phi_hat)
v_hat <- trigamma(mu_hat * phi_hat) + trigamma((1 - mu_hat) * phi_hat)

# Predict for testing set
mu_test <- predict(beta_model, newdata = testing_set, type = "response")
phi_test <- predict(beta_model, newdata = testing_set, type = "precision")
mu_star_test <- digamma(mu_test * phi_test) - digamma((1 - mu_test) * phi_test)
v_hat_test <- trigamma(mu_test * phi_test) + trigamma((1 - mu_test) * phi_test)

# Extract the standardized weighted residual 2 from the full model
residuals_transformed <- residuals(beta_model, type = "sweighted2")

# Bootstrap loop to generate predictive residuals
for (b in 1:B){
  
  # Resample residuals (Step 1 of Algorithm in Section 3.5)
  set.seed(b+B)
  r_tb <- sample(residuals_transformed, size = n_full, replace = TRUE)
  
  # Generate pseudo-responses (Step 2 of Algorithm in Section 3.5)
  y_tb <- (exp(mu_star + r_tb * sqrt(v_hat))) / (1 + exp(mu_star + r_tb * sqrt(v_hat)))
  
  # Refit model on bootstrap sample (Step 3 of Algorithm in Section 3.5)
  data_refit <- data.frame(X1 = data_full$X1, X2 = data_full$X2, X3 = data_full$X3,
                           X4 = data_full$X4, X5 = data_full$X5, X6 = data_full$X6,
                           X7 = data_full$X7, X8 = data_full$X8,
                           y = y_tb)
  beta_model_refit <- betareg(y ~  X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 |  X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, 
                              data = data_refit, link = "logit")
  
  # Obtain estimated parameters and calculate transformed residual components for the testing set
  mu_pb <- predict(beta_model_refit, newdata = testing_set, type = "response")
  phi_pb <- predict(beta_model_refit, newdata = testing_set, type = "precision")
  mu_star_pb <- digamma(mu_pb * phi_pb) - digamma((1 - mu_pb) * phi_pb)
  v_hat_pb <- trigamma(mu_pb * phi_pb) + trigamma((1 - mu_pb) * phi_pb)
  
  # Resample residuals for the testing set (Step 4(a) of Algorithm in Section 3.5)
  set.seed(b+2*B)
  r_pb <- sample(residuals_transformed, size = n_test, replace = TRUE)
  
  # Compute pseudo-responses for the testing set (Step 4(b) of Algorithm in Section 3.5))
  y_pb <- (exp(mu_star_test + r_pb * sqrt(v_hat_test))) / (1 + exp(mu_star_test + r_pb * sqrt(v_hat_test)))
  
  # Compute the normalized residual (Step 4(c) of Algorithm in Section 3.5)
  y_star_pb <- log(y_pb / (1 - y_pb))
  r_testing <- (y_star_pb - mu_star_pb) / sqrt(v_hat_pb)
  
  # Store bootstrap residuals
  residual_matrix[b, ] <- r_testing
  
}

# Construct percentile prediction intervals
lower_delta <- apply(residual_matrix, 2, quantile, probs = alpha / 2)
upper_delta <- apply(residual_matrix, 2, quantile, probs = 1 - alpha / 2)

y_lower <- (exp(mu_star_test + lower_delta * sqrt(v_hat_test))) / (1 + exp(mu_star_test + lower_delta * sqrt(v_hat_test)))
y_upper <- (exp(mu_star_test + upper_delta * sqrt(v_hat_test))) / (1 + exp(mu_star_test + upper_delta * sqrt(v_hat_test)))

y_lower <- y_lower
y_upper <- y_upper

# Assemble prediction interval data
prediction_intervals <- data.frame(
  Lower = y_lower,
  Upper = y_upper,
  Predicted = mu_test,
  Observed = testing_set$y
)

# Compute coverage and interval width
covered <- (prediction_intervals$Observed >= prediction_intervals$Lower) & (prediction_intervals$Observed <= prediction_intervals$Upper)
coverage_rate <- mean(covered)

interval_width <- prediction_intervals$Upper - prediction_intervals$Lower
mean_width <- mean(interval_width)

# Output results
cat("Mean coverage rate:", round(coverage_rate, 4), "\n")
cat("Mean interval width:", round(mean_width, 4), "\n")

# Visualization: sort testing set by true values
plot_data <- prediction_intervals
plot_data$Index <- 1:nrow(plot_data)
plot_data <- plot_data[order(plot_data$Observed), ]
plot_data$SortedIndex <- 1:nrow(plot_data)

# Plot prediction intervals, predictions, and true values
ggplot(plot_data, aes(x = SortedIndex)) +
  geom_point(aes(y = Observed), size = 2, alpha = 0.8) +  # True values (dots)
  geom_point(aes(y = Predicted), shape = 17, color = "green", size = 2) +  # Predicted values (triangles)
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.25, color = "blue", alpha = 0.5) +
  labs(
    title = "Bootstrap Prediction Interval",
    x = "Test Observation (sorted by True y)",
    y = "Response (y)"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 14)