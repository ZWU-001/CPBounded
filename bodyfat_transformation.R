# Clear the current R environment
rm(list=ls())

# Load necessary libraries
library(ggplot2) # For plotting

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

# Function to insert midpoints between elements in a list of indices (for full CP)
insert_averages <- function(lst) {
  new_list <- numeric(2 * length(lst) - 1)
  new_list[seq(1, length(new_list), by = 2)] <- lst
  new_list[seq(2, length(new_list) - 1, by = 2)] <- floor((lst[-length(lst)] + lst[-1]) / 2)
  return(unique(sort(new_list)))
}

# Function to find the interval in a grid where a condition holds (for full CP)
find_true_interval <- function(f, grid) {
  evaluated_vals <- list()
  
  # Helper function to evaluate and memoize function values
  evaluate_f <- function(index) {
    key <- as.character(index)
    if (!key %in% names(evaluated_vals)) {
      evaluated_vals[[key]] <<- f(grid[index])
    }
    return(evaluated_vals[[key]])
  }
  
  n <- length(grid)
  
  # Set FALSE for the boundaries of the grid
  evaluated_vals[["1"]] <- FALSE
  evaluated_vals[[as.character(n)]] <- FALSE
  
  # Initialize search indices
  lst_index <- c(1, n)
  
  # Expand the index list until a TRUE value is found
  repeat {
    new_lst_index <- insert_averages(lst_index)
    new_points_index <- setdiff(new_lst_index, names(evaluated_vals) |> as.numeric())
    lst_index <- sort(unique(c(lst_index, new_points_index)))
    found <- FALSE
    
    # Evaluate new indices one by one
    for (idx in new_points_index) {
      if (evaluate_f(idx)) {
        found_idx <- idx
        
        # Search for nearest neighbor indices on both sides
        # (they must exist from previous loop and must correspond to values outside the inclusive region)
        left_idx <- max(lst_index[lst_index < idx & !sapply(lst_index[lst_index < idx], evaluate_f)])
        right_idx <- min(lst_index[lst_index > idx & !sapply(lst_index[lst_index > idx], evaluate_f)])
        found <- TRUE
        break # exit the for-loop
      }
    }
    if (found) break # exit the repeat-loop
  }
  
  # Binary search to find lower bound of the interval
  lo <- left_idx
  hi <- found_idx
  while (lo < hi) {
    mid <- floor((lo + hi) / 2)
    if (evaluate_f(mid)) {
      hi <- mid
    } else {
      lo <- mid + 1
    }
  }
  a <- grid[hi]
  
  # Binary search to find upper bound of the interval
  lo <- found_idx
  hi <- right_idx
  while (lo < hi) {
    mid <- ceiling((lo + hi) / 2)
    if (evaluate_f(mid)) {
      lo <- mid
    } else {
      hi <- mid - 1
    }
  }
  b <- grid[lo]
  
  return(c(a, b))
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

# True response values
y_test_true <- testing_set$y

# Set significance level
alpha <- 0.1

################################################################################
# Transformation Model: Split Conformal Prediction
################################################################################

# Fit a linear model in logit space (real line) on the training set
split_model <- lm(logit(y) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = training_set)
y_cal_pred <- predict(split_model, newdata = calibration_set)

# Compute absolute residuals on the calibration set in logit space
residuals_cal <- abs(logit(calibration_set$y) - y_cal_pred)

# Compute the split conformal quantile of residuals
split_level <- ceiling((1 - alpha) * (nrow(calibration_set) + 1))
q_alpha <- sort(residuals_cal)[split_level]

# Predict on the test set
y_test_pred <- predict(split_model, newdata = testing_set)

# Construct logit-space prediction intervals and transform back to (0,1) scale
lower_bounds <- y_test_pred - q_alpha
upper_bounds <- y_test_pred + q_alpha
lower_bounds_real <- expit(lower_bounds)
upper_bounds_real <- expit(upper_bounds)

# Compute coverage and average width of intervals
coverage_split <- mean((y_test_true >= lower_bounds_real) & (y_test_true <= upper_bounds_real))
avg_width_split <- mean(upper_bounds_real - lower_bounds_real)

################################################################################
# Transformation Model: Full Conformal Prediction
################################################################################

grid_margin <- 3      # Controls the grid size for search

full_lower <- numeric(length(y_test_true))
full_upper <- numeric(length(y_test_true))

# Combine training and calibration sets for full CP
full_data <- rbind(training_set, calibration_set)

# Linear model for classic prediction interval center
full_model <- lm(logit(y) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = full_data)

# Quantile level for full conformal
full_level <- ceiling((1 - alpha) * (nrow(full_data) + 1))

# Loop over each test point to compute full CP intervals
for (i in 1:length(y_test_true)){
  
  # Use a wide grid around the linear model prediction to search for valid interval
  classic_pred <- predict(full_model, newdata = testing_set[i, ], interval = "prediction", level = 1 - alpha)
  grid <- seq(from = classic_pred[2] - grid_margin * abs(classic_pred[3] - classic_pred[2]),
              to = classic_pred[3] + grid_margin * abs(classic_pred[3] - classic_pred[2]),
              by = 1e-4)
  
  # Function to determine if a candidate value should be included
  testFCP <- Vectorize(function(value){
    
    # Construct augmented dataset with new covariates and the candidate value
    y_aug <- c(logit(full_data$y), value)
    X_aug <- rbind(full_data[, 1:8], testing_set[i, 1:8])
    X_aug$y_aug <- y_aug
    
    full_model_aug <- lm(y_aug ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = X_aug)
    resids <- abs(y_aug - predict(full_model_aug))
    resids_full <- resids[1:nrow(full_data)]        # residuals for training+calib
    resids_new <- resids[length(resids)]            # residual for the candidate value
    
    # Compute the full conformal quantile of residuals
    q_alpha_full <- sort(resids_full)[full_level]
    return(resids_new <= q_alpha_full)
  })
  
  # Find lower and upper logit bounds
  full_lower[i] <- find_true_interval(testFCP, grid)[1]
  full_upper[i] <- find_true_interval(testFCP, grid)[2]
}

# Transform intervals back to (0,1)
lower_bounds_full <- expit(full_lower)
upper_bounds_full <- expit(full_upper)

# Compute coverage and average width for full CP
coverage_full <- mean((y_test_true >= lower_bounds_full) & (y_test_true <= upper_bounds_full))
avg_width_full <- mean(upper_bounds_full - lower_bounds_full)

# Get predicted values for plotting
y_test_pred_split <- expit(y_test_pred)
y_test_pred_full <- expit(predict(full_model, newdata = testing_set))

# Create dataframe for plotting results
plot_data <- data.frame(
  Index = 1:length(y_test_true),
  True_y = y_test_true,
  Predicted_split = y_test_pred_split,
  Predicted_full = y_test_pred_full,
  Split_Lower = lower_bounds_real,
  Split_Upper = upper_bounds_real,
  Full_Lower = lower_bounds_full,
  Full_Upper = upper_bounds_full
)

# Sort observations by true values for better visualization
plot_data <- plot_data[order(plot_data$True_y), ]
plot_data$Index <- 1:nrow(plot_data)

# Plot split CP intervals
plot_split <- ggplot(plot_data, aes(x = Index)) +
  geom_point(aes(y = True_y), color = "black", size = 2, alpha = 0.8) +
  geom_point(aes(y = Predicted_split), color = "green", shape = 17, size = 2) +
  geom_errorbar(aes(ymin = Split_Lower, ymax = Split_Upper),
                width = 0.3, color = "red", alpha = 0.5) +
  labs(title = "Transformation Method Split CP",
       y = "Response (y)", x = "Test Observation (sorted by True y)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

print(plot_split)

# Plot full CP intervals
plot_full <- ggplot(plot_data, aes(x = Index)) +
  geom_point(aes(y = True_y), color = "black", size = 2, alpha = 0.8) +
  geom_point(aes(y = Predicted_full), color = "green", shape = 17, size = 2) +
  geom_errorbar(aes(ymin = Full_Lower, ymax = Full_Upper),
                width = 0.3, color = "blue", alpha = 0.5) +
  labs(title = "Transformation Method Full CP",
       y = "Response (y)", x = "Test Observation (sorted by True y)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

print(plot_full)

# Output summary statistics
cat("Coverage rate split:", coverage_split, "\n")
cat("Average interval width split:", avg_width_split, "\n")
cat("Coverage rate full:", coverage_full, "\n")
cat("Average interval width full:", avg_width_full, "\n")