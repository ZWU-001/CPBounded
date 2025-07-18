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

# Function to insert midpoints between elements in a list of values (for full CP)
insert_averages <- function(lst) {
  new_list <- numeric(2 * length(lst) - 1)
  new_list[seq(1, length(new_list), by = 2)] <- lst
  new_list[seq(2, length(new_list) - 1, by = 2)] <- (lst[-length(lst)] + lst[-1]) / 2
  return(unique(sort(new_list)))
}

# Function to find the interval in the unit interval where a condition holds (for full CP)
find_true_interval <- function(f, tol, prediction_temp) {
  evaluated_vals <- list()
  
  # Helper function to evaluate and memoize function values
  evaluate_f <- function(x) {
    key <- format(round(x, 10), scientific = FALSE)
    if (!key %in% names(evaluated_vals)) {
      evaluated_vals[[key]] <<- f(x)
    }
    return(evaluated_vals[[key]])
  }
  
  if (f(prediction_temp)) {
    # Case 1: The initial guess lies within the inclusion region -- 
    # directly use the binary search
    true_start1 <- prediction_temp
    true_start2 <- prediction_temp
    
    # Binary search to find the lower bound of the interval
    lower_bound <- 0
    while ((true_start1 - lower_bound) > tol) {
      mid <- (lower_bound + true_start1) / 2
      if (evaluate_f(mid)) {
        true_start1 <- mid
      } else {
        lower_bound <- mid
      }
    }
    
    # Binary search to find the upper bound of the interval
    upper_bound <- 1
    while ((upper_bound - true_start2) > tol) {
      mid <- (upper_bound + true_start2) / 2
      if (evaluate_f(mid)) {
        true_start2 <- mid
      } else {
        upper_bound <- mid
      }
    }
    
    a <- lower_bound
    b <- upper_bound
    
  } else {
    # Case 2: The initial guess lies outside the inclusion region -- 
    # find a midpoint inside the region and then use the binary search
    
    # Initialize search values
    lst <- c(0, 1)
    
    # Set FALSE for the boundaries of the unit interval
    evaluated_vals[["0"]] <- FALSE
    evaluated_vals[["1"]] <- FALSE
    
    # Expand the value list until a TRUE value is found
    repeat {
      new_lst <- insert_averages(lst)
      new_points <- setdiff(new_lst, names(evaluated_vals) |> as.numeric())
      lst <- new_lst
      
      # Evaluate new points one by one
      for (x in new_points) {
        if (evaluate_f(x)) {
          
          # Find the position in sorted lst
          idx <- which(lst == x)
          
          # Search for nearest neighbors on both sides
          # (they must exist from previous loop and must be outside the inclusive region)
          i_left <- idx - 1
          i_right <- idx + 1
          lower_found_value <- lst[i_left]
          found_value_lower <- lst[idx]
          found_value_upper <- lst[idx]
          upper_found_value <- lst[i_right]
          break  # exit the for-loop
        }
      }
      
      if (exists("lower_found_value")) break  # exit the repeat-loop
    }
    
    # Binary search to find lower bound of the interval
    while ((found_value_lower - lower_found_value) > tol) {
      mid <- (lower_found_value + found_value_lower) / 2
      if (evaluate_f(mid)) {
        found_value_lower <- mid
      } else {
        lower_found_value <- mid
      }
    }
    a <- lower_found_value
    
    # Binary search to find upper bound of the interval
    while ((upper_found_value - found_value_upper) > tol) {
      mid <- (upper_found_value + found_value_upper) / 2
      if (evaluate_f(mid)) {
        found_value_upper <- mid
      } else {
        upper_found_value <- mid
      }
    }
    b <- upper_found_value
  }
  
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
n1 <- floor(((n - n3) / 2))
n2 <- n - n3 - n1

idx1 <- split_indices[1:n1]               # Training indices
idx2 <- split_indices[(n1 + 1):(n1 + n2)] # Calibration indices
idx3 <- split_indices[(n1 + n2 + 1):n]    # Testing indices

training_set <- dataset[idx1, ]
calibration_set <- dataset[idx2, ]
testing_set <- dataset[idx3, ]

y_test_true <- testing_set$y

# Combine training and calibration sets for full CP
data_full <- rbind(training_set, calibration_set)

# Set significance level
alpha <- 0.1

# Size of the combined set
n_full <- nrow(data_full)

# Quantile level for full conformal
k <- ceiling((1 - alpha) * (n_full + 1))

lower_full <- numeric(nrow(testing_set))
upper_full <- numeric(nrow(testing_set))

# Beta model based on training+calibration sets used to obtain the initial guess (i.e., prediction_temp)
# with only mu (mean) modeled
beta_model_full <- betareg(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = data_full, link = "logit")

for (i in 1:nrow(testing_set)){

  # Initial guess
  prediction_temp <- predict(beta_model_full, newdata = testing_set[i, ], type = "response")
  
  # Function to determine if a candidate value should be included
  testFCP_quantile <- Vectorize(function(value) {
    
    # Construct augmented dataset with new covariates and the candidate value
    datan = data.frame(X1 = c(data_full$X1,testing_set[i, ]$X1), X2 = c(data_full$X2,testing_set[i, ]$X2),
                       X3 = c(data_full$X3,testing_set[i, ]$X3), X4 = c(data_full$X4,testing_set[i, ]$X4),
                       X5 = c(data_full$X5,testing_set[i, ]$X5), X6 = c(data_full$X6,testing_set[i, ]$X6),
                       X7 = c(data_full$X7,testing_set[i, ]$X7), X8 = c(data_full$X8,testing_set[i, ]$X8),
                       y = c(data_full$y,value))
    beta_model_new <- betareg(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = datan, link = "logit")
    
    # Get predicted means
    mu_hat_full <- predict(beta_model_new, type = "response")
    
    # Extract the estimated precision parameter
    phi_hat <- beta_model_new$coefficients$precision
    if (is.null(phi_hat)) phi_hat <- beta_model_new$coefficients$phi      # For older betareg versions
    
    # Get the quantile residual for combined set and the candidate value
    quantile_resids <- abs(qnorm(pbeta(
      datan$y,
      shape1 = mu_hat_full * phi_hat,
      shape2 = (1 - mu_hat_full) * phi_hat
    )))
    
    # Compute the full conformal quantile of residuals
    res_quantile_threshold <- sort(quantile_resids[1:n_full])[k]
    res_new <- quantile_resids[n_full + 1]    # residual for candidate value
    return(res_new <= res_quantile_threshold)
  })
  
  # Compute conformal prediction interval
  CPI_quantile <- find_true_interval(testFCP_quantile, tol = 1e-4, prediction_temp)
  lower_full[i] <- CPI_quantile[1]
  upper_full[i] <- CPI_quantile[2]
}

# Compute coverage and average width for full CP
coverage_full <- mean((y_test_true >= lower_full) & (y_test_true <= upper_full))
width_full <- mean(upper_full - lower_full)

mu_test_predicted <- predict(beta_model_full, newdata = testing_set, type = "response")

# Create dataframe for plotting results
plot_data_full <- data.frame(
  Index = 1:length(y_test_true),
  True_y = y_test_true,
  Lower = lower_full,
  Upper = upper_full,
  Predicted = mu_test_predicted
)

# Sort observations by true values for better visualization
plot_data_full <- plot_data_full[order(plot_data_full$True_y), ]
plot_data_full$Index <- 1:nrow(plot_data_full)

# Plot full CP intervals
ggplot(plot_data_full, aes(x = Index)) +
  geom_point(aes(y = True_y), color = "black", size = 2, alpha = 0.8) +
  geom_point(aes(y = Predicted), color = "green", shape = 17, size = 2) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.3, color = "blue", alpha = 0.5) +
  labs(title = "Beta Method (Quantile with mu) Full CP",
       y = "Response (y)",
       x = "Test Observation (sorted by True y)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

# Output summary statistics
cat("Coverage rate (full CP):", coverage_full, "\n")
cat("Average interval width (full CP):", width_full, "\n")