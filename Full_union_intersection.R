# Clear the current R environment
rm(list=ls())

# -------------------------------
# Step 1: Define prediction bounds for all model framworks
# -------------------------------

# Transformation model
lower1 <- c(0.14040331, 0.21916919, 0.13701169, 0.13263371, 0.12356535, 0.12047769, 0.12912707, 0.19952688, 0.12430843, 0.20420943, 0.31013733, 0.16519188, 0.22503263, 0.24336033, 0.12570957, 0.15811572, 0.15286945, 0.09515322, 0.15456536, 0.18211763)
upper1 <- c(0.2618946, 0.3862235, 0.2527054, 0.2456646, 0.2696317, 0.2412728, 0.2444751, 0.3554303, 0.2319117, 0.3645916, 0.5050805, 0.2966516, 0.3945621, 0.4628078, 0.2369927, 0.2867307, 0.2814722, 0.1928375, 0.2883659, 0.3245662)

# Heteroscedastic transformation
lower2 <- c(0.1495870, 0.2053290, 0.1326953, 0.1268092, 0.1119384, 0.1183327, 0.1228300, 0.1913977, 0.1284931, 0.1800380, 0.2787888, 0.1591329, 0.2134295, 0.2324116, 0.1362681, 0.1528118, 0.1599809, 0.1003197, 0.1489026, 0.1630966)
upper2 <- c(0.2477508, 0.4083201, 0.2592376, 0.2645660, 0.2951935, 0.2482973, 0.2633170, 0.3597720, 0.2393133, 0.3889353, 0.5235881, 0.3092572, 0.4094655, 0.4753340, 0.2293759, 0.3018897, 0.2860859, 0.1995551, 0.3108531, 0.3541914)

# Beta model based on Pearson residuals with covariate-dependent mu
lower3 <- c(0.13565818, 0.22463010, 0.13404882, 0.12844073, 0.11900834, 0.11467144, 0.12404900, 0.20297479, 0.11961067, 0.20898534, 0.31983870, 0.16460541, 0.23052301, 0.24994250, 0.12000433, 0.15682524, 0.15104902, 0.08634374, 0.15385001, 0.18381117)
upper3 <- c(0.2559851, 0.3684044, 0.2478843, 0.2415873, 0.2620443, 0.2371715, 0.2398185, 0.3416604, 0.2304149, 0.3512321, 0.4810714, 0.2882503, 0.3757784, 0.4394355, 0.2337559, 0.2794072, 0.2744912, 0.1928756, 0.2800009, 0.3133378)

# Beta model based on Pearson residuals with covariate-dependent mu and phi
lower4 <- c(0.09908479, 0.22800010, 0.12939812, 0.13561019, 0.13700267, 0.10619905, 0.13315743, 0.19010288, 0.11436394, 0.22131412, 0.32818412, 0.16625408, 0.22455554, 0.25395303, 0.09782657, 0.15830752, 0.13682116, 0.07887642, 0.16309620, 0.20705722)
upper4 <- c(0.2858934, 0.3673153, 0.2514287, 0.2360481, 0.2432744, 0.2360948, 0.2359119, 0.3461995, 0.2454833, 0.3274496, 0.4669644, 0.2869039, 0.3856391, 0.4526435, 0.2639039, 0.2864554, 0.3005521, 0.2044918, 0.2731933, 0.2907158)

# Beta model based on quantile residuals with covariate-dependent mu
lower5 <- c(0.13968539, 0.22596763, 0.13580765, 0.13153100, 0.12305201, 0.11864223, 0.12720323, 0.20660763, 0.12321510, 0.21241300, 0.32317221, 0.16903984, 0.23333247, 0.25252302, 0.12339286, 0.16101455, 0.15491410, 0.09041588, 0.15646013, 0.18779326)
upper5 <- c(0.2579910, 0.3690308, 0.2481667, 0.2427085, 0.2663082, 0.2402642, 0.2412858, 0.3442431, 0.2321619, 0.3524538, 0.4798932, 0.2897303, 0.3773209, 0.4439223, 0.2351392, 0.2812304, 0.2759633, 0.1960289, 0.2823224, 0.3141200)

# Beta model based on quantile residuals with covariate-dependent mu and phi
lower6 <- c(0.11445502, 0.23112140, 0.13450716, 0.13932678, 0.13905950, 0.11363039, 0.13694646, 0.19779184, 0.12049840, 0.22475319, 0.33088285, 0.17154702, 0.22589440, 0.25639246, 0.10572726, 0.16285534, 0.14522528, 0.08872718, 0.16832952, 0.20929862)
upper6 <- c(0.2885368, 0.3660287, 0.2522195, 0.2365452, 0.2451690, 0.2363459, 0.2362105, 0.3461106, 0.2457838, 0.3281565, 0.4637137, 0.2870923, 0.3849600, 0.4484829, 0.2671000, 0.2875004, 0.3021723, 0.2069993, 0.2721473, 0.2905323)

# -------------------------------
# Step 2: Compute union/intersection
# -------------------------------

all_lowers <- list(lower1, lower2, lower3, lower4, lower5, lower6)
all_uppers <- list(upper1, upper2, upper3, upper4, upper5, upper6)

lower_mat <- do.call(cbind, all_lowers)
upper_mat <- do.call(cbind, all_uppers)

union_lower <- apply(lower_mat, 1, min)
union_upper <- apply(upper_mat, 1, max)

inter_lower <- apply(lower_mat, 1, max)
inter_upper <- apply(upper_mat, 1, min)

union_width <- union_upper - union_lower
inter_width <- inter_upper - inter_lower
intersection_empty <- inter_upper < inter_lower

# -------------------------------
# Step 3: Output results
# -------------------------------

cat("Average Union Width: ", mean(union_width), "\n")
cat("Average Intersection Width: ", mean(inter_width), "\n")
cat("Proportion of Empty Intersections: ", mean(intersection_empty), "\n")

# Optional: Data frame
df_union <- data.frame(
  Index = 1:length(union_lower),
  Union_Lower = union_lower,
  Union_Upper = union_upper,
  Intersect_Lower = inter_lower,
  Intersect_Upper = inter_upper,
  Union_Width = union_width,
  Intersect_Width = inter_width,
  Empty_Intersection = intersection_empty
)

print(df_union)


# -------------------------------
# Step 4: Check the performance of the union/intersection intervals
# -------------------------------

# Load the dataset with 183 observations of body measurements and body fat
bodyfat <- read.csv("/Users/jameswu/Downloads/BodyFat_data.csv")
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

y_test_true <- testing_set$y

# Compute coverage and average width of union and intersection prediction intervals
union_covered <- (y_test_true >= union_lower) & (y_test_true <= union_upper)
intersection_covered <- (y_test_true >= inter_lower) & (y_test_true <= inter_upper)

coverage_union <- mean(union_covered)
coverage_intersection <- mean(intersection_covered)

cat("Union coverage rate:", coverage_union, "\n")
cat("Intersection coverage rate:", coverage_intersection, "\n")
cat("Average union width:", mean(union_width), "\n")
cat("Average intersection width:", mean(inter_width), "\n")
cat("Proportion of empty intersections:", mean(intersection_empty), "\n")











