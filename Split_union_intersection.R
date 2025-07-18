# Clear the current R environment
rm(list=ls())

# -------------------------------
# Step 1: Define prediction bounds for all model framworks
# -------------------------------

lower1 <- c(0.12631929, 0.21563765, 0.13234003, 0.13017040, 0.15380689, 0.11937544, 0.12723013, 0.18060387, 0.11594184, 0.20272912, 0.29931086, 0.15405159, 0.21365592, 0.25405715, 0.11502474, 0.14705407, 0.13965679, 0.09337768, 0.15209324, 0.17914323)
upper1 <- c(0.2529428, 0.3916582, 0.2631808, 0.2595077, 0.2985681, 0.2409581, 0.2545008, 0.3404392, 0.2349605, 0.3732265, 0.5000856, 0.2989618, 0.3888608, 0.4436984, 0.2333505, 0.2876199, 0.2754351, 0.1943248, 0.2958054, 0.3382195)

lower2 <- c(0.11758172, 0.21777612, 0.12607034, 0.13118584, 0.14270368, 0.13975209, 0.12751503, 0.17842941, 0.10104047, 0.23158513, 0.33447432, 0.16514228, 0.20508749, 0.23615846, 0.07393265, 0.11496291, 0.09798419, 0.08472819, 0.19439316, 0.18644373)
upper2 <- c(0.3072977, 0.3868608, 0.2816498, 0.2612939, 0.2267687, 0.2276485, 0.2622115, 0.3504052, 0.2986650, 0.3178786, 0.4328829, 0.3012702, 0.4084132, 0.3931052, 0.3746968, 0.3619737, 0.4040315, 0.2356185, 0.2709832, 0.3007118)

lower3 <- c(0.12252118, 0.22296965, 0.12806595, 0.12587783, 0.14790870, 0.11241130, 0.12293711, 0.18432315, 0.11118978, 0.20671418, 0.31212457, 0.15477918, 0.22236139, 0.26174176, 0.11026915, 0.14661511, 0.14006026, 0.08404471, 0.15338679, 0.18040674)
upper3 <- c(0.2465193, 0.3688802, 0.2536895, 0.2508674, 0.2788733, 0.2332784, 0.2470593, 0.3234493, 0.2316631, 0.3499805, 0.4680442, 0.2874351, 0.3681782, 0.4128735, 0.2304434, 0.2772527, 0.2689967, 0.1947643, 0.2857061, 0.3187446)

lower4 <- c(0.11254130, 0.21554746, 0.12250864, 0.12925024, 0.14233285, 0.14051116, 0.12508977, 0.17976592, 0.09218867, 0.23214987, 0.33275626, 0.16531353, 0.20257113, 0.23742792, 0.04754440, 0.10421164, 0.07723914, 0.07755985, 0.19407659, 0.18753756)
upper4 <- c(0.2834859, 0.3763100, 0.2649764, 0.2485003, 0.2201452, 0.2209755, 0.2490981, 0.3350432, 0.2711607, 0.3116109, 0.4297116, 0.2894579, 0.3957546, 0.3834038, 0.3184023, 0.3289327, 0.3562716, 0.2142768, 0.2647342, 0.2921668)

lower5 <- c(0.12518127, 0.22369729, 0.13060925, 0.12846704, 0.15004571, 0.11528844, 0.12558838, 0.18575446, 0.11409352, 0.20773276, 0.31136377, 0.15677937, 0.22309980, 0.26180141, 0.11319300, 0.14877810, 0.14235591, 0.08756457, 0.15541456, 0.18191178)
upper5 <- c(0.2527002, 0.3738770, 0.2598099, 0.2570117, 0.2847711, 0.2395679, 0.2532358, 0.3289199, 0.2379655, 0.3551788, 0.4718891, 0.2932540, 0.3731825, 0.4173782, 0.2367555, 0.2831652, 0.2749835, 0.2013410, 0.2915410, 0.3242622)

lower6 <- c(0.11666833, 0.21485143, 0.12472605, 0.13029164, 0.14197549, 0.14022966, 0.12645569, 0.18009986, 0.09830807, 0.23079204, 0.33044130, 0.16538601, 0.20283123, 0.23604814, 0.06666080, 0.11201654, 0.09193167, 0.08215430, 0.19305434, 0.18679762)
upper6 <- c(0.2974334, 0.3852538, 0.2757122, 0.2568638, 0.2247414, 0.2258062, 0.2580342, 0.3446771, 0.2873285, 0.3153409, 0.4335698, 0.2971770, 0.4071378, 0.3909343, 0.3495819, 0.3485328, 0.3836760, 0.2269435, 0.2682479, 0.2979977)

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
coverage_union <- mean(union_covered)

intersection_covered <- (y_test_true >= inter_lower) & (y_test_true <= inter_upper)
coverage_intersection <- mean(intersection_covered)

width_union <- union_upper - union_lower
width_intersection <- inter_upper - inter_lower

avg_width_union <- mean(width_union)
avg_width_intersection <- mean(width_intersection)

cat("Union coverage rate:", coverage_union, "\n")
cat("Intersection coverage rate:", coverage_intersection, "\n")
cat("Average union width:", avg_width_union, "\n")
cat("Average intersection width:", avg_width_intersection, "\n")