# Clear the current R environment
rm(list=ls())

# Load required libraries
library(ggplot2) # For plotting
library(dplyr) # For data manipulation and transformation
library(readr) # For reading CSV files

# Read the prediction interval data and create a model label combining model type and CP method
df <- read_csv("/Users/jameswu/Downloads/Combined_prediction_interval.csv") %>%
  mutate(ModelLabel = paste0(Model, " - ", Submodel, " CP"))

# Define the ordering of facets: ensure 'Split' conformal methods appear before 'Full'
ordered_labels <- df %>%
  distinct(Model, Submodel) %>%
  mutate(SubmodelOrder = ifelse(Submodel == "Split", 0, 1)) %>%
  arrange(Model, SubmodelOrder) %>%
  transmute(ModelLabel = paste0(Model, " - ", Submodel, " CP")) %>%
  pull(ModelLabel)

# Update factor levels for consistent facet ordering and define marker for true values
df <- df %>%
  mutate(
    ModelLabel = factor(ModelLabel, levels = ordered_labels),
    PointType = "True value"
  )

# Generate faceted plot of prediction intervals across models and CP methods
ggplot(df, aes(x = TestPoint)) +
  # Add error bars representing prediction intervals
  geom_errorbar(aes(ymin = Lower, ymax = Upper, color = Covered), width = 0.2) +
  # Add points for predicted values
  geom_point(aes(y = Prediction, color = Covered), size = 2) +
  # Overlay true values using cross symbols
  geom_point(aes(y = True, shape = PointType), size = 2, stroke = 0.7, color = "black") +
  # Create one panel per model-submodel configuration
  facet_wrap(~ ModelLabel, ncol = 4) +
  # Define color scheme based on whether the interval covers the true value
  scale_color_manual(
    values = c("TRUE" = "forestgreen", "FALSE" = "red"),
    name = "Interval Coverage",
    labels = c("TRUE" = "Covers true value", "FALSE" = "Misses true value")
  ) +
  # Define shape for true value markers
  scale_shape_manual(
    name = "",
    values = c("True value" = 4)
  ) +
  # Set axis labels
  labs(
    x = "Testing Point",
    y = "Prediction Interval"
  ) +
  # Apply minimal theme with customized legend and facet strip text
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )
