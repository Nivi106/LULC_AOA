# Load the required packages
library(terra)
library(caret)
library(mapview)
library(sf)
library(CAST)
library(tmap)
library(doParallel)
library(parallel)
library(Orcs)
library(terra)
library(caret)
library(ggplot2)

# Load the AoA raster (binary raster with 1 = inside AoA, 0 = outside AoA)
aoa_raster <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")

# Load the reference raster for comparison
reference_raster <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH FLORENCE.png")

# Load predicted raster 
predicted_raster <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")
# Crop the reference raster to match the extent of the AoA raster
reference_raster_cropped <- crop(reference_raster, ext(aoa_raster))

# Ensure both rasters have the same extent 
print(ext(reference_raster_cropped))  # Check cropped reference raster extent
print(ext(aoa_raster))               # Check AoA raster extent

# Check if both rasters have the same resolution
if (!all(res(reference_raster_cropped) == res(aoa_raster))) {
  # If the resolution is different, resample the reference raster to match AoA raster resolution
  reference_raster_cropped <- resample(reference_raster_cropped, aoa_raster, method = "bilinear")
}

# Now apply the mask to select only the values inside AoA (1 = inside AoA)
inside_aoa_reference <- mask(reference_raster_cropped, aoa_raster, maskvalues = 0)  # Mask out outside AoA

# Similarly, mask the predicted raster inside AoA

inside_aoa_predicted <- mask(predicted_raster, aoa_raster, maskvalues = 0)  # Mask out outside AoA


# Mask reference and predicted values inside AoA (1 = inside AoA)
inside_aoa_reference <- mask(reference_raster_cropped, aoa_raster, maskvalues = 0)  # Mask out outside AoA
inside_aoa_predicted <- mask(predicted_raster, aoa_raster, maskvalues = 0)  # Mask out outside AoA

# Mask reference and predicted values outside AoA (0 = outside AoA)
outside_aoa_reference <- mask(reference_raster_cropped, aoa_raster, maskvalues = 1)  # Mask out inside AoA
outside_aoa_predicted <- mask(predicted_raster, aoa_raster, maskvalues = 1)  # Mask out inside AoA

# Convert rasters to vectors for comparison (removing NA values)
inside_aoa_reference_values <- as.vector(inside_aoa_reference)
inside_aoa_predicted_values <- as.vector(inside_aoa_predicted)

outside_aoa_reference_values <- as.vector(outside_aoa_reference)
outside_aoa_predicted_values <- as.vector(outside_aoa_predicted)


# Ensure valid values are present for inside AoA
valid_inside <- !is.na(inside_aoa_reference_values) & !is.na(inside_aoa_predicted_values)
inside_aoa_reference_values <- inside_aoa_reference_values[valid_inside]
inside_aoa_predicted_values <- inside_aoa_predicted_values[valid_inside]



#Calculate MAE, RMSE, R^2 for Inside AoA
mae_inside <- mean(abs(inside_aoa_predicted_values - inside_aoa_reference_values))
rmse_inside <- sqrt(mean((inside_aoa_predicted_values - inside_aoa_reference_values)^2))
rsq_inside <- cor(inside_aoa_predicted_values, inside_aoa_reference_values)^2

# Print the accuracy metrics for inside AoA
cat("Inside AoA Metrics:\n")
cat("MAE:", mae_inside, "\n")
cat("RMSE:", rmse_inside, "\n")
cat("R^2:", rsq_inside, "\n")




#Spatial Accuracy Metrics (Outside AoA)
valid_outside <- !is.na(outside_aoa_reference_values) & !is.na(outside_aoa_predicted_values)
outside_aoa_reference_values <- outside_aoa_reference_values[valid_outside]
outside_aoa_predicted_values <- outside_aoa_predicted_values[valid_outside]


# Calculate MAE, RMSE, R^2 for Outside AoA
mae_outside <- mean(abs(outside_aoa_predicted_values - outside_aoa_reference_values))
rmse_outside <- sqrt(mean((outside_aoa_predicted_values - outside_aoa_reference_values)^2))
rsq_outside <- cor(outside_aoa_predicted_values, outside_aoa_reference_values)^2

# Print the accuracy metrics for outside AoA
cat("\nOutside AoA Metrics:\n")
cat("MAE:", mae_outside, "\n")
cat("RMSE:", rmse_outside, "\n")
cat("R^2:", rsq_outside, "\n")










# Visualize Residuals - Spatially

# Residuals inside AoA
residuals_inside <- inside_aoa_predicted - inside_aoa_reference
residuals_raster_inside <- rast(residuals_inside)

# Residuals outside AoA
residuals_outside <- outside_aoa_predicted - outside_aoa_reference
residuals_raster_outside <- rast(residuals_outside)

# Plot residuals for inside AoA
plot(residuals_raster_inside, main = "Residuals Inside AoA", col = terrain.colors(100))

# Plot residuals for outside AoA
plot(residuals_raster_outside, main = "Residuals Outside AoA", col = terrain.colors(100))
# Remove NA values from inside_aoa_reference_values and residuals_inside
valid_inside_residuals <- !is.na(inside_aoa_reference_values) & !is.na(residuals_inside)

# Convert raster layers to vectors and remove NAs
inside_aoa_reference_values <- as.vector(inside_aoa_reference)
residuals_inside <- as.vector(residuals_inside)

# Remove NA values from both vectors
valid_inside_residuals <- !is.na(inside_aoa_reference_values) & !is.na(residuals_inside)

# Keep only valid values
inside_aoa_reference_values_clean <- inside_aoa_reference_values[valid_inside_residuals]
residuals_inside_clean <- residuals_inside[valid_inside_residuals]

# Create the data frame with the cleaned data
inside_residual_data <- data.frame(actual = inside_aoa_reference_values_clean, residual = residuals_inside_clean)

# Now you can proceed with plotting
ggplot(inside_residual_data, aes(x = actual, y = residual)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot: Inside AoA", x = "Actual Values", y = "Residuals") +
  theme_minimal()

# Convert raster layers to vectors and remove NAs for outside AoA
outside_aoa_reference_values <- as.vector(outside_aoa_reference)  # Assuming you have this data
residuals_outside <- as.vector(residuals_outside)  # Assuming residuals for outside AoA have been calculated

# Remove NA values from both vectors
valid_outside_residuals <- !is.na(outside_aoa_reference_values) & !is.na(residuals_outside)

# Keep only valid values
outside_aoa_reference_values_clean <- outside_aoa_reference_values[valid_outside_residuals]
residuals_outside_clean <- residuals_outside[valid_outside_residuals]

# Create the data frame with the cleaned data for outside AoA
outside_residual_data <- data.frame(actual = outside_aoa_reference_values_clean, residual = residuals_outside_clean)
windows()
# Plotting
ggplot(outside_residual_data, aes(x = actual, y = residual)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot: Outside AoA", x = "Actual Values", y = "Residuals") +
  theme_minimal()


# Scatter Plot for Predicted vs Actual (Inside AoA)
inside_comparison_data <- data.frame(actual = inside_aoa_reference_values, predicted = inside_aoa_predicted_values)
ggplot(inside_comparison_data, aes(x = actual, y = predicted)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual: Inside AoA", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Scatter Plot for Predicted vs Actual (Outside AoA)
outside_comparison_data <- data.frame(actual = outside_aoa_reference_values, predicted = outside_aoa_predicted_values)
ggplot(outside_comparison_data, aes(x = actual, y = predicted)) +
  geom_point(color = "red", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Predicted vs Actual: Outside AoA", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Line Plot for Predicted vs Actual (Inside AoA)
ggplot(inside_comparison_data, aes(x = actual, y = predicted)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual Line Plot: Inside AoA", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Line Plot for Predicted vs Actual (Outside AoA)
ggplot(outside_comparison_data, aes(x = actual, y = predicted)) +
  geom_line(color = "red", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Predicted vs Actual Line Plot: Outside AoA", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Histogram of Residuals (Inside AoA)
ggplot(inside_residual_data, aes(x = residual)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals: Inside AoA", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Histogram of Residuals (Outside AoA)
ggplot(outside_residual_data, aes(x = residual)) +
  geom_histogram(bins = 30, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals: Outside AoA", x = "Residuals", y = "Frequency") +
  theme_minimal()


# Load necessary packages
library(terra)
library(caret)
library(ggplot2)

# Load the AoA raster (binary raster with 1 = inside AoA, 0 = outside AoA)
aoa_raster_uppsala <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Uppsala aoa.tiff")

# Load the reference (ground truth or classified) raster for comparison
reference_raster_uppsala <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH UPPSALA.png")

# Load predicted raster (this can be your model's predictions)
predicted_raster_uppsala <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Uppsala prediction.tiff")

# Step 1: Crop reference and predicted rasters to match the extent of AoA raster
reference_raster_cropped_uppsala <- crop(reference_raster_uppsala, ext(aoa_raster_uppsala))
predicted_raster_cropped_uppsala <- crop(predicted_raster_uppsala, ext(aoa_raster_uppsala))

# Step 2: Mask reference and predicted values for inside AoA (1 = inside AoA)
inside_aoa_reference_uppsala <- mask(reference_raster_cropped_uppsala, aoa_raster_uppsala, maskvalues = 0)
inside_aoa_predicted_uppsala <- mask(predicted_raster_cropped_uppsala, aoa_raster_uppsala, maskvalues = 0)

# Step 3: Mask reference and predicted values for outside AoA (0 = outside AoA)
outside_aoa_reference_uppsala <- mask(reference_raster_cropped_uppsala, aoa_raster_uppsala, maskvalues = 1)
outside_aoa_predicted_uppsala <- mask(predicted_raster_cropped_uppsala, aoa_raster_uppsala, maskvalues = 1)

# Step 4: Convert rasters to vectors for comparison (removing NA values)
inside_aoa_reference_values_uppsala <- as.vector(inside_aoa_reference_uppsala)
inside_aoa_predicted_values_uppsala <- as.vector(inside_aoa_predicted_uppsala)
outside_aoa_reference_values_uppsala <- as.vector(outside_aoa_reference_uppsala)
outside_aoa_predicted_values_uppsala <- as.vector(outside_aoa_predicted_uppsala)



# Ensure the reference and predicted rasters are properly masked and aligned
outside_aoa_reference_values_uppsala <- as.vector(outside_aoa_reference_uppsala)
outside_aoa_predicted_values_uppsala <- as.vector(outside_aoa_predicted_uppsala)

# Step: Check the lengths of both vectors
cat("Length of outside_aoa_reference_values_uppsala:", length(outside_aoa_reference_values_uppsala), "\n")
cat("Length of outside_aoa_predicted_values_uppsala:", length(outside_aoa_predicted_values_uppsala), "\n")

# If lengths do not match, align them
if (length(outside_aoa_reference_values_uppsala) != length(outside_aoa_predicted_values_uppsala)) {
  # Align the two vectors by removing elements that are NA from either vector
  valid_outside_uppsala <- !is.na(outside_aoa_reference_values_uppsala) & !is.na(outside_aoa_predicted_values_uppsala)
  outside_aoa_reference_values_uppsala <- outside_aoa_reference_values_uppsala[valid_outside_uppsala]
  outside_aoa_predicted_values_uppsala <- outside_aoa_predicted_values_uppsala[valid_outside_uppsala]
}

# Both vectors have the same length, and calculate the metrics
mae_outside_uppsala <- mean(abs(outside_aoa_predicted_values_uppsala - outside_aoa_reference_values_uppsala))
rmse_outside_uppsala <- sqrt(mean((outside_aoa_predicted_values_uppsala - outside_aoa_reference_values_uppsala)^2))
rsq_outside_uppsala <- cor(outside_aoa_predicted_values_uppsala, outside_aoa_reference_values_uppsala)^2



# Check the lengths of both vectors before removing NAs
cat("Length of outside_aoa_reference_values_uppsala:", length(outside_aoa_reference_values_uppsala), "\n")
cat("Length of outside_aoa_predicted_values_uppsala:", length(outside_aoa_predicted_values_uppsala), "\n")

# Check for any discrepancy in the two vectors
if (length(outside_aoa_reference_values_uppsala) != length(outside_aoa_predicted_values_uppsala)) {
  
  cat("Outside AoA Reference NA values: ", sum(is.na(outside_aoa_reference_values_uppsala)), "\n")
  cat("Outside AoA Predicted NA values: ", sum(is.na(outside_aoa_predicted_values_uppsala)), "\n")
  
  # Ensure both vectors have valid values after masking
  valid_outside_uppsala <- !is.na(outside_aoa_reference_values_uppsala) & !is.na(outside_aoa_predicted_values_uppsala)
  
  # Filter vectors to only keep valid values
  outside_aoa_reference_values_uppsala <- outside_aoa_reference_values_uppsala[valid_outside_uppsala]
  outside_aoa_predicted_values_uppsala <- outside_aoa_predicted_values_uppsala[valid_outside_uppsala]
  
  # Check if lengths match after the alignment
  cat("After alignment:\n")
  cat("Length of outside_aoa_reference_values_uppsala:", length(outside_aoa_reference_values_uppsala), "\n")
  cat("Length of outside_aoa_predicted_values_uppsala:", length(outside_aoa_predicted_values_uppsala), "\n")
}


# Check the lengths of both vectors before removing NAs
cat("Length of inside_aoa_reference_values_uppsala:", length(inside_aoa_reference_values_uppsala), "\n")
cat("Length of inside_aoa_predicted_values_uppsala:", length(inside_aoa_predicted_values_uppsala), "\n")

# Check for any discrepancy in the two vectors
cat("Inside AoA Reference NA values: ", sum(is.na(inside_aoa_reference_values_uppsala)), "\n")
cat("Inside AoA Predicted NA values: ", sum(is.na(inside_aoa_predicted_values_uppsala)), "\n")

# Ensure both vectors have valid values after masking
valid_inside_uppsala <- !is.na(inside_aoa_reference_values_uppsala) & !is.na(inside_aoa_predicted_values_uppsala)

# Filter vectors to only keep valid values
inside_aoa_reference_values_uppsala <- inside_aoa_reference_values_uppsala[valid_inside_uppsala]
inside_aoa_predicted_values_uppsala <- inside_aoa_predicted_values_uppsala[valid_inside_uppsala]

# Check the lengths again after alignment
cat("After alignment:\n")
cat("Length of inside_aoa_reference_values_uppsala:", length(inside_aoa_reference_values_uppsala), "\n")
cat("Length of inside_aoa_predicted_values_uppsala:", length(inside_aoa_predicted_values_uppsala), "\n")

# Verify the first few values in the reference and predicted arrays to check the content
cat("\nFirst few values of reference values (inside AoA):\n")
print(head(inside_aoa_reference_values_uppsala))

cat("\nFirst few values of predicted values (inside AoA):\n")
print(head(inside_aoa_predicted_values_uppsala))

# Calculate the metrics only if lengths match
if (length(inside_aoa_reference_values_uppsala) == length(inside_aoa_predicted_values_uppsala)) {
  # Calculate MAE, RMSE, and R^2 for Inside AoA
  mae_inside_uppsala <- mean(abs(inside_aoa_reference_values_uppsala - inside_aoa_predicted_values_uppsala))
  rmse_inside_uppsala <- sqrt(mean((inside_aoa_reference_values_uppsala - inside_aoa_predicted_values_uppsala)^2))
  rsq_inside_uppsala <- cor(inside_aoa_reference_values_uppsala, inside_aoa_predicted_values_uppsala)^2
  
  # Print metrics for Inside AoA
  cat("\nInside AoA Metrics for Uppsala:\n")
  cat("MAE:", mae_inside_uppsala, "\n")
  cat("RMSE:", rmse_inside_uppsala, "\n")
  cat("R^2:", rsq_inside_uppsala, "\n")
} else {
  cat("Error: The lengths of reference and predicted vectors for Inside AoA do not match after alignment.\n")
}

# Check the lengths of both vectors before removing NAs
cat("Length of inside_aoa_reference_values_uppsala:", length(inside_aoa_reference_values_uppsala), "\n")
cat("Length of inside_aoa_predicted_values_uppsala:", length(inside_aoa_predicted_values_uppsala), "\n")

# Check for any discrepancy in the two vectors
cat("Inside AoA Reference NA values: ", sum(is.na(inside_aoa_reference_values_uppsala)), "\n")
cat("Inside AoA Predicted NA values: ", sum(is.na(inside_aoa_predicted_values_uppsala)), "\n")

# Ensure both vectors have valid values after masking
valid_inside_uppsala <- !is.na(inside_aoa_reference_values_uppsala) & !is.na(inside_aoa_predicted_values_uppsala)

# Filter vectors to only keep valid values
inside_aoa_reference_values_uppsala <- inside_aoa_reference_values_uppsala[valid_inside_uppsala]
inside_aoa_predicted_values_uppsala <- inside_aoa_predicted_values_uppsala[valid_inside_uppsala]

# Check the lengths again after alignment
cat("After alignment:\n")
cat("Length of inside_aoa_reference_values_uppsala:", length(inside_aoa_reference_values_uppsala), "\n")
cat("Length of inside_aoa_predicted_values_uppsala:", length(inside_aoa_predicted_values_uppsala), "\n")

# Verify the first few values in the reference and predicted arrays to check the content
cat("\nFirst few values of reference values (inside AoA):\n")
print(head(inside_aoa_reference_values_uppsala))

cat("\nFirst few values of predicted values (inside AoA):\n")
print(head(inside_aoa_predicted_values_uppsala))

# Calculate the metrics only if lengths match
if (length(inside_aoa_reference_values_uppsala) == length(inside_aoa_predicted_values_uppsala)) {
  # Calculate MAE, RMSE, and R^2 for Inside AoA
  mae_inside_uppsala <- mean(abs(inside_aoa_reference_values_uppsala - inside_aoa_predicted_values_uppsala))
  rmse_inside_uppsala <- sqrt(mean((inside_aoa_reference_values_uppsala - inside_aoa_predicted_values_uppsala)^2))
  rsq_inside_uppsala <- cor(inside_aoa_reference_values_uppsala, inside_aoa_predicted_values_uppsala)^2
  
  # Print metrics for Inside AoA
  cat("\nInside AoA Metrics for Uppsala:\n")
  cat("MAE:", mae_inside_uppsala, "\n")
  cat("RMSE:", rmse_inside_uppsala, "\n")
  cat("R^2:", rsq_inside_uppsala, "\n")
} else {
  cat("Error: The lengths of reference and predicted vectors for Inside AoA do not match after alignment.\n")
}

# Check the lengths of both vectors before removing NAs
cat("Length of outside_aoa_reference_values_uppsala:", length(outside_aoa_reference_values_uppsala), "\n")
cat("Length of outside_aoa_predicted_values_uppsala:", length(outside_aoa_predicted_values_uppsala), "\n")

# Check for any discrepancy in the two vectors
cat("Outside AoA Reference NA values: ", sum(is.na(outside_aoa_reference_values_uppsala)), "\n")
cat("Outside AoA Predicted NA values: ", sum(is.na(outside_aoa_predicted_values_uppsala)), "\n")

# Ensure both vectors have valid values after masking
valid_outside_uppsala <- !is.na(outside_aoa_reference_values_uppsala) & !is.na(outside_aoa_predicted_values_uppsala)

# Filter vectors to only keep valid values
outside_aoa_reference_values_uppsala <- outside_aoa_reference_values_uppsala[valid_outside_uppsala]
outside_aoa_predicted_values_uppsala <- outside_aoa_predicted_values_uppsala[valid_outside_uppsala]

# Check the lengths again after alignment
cat("After alignment:\n")
cat("Length of outside_aoa_reference_values_uppsala:", length(outside_aoa_reference_values_uppsala), "\n")
cat("Length of outside_aoa_predicted_values_uppsala:", length(outside_aoa_predicted_values_uppsala), "\n")

# Verify the first few values in the reference and predicted arrays to check the content
cat("\nFirst few values of reference values (inside AoA):\n")
print(head(outside_aoa_reference_values_uppsala))

cat("\nFirst few values of predicted values (inside AoA):\n")
print(head(outside_aoa_predicted_values_uppsala))

# Calculate the metrics only if lengths match
if (length(outside_aoa_reference_values_uppsala) == length(outside_aoa_predicted_values_uppsala)) {
  # Calculate MAE, RMSE, and R^2 for Inside AoA
  mae_outside_uppsala <- mean(abs(outside_aoa_reference_values_uppsala - outside_aoa_predicted_values_uppsala))
  rmse_outside_uppsala <- sqrt(mean((outside_aoa_reference_values_uppsala - outside_aoa_predicted_values_uppsala)^2))
  rsq_outside_uppsala <- cor(outside_aoa_reference_values_uppsala, outside_aoa_predicted_values_uppsala)^2
  
  # Print metrics for Inside AoA
  cat("\nOutside AoA Metrics for Uppsala:\n")
  cat("MAE:", mae_outside_uppsala, "\n")
  cat("RMSE:", rmse_outside_uppsala, "\n")
  cat("R^2:", rsq_outside_uppsala, "\n")
} else {
  cat("Error: The lengths of reference and predicted vectors for Outside AoA do not match after alignment.\n")
}





#Florence
# Load necessary packages
library(terra)
library(caret)
library(ggplot2)

# Load the AoA raster (binary raster with 1 = inside AoA, 0 = outside AoA)
aoa_raster_florence <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")

# Load the reference (ground truth or classified) raster for comparison
reference_raster_florence <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH FLORENCE.png")

# Load predicted raster (this can be your model's predictions)
predicted_raster_florence <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")

# Step 1: Crop reference and predicted rasters to match the extent of AoA raster
reference_raster_cropped_florence <- crop(reference_raster_florence, ext(aoa_raster_florence))
predicted_raster_cropped_florence <- crop(predicted_raster_florence, ext(aoa_raster_florence))

# Step 2: Mask reference and predicted values for inside AoA (1 = inside AoA)
inside_aoa_reference_florence <- mask(reference_raster_cropped_florence, aoa_raster_florence, maskvalues = 0)
inside_aoa_predicted_florence <- mask(predicted_raster_cropped_florence, aoa_raster_florence, maskvalues = 0)

# Step 3: Mask reference and predicted values for outside AoA (0 = outside AoA)
outside_aoa_reference_florence <- mask(reference_raster_cropped_florence, aoa_raster_florence, maskvalues = 1)
outside_aoa_predicted_florence <- mask(predicted_raster_cropped_florence, aoa_raster_florence, maskvalues = 1)

# Step 4: Convert rasters to vectors for comparison (removing NA values)
inside_aoa_reference_values_florence <- as.vector(inside_aoa_reference_florence)
inside_aoa_predicted_values_florence <- as.vector(inside_aoa_predicted_florence)
outside_aoa_reference_values_florence <- as.vector(outside_aoa_reference_florence)
outside_aoa_predicted_values_florence <- as.vector(outside_aoa_predicted_florence)

# Ensure the reference and predicted rasters are properly masked and aligned
outside_aoa_reference_values_florence <- as.vector(outside_aoa_reference_florence)
outside_aoa_predicted_values_florence <- as.vector(outside_aoa_predicted_florence)

# Step: Check the lengths of both vectors
cat("Length of outside_aoa_reference_values_florence:", length(outside_aoa_reference_values_florence), "\n")
cat("Length of outside_aoa_predicted_values_florence:", length(outside_aoa_predicted_values_florence), "\n")

# If lengths do not match, we will align them
if (length(outside_aoa_reference_values_florence) != length(outside_aoa_predicted_values_florence)) {
  # Align the two vectors by removing elements that are NA from either vector
  valid_outside_florence <- !is.na(outside_aoa_reference_values_florence) & !is.na(outside_aoa_predicted_values_florence)
  outside_aoa_reference_values_florence <- outside_aoa_reference_values_florence[valid_outside_florence]
  outside_aoa_predicted_values_florence <- outside_aoa_predicted_values_florence[valid_outside_florence]
}

# Now both vectors should have the same length, and we can calculate the metrics
mae_outside_florence <- mean(abs(outside_aoa_predicted_values_florence - outside_aoa_reference_values_florence))
rmse_outside_florence <- sqrt(mean((outside_aoa_predicted_values_florence - outside_aoa_reference_values_florence)^2))
rsq_outside_florence <- cor(outside_aoa_predicted_values_florence, outside_aoa_reference_values_florence)^2

# Check the lengths of both vectors before removing NAs
cat("Length of outside_aoa_reference_values_florence:", length(outside_aoa_reference_values_florence), "\n")
cat("Length of outside_aoa_predicted_values_florence:", length(outside_aoa_predicted_values_florence), "\n")

# Check for any discrepancy in the two vectors
if (length(outside_aoa_reference_values_florence) != length(outside_aoa_predicted_values_florence)) {
  # Investigate why they are of different lengths by printing unique values
  cat("Outside AoA Reference NA values: ", sum(is.na(outside_aoa_reference_values_florence)), "\n")
  cat("Outside AoA Predicted NA values: ", sum(is.na(outside_aoa_predicted_values_florence)), "\n")
  
  # Ensure both vectors have valid values after masking
  valid_outside_florence <- !is.na(outside_aoa_reference_values_florence) & !is.na(outside_aoa_predicted_values_florence)
  
  # Filter vectors to only keep valid values
  outside_aoa_reference_values_florence <- outside_aoa_reference_values_florence[valid_outside_florence]
  outside_aoa_predicted_values_florence <- outside_aoa_predicted_values_florence[valid_outside_florence]
  
  # Check if lengths match after the alignment
  cat("After alignment:\n")
  cat("Length of outside_aoa_reference_values_florence:", length(outside_aoa_reference_values_florence), "\n")
  cat("Length of outside_aoa_predicted_values_florence:", length(outside_aoa_predicted_values_florence), "\n")
}

# Check the lengths of both vectors before removing NAs
cat("Length of inside_aoa_reference_values_florence:", length(inside_aoa_reference_values_florence), "\n")
cat("Length of inside_aoa_predicted_values_florence:", length(inside_aoa_predicted_values_florence), "\n")

# Check for any discrepancy in the two vectors
cat("Inside AoA Reference NA values: ", sum(is.na(inside_aoa_reference_values_florence)), "\n")
cat("Inside AoA Predicted NA values: ", sum(is.na(inside_aoa_predicted_values_florence)), "\n")

# Ensure both vectors have valid values after masking
valid_inside_florence <- !is.na(inside_aoa_reference_values_florence) & !is.na(inside_aoa_predicted_values_florence)

# Filter vectors to only keep valid values
inside_aoa_reference_values_florence <- inside_aoa_reference_values_florence[valid_inside_florence]
inside_aoa_predicted_values_florence <- inside_aoa_predicted_values_florence[valid_inside_florence]

# Check the lengths again after alignment
cat("After alignment:\n")
cat("Length of inside_aoa_reference_values_florence:", length(inside_aoa_reference_values_florence), "\n")
cat("Length of inside_aoa_predicted_values_florence:", length(inside_aoa_predicted_values_florence), "\n")

# Verify the first few values in the reference and predicted arrays to check the content
cat("\nFirst few values of reference values (inside AoA):\n")
print(head(inside_aoa_reference_values_florence))

cat("\nFirst few values of predicted values (inside AoA):\n")
print(head(inside_aoa_predicted_values_florence))

# Calculate the metrics only if lengths match
if (length(inside_aoa_reference_values_florence) == length(inside_aoa_predicted_values_florence)) {
  # Calculate MAE, RMSE, and R^2 for Inside AoA
  mae_inside_florence <- mean(abs(inside_aoa_reference_values_florence - inside_aoa_predicted_values_florence))
  rmse_inside_florence <- sqrt(mean((inside_aoa_reference_values_florence - inside_aoa_predicted_values_florence)^2))
  rsq_inside_florence <- cor(inside_aoa_reference_values_florence, inside_aoa_predicted_values_florence)^2
  
  # Print metrics for Inside AoA
  cat("\nInside AoA Metrics for Florence:\n")
  cat("MAE:", mae_inside_florence, "\n")
  cat("RMSE:", rmse_inside_florence, "\n")
  cat("R^2:", rsq_inside_florence, "\n")
} else {
  cat("Error: The lengths of reference and predicted vectors for Inside AoA do not match after alignment.\n")
}

# Check the lengths of both vectors before removing NAs
cat("Length of inside_aoa_reference_values_florence:", length(inside_aoa_reference_values_florence), "\n")
cat("Length of inside_aoa_predicted_values_florence:", length(inside_aoa_predicted_values_florence), "\n")

# Check for any discrepancy in the two vectors
cat("Inside AoA Reference NA values: ", sum(is.na(inside_aoa_reference_values_florence)), "\n")
cat("Inside AoA Predicted NA values: ", sum(is.na(inside_aoa_predicted_values_florence)), "\n")

# Ensure both vectors have valid values after masking
valid_inside_florence <- !is.na(inside_aoa_reference_values_florence) & !is.na(inside_aoa_predicted_values_florence)

# Filter vectors to only keep valid values
inside_aoa_reference_values_florence <- inside_aoa_reference_values_florence[valid_inside_florence]
inside_aoa_predicted_values_florence <- inside_aoa_predicted_values_florence[valid_inside_florence]

# Check the lengths again after alignment
cat("After alignment:\n")
cat("Length of inside_aoa_reference_values_florence:", length(inside_aoa_reference_values_florence), "\n")
cat("Length of inside_aoa_predicted_values_florence:", length(inside_aoa_predicted_values_florence), "\n")

#Florence
# Load necessary packages
# Step 6: Calculate metrics for inside AoA
if (length(inside_aoa_reference_values_florence) > 0 && length(inside_aoa_predicted_values_florence) > 0) {
  mae_inside_florence <- mean(abs(inside_aoa_reference_values_florence - inside_aoa_predicted_values_florence))
  rmse_inside_florence <- sqrt(mean((inside_aoa_reference_values_florence - inside_aoa_predicted_values_florence)^2))
  rsq_inside_florence <- cor(inside_aoa_reference_values_florence, inside_aoa_predicted_values_florence)^2
  
  # Print metrics for Inside AoA
  cat("\nInside AoA Metrics for Florence:\n")
  cat("MAE:", mae_inside_florence, "\n")
  cat("RMSE:", rmse_inside_florence, "\n")
  cat("R^2:", rsq_inside_florence, "\n")
} else {
  cat("\nNo valid values for Inside AoA.\n")
}

# Step 6: Calculate metrics for inside AoA
if (length(inside_aoa_reference_values_florence) > 0 && length(inside_aoa_predicted_values_florence) > 0) {
  mae_inside_florence <- mean(abs(inside_aoa_reference_values_florence - inside_aoa_predicted_values_florence))
  rmse_inside_florence <- sqrt(mean((inside_aoa_reference_values_florence - inside_aoa_predicted_values_florence)^2))
  rsq_inside_florence <- cor(inside_aoa_reference_values_florence, inside_aoa_predicted_values_florence)^2
  
  # Print metrics for Inside AoA
  cat("\nInside AoA Metrics for Florence:\n")
  cat("MAE:", mae_inside_florence, "\n")
  cat("RMSE:", rmse_inside_florence, "\n")
  cat("R^2:", rsq_inside_florence, "\n")
} else {
  cat("\nNo valid values for Inside AoA.\n")
}

# Ensure valid values for outside AoA by removing NAs
valid_outside_florence <- !is.na(outside_aoa_reference_values_florence) & !is.na(outside_aoa_predicted_values_florence)

# Filter out the NAs from both vectors
outside_aoa_reference_values_florence <- outside_aoa_reference_values_florence[valid_outside_florence]
outside_aoa_predicted_values_florence <- outside_aoa_predicted_values_florence[valid_outside_florence]

# Check lengths after alignment
cat("Length of outside_aoa_reference_values_florence:", length(outside_aoa_reference_values_florence), "\n")
cat("Length of outside_aoa_predicted_values_florence:", length(outside_aoa_predicted_values_florence), "\n")


# Step 2: Calculate metrics for outside AoA
if (length(outside_aoa_reference_values_florence) > 0 && length(outside_aoa_predicted_values_florence) > 0) {
  mae_outside_florence <- mean(abs(outside_aoa_reference_values_florence - outside_aoa_predicted_values_florence))
  rmse_outside_florence <- sqrt(mean((outside_aoa_reference_values_florence - outside_aoa_predicted_values_florence)^2))
  rsq_outside_florence <- cor(outside_aoa_reference_values_florence, outside_aoa_predicted_values_florence)^2
  
  # Print metrics for Outside AoA
  cat("\nOutside AoA Metrics for Florence:\n")
  cat("MAE:", mae_outside_florence, "\n")
  cat("RMSE:", rmse_outside_florence, "\n")
  cat("R^2:", rsq_outside_florence, "\n")
} else {
  cat("\nNo valid values for Outside AoA.\n")
}

























# Visualize Residuals - Spatially

# Residuals inside AoA
residuals_inside <- inside_aoa_predicted - inside_aoa_reference
residuals_raster_inside <- rast(residuals_inside)

# Residuals outside AoA
residuals_outside <- outside_aoa_predicted - outside_aoa_reference
residuals_raster_outside <- rast(residuals_outside)

# Plot residuals for inside AoA
plot(residuals_raster_inside, main = "Residuals Inside AoA", col = terrain.colors(100))

# Plot residuals for outside AoA
plot(residuals_raster_outside, main = "Residuals Outside AoA", col = terrain.colors(100))
# Remove NA values from inside_aoa_reference_values and residuals_inside
valid_inside_residuals <- !is.na(inside_aoa_reference_values) & !is.na(residuals_inside)

# Keep only the valid (non-NA) values
# Convert raster layers to vectors and remove NAs
inside_aoa_reference_values <- as.vector(inside_aoa_reference)
residuals_inside <- as.vector(residuals_inside)

# Remove NA values from both vectors
valid_inside_residuals <- !is.na(inside_aoa_reference_values) & !is.na(residuals_inside)

# Keep only valid (non-NA) values
inside_aoa_reference_values_clean <- inside_aoa_reference_values[valid_inside_residuals]
residuals_inside_clean <- residuals_inside[valid_inside_residuals]

# Create the data frame with the cleaned data
inside_residual_data <- data.frame(actual = inside_aoa_reference_values_clean, residual = residuals_inside_clean)

# Now you can proceed with plotting
ggplot(inside_residual_data, aes(x = actual, y = residual)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot: Inside AoA", x = "Actual Values", y = "Residuals") +
  theme_minimal()

# Convert raster layers to vectors and remove NAs for outside AoA
outside_aoa_reference_values <- as.vector(outside_aoa_reference)  # Assuming you have this data
residuals_outside <- as.vector(residuals_outside)  # Assuming residuals for outside AoA have been calculated

# Remove NA values from both vectors
valid_outside_residuals <- !is.na(outside_aoa_reference_values) & !is.na(residuals_outside)

# Keep only valid (non-NA) values
outside_aoa_reference_values_clean <- outside_aoa_reference_values[valid_outside_residuals]
residuals_outside_clean <- residuals_outside[valid_outside_residuals]

# Create the data frame with the cleaned data for outside AoA
outside_residual_data <- data.frame(actual = outside_aoa_reference_values_clean, residual = residuals_outside_clean)
windows()
# Now you can proceed with plotting
ggplot(outside_residual_data, aes(x = actual, y = residual)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot: Outside AoA", x = "Actual Values", y = "Residuals") +
  theme_minimal()


...# Scatter Plot for Predicted vs Actual (Inside AoA)
inside_comparison_data <- data.frame(actual = inside_aoa_reference_values, predicted = inside_aoa_predicted_values)
ggplot(inside_comparison_data, aes(x = actual, y = predicted)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual: Inside AoA", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Scatter Plot for Predicted vs Actual (Outside AoA)
outside_comparison_data <- data.frame(actual = outside_aoa_reference_values, predicted = outside_aoa_predicted_values)
ggplot(outside_comparison_data, aes(x = actual, y = predicted)) +
  geom_point(color = "red", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Predicted vs Actual: Outside AoA", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Line Plot for Predicted vs Actual (Inside AoA)
ggplot(inside_comparison_data, aes(x = actual, y = predicted)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual Line Plot: Inside AoA", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Line Plot for Predicted vs Actual (Outside AoA)
ggplot(outside_comparison_data, aes(x = actual, y = predicted)) +
  geom_line(color = "red", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Predicted vs Actual Line Plot: Outside AoA", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Histogram of Residuals (Inside AoA)
ggplot(inside_residual_data, aes(x = residual)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals: Inside AoA", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Histogram of Residuals (Outside AoA)
ggplot(outside_residual_data, aes(x = residual)) +
  geom_histogram(bins = 30, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals: Outside AoA", x = "Residuals", y = "Frequency") +
  theme_minimal()


# Load necessary packages
library(terra)
library(caret)
library(ggplot2)

# Load the AoA raster (binary raster with 1 = inside AoA, 0 = outside AoA)
aoa_raster_uppsala <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Uppsala aoa.tiff")

# Load the reference (ground truth or classified) raster for comparison
reference_raster_uppsala <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH UPPSALA.png")

# Load predicted raster (this can be your model's predictions)
predicted_raster_uppsala <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Uppsala prediction.tiff")

# Step 1: Crop reference and predicted rasters to match the extent of AoA raster
reference_raster_cropped_uppsala <- crop(reference_raster_uppsala, ext(aoa_raster_uppsala))
predicted_raster_cropped_uppsala <- crop(predicted_raster_uppsala, ext(aoa_raster_uppsala))

# Step 2: Mask reference and predicted values for inside AoA (1 = inside AoA)
inside_aoa_reference_uppsala <- mask(reference_raster_cropped_uppsala, aoa_raster_uppsala, maskvalues = 0)
inside_aoa_predicted_uppsala <- mask(predicted_raster_cropped_uppsala, aoa_raster_uppsala, maskvalues = 0)

# Step 3: Mask reference and predicted values for outside AoA (0 = outside AoA)
outside_aoa_reference_uppsala <- mask(reference_raster_cropped_uppsala, aoa_raster_uppsala, maskvalues = 1)
outside_aoa_predicted_uppsala <- mask(predicted_raster_cropped_uppsala, aoa_raster_uppsala, maskvalues = 1)

# Step 4: Convert rasters to vectors for comparison (removing NA values)
inside_aoa_reference_values_uppsala <- as.vector(inside_aoa_reference_uppsala)
inside_aoa_predicted_values_uppsala <- as.vector(inside_aoa_predicted_uppsala)
outside_aoa_reference_values_uppsala <- as.vector(outside_aoa_reference_uppsala)
outside_aoa_predicted_values_uppsala <- as.vector(outside_aoa_predicted_uppsala)



# Ensure the reference and predicted rasters are properly masked and aligned
outside_aoa_reference_values_uppsala <- as.vector(outside_aoa_reference_uppsala)
outside_aoa_predicted_values_uppsala <- as.vector(outside_aoa_predicted_uppsala)

# Step: Check the lengths of both vectors
cat("Length of outside_aoa_reference_values_uppsala:", length(outside_aoa_reference_values_uppsala), "\n")
cat("Length of outside_aoa_predicted_values_uppsala:", length(outside_aoa_predicted_values_uppsala), "\n")

# If lengths do not match, we will align them
if (length(outside_aoa_reference_values_uppsala) != length(outside_aoa_predicted_values_uppsala)) {
  # Align the two vectors by removing elements that are NA from either vector
  valid_outside_uppsala <- !is.na(outside_aoa_reference_values_uppsala) & !is.na(outside_aoa_predicted_values_uppsala)
  outside_aoa_reference_values_uppsala <- outside_aoa_reference_values_uppsala[valid_outside_uppsala]
  outside_aoa_predicted_values_uppsala <- outside_aoa_predicted_values_uppsala[valid_outside_uppsala]
}

# Now both vectors should have the same length, and we can calculate the metrics
mae_outside_uppsala <- mean(abs(outside_aoa_predicted_values_uppsala - outside_aoa_reference_values_uppsala))
rmse_outside_uppsala <- sqrt(mean((outside_aoa_predicted_values_uppsala - outside_aoa_reference_values_uppsala)^2))
rsq_outside_uppsala <- cor(outside_aoa_predicted_values_uppsala, outside_aoa_reference_values_uppsala)^2



# Check the lengths of both vectors before removing NAs
cat("Length of outside_aoa_reference_values_uppsala:", length(outside_aoa_reference_values_uppsala), "\n")
cat("Length of outside_aoa_predicted_values_uppsala:", length(outside_aoa_predicted_values_uppsala), "\n")

# Check for any discrepancy in the two vectors
if (length(outside_aoa_reference_values_uppsala) != length(outside_aoa_predicted_values_uppsala)) {
  # Investigate why they are of different lengths by printing unique values
  cat("Outside AoA Reference NA values: ", sum(is.na(outside_aoa_reference_values_uppsala)), "\n")
  cat("Outside AoA Predicted NA values: ", sum(is.na(outside_aoa_predicted_values_uppsala)), "\n")
  
  # Ensure both vectors have valid values after masking
  valid_outside_uppsala <- !is.na(outside_aoa_reference_values_uppsala) & !is.na(outside_aoa_predicted_values_uppsala)
  
  # Filter vectors to only keep valid values
  outside_aoa_reference_values_uppsala <- outside_aoa_reference_values_uppsala[valid_outside_uppsala]
  outside_aoa_predicted_values_uppsala <- outside_aoa_predicted_values_uppsala[valid_outside_uppsala]
  
  # Check if lengths match after the alignment
  cat("After alignment:\n")
  cat("Length of outside_aoa_reference_values_uppsala:", length(outside_aoa_reference_values_uppsala), "\n")
  cat("Length of outside_aoa_predicted_values_uppsala:", length(outside_aoa_predicted_values_uppsala), "\n")
}


# Check the lengths of both vectors before removing NAs
cat("Length of inside_aoa_reference_values_uppsala:", length(inside_aoa_reference_values_uppsala), "\n")
cat("Length of inside_aoa_predicted_values_uppsala:", length(inside_aoa_predicted_values_uppsala), "\n")

# Check for any discrepancy in the two vectors
cat("Inside AoA Reference NA values: ", sum(is.na(inside_aoa_reference_values_uppsala)), "\n")
cat("Inside AoA Predicted NA values: ", sum(is.na(inside_aoa_predicted_values_uppsala)), "\n")

# Ensure both vectors have valid values after masking
valid_inside_uppsala <- !is.na(inside_aoa_reference_values_uppsala) & !is.na(inside_aoa_predicted_values_uppsala)

# Filter vectors to only keep valid values
inside_aoa_reference_values_uppsala <- inside_aoa_reference_values_uppsala[valid_inside_uppsala]
inside_aoa_predicted_values_uppsala <- inside_aoa_predicted_values_uppsala[valid_inside_uppsala]

# Check the lengths again after alignment
cat("After alignment:\n")
cat("Length of inside_aoa_reference_values_uppsala:", length(inside_aoa_reference_values_uppsala), "\n")
cat("Length of inside_aoa_predicted_values_uppsala:", length(inside_aoa_predicted_values_uppsala), "\n")

# Verify the first few values in the reference and predicted arrays to check the content
cat("\nFirst few values of reference values (inside AoA):\n")
print(head(inside_aoa_reference_values_uppsala))

cat("\nFirst few values of predicted values (inside AoA):\n")
print(head(inside_aoa_predicted_values_uppsala))

# Calculate the metrics only if lengths match
if (length(inside_aoa_reference_values_uppsala) == length(inside_aoa_predicted_values_uppsala)) {
  # Calculate MAE, RMSE, and R^2 for Inside AoA
  mae_inside_uppsala <- mean(abs(inside_aoa_reference_values_uppsala - inside_aoa_predicted_values_uppsala))
  rmse_inside_uppsala <- sqrt(mean((inside_aoa_reference_values_uppsala - inside_aoa_predicted_values_uppsala)^2))
  rsq_inside_uppsala <- cor(inside_aoa_reference_values_uppsala, inside_aoa_predicted_values_uppsala)^2
  
  # Print metrics for Inside AoA
  cat("\nInside AoA Metrics for Uppsala:\n")
  cat("MAE:", mae_inside_uppsala, "\n")
  cat("RMSE:", rmse_inside_uppsala, "\n")
  cat("R^2:", rsq_inside_uppsala, "\n")
} else {
  cat("Error: The lengths of reference and predicted vectors for Inside AoA do not match after alignment.\n")
}

# Check the lengths of both vectors before removing NAs
cat("Length of inside_aoa_reference_values_uppsala:", length(inside_aoa_reference_values_uppsala), "\n")
cat("Length of inside_aoa_predicted_values_uppsala:", length(inside_aoa_predicted_values_uppsala), "\n")

# Check for any discrepancy in the two vectors
cat("Inside AoA Reference NA values: ", sum(is.na(inside_aoa_reference_values_uppsala)), "\n")
cat("Inside AoA Predicted NA values: ", sum(is.na(inside_aoa_predicted_values_uppsala)), "\n")

# Ensure both vectors have valid values after masking
valid_inside_uppsala <- !is.na(inside_aoa_reference_values_uppsala) & !is.na(inside_aoa_predicted_values_uppsala)

# Filter vectors to only keep valid values
inside_aoa_reference_values_uppsala <- inside_aoa_reference_values_uppsala[valid_inside_uppsala]
inside_aoa_predicted_values_uppsala <- inside_aoa_predicted_values_uppsala[valid_inside_uppsala]

# Check the lengths again after alignment
cat("After alignment:\n")
cat("Length of inside_aoa_reference_values_uppsala:", length(inside_aoa_reference_values_uppsala), "\n")
cat("Length of inside_aoa_predicted_values_uppsala:", length(inside_aoa_predicted_values_uppsala), "\n")

# Verify the first few values in the reference and predicted arrays to check the content
cat("\nFirst few values of reference values (inside AoA):\n")
print(head(inside_aoa_reference_values_uppsala))

cat("\nFirst few values of predicted values (inside AoA):\n")
print(head(inside_aoa_predicted_values_uppsala))

# Calculate the metrics only if lengths match
if (length(inside_aoa_reference_values_uppsala) == length(inside_aoa_predicted_values_uppsala)) {
  # Calculate MAE, RMSE, and R^2 for Inside AoA
  mae_inside_uppsala <- mean(abs(inside_aoa_reference_values_uppsala - inside_aoa_predicted_values_uppsala))
  rmse_inside_uppsala <- sqrt(mean((inside_aoa_reference_values_uppsala - inside_aoa_predicted_values_uppsala)^2))
  rsq_inside_uppsala <- cor(inside_aoa_reference_values_uppsala, inside_aoa_predicted_values_uppsala)^2
  
  # Print metrics for Inside AoA
  cat("\nInside AoA Metrics for Uppsala:\n")
  cat("MAE:", mae_inside_uppsala, "\n")
  cat("RMSE:", rmse_inside_uppsala, "\n")
  cat("R^2:", rsq_inside_uppsala, "\n")
} else {
  cat("Error: The lengths of reference and predicted vectors for Inside AoA do not match after alignment.\n")
}

# Check the lengths of both vectors before removing NAs
cat("Length of inside_aoa_reference_values_uppsala:", length(inside_aoa_reference_values_uppsala), "\n")
cat("Length of inside_aoa_predicted_values_uppsala:", length(inside_aoa_predicted_values_uppsala), "\n")

# Check for any discrepancy in the two vectors
cat("Inside AoA Reference NA values: ", sum(is.na(inside_aoa_reference_values_uppsala)), "\n")
cat("Inside AoA Predicted NA values: ", sum(is.na(inside_aoa_predicted_values_uppsala)), "\n")

# Ensure both vectors have valid values after masking
valid_inside_uppsala <- !is.na(inside_aoa_reference_values_uppsala) & !is.na(inside_aoa_predicted_values_uppsala)

# Filter vectors to only keep valid values
inside_aoa_reference_values_uppsala <- inside_aoa_reference_values_uppsala[valid_inside_uppsala]
inside_aoa_predicted_values_uppsala <- inside_aoa_predicted_values_uppsala[valid_inside_uppsala]

# Check the lengths again after alignment
cat("After alignment:\n")
cat("Length of inside_aoa_reference_values_uppsala:", length(inside_aoa_reference_values_uppsala), "\n")
cat("Length of inside_aoa_predicted_values_uppsala:", length(inside_aoa_predicted_values_uppsala), "\n")

# Verify the first few values in the reference and predicted arrays to check the content
cat("\nFirst few values of reference values (inside AoA):\n")
print(head(inside_aoa_reference_values_uppsala))

cat("\nFirst few values of predicted values (inside AoA):\n")
print(head(inside_aoa_predicted_values_uppsala))

# Calculate the metrics only if lengths match
if (length(inside_aoa_reference_values_uppsala) == length(inside_aoa_predicted_values_uppsala)) {
  # Calculate MAE, RMSE, and R^2 for Inside AoA
  mae_inside_uppsala <- mean(abs(inside_aoa_reference_values_uppsala - inside_aoa_predicted_values_uppsala))
  rmse_inside_uppsala <- sqrt(mean((inside_aoa_reference_values_uppsala - inside_aoa_predicted_values_uppsala)^2))
  rsq_inside_uppsala <- cor(inside_aoa_reference_values_uppsala, inside_aoa_predicted_values_uppsala)^2
  
  # Print metrics for Inside AoA
  cat("\nInside AoA Metrics for Uppsala:\n")
  cat("MAE:", mae_inside_uppsala, "\n")
  cat("RMSE:", rmse_inside_uppsala, "\n")
  cat("R^2:", rsq_inside_uppsala, "\n")
} else {
  cat("Error: The lengths of reference and predicted vectors for Inside AoA do not match after alignment.\n")
}

#for florence
# Load necessary packages
library(terra)
library(caret)
library(ggplot2)

# Load the AoA raster (binary raster with 1 = inside AoA, 0 = outside AoA)
aoa_raster<- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")

# Load the reference (ground truth or classified) raster for comparison
reference_raster <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH FLORENCE.png")

# Load predicted raster (this can be your model's predictions)
predicted_raster <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")

# Step 1: Crop reference and predicted rasters to match the extent of AoA raster
reference_raster_cropped <- crop(reference_raster, ext(aoa_raster))
predicted_raster_cropped <- crop(predicted_raster, ext(aoa_raster))

# Step 2: Mask reference and predicted values for inside AoA (1 = inside AoA)
inside_aoa_reference <- mask(reference_raster_cropped, aoa_raster, maskvalues = 0)
inside_aoa_predicted <- mask(predicted_raster_cropped, aoa_raster, maskvalues = 0)

# Step 3: Mask reference and predicted values for outside AoA (0 = outside AoA)
outside_aoa_reference <- mask(reference_raster_cropped, aoa_raster, maskvalues = 1)
outside_aoa_predicted <- mask(predicted_raster_cropped, aoa_raster, maskvalues = 1)

# Step 4: Convert rasters to vectors for comparison (removing NA values)
inside_aoa_reference_values<- as.vector(inside_aoa_reference)
inside_aoa_predicted_values <- as.vector(inside_aoa_predicted)
outside_aoa_reference_values <- as.vector(outside_aoa_reference)
outside_aoa_predicted_values <- as.vector(outside_aoa_predicted)



# Ensure the reference and predicted rasters are properly masked and aligned
outside_aoa_reference_values <- as.vector(outside_aoa_reference)
outside_aoa_predicted_values <- as.vector(outside_aoa_predicted)

# Step: Check the lengths of both vectors
cat("Length of outside_aoa_reference_values:", length(outside_aoa_reference_values), "\n")
cat("Length of outside_aoa_predicted_values:", length(outside_aoa_predicted_values), "\n")

# If lengths do not match, we will align them
if (length(outside_aoa_reference_values) != length(outside_aoa_predicted_values)) {
  # Align the two vectors by removing elements that are NA from either vector
  valid_outside <- !is.na(outside_aoa_reference_values) & !is.na(outside_aoa_predicted_values)
  outside_aoa_reference_values <- outside_aoa_reference_values[valid_outside]
  outside_aoa_predicted_values <- outside_aoa_predicted_values[valid_outside]
}


# Calculate errors for areas outside the AOA
error_outside <- abs(outside_aoa_predicted_values - outside_aoa_reference_values)
error_inside <- abs(inside_aoa_predicted_values- inside_aoa_reference_values)

# 6. Hypothesis Testing (Wilcoxon Test for Error Differences)
# Combine error data into a data frame
error_df <- data.frame(
  Area = rep(c("Inside AOA", "Outside AOA"), each = length(error_inside)),
  Error = c(error_inside, error_outside)
)

# Run a Wilcoxon test for differences in error distributions
wilcox_test <- wilcox.test(Error ~ Area, data = error_df)
cat("Wilcoxon Test Result:\n")
print(wilcox_test)

# 7. Visualize Results

# Load necessary libraries
library(ggplot2)
library(terra)
library(dplyr)
# Check the total number of elements
length(error_inside)  # Should return 1155248

# Find possible divisors for the number of rows
divisors <- which(1155248 %% 1:1155248 == 0)
divisors
nrows <- 1075  # Example value
ncols <- length(error_outside) / nrows  # Calculate columns based on length

# Ensure ncols is an integer
ncols <- round(ncols)

# Reshape the vector
error_matrix <- matrix(error_inside, nrow = nrows, ncol = ncols, byrow = TRUE)

# Check the matrix dimensions
dim(error_matrix)


# Assuming error_inside and error_outside are raster data or matrices
# Let's assume they are matrices or spatial rasters that you want to plot
# Load necessary package
library(terra)
# Calculate errors for areas outside the AOA

error_inside <- abs(inside_aoa_predicted_values- inside_aoa_reference_values)
# Let's assume error_inside is a vector of values.
# First, we check the length of the error vector
length(error_inside)  # Check the length of the vector

# Reshape the error vector into a matrix to match your grid (modify dimensions as necessary)
# Example: If you have a 4x4 grid, reshape it like this:
error_matrix_inside <- matrix(error_inside, nrow = 4, ncol = 4)

# Convert the matrix to a SpatRaster
raster_inside <- rast(error_matrix_inside)

# Plotting the raster
library(ggplot2)
error_df_inside <- as.data.frame(raster_inside, xy = TRUE)
colnames(error_df_inside) <- c("x", "y", "Error")

ggplot(error_df_inside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Inside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),
    axis.ticks = element_line(size = 0.5)
  )
# Convert to raster (if it's not already)

# Extract raster values and coordinates (xy)
error_df_inside <- as.data.frame(raster_inside, xy = TRUE)

# Check the structure of the new data frame
str(error_df_inside)
# Check the structure of error_inside
str(error_inside)
nrows <- sqrt(length(error_inside))  # Try to make it a square raster
ncols <- nrows
# Set the number of rows and columns
nrows <- 1075  # Example, adjust based on your data
ncols <- length(error_inside) / nrows  # Calculate number of columns
# Reshape the vector into a matrix
error_matrix <- matrix(error_inside, nrow = nrows, ncol = ncols, byrow = TRUE)
# Reshape the vector into a matrix
# Calculate the number of rows and columns based on the square root
sqrt_length <- sqrt(length(error_inside))  # Take square root of the total number of elements
nrows <- floor(sqrt_length)
ncols <- ceiling(sqrt_length)

# for reshaping
error_matrix <- matrix(error_inside, nrow = nrows, ncol = ncols, byrow = TRUE)
library(terra)

# Convert the matrix to a raster
raster_inside <- rast(error_matrix)

# Check the structure of the raster
print(raster_inside)

# Extract raster values and coordinates (xy)
error_df_inside <- as.data.frame(raster_inside, xy = TRUE)

# Rename the columns to make them more understandable
colnames(error_df_inside) <- c("x", "y", "Error")

# Check the structure of the resulting data frame
str(error_df_inside)
# Load necessary library for plotting


# Check the total number of elements
length(error_inside)  # Should return 1155248

# Find possible divisors for the number of rows
divisors <- which(1155248 %% 1:1155248 == 0)
divisors


error_df_inside <- as.data.frame(raster_inside, xy = TRUE)

# Rename the columns to make them clearer
colnames(error_df_inside) <- c("x", "y", "Error")

# Verify the structure of the data frame
str(error_df_inside)
# Load ggplot2 for plotting
library(ggplot2)

# Create a heatmap for "Inside AOA"
ggplot(error_df_inside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Inside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  )



# Check the total number of elements
length(error_outside)  # Should return 1155248

# Find possible divisors for the number of rows
divisors <- which(1155248 %% 1:1155248 == 0)
divisors


error_df_outside <- as.data.frame(raster_outside, xy = TRUE)

# Rename the columns to make them clearer
colnames(error_df_inside) <- c("x", "y", "Error")

# Verify the structure of the data frame
str(error_df_inside)
# Load ggplot2 for plotting
library(ggplot2)

# Create a heatmap for "Inside AOA"
ggplot(error_df_inside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Inside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  )


# Load necessary libraries
library(ggplot2)
library(terra)
library(dplyr)
# Check the total number of elements
length(error_inside)  # Should return 1155248

# Find possible divisors for the number of rows
divisors <- which(1155248 %% 1:1155248 == 0)
divisors
nrows <- 1075  # Example value
ncols <- length(error_outside) / nrows  # Calculate columns based on length

# Ensure ncols is an integer
ncols <- round(ncols)

# Reshape the vector
error_matrix <- matrix(error_inside, nrow = nrows, ncol = ncols, byrow = TRUE)

# Check the matrix dimensions
dim(error_matrix)


# Assuming error_inside and error_outside are raster data or matrices
# Let's assume they are matrices or spatial rasters that you want to plot
# Load necessary package
library(terra)
# Calculate errors for areas outside the AOA

error_inside <- abs(inside_aoa_predicted_values- inside_aoa_reference_values)
# Assuming error_inside is a raster
# Assuming your error vectors have equal length
# Check the length of the two vectors
length(inside_aoa_predicted_values)
length(inside_aoa_reference_values)

# If lengths are unequal, you may need to adjust them (e.g., trimming or interpolation). Here I assume they are equal for now.

# Calculate errors
error_inside <- abs(inside_aoa_predicted_values - inside_aoa_reference_values)

# Example: Reshape error_inside to a matrix (make sure the dimensions match your data)
# Assuming 2x2 grid for this example (adjust dimensions as needed)
error_matrix_inside <- matrix(error_inside, nrow = 2, ncol = 2)

# Create raster from the error matrix
raster_inside <- rast(error_matrix_inside)

# Plotting the raster to visualize the error heatmap
library(ggplot2)
error_df_inside <- as.data.frame(raster_inside, xy = TRUE)
colnames(error_df_inside) <- c("x", "y", "Error")

ggplot(error_df_inside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Inside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),
    axis.ticks = element_line(size = 0.5)
  )
# Convert to raster (if it's not already)

# Extract raster values and coordinates (xy)
error_df_inside <- as.data.frame(raster_inside, xy = TRUE)

# Check the structure of the new data frame
str(error_df_inside)
# Check the structure of error_inside
str(error_inside)
nrows <- sqrt(length(error_inside))  # Try to make it a square raster
ncols <- nrows
# Set the number of rows and columns
nrows <- 1075  # Example, adjust based on your data
ncols <- length(error_inside) / nrows  # Calculate number of columns
# Reshape the vector into a matrix
error_matrix <- matrix(error_inside, nrow = nrows, ncol = ncols, byrow = TRUE)
# Reshape the vector into a matrix
# Calculate the number of rows and columns based on the square root
sqrt_length <- sqrt(length(error_inside))  # Take square root of the total number of elements
nrows <- floor(sqrt_length)
ncols <- ceiling(sqrt_length)

# If it works for reshaping
error_matrix <- matrix(error_inside, nrow = nrows, ncol = ncols, byrow = TRUE)
library(terra)

# Convert the matrix to a raster
raster_inside <- rast(error_matrix)

# Check the structure of the raster
print(raster_inside)

# Extract raster values and coordinates (xy)
error_df_inside <- as.data.frame(raster_inside, xy = TRUE)

# Rename the columns to make them more understandable
colnames(error_df_inside) <- c("x", "y", "Error")

# Check the structure of the resulting data frame
str(error_df_inside)
# Load necessary library for plotting


# Check the total number of elements
length(error_inside)  # Should return 1155248

# Find possible divisors for the number of rows
divisors <- which(1155248 %% 1:1155248 == 0)
divisors


error_df_inside <- as.data.frame(raster_inside, xy = TRUE)

# Rename the columns to make them clearer
colnames(error_df_inside) <- c("x", "y", "Error")

# Verify the structure of the data frame
str(error_df_inside)
# Load ggplot2 for plotting
library(ggplot2)

# Create a heatmap for "Inside AOA"
ggplot(error_df_inside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Inside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  )



# Check the total number of elements
length(error_inside)  # Should return 1155248

# Find possible divisors for the number of rows
divisors <- which(1155248 %% 1:1155248 == 0)
divisors


error_df_inside <- as.data.frame(raster_inside, xy = TRUE)

# Rename the columns to make them clearer
colnames(error_df_inside) <- c("x", "y", "Error")

# Verify the structure of the data frame
str(error_df_inside)
# Load ggplot2 for plotting
library(ggplot2)

# Create a heatmap for "Inside AOA"
ggplot(error_df_inside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Inside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  )

# Load necessary libraries
library(ggplot2)
library(terra)
library(dplyr)
# Check the total number of elements
length(error_inside)  # Should return 1155248

# Find possible divisors for the number of rows
divisors <- which(1155248 %% 1:1155248 == 0)
divisors
nrows <- 1075  # Example value
ncols <- length(error_outside) / nrows  # Calculate columns based on length

# Ensure ncols is an integer
ncols <- round(ncols)

# Reshape the vector
error_matrix <- matrix(error_inside, nrow = nrows, ncol = ncols, byrow = TRUE)

# Check the matrix dimensions
dim(error_matrix)


# Assuming error_inside and error_outside are raster data or matrices
# Let's assume they are matrices or spatial rasters that you want to plot
# Load necessary package
library(terra)
# Calculate errors for areas outside the AOA

error_inside <- abs(inside_aoa_predicted_values- inside_aoa_reference_values)
# Assuming error_inside is a raster
raster_inside <- rast(error_inside)  # Convert to raster (if it's not already)

# Extract raster values and coordinates (xy)
error_df_inside <- as.data.frame(raster_inside, xy = TRUE)

# Check the structure of the new data frame
str(error_df_inside)
# Check the structure of error_inside
str(error_inside)
nrows <- sqrt(length(error_inside))  # Try to make it a square raster
ncols <- nrows
# Set the number of rows and columns
nrows <- 1075  # Example, adjust based on your data
ncols <- length(error_inside) / nrows  # Calculate number of columns
# Reshape the vector into a matrix
error_matrix <- matrix(error_inside, nrow = nrows, ncol = ncols, byrow = TRUE)
# Reshape the vector into a matrix
# Calculate the number of rows and columns based on the square root
sqrt_length <- sqrt(length(error_inside))  # Take square root of the total number of elements
nrows <- floor(sqrt_length)
ncols <- ceiling(sqrt_length)

# If it works for reshaping
error_matrix <- matrix(error_inside, nrow = nrows, ncol = ncols, byrow = TRUE)
library(terra)

# Convert the matrix to a raster
raster_inside <- rast(error_matrix)

# Check the structure of the raster
print(raster_inside)

# Extract raster values and coordinates (xy)
error_df_inside <- as.data.frame(raster_inside, xy = TRUE)

# Rename the columns to make them more understandable
colnames(error_df_inside) <- c("x", "y", "Error")

# Check the structure of the resulting data frame
str(error_df_inside)
# Load necessary library for plotting


# Check the total number of elements
length(error_inside)  # Should return 1155248

# Find possible divisors for the number of rows
divisors <- which(1155248 %% 1:1155248 == 0)
divisors


error_df_inside <- as.data.frame(raster_inside, xy = TRUE)

# Rename the columns to make them clearer
colnames(error_df_inside) <- c("x", "y", "Error")

# Verify the structure of the data frame
str(error_df_inside)
# Load ggplot2 for plotting
library(ggplot2)

# Create a heatmap for "Inside AOA"
ggplot(error_df_inside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Inside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  )

#outside Heat Map
# Assuming error_inside and error_outside are raster data or matrices
# Let's assume they are matrices or spatial rasters that you want to plot
# Load necessary package
library(terra)
# Calculate errors for areas outside the AOA
# Check the structure and class of error_outside
str(error_outside)
class(error_outside)

error_outside <- abs(outside_aoa_predicted_values- outside_aoa_reference_values)
# Assuming error_inside is a raster
raster_outside <- rast(error_outside)  # Convert to raster (if it's not already)

# Extract raster values and coordinates (xy)
error_df_outside <- as.data.frame(raster_outside, xy = TRUE)

# Check the structure of the new data frame
str(error_df_outside)
# Check the structure of error_inside
str(error_outside)
nrows <- sqrt(length(error_outside))  # Try to make it a square raster
ncols <- nrows
# Set the number of rows and columns
nrows <- 1075  # Example, adjust based on your data
ncols <- length(error_outside) / nrows  # Calculate number of columns
# Reshape the vector into a matrix
error_matrix <- matrix(error_outside, nrow = nrows, ncol = ncols, byrow = TRUE)
# Reshape the vector into a matrix
# Calculate the number of rows and columns based on the square root
sqrt_length <- sqrt(length(error_outside))  # Take square root of the total number of elements
nrows <- floor(sqrt_length)
ncols <- ceiling(sqrt_length)

# If it works for reshaping
error_matrix <- matrix(error_outside, nrow = nrows, ncol = ncols, byrow = TRUE)
# First, check the dimensions of your existing raster (e.g., predicted_raster)
nrows <- nrow(predicted_raster)  # Get number of rows from an existing raster
ncols <- ncol(predicted_raster)  # Get number of columns from an existing raster

# Reshape the error_outside vector into a matrix that matches the raster's dimensions


# Convert the matrix into a SpatRaster
raster_outside <- rast(error_matrix)

# Check the result
print(raster_outside)

# Convert the matrix to a raster
raster_outside <- rast(error_matrix)

# Check the structure of the raster
print(raster_outside)

# Extract raster values and coordinates (xy)
error_df_outside <- as.data.frame(raster_outside, xy = TRUE)

# Rename the columns to make them more understandable
colnames(error_df_outside) <- c("x", "y", "Error")

# Check the structure of the resulting data frame
str(error_df_outside)
# Load necessary library for plotting


# Check the total number of elements
length(error_outside)  # Should return 1155248

# Find possible divisors for the number of rows
divisors <- which(1155248 %% 1:1155248 == 0)
divisors


error_df_outside <- as.data.frame(raster_outside, xy = TRUE)

# Rename the columns to make them clearer
colnames(error_df_inside) <- c("x", "y", "Error")

# Verify the structure of the data frame
str(error_df_outside)
# Load ggplot2 for plotting
library(ggplot2)

# Create a heatmap for "Inside AOA"
ggplot(error_df_outside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Outside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  )



#outside heat map
library(terra)
library(ggplot2)

# Step 1: Reshape the vector into a matrix
nrows <- 1075  # Define the number of rows (adjust if needed)
ncols <- ceiling(length(error_outside) / nrows)  # Calculate the number of columns to fit the data

# Reshape the vector into a matrix
error_matrix_outside <- matrix(error_outside, nrow = nrows, ncol = ncols, byrow = TRUE)

# Step 2: Handle any extra elements that do not fit exactly into the matrix
# If the total length of the data doesn't match perfectly, add NA values
total_elements <- nrows * ncols
if (length(error_outside) < total_elements) {
  error_outside <- c(error_outside, rep(NA, total_elements - length(error_outside)))
}

# Now, reshape the vector again to fit the matrix dimensions
error_matrix_outside <- matrix(error_outside, nrow = nrows, ncol = ncols, byrow = TRUE)

# Step 3: Convert the matrix into a SpatRaster
raster_outside <- rast(error_matrix_outside)

# Step 4: Check the structure of the raster
print(raster_outside)

# Step 5: Extract data from the raster and convert to a data frame
error_df_outside <- as.data.frame(raster_outside, xy = TRUE)

# Rename columns for clarity
colnames(error_df_outside) <- c("x", "y", "Error")

# Step 6: Plot the heatmap for "Outside AOA"
ggplot(error_df_outside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Outside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  ) +
  coord_cartesian(ylim = c(min(error_df_outside$y), max(error_df_outside$y)))  # Ensuring bottom part shows























# Check the total number of elements
length(error_outside)  # Should return 1155248

# Find possible divisors for the number of rows
divisors <- which(1155248 %% 1:1155248 == 0)
divisors


error_df_outside <- as.data.frame(raster_outside, xy = TRUE)

# Rename the columns to make them clearer
colnames(error_df_outside) <- c("x", "y", "Error")

# Verify the structure of the data frame
str(error_df_inside)
# Load ggplot2 for plotting
library(ggplot2)

# Create a heatmap for "Inside AOA"
ggplot(error_df_inside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Inside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  )

library(terra)
library(ggplot2)
library(terra)

# Step 1: Reshape the vector into a matrix
nrows <- 1075  # Define the number of rows (you can adjust this)
ncols <- length(error_outside) / nrows  # Calculate number of columns
ncols <- round(ncols)  # Ensure ncols is an integer

# Reshape the vector into a matrix
error_matrix_outside <- matrix(error_outside, nrow = nrows, ncol = ncols, byrow = TRUE)

# Step 2: Convert the matrix into a SpatRaster
raster_outside <- rast(error_matrix_outside)

# Step 3: Check the structure of the raster
print(raster_outside)

# Step 4: Extract data from the raster and convert to a data frame
error_df_outside <- as.data.frame(raster_outside, xy = TRUE)

# Rename columns for clarity
colnames(error_df_outside) <- c("x", "y", "Error")

# Step 5: Plotting Heatmap for Outside AOA
library(ggplot2)
ggplot(error_df_outside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Outside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  )

# Calculate errors for areas outside the AOA
error_outside <- abs(outside_aoa_predicted_values - outside_aoa_reference_values)

library(terra)

# Step 1: Reshape the vector into a matrix
nrows <- 1075  # Define the number of rows (you can adjust this)
ncols <- length(error_outside) / nrows  # Calculate number of columns
ncols <- round(ncols)  # Ensure ncols is an integer

# Reshape the vector into a matrix
error_matrix_outside <- matrix(error_outside, nrow = nrows, ncol = ncols, byrow = TRUE)

# Step 2: Convert the matrix into a SpatRaster
raster_outside <- rast(error_matrix_outside)

# Step 3: Check the structure of the raster
print(raster_outside)

# Step 4: Extract data from the raster and convert to a data frame
error_df_outside <- as.data.frame(raster_outside, xy = TRUE)

# Rename columns for clarity
colnames(error_df_outside) <- c("x", "y", "Error")

# Step 5: Plotting Heatmap for Outside AOA
library(ggplot2)
ggplot(error_df_outside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Outside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  )

library(terra)
library(ggplot2)

# Step 1: Reshape the vector into a matrix
nrows <- 1075  # Define the number of rows (adjust if needed)
ncols <- ceiling(length(error_outside) / nrows)  # Calculate the number of columns to fit the data

# Reshape the vector into a matrix with appropriate dimensions
error_matrix_outside <- matrix(error_outside, nrow = nrows, ncol = ncols, byrow = TRUE)

# Handle any extra elements that don't fit (pad the matrix with NAs if needed)
if (length(error_outside) %% (nrows * ncols) != 0) {
  error_matrix_outside <- rbind(error_matrix_outside, rep(NA, ncols))
}

# Step 2: Convert the matrix into a SpatRaster
raster_outside <- rast(error_matrix_outside)

# Step 3: Check the structure of the raster
print(raster_outside)

# Step 4: Extract data from the raster and convert to a data frame
error_df_outside <- as.data.frame(raster_outside, xy = TRUE)

# Rename columns for clarity
colnames(error_df_outside) <- c("x", "y", "Error")

# Step 5: Plotting Heatmap for Outside AOA
ggplot(error_df_outside, aes(x = x, y = y, fill = Error)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), name = "Error") +
  labs(title = "Error Heatmap Outside AOA", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),  # Adjust the size if the coordinates are not readable
    axis.ticks = element_line(size = 0.5)
  )



# Plot histograms for error distributions inside vs outside AOA
par(mfrow = c(1, 2))  # Layout side-by-side histograms
hist(error_inside, main = "Error Distribution Inside AOA", xlab = "Error", col = "blue", breaks = 30)
hist(error_outside, main = "Error Distribution Outside AOA", xlab = "Error", col = "red", breaks = 30)

# Boxplot to compare errors inside vs outside AOA
boxplot(Error ~ Area, data = error_df, main = "Comparison of Errors Inside vs Outside AOA", ylab = "Absolute Error")



# Extract values from the first layer
values_layer_1 <- values(predicted_raster[[1]])

# Create valid_indices for the first layer (TRUE for non-NA values)
valid_indices_layer_1 <- !is.na(values_layer_1)

# Subset valid data based on the valid indices for layer 1
valid_data <- values_layer_1[valid_indices_layer_1]

# Subset the valid coordinates (make sure coords_valid is aligned with the raster cells)
valid_coords <- coords_valid[valid_indices_layer_1, , drop = FALSE]
cat("Number of valid data points: ", length(valid_data), "\n")
cat("Number of valid coordinates: ", nrow(valid_coords), "\n")
# Create the spatial weights object
listw_valid <- nb2listw(weights_valid, style = "W", zero.policy = TRUE)

# Perform the Moran's I test
moran_result <- moran.test(valid_data, listw_valid)
cat("Number of valid cells in original raster (inside AoA):", sum(inside_aoa_valid_indices_clean), "\n")

# Print the result
print(moran_result)
# Step 1: Ensure that AoA indices are consistent with the raster data
# We assume that both rasters are aligned in terms of extent and resolution
# Create valid_indices for layer 1 (TRUE for non-NA values)
# Check the extents, resolution, and CRS of each raster
cat("AoA raster: \n")
print(ext(aoa_raster))
cat("Resolution: ", res(aoa_raster), "\n")
cat("CRS: ", crs(aoa_raster), "\n")

cat("Reference raster: \n")
print(ext(reference_raster))
cat("Resolution: ", res(reference_raster), "\n")
cat("CRS: ", crs(reference_raster), "\n")

cat("Predicted raster: \n")
print(ext(predicted_raster))
cat("Resolution: ", res(predicted_raster), "\n")
cat("CRS: ", crs(predicted_raster), "\n")
# Load the raster object (if not already loaded)
aoa_raster <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")  # Make sure the correct raster path is specified
# Check if there are NA values in coordinates
sum(is.na(valid_coords_inside_aoa))  # For inside AoA
sum(is.na(valid_coords_outside_aoa)) # For outside AoA

# Optional: You can remove any rows with NA coordinates
valid_coords_inside_aoa <- valid_coords_inside_aoa[!is.na(valid_coords_inside_aoa[,1]) & !is.na(valid_coords_inside_aoa[,2]), ]
# Remove rows with NA values in valid_coords_outside_aoa
valid_coords_outside_aoa <- valid_coords_outside_aoa[!is.na(valid_coords_outside_aoa[,1]) & !is.na(valid_coords_outside_aoa[,2]), ]

# Double-check for NA values again
sum(is.na(valid_coords_outside_aoa))  # Should now be 0
# Combine valid coordinates for both inside and outside AoA
coords_all <- rbind(valid_coords_inside_aoa, valid_coords_outside_aoa)

# Compute the spatial neighbors using a distance threshold (e.g., 100 units)
neighbors <- dnearneigh(coords_all, 0, 100)  # Adjust the distance threshold as needed





library(spdep)
library(terra)

# Step 1: Load the raster objects
aoa_raster <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")
predicted_raster <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")

# Step 2: Reclassify the AoA raster to logical TRUE/FALSE (inside/outside AoA)
inside_aoa_raster <- aoa_raster == 1  
outside_aoa_raster <- aoa_raster == 0 

# Step 3: Get the valid data points inside and outside AoA
valid_cells_inside_aoa <- which(inside_aoa_raster[] == TRUE)  # Cells inside AoA
valid_cells_outside_aoa <- which(outside_aoa_raster[] == TRUE)  # Cells outside AoA

# Step 4: Extract coordinates for valid cells inside and outside AoA
valid_coords_inside_aoa <- xyFromCell(aoa_raster, valid_cells_inside_aoa)
valid_coords_outside_aoa <- xyFromCell(aoa_raster, valid_cells_outside_aoa)

# Step 5: Extract raster values at valid coordinates
values_inside_aoa <- extract(predicted_raster, valid_coords_inside_aoa)
values_outside_aoa <- extract(predicted_raster, valid_coords_outside_aoa)

# Step 6: Create a spatial weights matrix (e.g., queen contiguity)
# Create neighbors for inside AoA (use Euclidean distance or contiguity based on coordinates)
# You could use spatial weights directly based on coordinates of inside/outside cells
coords_all <- rbind(valid_coords_inside_aoa, valid_coords_outside_aoa) 
# Step 1: Remove NA values from the coordinates
coords_all_clean <- na.omit(coords_all)

# Step 2: Create neighbors based on the cleaned coordinates (without NA values)
neighbors <- dnearneigh(coords_all_clean, 0, 100)  # Adjust distance threshold as needed

# Check the number of neighbors
length(neighbors)
# Combine coords for both inside/outside
# Example: distance threshold of 100 units
# Ensure the predicted values correspond to the cleaned coordinates
# Remove NAs from predicted values, ensuring the values correspond to the valid coordinates
predicted_values_clean <- predicted_values_all[!is.na(coords_all[])]
# Double check for NAs again
sum(is.na(predicted_values_clean)) 
# Remove NAs from the coordinates (coords_all)
valid_coords_clean <- coords_all[complete.cases(coords_all), ]

# Check if the coordinates are cleaned up
sum(is.na(valid_coords_clean))  # Should be 0 if no NAs
# Clean predicted values by removing NAs corresponding to valid coordinates
predicted_values_clean <- predicted_values_all[!is.na(coords_all[])]
# Check lengths to ensure they're the same
length(predicted_values_clean)  # Length of cleaned predicted values
length(valid_coords_clean)      # Length of cleaned coordinates

# Should be 0 if there are no NAs
# Remove corresponding NA values
# Step 2: Create spatial weights based on the neighbors
weights <- nb2listw(neighbors)  # Convert neighbors to listw object for Moran's I

# Step 3: Compute Moran's I for the predicted values using the weights
moran_result <- moran.test(predicted_values_clean, weights)

# Print the result
print(moran_result)

# Step 3: Extract the predicted values from the raster for valid coordinates
predicted_values_inside_aoa <- extract(predicted_raster, valid_coords_inside_aoa)
predicted_values_outside_aoa <- extract(predicted_raster, valid_coords_outside_aoa)

# Combine predicted values for inside and outside AoA
predicted_values_all <- c(predicted_values_inside_aoa, predicted_values_outside_aoa)

# Check if the length of predicted values and neighbors now match
length(predicted_values_all)
length(neighbors)

# Create spatial weights based on the neighbors
weights <- nb2listw(neighbors)  # Convert neighbors to listw object for Moran's I

# Compute Moran's I for the predicted values using the weights
moran_result <- moran.test(predicted_values_clean, weights)

# Print the result
print(moran_result)
library(terra)

# Step 1: Load the raster objects
aoa_raster <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")
predicted_raster <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")

# Step 2: Reclassify the AoA raster (assuming 0 is outside AoA and non-zero values are inside AoA)
inside_aoa_valid_indices <- ifel(aoa_raster > 0, TRUE, FALSE)  # Inside AoA if value > 0 (non-zero values)
outside_aoa_valid_indices <- ifel(aoa_raster == 0, TRUE, FALSE)  # Outside AoA if value == 0

# Step 3: Get the valid data points inside and outside AoA
valid_cells_inside_aoa <- which(inside_aoa_valid_indices[] == TRUE)  # Cells inside AoA
valid_cells_outside_aoa <- which(outside_aoa_valid_indices[] == TRUE)  # Cells outside AoA

# Step 4: Extract coordinates for valid cells inside and outside AoA
valid_coords_inside_aoa <- terra::xyFromCell(aoa_raster, valid_cells_inside_aoa)
valid_coords_outside_aoa <- terra::xyFromCell(aoa_raster, valid_cells_outside_aoa)

# Step 5: Extract valid data for inside and outside AoA from the predicted raster
valid_data_inside_aoa <- values(predicted_raster)[valid_cells_inside_aoa]
valid_data_outside_aoa <- values(predicted_raster)[valid_cells_outside_aoa]

# Check the dimensions and number of valid coordinates
cat("Number of valid coordinates inside AoA: ", nrow(valid_coords_inside_aoa), "\n")
cat("Number of valid coordinates outside AoA: ", nrow(valid_coords_outside_aoa), "\n")

library(spdep)

# Remove NAs from valid coordinates
valid_coords_inside_aoa_clean <- valid_coords_inside_aoa[complete.cases(valid_coords_inside_aoa), ]
valid_coords_outside_aoa_clean <- valid_coords_outside_aoa[complete.cases(valid_coords_outside_aoa), ]

# Step 6: Create spatial weights objects using k-nearest neighbors (k = 8 for example) 
listw_valid_inside_aoa <- nb2listw(knn2nb(knearneigh(valid_coords_inside_aoa_clean, k = 8)), style = "W", zero.policy = TRUE)
listw_valid_outside_aoa <- nb2listw(knn2nb(knearneigh(valid_coords_outside_aoa_clean, k = 8)), style = "W", zero.policy = TRUE)
# Check the length of valid data and coordinates for inside AoA
# Check the length of valid data and coordinates for inside AoA
cat("Length of valid_data_inside_aoa: ", length(valid_data_inside_aoa), "\n")
cat("Number of rows in valid_coords_inside_aoa_clean: ", nrow(valid_coords_inside_aoa_clean), "\n")
# Clean valid cells to ensure they are valid (e.g., no NAs)
valid_cells_inside_aoa_clean <- valid_cells_inside_aoa[!is.na(valid_data_inside_aoa)]
cat("Length of valid_cells_inside_aoa_clean: ", length(valid_cells_inside_aoa_clean), "\n")
# Extract coordinates for cleaned valid cells inside AoA
valid_coords_inside_aoa_clean <- terra::xyFromCell(aoa_raster, valid_cells_inside_aoa_clean)
cat("Number of valid coordinates inside AoA (cleaned): ", nrow(valid_coords_inside_aoa_clean), "\n")

# Ensure that the valid data length matches cleaned coordinates
cat("Length of valid data inside AoA: ", length(valid_data_inside_aoa), "\n")
cat("Length of valid_coords_inside_aoa_clean: ", nrow(valid_coords_inside_aoa_clean), "\n")

# Step 6: Create spatial weights objects using k-nearest neighbors (k = 8 for example)
listw_valid_inside_aoa <- nb2listw(knn2nb(knearneigh(valid_coords_inside_aoa_clean, k = 8)), style = "W", zero.policy = TRUE)

# Step 7: Perform Moran's I test for inside AoA
if (length(valid_data_inside_aoa) == nrow(valid_coords_inside_aoa_clean)) {
  moran_result_inside_aoa <- moran.test(valid_data_inside_aoa, listw_valid_inside_aoa)
  cat("Moran's I for inside AoA:\n")
  print(moran_result_inside_aoa)
} else {
  print("Mismatch in length of valid data and valid coordinates for inside AoA.")
}
cat("Length of valid data inside AoA: ", length(valid_data_inside_aoa), "\n")
cat("Number of rows in valid_coords_inside_aoa_clean: ", nrow(valid_coords_inside_aoa_clean), "\n")

# Remove NA values from valid data and coordinates
valid_data_inside_aoa_clean <- valid_data_inside_aoa[!is.na(valid_data_inside_aoa)]  # Remove NA values from valid data
valid_coords_inside_aoa_clean <- valid_coords_inside_aoa_clean[!is.na(valid_data_inside_aoa), ]  # Align coordinates with cleaned valid data

# Check the new lengths
cat("Length of cleaned valid data inside AoA: ", length(valid_data_inside_aoa_clean), "\n")
cat("Number of rows in cleaned valid_coords_inside_aoa_clean: ", nrow(valid_coords_inside_aoa_clean), "\n")

# Step 1: Check the initial lengths and presence of NAs in both valid data and coordinates
cat("Initial length of valid_data_inside_aoa: ", length(valid_data_inside_aoa), "\n")
cat("Initial number of coordinates inside AoA: ", nrow(valid_coords_inside_aoa_clean), "\n")

# Step 2: Clean the data by removing NAs from both valid data and valid coordinates
valid_coords_inside_aoa_clean <- valid_coords_inside_aoa_clean[!is.na(valid_data_inside_aoa), ]
valid_data_inside_aoa_clean <- valid_data_inside_aoa[!is.na(valid_data_inside_aoa)]

# Step 3: Ensure lengths are now aligned
cat("Length of valid_data_inside_aoa_clean: ", length(valid_data_inside_aoa_clean), "\n")
cat("Number of valid coordinates inside AoA after cleaning: ", nrow(valid_coords_inside_aoa_clean), "\n")

# Step 4: Confirm no NAs exist in the cleaned data and coordinates
cat("Number of NAs in valid_data_inside_aoa_clean: ", sum(is.na(valid_data_inside_aoa_clean)), "\n")
cat("Number of NAs in valid_coords_inside_aoa_clean (x): ", sum(is.na(valid_coords_inside_aoa_clean[, 1])), "\n")
cat("Number of NAs in valid_coords_inside_aoa_clean (y): ", sum(is.na(valid_coords_inside_aoa_clean[, 2])), "\n")

# Step 5: Now, check if the lengths match
if (length(valid_data_inside_aoa_clean) == nrow(valid_coords_inside_aoa_clean)) {
  cat("Lengths match! Ready for further processing.\n")
} else {
  cat("Mismatch in length of valid data and valid coordinates for inside AoA.\n")
}


# Step 1: Create a spatial weights object using k-nearest neighbors (k = 8 for example)
library(spdep)

# Create the k-nearest neighbor weights object
listw_valid_inside_aoa <- nb2listw(knn2nb(knearneigh(valid_coords_inside_aoa_clean, k = 8)), style = "W", zero.policy = TRUE)

# Step 2: Perform Moran's I test for the cleaned data inside AoA
moran_result_inside_aoa <- moran.test(valid_data_inside_aoa_clean, listw_valid_inside_aoa)

# Step 3: Print the Moran's I result
cat("Moran's I for inside AoA:\n")
print(moran_result_inside_aoa)





#
# Remove rows with NA values in coordinates (both x and y)
valid_coords_inside_aoa_clean <- valid_coords_inside_aoa_clean[complete.cases(valid_coords_inside_aoa_clean), ]

# Ensure the lengths match
cat("Length of valid data after cleaning: ", length(valid_data_inside_aoa_clean), "\n")
cat("Number of coordinates after cleaning: ", nrow(valid_coords_inside_aoa_clean), "\n")

# Double-check there are no NAs left
cat("Number of NAs in valid_coords_inside_aoa_clean (x): ", sum(is.na(valid_coords_inside_aoa_clean[, 1])), "\n")
cat("Number of NAs in valid_coords_inside_aoa_clean (y): ", sum(is.na(valid_coords_inside_aoa_clean[, 2])), "\n")



# Load the necessary library for Moran's I
library(spdep)

# Create spatial weights using k-nearest neighbors (k = 8)
listw_valid_inside_aoa <- nb2listw(knn2nb(knearneigh(valid_coords_inside_aoa_clean, k = 8)), style = "W", zero.policy = TRUE)

# Load the necessary library for Moran's I
# Check if there are any NAs in the coordinates
cat("Number of NAs in valid_coords_inside_aoa_clean (x): ", sum(is.na(valid_coords_inside_aoa_clean[, 1])), "\n")
cat("Number of NAs in valid_coords_inside_aoa_clean (y): ", sum(is.na(valid_coords_inside_aoa_clean[, 2])), "\n")
cat("Length of valid data inside AoA: ", length(valid_data_inside_aoa_clean), "\n")
cat("Number of coordinates inside AoA: ", nrow(valid_coords_inside_aoa_clean), "\n")
# Check if there are any NA values in valid_coords_inside_aoa
cat("Number of NAs in valid_coords_inside_aoa (x): ", sum(is.na(valid_coords_inside_aoa[, 1])), "\n")
cat("Number of NAs in valid_coords_inside_aoa (y): ", sum(is.na(valid_coords_inside_aoa[, 2])), "\n")
# Clean the corresponding data in sync with the cleaned coordinates
valid_data_inside_aoa_clean <- valid_data_inside_aoa[complete.cases(valid_coords_inside_aoa_clean)]

# Check the length of the cleaned data
cat("Length of valid data after cleaning: ", length(valid_data_inside_aoa_clean), "\n")

# Ensure no NAs exist in the cleaned data and coordinates
cat("Number of NAs in valid_data_inside_aoa_clean: ", sum(is.na(valid_data_inside_aoa_clean)), "\n")
cat("Number of NAs in valid_coords_inside_aoa_clean (x): ", sum(is.na(valid_coords_inside_aoa_clean[, 1])), "\n")
cat("Number of NAs in valid_coords_inside_aoa_clean (y): ", sum(is.na(valid_coords_inside_aoa_clean[, 2])), "\n")

cat("Length of valid data inside AoA: ", length(valid_data_inside_aoa_clean), "\n")
cat("Number of coordinates inside AoA after cleaning: ", nrow(valid_coords_inside_aoa_clean), "\n")
# Use complete.cases() on both data and coordinates
valid_data_inside_aoa_clean <- valid_data_inside_aoa[complete.cases(valid_coords_inside_aoa), ]
valid_coords_inside_aoa_clean <- valid_coords_inside_aoa[complete.cases(valid_coords_inside_aoa), ]

# Create a logical index to identify valid (non-NA) coordinates
valid_coords_index <- !is.na(valid_coords_inside_aoa[, 1]) & !is.na(valid_coords_inside_aoa[, 2])

# Apply this logical index to filter both data and coordinates
valid_coords_inside_aoa_clean <- valid_coords_inside_aoa[valid_coords_index, ]
valid_data_inside_aoa_clean <- valid_data_inside_aoa[valid_coords_index]

# Verify the lengths and ensure data and coordinates are aligned
cat("Length of valid data inside AoA after cleaning: ", length(valid_data_inside_aoa_clean), "\n")
cat("Number of coordinates inside AoA after cleaning: ", nrow(valid_coords_inside_aoa_clean), "\n")
# Create a logical index to identify valid (non-NA) coordinates
valid_coords_index <- !is.na(valid_coords_inside_aoa[, 1]) & !is.na(valid_coords_inside_aoa[, 2])

# Apply this logical index to filter both data and coordinates
valid_coords_inside_aoa_clean <- valid_coords_inside_aoa[valid_coords_index, ]
valid_data_inside_aoa_clean <- valid_data_inside_aoa[valid_coords_index]

# Verify the lengths and ensure data and coordinates are aligned
cat("Length of valid data inside AoA after cleaning: ", length(valid_data_inside_aoa_clean), "\n")
cat("Number of coordinates inside AoA after cleaning: ", nrow(valid_coords_inside_aoa_clean), "\n")
# Create a logical index to identify valid (non-NA) coordinates
valid_coords_index <- !is.na(valid_coords_inside_aoa[, 1]) & !is.na(valid_coords_inside_aoa[, 2])

# Apply this logical index to filter both data and coordinates
valid_coords_inside_aoa_clean <- valid_coords_inside_aoa[valid_coords_index, ]
valid_data_inside_aoa_clean <- valid_data_inside_aoa[valid_coords_index]

# Verify the lengths and ensure data and coordinates are aligned
cat("Length of valid data inside AoA after cleaning: ", length(valid_data_inside_aoa_clean), "\n")
cat("Number of coordinates inside AoA after cleaning: ", nrow(valid_coords_inside_aoa_clean), "\n")
# Create a logical index to identify valid (non-NA) coordinates
valid_coords_index <- !is.na(valid_coords_inside_aoa[, 1]) & !is.na(valid_coords_inside_aoa[, 2])

# Apply this logical index to filter both data and coordinates
valid_coords_inside_aoa_clean <- valid_coords_inside_aoa[valid_coords_index, ]
valid_data_inside_aoa_clean <- valid_data_inside_aoa[valid_coords_index]

# Verify the lengths and ensure data and coordinates are aligned
cat("Length of valid data inside AoA after cleaning: ", length(valid_data_inside_aoa_clean), "\n")
cat("Number of coordinates inside AoA after cleaning: ", nrow(valid_coords_inside_aoa_clean), "\n")
# Step 1: Create spatial weights objects for inside AoA
listw_valid_inside_aoa <- nb2listw(knn2nb(knearneigh(valid_coords_inside_aoa_clean, k = 8)), style = "W", zero.policy = TRUE)

# Step 2: Perform Moran's I test for inside AoA
moran_result_inside_aoa <- moran.test(valid_data_inside_aoa_clean, listw_valid_inside_aoa)

# Step 3: Print the result for Moran's I test
print(moran_result_inside_aoa)

library(spdep)

# Create neighbors based on valid coordinates for inside AoA (after cleaning)
neighbors_inside_aoa <- knearneigh(valid_coords_inside_aoa_clean, k = 8)
listw_valid_inside_aoa <- nb2listw(knn2nb(neighbors_inside_aoa), style = "W", zero.policy = TRUE)
# Perform Moran's I test for inside AoA again
moran_result_inside_aoa <- moran.test(valid_data_inside_aoa_clean, listw_valid_inside_aoa)
cat("Moran's I for inside AoA:\n")
print(moran_result_inside_aoa)


# Check the structure of valid_data_outside_aoa
str(valid_data_outside_aoa)

# If valid_data_outside_aoa is a vector, you can subset it like this:
valid_data_outside_aoa_clean <- valid_data_outside_aoa[!is.na(valid_coords_outside_aoa[, 1]) & !is.na(valid_coords_outside_aoa[, 2])]

# Now check the lengths
cat("Length of valid data outside AoA after cleaning: ", length(valid_data_outside_aoa_clean), "\n")
cat("Number of coordinates outside AoA after cleaning: ", nrow(valid_coords_outside_aoa_clean), "\n")
# Step 1: Create spatial weights object for outside AoA
listw_valid_outside_aoa <- nb2listw(knn2nb(knearneigh(valid_coords_outside_aoa_clean, k = 8)), style = "W", zero.policy = TRUE)

# Step 2: Perform Moran's I test for outside AoA
moran_result_outside_aoa <- moran.test(valid_data_outside_aoa_clean, listw_valid_outside_aoa)

# Check the result
moran_result_outside_aoa
# Step 1: Create spatial weights object for outside AoA
listw_valid_outside_aoa <- nb2listw(knn2nb(knearneigh(valid_coords_outside_aoa_clean, k = 8)), style = "W", zero.policy = TRUE)

# Step 2: Perform Moran's I test for outside AoA
moran_result_outside_aoa <- moran.test(valid_data_outside_aoa_clean, listw_valid_outside_aoa)

# Check the result
moran_result_outside_aoa


library(terra)
library(sf)


library(terra)
library(sf)

# Step 1: Load the raster objects
aoa_boundary <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")
lulc_raster <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")
reference_png<- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH FLORENCE.png")





# Convert the AOA boundary image into a binary raster (1 for AOA, 0 for outside)
aoa_boundary_binary <- ifel(aoa_boundary == 255, 1, 0)  # Assuming AOA is white (255) and background is black (0)

# Assuming AOA is white (255) and background is black (0)

# Crop the reference data to match the extent of the LULC raster (no resampling)
reference_png_cropped <- crop(reference_png, lulc_raster)

# Mask the LULC raster with the AOA boundary to get inside the AOA
# Inside AOA (masked LULC inside AOA)
aoa_masked <- mask(lulc_raster, aoa_boundary_binary)

# Create the "outside AOA" by using the inverse of the binary mask
# Since AOA is 1 inside the boundary, outside the AOA will be 0
aoa_outside_binary <- ifel(aoa_boundary_binary == 1, NA, 1)

# Mask the LULC raster outside the AOA
aoa_inverse <- mask(lulc_raster, aoa_outside_binary)

# Compare LULC classification to the reference data (for correct vs incorrect classification)
# Inside AOA: 1 for correct, 0 for misclassified
inside_comparison <- ifel(aoa_masked == reference_png_cropped, 1, 0)

# Outside AOA: 1 for correct, 0 for misclassified
outside_comparison <- ifel(aoa_inverse == reference_png_cropped, 1, 0)

# Visualize the results
par(mfrow=c(1,2))  # Plot side by side

# Plot the misclassifications inside the AOA
plot(inside_comparison, main = "Misclassifications Inside AOA", col = c("green", "red"))
legend("topright", legend = c("Correct", "Misclassified"), fill = c("green", "red"))

# Plot the misclassifications outside the AOA
plot(outside_comparison, main = "Misclassifications Outside AOA", col = c("green", "red"))
legend("topright", legend = c("Correct", "Misclassified"), fill = c("green", "red"))

# Calculate the proportion of misclassified pixels inside the AOA
inside_error_rate <- cellStats(inside_comparison, sum) / ncell(inside_comparison)
print(paste("Error rate inside AOA:", inside_error_rate))

# Calculate the proportion of misclassified pixels outside the AOA
outside_error_rate <- cellStats(outside_comparison, sum) / ncell(outside_comparison)
print(paste("Error rate outside AOA:", outside_error_rate))





library(terra)

# Visualize the results
par(mfrow=c(1,2))  # Plot side by side

# Plot the misclassifications inside the AOA
plot(inside_comparison, main = "Misclassifications Inside AOA", col = c("green", "red"))
legend("topright", legend = c("Correct", "Misclassified"), fill = c("green", "red"))

# Plot the misclassifications outside the AOA
plot(outside_comparison, main = "Misclassifications Outside AOA", col = c("green", "red"))
legend("topright", legend = c("Correct", "Misclassified"), fill = c("green", "red"))
library(terra)

# Define color mapping: 
# 0 = Misclassified (Red), 1 = Correctly Classified (Green)
col_map <- c("red", "green")  

# Open a new window for misclassifications inside AOA
windows()  # For Windows users; use `X11()` on Linux/Mac
plot(inside_comparison, main="Misclassifications Inside AOA", col=col_map, legend=FALSE)
legend("topright", legend=c("Misclassified", "Correctly Classified"), fill=c("red", "green"))

# Open another new window for misclassifications outside AOA
windows()  # Open separate window
plot(outside_comparison, main="Misclassifications Outside AOA", col=col_map, legend=FALSE)
legend("topright", legend=c("Misclassified", "Correctly Classified"), fill=c("red", "green"))


# Inside AOA error rate calculation
inside_error_rate <- global(inside_comparison_binary, fun = sum, na.rm = TRUE) / ncell(inside_comparison_binary)
print(paste("Error rate inside AOA:", inside_error_rate))

# Outside AOA error rate calculation
outside_error_rate <- global(outside_comparison_binary, fun = sum, na.rm = TRUE) / ncell(outside_comparison_binary)
print(paste("Error rate outside AOA:", outside_error_rate))

print(length(inside_error_rate))
print(length(outside_error_rate))
library(terra)

# Assuming `inside_comparison` and `outside_comparison` are SpatRaster objects

# Function to calculate error rates for each class (1-7)
calculate_error_rate <- function(raster, total_classes = 7) {
  error_rate_per_class <- numeric(total_classes)
  
  for (i in 1:total_classes) {
    # Calculate the number of pixels for class i
    class_pixels <- global(raster == i, fun = "sum", na.rm = TRUE)
    total_pixels <- ncell(raster)
    
    # Error rate per class is the proportion of misclassified pixels
    error_rate_per_class[i] <- class_pixels / total_pixels
  }
  
  return(error_rate_per_class)
}
print(length(inside_error_rate_per_class))
print(length(outside_error_rate_per_class))


# Replace zero values with a valid error rate or exclude classes with zero error rates


inside_error_rate_flat <- unlist(inside_error_rate_per_class)
outside_error_rate_flat <- unlist(outside_error_rate_per_class)



non_zero_error_rate_inside <- ifelse(inside_error_rate_flat > 0, inside_error_rate_flat, 0)
non_zero_error_rate_outside <- ifelse(outside_error_rate_flat > 0, outside_error_rate_flat, 0)

# Ensure both vectors have 7 values each
error_df <- data.frame(
  class = rep(1:7, times = 2),  # 7 classes
  location = rep(c("Inside AOA", "Outside AOA"), each = 7),  # Two locations
  error_rate = c(non_zero_error_rate_inside, non_zero_error_rate_outside)
)

# View the corrected data frame
print(error_df)
# Filter out classes with zero error rates for both Inside and Outside AOA
filtered_error_df <- error_df[error_df$error_rate > 0, ]

# View the filtered data frame
print(filtered_error_df)
# Check the actual values of outside_error_rate_per_class
print(outside_error_rate_per_class)


library(raster)
library(terra)
library(sf)

# Step 1: Load the raster objects
aoa_boundary <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")
lulc_raster <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")
# Load the PNG file as a raster object
reference_png <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH FLORENCE.png")

# Check the raster object
print(reference_png)
# Verify the types of rasters
print(typeof(aoa_boundary))
print(typeof(lulc_raster))
print(typeof(reference_png))
# Extract the first layer from the reference_png raster
reference_layer1 <- reference_png[[1]]

# Check the summary of values for this layer
summary(values(reference_layer1))
# Reclassify the LULC raster and the reference layer
reclassified_lulc <- classify(lulc_raster, reclassification_matrix)
reclassified_gt <- classify(reference_layer1, reclassification_matrix)

# Check the reclassified values for both rasters
summary(values(reclassified_lulc))
summary(values(reclassified_gt))

# Calculate the confusion matrix for accuracy
conf_matrix <- confusionMatrix(factor(values(reclassified_lulc)), factor(values(reclassified_gt)))

# Print the confusion matrix
print(conf_matrix)

# Calculate Area of Applicability (AoA) for each class
area_of_applicability <- table(values(reclassified_lulc))

# Print the Area of Applicability for each LULC class
print(area_of_applicability)

# Reclassify the LULC raster and the reference layer
reclassified_lulc <- classify(lulc_raster, reclassification_matrix)
reclassified_gt <- classify(reference_layer1, reclassification_matrix)
# Get the values of both rasters (removing NAs)
lulc_values <- values(reclassified_lulc)
gt_values <- values(reclassified_gt)
















library(terra)

# Step 1: Load the rasters
aoa_boundary <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")
lulc_raster <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")
reference_png <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH FLORENCE.png")
# Check the values before reclassification


# Check the values before reclassification (if you intended to check a cropped version)
# Assuming you're using reference_png directly
summary(values(reference_png))

# Check unique values in both rasters
unique_lulc <- unique(values(lulc_raster))  # Unique values in the LULC raster
unique_gt <- unique(values(reference_png))  # Unique values in the Ground Truth raster

# Print the unique values to compare
print("Unique values in LULC Raster:")
print(unique_lulc)

print("Unique values in Ground Truth Raster:")
print(unique_gt)

# Check the LULC classes (you may want to set the names based on the classes you have)
print("LULC raster class names:")
print(lulc_raster)

# Compute frequency of each LULC class in the raster (this gives the number of pixels per class)
lulc_freq <- freq(lulc_raster)

# Convert to data frame and calculate the area in square kilometers (or other unit)
lulc_freq_df <- as.data.frame(lulc_freq)
lulc_freq_df$Area <- lulc_freq_df$count * res(lulc_raster)[1] * res(lulc_raster)[2]  # area in square units

# View the area of each LULC class
print(lulc_freq_df)

# Now, if you want to compute accuracy:
# Step 2: Create a data frame comparing ground truth (reference_png) and predicted labels (lulc_raster)
gt_values <- values(reference_png)
lulc_values <- values(lulc_raster)



# Check resolutions and extents of both rasters
res(lulc_raster)  # Resolution of the predicted raster
res(reference_png)  # Resolution of the ground truth raster

ext(lulc_raster)  # Extent of the predicted raster
ext(reference_png)  # Extent of the ground truth raster
# Crop both rasters to the smaller extent (intersection of both rasters)
common_extent <- intersect(ext(lulc_raster), ext(reference_png))

lulc_cropped <- crop(lulc_raster, common_extent)
reference_cropped <- crop(reference_png, common_extent)

# Extract values again after resampling (or cropping)
lulc_values <- values(lulc_cropped)
gt_values <- values(reference_cropped)
# Check the number of cells in each raster
ncell(lulc_cropped)  # Number of cells in the cropped predicted raster
ncell(reference_cropped)  # Number of cells in the cropped ground truth raster
# Extract values again after resampling or cropping
lulc_values <- values(lulc_cropped)
gt_values <- values(reference_cropped)
# Remove NA values from both rasters
# Crop both rasters to the intersection extent
common_extent <- intersect(ext(lulc_raster), ext(reference_png))

lulc_cropped <- crop(lulc_raster, common_extent)
reference_cropped <- crop(reference_png, common_extent)


# Create the final data frame for analysis using valid data only
accuracy_data <- data.frame(
  Accuracy = ifelse(lulc_values[valid_data] == gt_values[valid_data], 1, 0),
  LULC_Class = factor(lulc_values[valid_data])
)

# Extract values again after resampling or cropping
lulc_values <- values(lulc_cropped)
gt_values <- values(reference_cropped)
length(lulc_values)
length(gt_values)
# Aggregate the layers into a single layer by summing them (or using other methods such as mean)
reference_cropped_aggregated <- sum(reference_cropped)

# Extract values from the selected or aggregated layers
lulc_values <- values(lulc_cropped)
gt_values <- values(reference_cropped_layer1)  # or reference_cropped_aggregated if you aggregated

# Check if the lengths of both arrays match
length(lulc_values)
length(gt_values)

# Remove NA values
valid_data <- !is.na(lulc_values) & !is.na(gt_values)

# Create the final data frame for analysis
accuracy_data <- data.frame(
  Accuracy = ifelse(lulc_values[valid_data] == gt_values[valid_data], 1, 0),
  LULC_Class = factor(lulc_values[valid_data])
)

# Perform ANOVA to test for differences in accuracy between LULC classes
anova_accuracy <- aov(Accuracy ~ LULC_Class, data = accuracy_data)
summary(anova_accuracy)

# Visualize accuracy per LULC class
library(ggplot2)
ggplot(accuracy_data, aes(x = LULC_Class, y = Accuracy)) +
  geom_boxplot() +
  labs(title = "Accuracy Distribution by LULC Class", x = "LULC Class", y = "Accuracy") +
  theme_minimal()




# Check the lengths of both arrays to ensure they match
length(lulc_values)
length(gt_values)
# Extract values from both rasters after alignment
# Extract values from both rasters after alignment
lulc_values <- values(lulc_cropped)
gt_values <- values(reference_cropped_aggregated) 
# Or reference_cropped_aggregated if you aggregated
# Remove NA values from both rasters
valid_data <- !is.na(lulc_values) & !is.na(gt_values)
# Check the dimensions of both rasters
# Or reference_cropped_aggregated if you aggregated
reference_cropped_aggregated <- sum(reference_cropped)
lulc_cropped_aggregated <- sum(lulc_cropped)
dim(lulc_cropped_aggregated)
dim(reference_cropped_aggregated)
# Extract values
lulc_values <- values(lulc_cropped_aggregated)
gt_values <- values(reference_cropped_aggregated)  # Or reference_cropped_aggregated

# Check the lengths
length(lulc_values)
length(gt_values)

# Remove NA values by filtering out invalid cells (those with NA)
valid_lulc_values <- lulc_values[!is.na(lulc_values)]
valid_gt_values <- gt_values[!is.na(gt_values)]

# Check the lengths of both filtered arrays
length(valid_lulc_values)
length(valid_gt_values)

# Now create the final data frame using only valid data
accuracy_data <- data.frame(
  Accuracy = ifelse(valid_lulc_values == valid_gt_values, 1, 0),
  LULC_Class = factor(valid_lulc_values)
)
# Perform ANOVA to test for differences in accuracy between LULC classes
anova_accuracy <- aov(Accuracy ~ LULC_Class, data = accuracy_data)
summary(anova_accuracy)

# Visualize accuracy per LULC class
library(ggplot2)
ggplot(accuracy_data, aes(x = LULC_Class, y = Accuracy)) +
  geom_boxplot() +
  labs(title = "Accuracy Distribution by LULC Class", x = "LULC Class", y = "Accuracy") +
  theme_minimal()







# Create the final data frame for analysis
accuracy_data <- data.frame(Accuracy = ifelse(lulc_values == gt_values, 1, 0),
                            LULC_Class = factor(lulc_values))

# Perform ANOVA to test for differences in accuracy between LULC classes
anova_accuracy <- aov(Accuracy ~ LULC_Class, data = accuracy_data)
summary(anova_accuracy)
# Load ggplot2 for visualization
library(ggplot2)

# Create a boxplot for accuracy distribution across LULC classes
ggplot(accuracy_data, aes(x = LULC_Class, y = Accuracy, fill = LULC_Class)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Boxplot with outliers
  geom_jitter(color = "black", width = 0.2, size = 1) +  # Add jitter for individual points
  labs(title = "Accuracy Distribution by LULC Class",
       x = "LULC Class",
       y = "Accuracy") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove the legend


library(ggplot2)

# Create a boxplot for accuracy distribution across LULC classes
ggplot(accuracy_data, aes(x = LULC_Class, y = Accuracy, fill = LULC_Class)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Boxplot with outliers
  geom_jitter(color = "black", width = 0.2, size = 1) +  # Add jitter for individual points
  labs(title = "Accuracy Distribution by LULC Class",
       x = "LULC Class",
       y = "Accuracy") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 6),  # Rotate x-axis labels and increase size
        axis.title.x = element_text(size = 6),  # Increase size of x-axis title
        axis.title.y = element_text(size = 6),  # Increase size of y-axis title
        legend.position = "none")  # Remove the legend

library(ggplot2)

# Create a boxplot for accuracy distribution across LULC classes
ggplot(accuracy_data, aes(x = LULC_Class, y = Accuracy, fill = LULC_Class)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Boxplot with outliers
  geom_jitter(color = "black", width = 0.2, size = 1) +  # Add jitter for individual points
  labs(title = "Accuracy Distribution by LULC Class",
       x = "LULC Class",
       y = "Accuracy") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Adjust x-axis label rotation and size
        axis.title.x = element_text(size = 10),  # Increase size of x-axis title
        axis.title.y = element_text(size = 10),  # Increase size of y-axis title
        legend.position = "none")  # Remove the legend


library(ggplot2)

# Create a boxplot for accuracy distribution across LULC classes
ggplot(accuracy_data, aes(x = LULC_Class, y = Accuracy, fill = LULC_Class)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Boxplot with outliers
  geom_jitter(color = "black", width = 0.2, size = 1) +  # Add jitter for individual points
  labs(title = "Accuracy Distribution by LULC Class",
       x = "LULC Class",
       y = "Accuracy") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Adjust x-axis label rotation and size
        axis.title.x = element_text(size = 10),  # Increase size of x-axis title
        axis.title.y = element_text(size = 10),  # Increase size of y-axis title
        legend.position = "none")  # Remove the legend



library(ggplot2)

# Create a boxplot for accuracy distribution across LULC classes
ggplot(accuracy_data, aes(x = LULC_Class, y = Accuracy, fill = LULC_Class)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Boxplot with outliers
  geom_jitter(color = "black", width = 0.2, size = 1) +  # Add jitter for individual points
  labs(title = "Accuracy Distribution by LULC Class",
       x = "LULC Class",
       y = "Accuracy") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Adjust x-axis label rotation and size
        axis.title.x = element_text(size = 10),  # Increase size of x-axis title
        axis.title.y = element_text(size = 10),  # Increase size of y-axis title
        legend.position = "top left",  # Remove the legend
        plot.margin = margin(10, 10, 10, 10))  # Increase margins around the plot



str(accuracy_data)
table(accuracy_data$LULC_Class)
unique(predicted_raster)
unique_classes <- unique(predicted_raster)
print(unique_classes)
unique(predicted_raster$`Florence prediction_1`)
sen_le
values(predicted_raster)
levels(predicted_raster)
unique(training_sites$land_cover_class)  # LULC classes
unique(training_sites$DN)  # Numeric values used in training

str(trainDat)
# Extract the AOA mask (1 = inside AOA, 0 = outside AOA)
aoa_mask <- aoa_raster_florence$AOA

# Create a mask for areas inside AOA
inside_aoa <- reclassified_lulc
inside_aoa[aoa_mask == 0] <- NA  # Set areas outside AOA to NA

# Calculate class-wise Area of Applicability
aoa_per_class <- table(values(inside_aoa), useNA = "no")

# Print the Area of Applicability for each LULC class
print(aoa_per_class)

library(terra)
library(sf)

# Load the raster objects
aoa_boundary <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")
lulc_raster <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")
reference_png <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH FLORENCE.png")

# Convert AOA boundary to binary raster (1 = inside AOA, 0 = outside)
aoa_boundary_binary <- ifel(aoa_boundary == 255, 1, 0)  

# Crop reference data to match LULC raster extent
reference_png_cropped <- crop(reference_png, lulc_raster)

# Correctly classify misclassifications **inside AOA**
inside_comparison <- ifel(aoa_boundary_binary == 1 & lulc_raster != reference_png_cropped, 1, NA)

# Correctly classify misclassifications **outside AOA**
outside_comparison <- ifel(aoa_boundary_binary == 0 & lulc_raster != reference_png_cropped, 1, NA)

# Define color: Red for misclassified areas
col_map <- c("red")  

# Open new window and plot misclassifications inside AOA
windows()
plot(inside_comparison, main="Misclassifications Inside AOA", col=col_map, legend=FALSE)
legend("topright", legend="Misclassified", fill="red")

# Open new window and plot misclassifications outside AOA
windows()
plot(outside_comparison, main="Misclassifications Outside AOA", col=col_map, legend=FALSE)
legend("topright", legend="Misclassified", fill="red")


library(terra)
library(sf)

# Load raster objects
aoa_boundary <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")
lulc_raster <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")
reference_png <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH FLORENCE.png")

# Convert AOA boundary to binary (1 = inside AOA, 0 = outside AOA)
aoa_boundary_binary <- ifel(aoa_boundary >= 200, 1, 0)

# Crop the reference raster to match the prediction raster
reference_png_cropped <- crop(reference_png, lulc_raster)

# Separate inside and outside AOA
aoa_inside <- mask(lulc_raster, aoa_boundary_binary)  # Inside AOA
aoa_outside <- mask(lulc_raster, ifel(aoa_boundary_binary == 1, NA, 1))  # Outside AOA
unique(values(aoa_boundary))

# **Compare with ground truth**
inside_misclassified <- ifel(!is.na(aoa_inside) & aoa_inside != reference_png_cropped, 1, NA)  
outside_misclassified <- ifel(!is.na(aoa_outside) & aoa_outside != reference_png_cropped, 1, NA)  

#Plot results
par(mfrow=c(1,2))  

# **Plot inside AOA misclassifications (Red only)**
plot(inside_misclassified, main = "Misclassifications Inside AOA", col = "red", legend = FALSE)

# **Plot outside AOA misclassifications (Red only)**
plot(outside_misclassified, main = "Misclassifications Outside AOA", col = "red", legend = FALSE)






library(terra)
library(sf)

# Load raster objects
aoa_boundary <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")
lulc_raster <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")
reference_png <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH FLORENCE.png")

# Identify inside AOA dynamically
inside_aoa_value <- (aoa_boundary > 200)  # Assume max value represents inside AOA

# Convert AOA boundary to binary
aoa_boundary_binary <- ifel(aoa_boundary == inside_aoa_value, 1, 0)

# Crop the reference raster to match the prediction raster
reference_png_cropped <- crop(reference_png, lulc_raster)

# **Separate inside and outside AOA**
aoa_inside <- mask(lulc_raster, aoa_boundary_binary)  # Inside AOA
aoa_outside <- mask(lulc_raster, ifel(aoa_boundary_binary == 1, NA, 1))  # Outside AOA

# **Compare with ground truth**
inside_misclassified <- ifel(!is.na(aoa_inside) & !is.na(reference_png_cropped) & aoa_inside != reference_png_cropped, 1, NA)  
outside_misclassified <- ifel(!is.na(aoa_outside) & !is.na(reference_png_cropped) & aoa_outside != reference_png_cropped, 1, NA)  

# **Plot results**
par(mfrow=c(1,2))  

# Plot inside AOA misclassifications (Red only)
plot(inside_misclassified, main = "Misclassifications Inside AOA", col = "red", legend = FALSE)

#Plot outside AOA misclassifications (Red only)
plot(outside_misclassified, main = "Misclassifications Outside AOA", col = "red", legend = FALSE)






library(terra)
library(sf)

# Step 1: Load raster objects
aoa_boundary <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence aoa.tiff")
lulc_raster <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/Florence prediction.tiff")
reference_png <- terra::rast("C:/Users/Nivethitha/OneDrive/Desktop/Thesis/GROUND TRUTH FLORENCE.png")

# Step 2: Debug - Check unique values in AOA raster
unique_values <- unique(values(aoa_boundary))
print(unique_values)  # Verify actual values present

# Convert AOA boundary image into binary raster (1 for inside AOA, 0 for outside)
#aoa_boundary_binary <- ifel(aoa_boundary == max(unique_values, na.rm = TRUE), 1, 0)  
aoa_boundary_binary <- aoa_boundary / 255  # Values range 0-1

# Step 3: Crop the reference data to match LULC raster (no resampling)
reference_png_cropped <- crop(reference_png, lulc_raster)

# Step 4: Mask LULC raster to get inside AOA predictions
aoa_inside <- mask(lulc_raster, aoa_boundary_binary)  

# Create inverse mask for outside AOA
aoa_outside_binary <- ifel(aoa_boundary_binary == 1, NA, 1)
aoa_outside <- mask(lulc_raster, aoa_outside_binary)

# Step 5: Misclassification Analysis
inside_comparison <- ifel(!is.na(aoa_inside) & aoa_inside == reference_png_cropped, 1, 0)
outside_comparison <- ifel(!is.na(aoa_outside) & aoa_outside == reference_png_cropped, 1, 0)
windows()
# Step 6: Plot the misclassifications
par(mfrow=c(1,2))  

plot(inside_comparison, main = "Misclassifications Inside AOA", col = c("red", "white"), legend = FALSE)
legend("topleft", legend = c("Misclassified", "Correct"), fill = c("red", "white"))
windows()
plot(outside_comparison, main = "Misclassifications Outside AOA", col = c("white","red"), legend = FALSE)
legend("topleft", legend = c("Misclassified", "Correct"), fill = c("red", "white"))
hist(values(aoa_boundary), main="AOA Value Distribution")










# CODE TO CALCULATE THE AOA
# Load the Uppsala raster as a terra object
sen_le <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Uppsala_Stack.grd")
print(sen_le)

# RGB plotting using terra for Uppsala
plotRGB(sen_le, r = 3, g = 2, b = 1, stretch = "lin")




# Load the Florence raster
sen_es <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Florence_Stack.grd")
# RGB plotting using terra for Florence
plotRGB(sen_es, r = 3, g = 2, b = 1, stretch = "lin")

# Load the training sites
trainSites <- st_read("C:/Users/Nivethitha/Documents/uppsalatrain.gpkg")
print(trainSites)

# Stack the RGB bands into a single SpatRaster for Uppsala
rgb_raster_le <- c(sen_le[[3]], sen_le[[2]], sen_le[[1]])

# Simplify the legend by setting fewer breaks for mapview
breaks <- seq(0, 3000, length.out = 10)  # Adjust the number of breaks as needed

# Display the RGB raster and training sites in mapview with simplified legend
mapview(rgb_raster_le, rgb = TRUE, at = breaks, map.types = "Esri.WorldImagery", legend = TRUE) +
  mapview(trainSites)

# Extract values from terra object for Uppsala
extr <- extract(sen_le, vect(trainSites), df=TRUE)
extr <- merge(extr, trainSites, by.x="ID", by.y="PolygonID")
head(extr)

# Sample training data
set.seed(100)
trainids <- createDataPartition(extr$ID, list=FALSE, p=0.05)
trainDat <- extr[trainids,]

predictors <- names(sen_le)
response <- "Label"

# Create spatial folds
indices <- CreateSpacetimeFolds(trainDat, spacevar = "ID", k=3, class="Label")
ctrl <- trainControl(method="cv", index = indices$index, savePredictions = TRUE)

# Train the Random Forest model
set.seed(100)
model <- ffs(trainDat[,predictors], trainDat[,response], method="rf", metric="Kappa", trControl=ctrl, importance=TRUE, ntree=75)
print(model)
plot(varImp(model))

# Get cross-validated predictions
cvPredictions <- model$pred[model$pred$mtry == model$bestTune$mtry,]
table(cvPredictions$pred, cvPredictions$obs)



# Create the confusion matrix
confusion_matrix <- table(cvPredictions$obs, cvPredictions$pred)

# Check the confusion matrix
print(confusion_matrix)

# Calculate overall accuracy
overall_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Overall Accuracy: ", overall_accuracy))

# Calculate class-specific accuracies
class_accuracies <- diag(confusion_matrix) / rowSums(confusion_matrix)
class_accuracies[is.nan(class_accuracies)] <- 0  # Replace NaN with 0 for classes with no predictions

# Print class-specific accuracies
class_names <- colnames(confusion_matrix)  # Use column names for labeling
class_accuracy_df <- data.frame(Class = class_names, Accuracy = class_accuracies)
print(class_accuracy_df)


# Predict using the terra object for Uppsala
prediction <- predict(sen_le, model, na.rm = TRUE)
cols <- c("yellow", "darkgreen", "lightgreen", "darkorange", "brown", "red", "blue")

# Plot the prediction for Uppsala using tmap
pred_le<-tm_shape(prediction) +
  tm_raster(palette = cols, title = "Land Use Classification") +
  tm_scale_bar(bg.color = "white", bg.alpha = 0.75) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.75)
pred_le
# Calculate Area of Applicability (AOA) for Uppsala
cl <- makeCluster(4)
registerDoParallel(cl)
AOA <- aoa(sen_le, model, useWeight = FALSE)


summary(AOA$AOA)

highAOA <- AOA$AOA >= 1
tm_shape(highAOA) + 
  tm_raster(palette = "Blues", title = "High Applicability Areas") +
  tm_layout(main.title = "Filtered AOA (>= 1)")



# Create a mask for areas where AOA = 1 (valid prediction areas)
prediction_aoa <- mask(prediction, AOA$AOA == 1)

# Create a mask for areas where AOA = 0 (grey areas)
aoa_grey <- ifel(AOA$AOA == 0, 1, NA)  # Areas with AOA = 0 will be grey

# Plotting with custom legend for AOA
pred_aoa_le <- tm_shape(prediction_aoa) +
  tm_raster(palette = cols, title = "Land Use Classification (AOA = 1)", legend.show = TRUE) +
  tm_shape(aoa_grey) +
  tm_raster(palette = "grey", alpha = 1.0, title = "", legend.show = FALSE) +  # Increased opacity for AOA grey areas
  tm_scale_bar(bg.color = "white", bg.alpha = 0.75) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.75) +
  tm_add_legend(type = "fill", labels = c("AOA = 0"), col = "grey", alpha = 0.8)  # Custom legend entry with higher alpha

pred_aoa_le




tmap_arrange(pred_le,pred_aoa_le,ncol=2)


# Predict using the Florence raster
prediction_es <- predict(sen_es, model, na.rm=TRUE)
cols <- c("yellow", "darkgreen", "lightgreen", "darkorange", "brown", "red", "blue")
windows()
# Plot the prediction for Uppsala using tmap
plot(prediction_es, main = "Land use prediction", col = c("yellow", "darkgreen", "lightgreen", "darkorange", "brown", "red", "blue"))



# Load the required libraries
library(terra)
library(dplyr)

# Load the Florence raster
sen_es <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Florence_Stack.grd")

# Load the training sites for Florence
testSites_es <- st_read("C:/Users/Nivethitha/Documents/florencevector.gpkg")  # New GeoPackage

# Extract values from the Florence raster for the training sites
extr_es <- extract(sen_es, vect(testSites_es), df = TRUE)

# Merge extracted values with the labels from the training sites
extr_es <- merge(extr_es, testSites_es[, c("PolygonID", "Label")], by.x = "ID", by.y = "PolygonID", all.x = TRUE)

# Check the structure of extr_es after merging
print(names(extr_es))
head(extr_es)  # Inspect the first few rows to ensure it includes Label
# Predict using the model on the extracted values
extr_es$predicted <- predict(model, newdata = extr_es)
# Calculate confusion matrix for Florence
confusion_matrix_es <- table(extr_es$Label, extr_es$predicted)

# Print the confusion matrix
print(confusion_matrix_es)

# Calculate overall accuracy
overall_accuracy_es <- sum(diag(confusion_matrix_es), na.rm = TRUE) / sum(confusion_matrix_es, na.rm = TRUE)

# Calculate class-specific accuracies
class_accuracies_es <- diag(confusion_matrix_es) / rowSums(confusion_matrix_es, na.rm = TRUE)

# Print results
print(paste("Overall Accuracy: ", overall_accuracy_es))
print("Class-Specific Accuracies:")
print(class_accuracies_es)




# Calculate AOA for Florence
AOA_es <- aoa(sen_es, model, useWeight = FALSE)

# Create a mask for areas where AOA = 1 (valid prediction areas)
prediction_aoa <- mask(prediction_es, AOA_es$AOA == 1)

# Create a mask for areas where AOA = 0 (grey areas)
aoa_grey <- ifel(AOA_es$AOA == 0, 1, NA)  # Areas with AOA = 0 will be grey
# Create a final raster to display predictions only where AOA = 1
#final_prediction_map <- ifel(AOA_es$AOA == 1, prediction_es, NA)
windows()
# Plotting with custom legend for AOA
pred_aoa_es<-tm_shape(prediction_aoa) +
  tm_raster(palette = cols, title = "Land Use Classification (AOA = 1)", legend.show = FALSE) +
  tm_shape(aoa_grey) +
  tm_raster(palette = "grey", alpha = 1.0, title = "", legend.show = FALSE)   # No legend for AOA grey areas
pred_aoa_es
tmap_arrange(pred_es,pred_aoa_es,ncol=2)




