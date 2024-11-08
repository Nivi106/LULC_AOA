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

# Create a final raster to display predictions only where AOA = 1
final_prediction_map <- ifel(AOA$AOA == 1, prediction, NA)

# Plotting with custom legend for AOA
pred_aoa_le<-tm_shape(final_prediction_map) +
  tm_raster(palette = cols, title = "Land Use Classification (AOA = 1)", legend.show = TRUE) +
  tm_shape(aoa_grey) +
  tm_raster(palette = "grey", alpha = 0.5, title = "", legend.show = FALSE) +  # No legend for AOA grey areas
  tm_scale_bar(bg.color = "white", bg.alpha = 0.75) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.75) +
  tm_add_legend(type = "fill", labels = c("AOA = 0"), col = "grey", alpha = 0.5)  # Custom legend entry

pred_aoa_le
first_band_le <- sen_le[[1]]
first_band_plot_le <- tm_shape(first_band_le) +
  tm_raster(title = "First Band - Uppsala", palette = "viridis") +
  tm_layout(main.title = "First Band - Uppsala")


tmap_arrange(pred_le,pred_aoa_le,first_band_plot_le,ncol=2)


# Predict using the Florence raster
prediction_es <- predict(sen_es, model, na.rm=TRUE)
cols <- c("yellow", "darkgreen", "lightgreen", "darkorange", "brown", "red", "blue")

# Plot the prediction for Uppsala using tmap
pred_es<-tm_shape(prediction_es) +
  tm_raster(palette = cols, title = "Land Use Classification") +
  tm_scale_bar(bg.color = "white", bg.alpha = 0.75) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.75)
pred_es



# Load the required libraries
library(terra)
library(dplyr)

# Load the Florence raster
sen_es <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Florence_Stack.grd")

# Load the training sites for Florence
trainSites_es <- st_read("C:/Users/Nivethitha/Documents/florencevector.gpkg")  # New GeoPackage

# Extract values from the Florence raster for the training sites
extr_es <- extract(sen_es, vect(trainSites_es), df = TRUE)

# Merge extracted values with the labels from the training sites
extr_es <- merge(extr_es, trainSites_es[, c("PolygonID", "Label")], by.x = "ID", by.y = "PolygonID", all.x = TRUE)

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
final_prediction_map <- ifel(AOA_es$AOA == 1, prediction_es, NA)

# Plotting with custom legend for AOA
pred_aoa_es<-tm_shape(final_prediction_map) +
  tm_raster(palette = cols, title = "Land Use Classification (AOA = 1)", legend.show = TRUE) +
  tm_shape(aoa_grey) +
  tm_raster(palette = "grey", alpha = 0.5, title = "", legend.show = FALSE) +  # No legend for AOA grey areas
  tm_scale_bar(bg.color = "white", bg.alpha = 0.75) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.75) +
  tm_add_legend(type = "fill", labels = c("AOA = 0"), col = "grey", alpha = 0.5)  # Custom legend entry


pred_aoa_es
first_band_es <- sen_es[[1]]
first_band_plot_es <- tm_shape(first_band_es) +
  tm_raster(title = "First Band - Florence", palette = "viridis") +
  tm_layout(main.title = "First Band - Florence")


tmap_arrange(pred_es,pred_aoa_es,first_band_plot_es,ncol=2)










