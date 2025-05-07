# Load necessary packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")

library(dplyr)
library(tidyr)
library(ggplot2)

# Read the data
data <- read.table("GTEx_dataset.gct", header = TRUE, sep = "\t", skip = 2)

# Remove the first two columns
data <- data[, -c(1, 2)]

# Convert the data to numeric
data <- data.frame(lapply(data, function(x) type.convert(as.character(x), as.is = TRUE)))

# Convert the scaled data back to a data frame
data <- as.data.frame(data)

# Calculate the correlation coefficient between Lung and Liver
correlation_coefficient <- cor(data$Lung, data$Liver)
print(paste("Correlation coefficient between Lung and Liver:", correlation_coefficient))

# Create a simple linear regression plot using ggplot2 for Liver and Lung tissues
ggplot(data, aes(x = Liver, y = Lung)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(
    title = "Linear Regression of Lung vs Liver",
    x = "Liver (Scaled Expression Levels)",
    y = "Lung (Scaled Expression Levels)",
  )

# Calculate the correlation coefficient between Spleen and Stomach
correlation_coefficient <- cor(data$Spleen, data$Stomach)
print(paste("Correlation coefficient between Spleen and Stomach:", correlation_coefficient))

# Create a simple linear regression plot using ggplot2 for Spleen and Stomach tissues
ggplot(data, aes(x = Spleen, y = Stomach)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(
    title = "Linear Regression of Stomach vs Spleen",
    x = "Spleen (Scaled Expression Levels)",
    y = "Stomach (Scaled Expression Levels)",
  )

#Define a function to calculate RMSE
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Fit linear regression models and calculate RMSE for Lung vs Liver
model_lung_liver <- lm(Lung ~ Liver, data = data)
predictions_lung_liver <- predict(model_lung_liver, data)
rmse_lung_liver <- calculate_rmse(data$Lung, predictions_lung_liver)
print(paste("RMSE for Lung vs Liver:", rmse_lung_liver))

# Fit linear regression models and calculate RMSE for Spleen vs Stomach
model_spleen_stomach <- lm(Stomach ~ Spleen, data = data)
predictions_spleen_stomach <- predict(model_spleen_stomach, data)
rmse_spleen_stomach <- calculate_rmse(data$Stomach, predictions_spleen_stomach)
print(paste("RMSE for Spleen vs Stomach:", rmse_spleen_stomach))

