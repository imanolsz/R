# Clear Environment
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Clear plots & console
if(!is.null(dev.list())) dev.off()
cat("\014") 

# Install required packages
library(ggplot2)
library(lattice)
library(caret)

# Read data
filename = "../data/concrete.csv"
data <- read.csv(file=filename, sep=";", header = TRUE)

par(mfrow = c(2,4), mar=c(1,1,1,1))

# Scatter Plot - Check linear relationships
for (col_name in colnames(data)) {
  if (col_name != "Concrete.compressive.strength") {
    scatter.smooth(x=data$Concrete.compressive.strength, y=data[[col_name]], main=col_name, col="lightgreen")
  }
}

# Correlation between variables
print("Correlation between each attribute and Concrete.compressive.strength: A low correlation (-0.2 < x < 0.2)", quote=FALSE)

for (col_name in colnames(data)) {
  print(paste0(col_name, ": ", cor(data$Concrete.compressive.strength, data[[col_name]])), quote=FALSE)
}
#Dependencia y correlación en un único gráfico
for (col_name in colnames(data)) {
  correlation <- round(cor(data$Concrete.compressive.strength, data[[col_name]]), 2)
  label <- paste0("Correlation X & Y = ", correlation) 
    print(ggplot(data, aes(y=Concrete.compressive.strength,x=data[[col_name]])) + geom_point()
     + geom_smooth(method = "lm", se = FALSE) + 
      labs(title = label, y="Concrete.compressive.strength", x=col_name))
}

# Percentage of training examples
training_p <- 0.7

# Generate data partition 70% training / 30% test. The result is a vector with 
# the indexes of the examples that will be used for the training of the model.
training_samples <- createDataPartition(y = data$Concrete.compressive.strength, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_samples, ]
test_data     <- data[-training_samples, ]

# Create Linear Model using training data. Formula = all the columns except Concrete.compressive.strength
best_model <- lm(formula = training_data$Concrete.compressive.strength ~., data = training_data)
for (i in 1:10){
  model <- lm(formula = training_data$Concrete.compressive.strength ~., data = training_data)
}
# Make the prediction using the model and test data
prediction <- predict(model, test_data)

# Calculate Mean Average Error
mean_avg_error <- mean(abs(prediction - test_data$Concrete.compressive.strength))
if (mean_avg_error < 0) {
  best_model <- model
}
# Print Mean Absolute Error
print(paste0("- Mean average error: ", mean_avg_error))

# Print model summary
summary(best_model)
print(model)
#########################
print("Dada una muestra cualquiera, ¿Cuántas unidades deben añadirse o sustraerse para que la resistencia aumente 10 puntos?")
mean_prediction <- mean(predict(best_model, data))
difference <- (10 - mean_prediction) / coef(best_model)["Water"]
print(paste0(difference, " unidades de agua"))
#########################
print("Dada una muestra cualquiera, ¿Cuántas unidades deben añadirse o sustraerse para que la resistencia aumente 10 puntos?")
data_modified <- data
data_modified$Superplasticizer <- NULL
columns <- names(data_modified)
formula <- paste("Concrete.compressive.strength ~", paste(columns, collapse = " + "))
best_model_modified <- lm(formula = formula, data = data_modified)
prediction_modified <- predict(best_model_modified, newdata = data_modified)
difference <- data$Concrete.compressive.strength - prediction_modified
max_reduction_sample <- data[which.max(difference), ]
print(max_reduction_sample)
#########################
print("¿Cuales son las 3 muestras que aumentan mas su resistencia añadiendo 5 unidades de superplasticizer?")
data_modified <- data
data_modified$Superplasticizer <- data_modified$Superplasticizer + 5
prediction_modified <- predict(best_model, newdata = data_modified)
difference <- prediction_modified - data$Concrete.compressive.strength
top_3_samples <- data[order(difference, decreasing = TRUE), ][1:3, ]
cat("Las mejores 3 muestras son: ")
print(names(top_3_samples[1:3]))

