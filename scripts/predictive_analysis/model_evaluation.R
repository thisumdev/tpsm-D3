#SLR - Predicted by Respect_Index
simple_model <- lm(JobSatisfaction ~ Respect_Index, data = train)
summary(simple_model)

# Multiple Linear Regression — all selected predictors
multiple_model <- lm(JobSatisfaction ~ Respect_Index + EnvironmentSatisfaction +
                     WorkLifeBalance + Age + 
                       YearsAtCompany + JobLevel, data = train)
summary(multiple_model)



# Simple model predictions
y_pred_simple <- predict(simple_model, test)

# Multiple model predictions
y_pred_multiple <- predict(multiple_model, test)

# Actual values
y_actual <- test$JobSatisfaction

# Simple model evaluation
MSE_simple <- mean((y_actual - y_pred_simple)^2)
RMSE_simple <- sqrt(MSE_simple)

# Multiple model evaluation
MSE_multiple <- mean((y_actual - y_pred_multiple)^2)
RMSE_multiple <- sqrt(MSE_multiple)

# Print results
cat("Simple Model MSE:", MSE_simple, "\n")
cat("Simple Model RMSE:", RMSE_simple, "\n\n")
cat("Multiple Model MSE:", MSE_multiple, "\n")
cat("Multiple Model RMSE:", RMSE_multiple, "\n")


#MSE: SLR model - 1.158779: Average squared error of actual and predictions
     #: MLR - 1.163834: Worse than SLR

#RMSE: SLR - 1.076466: Models prediction is off by 1.08 points a large error as the scale is 1-4
      #MLR - 1.078812: Same as above