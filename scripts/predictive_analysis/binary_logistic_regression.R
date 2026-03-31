# Load data
data <- read.csv("C:\\Users\\Thism\\Desktop\\Sliit\\3rd-year\\2nd-sem\\TPSM\\Assignment\\data\\cleaned_data.csv")

# Satisfaction_Category is already in the dataset
# Convert to binary numeric: High = 1, Low = 0
data$Satisfaction_Binary <- ifelse(data$Satisfaction_Category == "High", 1, 0)

# Set seed and split (same split as before for consistency)
set.seed(42)
trainID <- sample(1:nrow(data), round(0.7 * nrow(data)))
train <- data[trainID, ]
test  <- data[-trainID, ]


# Binary logistic regression using glm with binomial family
binary_model <- glm(Satisfaction_Binary ~ Respect_Index + EnvironmentSatisfaction + 
                      WorkLifeBalance + Age + YearsAtCompany + JobLevel,
                    data = train, family = binomial)


summary(binary_model)

#Estimate says that if the predictor changes this is the probability of dependant variable change same as the predictor.
#Other ones same as metric explanation in SLR script.
#Deviation is the unexplained variation(lower better). How far the models predictions are from actual ones(given as probabilities)
#null deviation is predicting only using intercept and value is 1361.
#Residual deviation is 1358.5: not only intercept all predictors present.(Deviance only reduced by 3.1)
#Model converged on 4 iterations.