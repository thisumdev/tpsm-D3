data <- read.csv("C:\\Users\\Thism\\Desktop\\Sliit\\3rd-year\\2nd-sem\\TPSM\\Assignment\\data\\cleaned_data.csv")

#For reproducibility
set.seed(42)

#70/30 split
trainID <- sample(1:nrow(data), round(0.7 * nrow(data)))
train <- data[trainID, ]
test  <- data[-trainID, ]


#Assumption 1:Linearity
plot(train$Respect_Index, train$JobSatisfaction,
     main = "Scatterplot of Respect_Index vs JobSatisfaction",
     xlab = "Respect_Index",
     ylab = "JobSatisfaction",
     pch = 16, col = "steelblue")

abline(lm(JobSatisfaction ~ Respect_Index, data = train), col = "red")

#Assumption 2: Independence
# Multiple Linear Regression — all selected predictors
multiple_model <- lm(JobSatisfaction ~ Respect_Index + EnvironmentSatisfaction 
                     + MonthlyIncome + Age + 
                       YearsAtCompany + JobLevel, data = train)
summary(multiple_model)

library(lmtest)

dwtest(multiple_model)


#Assumption 3:Homoescedasticity

plot(fitted(multiple_model), residuals(multiple_model),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16, col = "steelblue")

abline(h = 0, col = "red")


#Assumption 4: Normality of Residuals
qqnorm(residuals(multiple_model), main = "Normal Q-Q Plot of Residuals")
qqline(residuals(multiple_model), col = "red")

# Shapiro-Wilk test
shapiro.test(residuals(multiple_model))

#Assumption 4: Multicollinearity
library(car)

vif(multiple_model)
