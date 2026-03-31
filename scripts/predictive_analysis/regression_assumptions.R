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
                     + JobLevel + Age + 
                       YearsAtCompany + WorkLifeBalance, data = train)


library(lmtest)
#Package containing Durbin-Watson test

dwtest(multiple_model)
#DW value is 2.0314 (should be close to 2 confirming assumption of no autocorrelation)
#P-value is high failing to reject null hypothesis confirming no auto-correlation

#Assumption 3:Homoscedasticity

plot(fitted(multiple_model), residuals(multiple_model),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16, col = "steelblue")

abline(h = 0, col = "red")
#Plot shows 4 levels of dots showing the ordinal variable levels. Therefore this assumption cannnot be confirmed due to the structural limitation

#Assumption 4: Normality of Residuals
#Plots the residual quantiles with what would they look like if perfectly normally distributed
qqnorm(residuals(multiple_model), main = "Normal Q-Q Plot of Residuals")
#Draws reference diagonal line(Perfect normality)
qqline(residuals(multiple_model), col = "red")
#Severe deviation as residuals form 4 levels due to ordinal variable

# Shapiro-Wilk test. A test done to check normality
shapiro.test(residuals(multiple_model))
#w stat value is 0.861(if normal value is 1).
#But the p value is extremely low rejecting null hypothesis which is that the residuals are normally distributed.
#This is also due to the structural issue of the ordinal variable

#Assumption 5: Multicollinearity
#VIF function
library(car)

#Variation Inflation Factor
vif(multiple_model)
#This assumption is fully met as all values are between 1-2 which is in the acceptable range of VIF value(low correlation is acceptable).
