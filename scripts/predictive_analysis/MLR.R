# Multiple Linear Regression — all selected predictors
multiple_model <- lm(JobSatisfaction ~ Respect_Index + EnvironmentSatisfaction 
                        + MonthlyIncome + Age + 
                       YearsAtCompany + JobLevel, data = train)
summary(multiple_model)

