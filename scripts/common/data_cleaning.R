# Dataset is loaded and the top rows are viewed to get an idea
hr_data = read.csv("C:\\Users\\Thism\\Desktop\\Sliit\\3rd-year\\2nd-sem\\TPSM\\Assignment\\data\\engineered_data.csv")


head(hr_data)


#Inspection of the data structure
str(hr_data)
summary(hr_data)
dim(hr_data)
colnames(hr_data)


#Handling missing values
table(is.na(hr_data))
colSums(is.na(hr_data))
#No null data found so need for removing or imputations


#Detecting Duplicated Rows and fixing
duplicated(hr_data)
hr_data[duplicated(hr_data), ]
sum(duplicated(hr_data))
#No duplicated data found


#Dropping useless columns - If no variation or not statistically important they are dropped
hr_data$EmployeeCount = NULL
hr_data$StandardHours = NULL
hr_data$Over18 = NULL
hr_data$EmployeeNumber = NULL
#The reason dropping those is columns containing zero-variance or near-zero variance doesn't contribute to the analytics part as they have exaclty same value for every row resulting in less affect to the other features. Therefore, they are useless. In the predicitive analysis stage this is crucial as they can mess up the model because even the slight change of value in near-zero variance columns result in drastic changes of the coefficents(unstable) making other predictor coefficients less reliable.

colnames(hr_data)
dim(hr_data)

#Verifying data types of important columns
str(hr_data)

#Converting categorical columns to factors
hr_data$Attrition      = factor(hr_data$Attrition)
hr_data$Department     = factor(hr_data$Department)
hr_data$Gender         = factor(hr_data$Gender)
hr_data$OverTime       = factor(hr_data$OverTime)
hr_data$MaritalStatus  = factor(hr_data$MaritalStatus)
hr_data$JobRole        = factor(hr_data$JobRole)
hr_data$BusinessTravel = factor(hr_data$BusinessTravel)
hr_data$EducationField = factor(hr_data$EducationField)

str(hr_data)

#Checking th factor categorical columns
table(hr_data$Attrition)
table(hr_data$OverTime)
table(hr_data$Gender)
table(hr_data$Department)
table(hr_data$MaritalStatus)
table(hr_data$BusinessTravel)
#This is done because when using analytic functions they need a valued input and factor does it. When using for predictive parts wrong results are produced as they are taken as plain character strings and dummy variables are created by factors automatically by R resolving the issue. Also needed when constructing boxplots for outlier detection.

#Creating a binary column for the Attrition
hr_data$Attrition_Binary = ifelse(hr_data$Attrition == "Yes", 1, 0)

table(hr_data$Attrition_Binary)
head(hr_data[, c("Attrition", "Attrition_Binary")])

#Detecting outliers visually
boxplot(hr_data$Respect_Index,
        main = "Boxplot: Respect Index",
        col  = "lightblue")

boxplot(hr_data$MonthlyIncome,
        main = "Boxplot: Monthly Income",
        col  = "lightyellow")

boxplot(hr_data$Age,
        main = "Boxplot: Age",
        col  = "lightgreen")

boxplot(hr_data$TotalWorkingYears,
        main = "Boxplot: Total Working Years",
        col  = "pink")
#As there were no outliers in the important column Respect_Index no issue. Other columns are real world values therefore entirely plausible values are there handling them would result in misintepretation of values.


#Creating binary category columns(important for infernetial and predicitive part)
median_respect = median(hr_data$Respect_Index)
cat("Median Respect Index:", median_respect, "\n")

hr_data$Respect_Category = ifelse(hr_data$Respect_Index > median_respect,
                                  "High", "Low")
hr_data$Respect_Category = factor(hr_data$Respect_Category)

table(hr_data$Respect_Category)

hr_data$Satisfaction_Category = ifelse(hr_data$JobSatisfaction >= 3,
                                       "High", "Low")
hr_data$Satisfaction_Category = factor(hr_data$Satisfaction_Category)

table(hr_data$Satisfaction_Category)
#The reasons for this is Regression models need a numeric versions of factor columns. And also when doing inferential tests such as Chi-squared, discrete categories are needed, continous ones cannot be used. The continous columns are kept beacuse other tests such as t-tests, ANOVA, correlation needs continuous numeric values.


#Final checkup

dim(hr_data)
colnames(hr_data)
colSums(is.na(hr_data))
str(hr_data)
summary(hr_data)

cat("Min Respect_Index:", min(hr_data$Respect_Index), "\n")
cat("Max Respect_Index:", max(hr_data$Respect_Index), "\n")

#Saving the cleaned file
write.csv(hr_data, "C:\\Users\\Thism\\Desktop\\Sliit\\3rd-year\\2nd-sem\\TPSM\\Assignment\\data\\cleaned_data.csv", row.names = FALSE)
