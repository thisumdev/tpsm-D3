data <- read.csv("C://Users//navoo//Downloads//TPSM project//cleaned_data(in).csv")

#A. Load data and inspect structure
# View first few rows
head(data)

# Check dimensions (no. of rows and columns)
dim(data)

# Check structure(column and data type)
str(data)

# Check column names
colnames(data)


#B. Respect_Index — summary statistics(mean, median, SD, minimum, maximum)

# Summary statistics for Respect_Index
mean(data$Respect_Index)
median(data$Respect_Index)
sd(data$Respect_Index)
min(data$Respect_Index)
max(data$Respect_Index)

# All together in one table
respect_summary <- data.frame(
  Mean = mean(data$Respect_Index),
  Median = median(data$Respect_Index),
  SD = sd(data$Respect_Index),
  Min = min(data$Respect_Index),
  Max = max(data$Respect_Index)
)

respect_summary

# Histogram with density curve for Respect_Index
hist(data$Respect_Index,
     probability = TRUE,
     main = "Histogram of Respect Index",
     xlab = "Respect Index",
     col = "lightblue",
     border = "black")




# Shapiro-Wilk normality test for Respect_Index
shapiro.test(data$Respect_Index)

# Q-Q plot for Respect_Index
#Check if data is normally distributed
#Hypotheses:
  #H₀: data is normal
  #H₁: data is not normal

#Shapiro test gives a statistical decision (p-value)
#Q-Q plot gives a visual check of normality
#Using both gives stronger justification

qqnorm(data$Respect_Index,
       main = "Q-Q Plot of Respect Index")
qqline(data$Respect_Index, col = "red", lwd = 2)


# Frequency table for JobSatisfaction
job_freq <- table(data$JobSatisfaction)
job_freq

# Combined frequency table
job_table <- data.frame(
  JobSatisfaction = names(job_freq),
  Count = as.vector(job_freq)
)

job_table

# Bar plot for JobSatisfaction
bar_positions <- barplot(job_freq,
                         main = "Bar Chart of Job Satisfaction",
                         xlab = "Job Satisfaction Level",
                         ylab = "Count",
                         col = "seagreen",
                         ylim = c(0, max(job_freq) + 100))



# Shapiro-Wilk normality test for JobSatisfaction
shapiro.test(data$JobSatisfaction)

# Q-Q plot for JobSatisfaction
qqnorm(data$JobSatisfaction,
       main = "Q-Q Plot of Job Satisfaction")
qqline(data$JobSatisfaction, col = "red", lwd = 2)



# Summary statistics for supporting numeric variables(MonthlyIncome, Age, TotalWorkingYears, YearsAtCompany)
numeric_summary <- data.frame(
  Variable = c("MonthlyIncome", "Age", "TotalWorkingYears", "YearsAtCompany"),
  Mean = c(mean(data$MonthlyIncome),
           mean(data$Age),
           mean(data$TotalWorkingYears),
           mean(data$YearsAtCompany)),
  
  Median = c(median(data$MonthlyIncome),
             median(data$Age),
             median(data$TotalWorkingYears),
             median(data$YearsAtCompany)),
  
  SD = c(sd(data$MonthlyIncome),
         sd(data$Age),
         sd(data$TotalWorkingYears),
         sd(data$YearsAtCompany))
)

numeric_summary

#Histogram for MonthlyIncome
hist(data$MonthlyIncome,
     main = "Histogram of Monthly Income",
     xlab = "Monthly Income",
     col = "lightpink",
     border = "black")

#Histogram for Age
hist(data$Age,
     main = "Histogram of Age",
     xlab = "Age",
     col = "lightyellow",
     border = "black")


# Department frequency
dept_freq <- table(data$Department)
dept_table <- data.frame(
  Department = names(dept_freq),
  Count = as.vector(dept_freq)
)
dept_table

# OverTime frequency
ot_freq <- table(data$OverTime)
ot_table <- data.frame(
  OverTime = names(ot_freq),
  Count = as.vector(ot_freq)
)
ot_table

# MaritalStatus frequency
marital_freq <- table(data$MaritalStatus)
marital_table <- data.frame(
  MaritalStatus = names(marital_freq),
  Count = as.vector(marital_freq)
)
marital_table

#Bar chart for Department
barplot(dept_freq,
        main = "Department Distribution",
        xlab = "Department",
        ylab = "Count",
        col = "orange")

#Bar chart for MaritalStatus
barplot(marital_freq,
        main = "Marital Status Distribution",
        xlab = "Marital Status",
        ylab = "Count",
        col = "violet")

#Bar chart for OverTime
barplot(ot_freq,
        main = "OverTime Distribution",
        xlab = "OverTime",
        ylab = "Count",
        col = "skyblue")

# Boxplot for Respect_Index
boxplot(data$Respect_Index,
        main = "Boxplot of Respect Index",
        col = "lightblue")

# Boxplot for MonthlyIncome
boxplot(data$MonthlyIncome,
        main = "Boxplot of Monthly Income",
        col = "lightgreen")

# Boxplot for Age
boxplot(data$Age,
        main = "Boxplot of Age",
        col = "lightpink")

# Boxplot for TotalWorkingYears
boxplot(data$TotalWorkingYears,
        main = "Boxplot of Total Working Years",
        col = "lightyellow")



#C. Bivariant Analysis

# Scatter plot
plot(data$Respect_Index, data$JobSatisfaction,
     main = "Respect Index vs Job Satisfaction",
     xlab = "Respect Index",
     ylab = "Job Satisfaction",
     col = "blue",
     pch = 16)

# Add trend line
abline(lm(JobSatisfaction ~ Respect_Index, data = data), col = "red", lwd = 2)

# Correlation (descriptive only)
cor(data$Respect_Index, data$JobSatisfaction)

# Boxplot (cleaner comparison)
boxplot(Respect_Index ~ JobSatisfaction, data = data,
        main = "Respect Index across Job Satisfaction Levels",
        xlab = "Job Satisfaction",
        ylab = "Respect Index",
        col = "lightpink")


# Boxplot
boxplot(JobSatisfaction ~ OverTime, data = data,
        main = "Job Satisfaction by OverTime",
        xlab = "OverTime",
        ylab = "Job Satisfaction",
        col = c("lightblue", "lightgreen"))



# Scatter plot
plot(data$MonthlyIncome, data$JobSatisfaction,
     main = "Monthly Income vs Job Satisfaction",
     xlab = "Monthly Income",
     ylab = "Job Satisfaction",
     col = "darkgreen",
     pch = 16)

# Trend line
abline(lm(JobSatisfaction ~ MonthlyIncome, data = data), col = "red", lwd = 2)

# Correlation
cor(data$MonthlyIncome, data$JobSatisfaction)

# Quartiles and IQR for Respect_Index
quantile(data$Respect_Index)
IQR(data$Respect_Index)

# MonthlyIncome
quantile(data$MonthlyIncome)
IQR(data$MonthlyIncome)

# Age
quantile(data$Age)
IQR(data$Age)

#Summary of each attribute
summary(data$Respect_Index)

summary(data$JobSatisfaction)

summary(data$MonthlyIncome)

summary(data$Age)

summary(data$TotalWorkingYears)

summary(data$YearsAtCompany)

















