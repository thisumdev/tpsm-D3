data <- read.csv("C://Users//navoo//Downloads//TPSM project//cleaned_data(in).csv")

#Load data and inspect structure
#View first few rows
head(data)

#Check dimensions (no. of rows and columns)
dim(data)

#Check structure(column and data type)
str(data)

#Check column names
colnames(data)


#Respect_Index — summary statistics(mean, median, SD, minimum, maximum)

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

#Histogram for Respect_Index
hist(data$Respect_Index,
     probability = TRUE,
     main = "Histogram of Respect Index",
     xlab = "Respect Index",
     col = "lightblue",
     border = "black")




# Frequency table for JobSatisfaction
job_freq <- table(data$JobSatisfaction)
job_freq

# Combined frequency table
job_table <-data.frame(
  JobSatisfaction =names(job_freq),
  Count =as.vector(job_freq)
)

job_table

# Bar plot for JobSatisfaction
bar_positions<- barplot(job_freq,
                         main = "Bar Chart of Job Satisfaction",
                         xlab = "Job Satisfaction Level",
                         ylab = "Count",
                         col = "seagreen",
                         ylim = c(0, max(job_freq) + 100))





# Summary statistics for supporting numeric variables(TrainingTimesLastYear, EnvironmentSatisfaction, WorkLifeBalance,
                                                      #Age, YearsAtCompany, JobLevel)
# Summary statistics for supporting numeric variables
numeric_summary <- data.frame(
  Variable = c("TrainingTimesLastYear", "EnvironmentSatisfaction", "WorkLifeBalance",
               "Age", "YearsAtCompany", "JobLevel"),
  
  Mean = c(mean(data$TrainingTimesLastYear),
           mean(data$EnvironmentSatisfaction),
           mean(data$WorkLifeBalance),
           mean(data$Age),
           mean(data$YearsAtCompany),
           mean(data$JobLevel)),
  
  Median = c(median(data$TrainingTimesLastYear),
             median(data$EnvironmentSatisfaction),
             median(data$WorkLifeBalance),
             median(data$Age),
             median(data$YearsAtCompany),
             median(data$JobLevel)),
  
  SD = c(sd(data$TrainingTimesLastYear),
         sd(data$EnvironmentSatisfaction),
         sd(data$WorkLifeBalance),
         sd(data$Age),
         sd(data$YearsAtCompany),
         sd(data$JobLevel))
)

numeric_summary


#Histogram for Respect_Index
hist(data$Respect_Index,
     main = "Histogram of Respect_Index",
     xlab = "Respect_Index",
     col = "blue",
     border = "black")

#Histogram for Training Times LastYear
hist(data$TrainingTimesLastYear,
     main = "Histogram of Training Times Last Year",
     xlab = "Training Times Last Year",
     col = "lightblue",
     border = "black")

#Histogram for Environment Satisfaction
hist(data$EnvironmentSatisfaction,
     main = "Histogram of Environment Satisfaction",
     xlab = "Environment Satisfaction",
     col = "lightgreen",
     border = "black")

#Histogram for WorkLife Balance
hist(data$WorkLifeBalance,
     main = "Histogram of Work Life Balance",
     xlab = "Work Life Balance",
     col = "lightpink",
     border = "black")

#Histogram for Age
hist(data$Age,
     main = "Histogram of Age",
     xlab = "Age",
     col = "lightyellow",
     border = "black")

#Histogram for Years at Company
hist(data$YearsAtCompany,
     main = "Histogram of Years at Company",
     xlab = "Years at Company",
     col = "seagreen",
     border = "black")

#Histogram for Job Level
hist(data$JobLevel,
     main = "Histogram of Job Level",
     xlab = "Job Level",
     col = "green",
     border = "black")




#Frequency tables

# TrainingTimesLastYear frequency
train_freq <- table(data$TrainingTimesLastYear)
train_table <- data.frame(
  TrainingTimesLastYear = names(train_freq),
  Count = as.vector(train_freq)
)
train_table

# EnvironmentSatisfaction frequency
env_freq <- table(data$EnvironmentSatisfaction)
env_table <- data.frame(
  EnvironmentSatisfaction = names(env_freq),
  Count = as.vector(env_freq)
)
env_table

# WorkLifeBalance frequency
wlb_freq <- table(data$WorkLifeBalance)
wlb_table <- data.frame(
  WorkLifeBalance = names(wlb_freq),
  Count = as.vector(wlb_freq)
)
wlb_table

# Age frequency
age_freq <- table(data$Age)
age_table <- data.frame(
  Age = names(age_freq),
  Count = as.vector(age_freq)
)
age_table

# YearsAtCompany frequency
years_freq <- table(data$YearsAtCompany)
years_table <- data.frame(
  YearsAtCompany = names(years_freq),
  Count = as.vector(years_freq)
)
years_table

# JobLevel frequency
joblevel_freq <- table(data$JobLevel)
joblevel_table <- data.frame(
  JobLevel = names(joblevel_freq),
  Count = as.vector(joblevel_freq)
)
joblevel_table

# JobSatisfaction frequency
JobSatisfaction_freq <- table(data$JobSatisfaction)
JobSatisfaction_table <- data.frame(
  JobSatisfaction = names(JobSatisfaction_freq),
  Count = as.vector(JobSatisfaction_freq)
)
JobSatisfaction_table




#Bar charts


# Bar chart for TrainingTimesLastYear
barplot(train_freq,
        main = "Training Times Last Year Distribution",
        xlab = "Training Times Last Year",
        ylab = "Count",
        col = "lightblue")

# Bar chart for EnvironmentSatisfaction
barplot(env_freq,
        main = "Environment Satisfaction Distribution",
        xlab = "Environment Satisfaction",
        ylab = "Count",
        col = "lightgreen")

# Bar chart for WorkLifeBalance
barplot(wlb_freq,
        main = "Work Life Balance Distribution",
        xlab = "Work Life Balance",
        ylab = "Count",
        col = "lightpink")

# Bar chart for Age
barplot(age_freq,
        main = "Age Distribution",
        xlab = "Age",
        ylab = "Count",
        col = "lightyellow")

# Bar chart for YearsAtCompany
barplot(years_freq,
        main = "Years at Company Distribution",
        xlab = "Years at Company",
        ylab = "Count",
        col = "lightgreen")

# Bar chart for JobLevel
barplot(joblevel_freq,
        main = "Job Level Distribution",
        xlab = "Job Level",
        ylab = "Count",
        col = "orange")

# Bar chart for Job satisfaction
barplot(JobSatisfaction_freq,
        main = "Job satisfaction Distribution",
        xlab = "Job satisfaction",
        ylab = "Count",
        col = "pink")





#Boxplots

#Boxplots for Respect Index
boxplot(data$Respect_Index,
        main = "Boxplot of Respect Index",
        col = "lightblue")

#Boxplots for Training Times Last Year
boxplot(data$TrainingTimesLastYear,
        main = "Boxplot of Training Times Last Year",
        col = "lightgreen")

#Boxplots for Environment Satisfaction
boxplot(data$EnvironmentSatisfaction,
        main = "Boxplot of Environment Satisfaction",
        col = "lightpink")

#Boxplots for Work Life Balance
boxplot(data$WorkLifeBalance,
        main = "Boxplot of Work Life Balance",
        col = "lightyellow")

#Boxplots for Age
boxplot(data$Age,
        main = "Boxplot of Age",
        col = "lavender")

#Boxplots for Years at Company
boxplot(data$YearsAtCompany,
        main = "Boxplot of Years at Company",
        col = "orange")

#Boxplots for Job Level
boxplot(data$JobLevel,
        main = "Boxplot of Job Level",
        col = "skyblue")




#Bivariant Analysis


#TrainingTimesLastYear vs JobSatisfaction
plot(data$TrainingTimesLastYear, data$JobSatisfaction,
     main = "Training Times Last Year vs Job Satisfaction",
     xlab = "Training Times Last Year",
     ylab = "Job Satisfaction",
     col = "blue",
     pch = 16)

abline(lm(JobSatisfaction ~ TrainingTimesLastYear, data = data), col = "red", lwd = 2)

cor(data$TrainingTimesLastYear, data$JobSatisfaction)

boxplot(TrainingTimesLastYear ~ JobSatisfaction, data = data,
        main = "Training Times Last Year across Job Satisfaction Levels",
        xlab = "Job Satisfaction",
        ylab = "Training Times Last Year",
        col = "lightblue")




#RelationshipSatisfaction vs JobSatisfaction
plot(data$RelationshipSatisfaction, data$JobSatisfaction,
     main = "Relationship Satisfaction vs Job Satisfaction",
     xlab = "Relationship Satisfaction",
     ylab = "Job Satisfaction",
     col = "darkgreen",
     pch = 16)

abline(lm(JobSatisfaction ~ RelationshipSatisfaction, data = data), col = "red", lwd = 2)

cor(data$RelationshipSatisfaction, data$JobSatisfaction)

boxplot(RelationshipSatisfaction ~ JobSatisfaction, data = data,
        main ="Environment Satisfaction across Job Satisfaction Levels",
        xlab ="Job Satisfaction",
        ylab ="Environment Satisfaction",
        col ="lightgreen")




#WorkLifeBalance vs JobSatisfaction
plot(data$WorkLifeBalance, data$JobSatisfaction,
     main ="Work Life Balance vs Job Satisfaction",
     xlab ="Work Life Balance",
     ylab ="Job Satisfaction",
     col = "purple",
     pch = 16)

abline(lm(JobSatisfaction ~ WorkLifeBalance, data = data), col = "red", lwd = 2)

cor(data$WorkLifeBalance, data$JobSatisfaction)

boxplot(WorkLifeBalance ~ JobSatisfaction, data = data,
        main = "Work Life Balance across Job Satisfaction Levels",
        xlab = "Job Satisfaction",
        ylab = "Work Life Balance",
        col = "lightpink")





#Quartiles and IQR for Respect_Index
quantile(data$Respect_Index)
IQR(data$Respect_Index)

#Quartiles and IQR Training Time Last Year
quantile(data$TrainingTimesLastYear)
IQR(data$TrainingTimesLastYear)

#Quartiles and IQR environment satisfaction
quantile(data$EnvironmentSatisfaction)
IQR(data$EnvironmentSatisfaction)

#Quartiles and IQR for work life balance
quantile(data$WorkLifeBalance)
IQR(data$WorkLifeBalance)

#Quartiles and IQR for age
quantile(data$Age)
IQR(data$Age)

#Quartiles and IQR for years at company
quantile(data$YearsAtCompany)
IQR(data$YearsAtCompany)

#Quartiles and IQR for Job Level
quantile(data$JobLevel)
IQR(data$JobLevel)




#Summary of each attribute
summary(data$Respect_Index)

summary(data$JobSatisfaction)

summary(data$TrainingTimesLastYear)

summary(data$EnvironmentSatisfaction)

summary(data$WorkLifeBalance)

summary(data$Age)

summary(data$YearsAtCompany)

summary(data$JobLevel)
















