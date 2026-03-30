hr_data = read.csv("../data/raw_data.csv")

print(head(hr_data))

print(class(hr_data$RelationshipSatisfaction))

#The variables chosen(TrainingTimesLastYear, WorkLifeBalance, RelationshipSatisfaction) to engineer the proxy variable respect_index are on different scales so TrainingTimesLastYear is normalized to be on the same scale as other 2 chosen variables(1-4) using Min-max scaling 
hr_data$Normalized_Training = 1 + ((hr_data$TrainingTimesLastYear - 0) * (4 - 1) / (6 - 0))

#Creating the Respect_index column by getting the average
hr_data$Respect_Index = (hr_data$WorkLifeBalance + hr_data$RelationshipSatisfaction + hr_data$Normalized_Training) / 3

#Remove temporary normalized column(just an intermediate feature derived to build the proxy variable)
hr_data$Normalized_Training = NULL

print(head(hr_data))

#Checking the engineered variable
summary(hr_data$Respect_Index)

#Saving the new dataset
write.csv(hr_data, "../data/engineered_data.csv", row.names = FALSE)
