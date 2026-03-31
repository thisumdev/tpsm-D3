data = read.csv("C:\\Users\\Thism\\Desktop\\Sliit\\3rd-year\\2nd-sem\\TPSM\\Assignment\\data\\cleaned_data.csv")

#For reproducibility
set.seed(42)

#70/30 split
trainID = sample(1:nrow(data), round(0.7 * nrow(data)))
train = data[trainID, ]
test  = data[-trainID, ]


#SLR - Predicted by Respect_Index
simple_model = lm(JobSatisfaction ~ Respect_Index, data = train)
summary(simple_model)
#This model finds the best straight line. Dependent and independant variable given.(JobSatisfaction = Intercept + (coefficient * respect_index))

#Residuals
#Residual is the gap of real value and predicted one. The range is from -1.83 to 1.32 confirming that model predicts poorly.

#Co-efficients
#Numbers building the prediction formular. Intercept is 2.9174(when respect_index = 0)
#Respect_Index co-efficient is -0.062 which means for every point increase of Respect_index, JobSatisfaction will be dropped by 0.062 points
#Standard error shows how reliable is estimate.(Smaller the better). our reading is 0.072 bigger than the estimate itself confirming that effect is meaningless.
#T-value is estimate/standar derror
#p-value is 0.388 which is no where near significant. 
#Residual standard error describes on how much the predictions are off. According to output 1.1 points.
#R^2 says what % of the variation of the JobSatsifaction or the dependant varibale does the model explains. In our only 0.07% of variation is explained.
#F-stat shows if model is useful. Our value is 0.7444 meaning weak model. P-value of the model is 0.3884, 38.8% chance that model results are by chance ( should be < 0.05 to be significant)

