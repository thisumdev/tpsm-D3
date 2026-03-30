data = read.csv("C:\\Users\\Thism\\Desktop\\Sliit\\3rd-year\\2nd-sem\\TPSM\\Assignment\\data\\cleaned_data.csv")

#For reproducibility
set.seed(42)

#70/30 split
trainID = sample(1:nrow(data), round(0.7 * nrow(data)))
train = data[trainID, ]
test  = data[-trainID, ]


#SLR - Predicted by Respect_Index
simple_model <- lm(JobSatisfaction ~ Respect_Index, data = train)
summary(simple_model)