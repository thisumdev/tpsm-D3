data = read.csv("C:\\Users\\Thism\\Desktop\\Sliit\\3rd-year\\2nd-sem\\TPSM\\Assignment\\data\\cleaned_data.csv")

#For reproducibility
set.seed(42)
#This is done so that the everytime the sampling is done it is initated from the 42 position of the sequence making all the splits uniform every time the split is done.

#70/30 split
trainID = sample(1:nrow(data), round(0.7 * nrow(data)))
train = data[trainID, ]
test  = data[-trainID, ]

#Verifying split
nrow(train)
nrow(test)

#Checking the dependent variable
table(train$JobSatisfaction)
table(test$JobSatisfaction)




