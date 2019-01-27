install.packages("caret")
library(caret)
install.packages("RANN")
library("RANN")

#read data in
setwd()
data <- read.csv("./HDdata.csv")

#1)
#Choosing logistic regression over linear, it is regressing for the probability of a categorical outcome. 
#considering just one outcome variable and two states of that variable- either 0 or 1. They had heart disease or they didn't.

#2) Divide into training and test, waiting till after preprocessing of data

#3) Preprocess the data

#There are 10 values in total that are null, all in age
sum(is.na(data)) 
sum(is.na(data$age))

#categorical variables are sex (binary), chest pain, fbs (binary), restecg, exang(binary), num (binary)
#numeric variables are age, trestbps, chol, thalach, oldpeak
#Impute missing values for age using k nearest neighbors

#preProcessedParams <- preProcess(data,method = c("medianImpute","center","scale"))
preProcessedParams <- preProcess(data,method = c("medianImpute"))
cleanData <- predict(preProcessedParams, data)
#confirm data was cleaned correctly
dim(cleanData)
sum(is.na(cleanData))
head(cleanData)
head(data)

#2) now split after data has been preprocessed
## 75% of the sample size
smp_size <- floor(0.75 * nrow(cleanData))

## set the seed to make partition reproducible
set.seed(975)
train_index <- sample(seq_len(nrow(cleanData)), size = smp_size)

## divide into training and test
train <- cleanData[train_index, ]
test <- cleanData[-train_index, ]

##confirm split was done correctly
dim(train)
dim(test)

#4) fit a logistic regression model
modelFit <- glm(num~., data = train,family = binomial)

#5)Assess the accuracy of the model
#a Choose a prediction threshold
#Choosing .5, because the danger is in saying someone does not have heart disease when they actually do
# so would rather err on the side of being overcautious with our predictions

#b Make a confusion matrix
fittedResults <- predict(modelFit,newdata=subset(test,select=c(1,2,3,4,5,6,7,8,9,10)),type='response')
fittedResults <- ifelse(fitted.results > 0.5,1,0)

# Confusion matrix
confusionMatrix(data=table(fittedResults,test$num))

#Calculate the accuracy, PPV, NPV, Sensitivity and Specificity.
misClasificError <- mean(fitted.results != test$num)
accuracy <- 1-misClasificError

#all produced from confusionMatrix function
#PPV = .8
#NPV = .6667
#Sensitivity = .7273
#Specificity = .75

