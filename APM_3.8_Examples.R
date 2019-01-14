#AMP 3.8 Computing examples

install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(segmentationOriginal)

segData <- subset(segmentationOriginal, Case == "Train")

cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
# Now remove the columns
segData <- segData[, -(1:3)]
statusColNum <- grep("Status", names(segData))
segData <- segData[, -statusColNum]

#Transformations
install.packages("e1071")
library(e1071)
# For one predictor:
skewness(segData$AngleCh1)
# Since all the predictors are numeric columns, the apply function can > # be used to compute the skewness across columns.
skewValues <- apply(segData, 2, skewness)
head(skewValues)

install.packages("caret")
library(caret)
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans

pcaObject <- prcomp(segData, center = TRUE, scale. = TRUE)
# Calculate the cumulative percentage of variance which each component > # accounts for.
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:3]

trans <- preProcess(segData, method = c("BoxCox", "center", "scale", "pca")) 
trans

# Apply the transformations:
transformed <- predict(trans, segData)
# These values are different than the previous PCA components since > # they were transformed prior to PCA
head(transformed[, 1:5])

#Filtering
nearZeroVar(segData)
correlations <- cor(segData)
correlations[1:4, 1:4]

install.packages("corrplot")
library(corrplot)
corrplot(correlations, order = "hclust")
highCorr <- findCorrelation(correlations, cutoff = .75)
filteredSegData <- segData[, -highCorr]

## Remove the variable name from the column name
simpleMod <- dummyVars(~Mileage + Type, data = carSubset,levelsOnly = TRUE)
predict(simpleMod, head(carSubset))

withInteraction <- dummyVars(~Mileage + Type + Mileage:Type, data = carSubset, levelsOnly = TRUE)
predict(withInteraction, head(carSubset))
