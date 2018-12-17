library(tidyverse)

features_info <- read.table('/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/dsa/GCAD_Project/UCIHARDataset/features_info.txt',fill = TRUE ,header = FALSE)
features <- read.table("/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/dsa/GCAD_Project/UCIHARDataset/features.txt",fill = TRUE ,header = FALSE)
activity_labels <- read.table("/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/dsa/GCAD_Project/UCIHARDataset/activity_labels.txt",fill = TRUE ,header = FALSE)
X_train <- read.table("/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/dsa/GCAD_Project/UCIHARDataset/train/X_train.txt",fill = TRUE ,header = FALSE)
y_train <- read.table("/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/dsa/GCAD_Project/UCIHARDataset/train/y_train.txt",fill = TRUE ,header = FALSE)
X_test <- read.table("/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/dsa/GCAD_Project/UCIHARDataset/test/X_test.txt",fill = TRUE ,header = FALSE)
y_test <- read.table("/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/dsa/GCAD_Project/UCIHARDataset/test/y_test.txt",fill = TRUE ,header = FALSE)
subject_train <- read.table("/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/dsa/GCAD_Project/UCIHARDataset/train/subject_train.txt",fill = TRUE ,header = FALSE)
subject_test <- read.table("/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/dsa/GCAD_Project/UCIHARDataset/test/subject_test.txt",fill = TRUE ,header = FALSE)

colnames(subject_train)[colnames(subject_train)=="V1"] <- "subject"
colnames(subject_test)[colnames(subject_test)=="V1"] <- "subject"

colnames(X_test) <- features[,2]
colnames(X_train) <- features[,2]

y_train_with_labels <- left_join(y_train,activity_labels,by="V1")
y_train_with_sub <- cbind(y_train_with_labels,subject_train)

y_test_with_lables <- left_join(y_test,activity_labels,by="V1")
y_test_with_sub <- cbind(y_test_with_lables, subject_test)

temp_train <- cbind(X_train,y_train_with_sub)
temp_test <- cbind(X_test,y_test_with_sub)

final <- rbind(temp_test,temp_train)

#2) Get the columns that are about mean + stdev
meanAndStdevCols <- cbind(final[grepl("mean\\(\\)",names(final))],final[grepl("std",names(final))],final$activity_lables, final$subject)

#3) already put in activity lables when creating the data set, but renaming the column
colnames(final)[colnames(final)=="V2"] <- "activity_lables"

#4) already did this when combining the data set (put feature vector as column names)

#5) take the averages of the mean and stdev cols
colnames(meanAndStdevCols)[colnames(meanAndStdevCols)=="final$activity_lables"] <- "activity_labels"
colnames(meanAndStdevCols)[colnames(meanAndStdevCols)=="final$subject"] <- "subject"
meanAndStdevCols %>% group_by(activity_labels,subject) %>% summarise_all(mean)

write.table(meanAndStdevCols,"/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/dsa/GCAD_Project/tidyDataSet.txt",sep="\t",row.names=FALSE)
