In run_analysis.R, the script does the following:

1) Reads in all the necessary data sets: 
- features.txt,
- activity_labels.txt,
- X_train.txt,
- y_train.txt,
- X_test.txt,
- y_test.txt,
- subject_train.txt
- subject_text.txt

2) Utilizes features as column names for X_test and X_train

3) Joins in the activity labels and subject information with the test and training sets

4) Combines the training and testing data

5) Takes only the columns that have mean or std in the title

6) Takes the means of each of those columns