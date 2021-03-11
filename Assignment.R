

setwd("C:/Users/CWU/Rcourse/Get and clean data/week4/Project")
#download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","peer-graded-files.zip")
#unzip("peer-graded-files.zip")
# check if needed packages are downloaded and download them if not
library(dplyr)
library(stringr)

# read test data 
x_test<-read.table("UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt",header=F)
test<-bind_cols(x_test,y_test,subject_test)

#read train data
x_train<-read.table("UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt", header=F)
train<-bind_cols(x_train,y_train,subject_train)

#Read colname
colname<-read.csv("UCI HAR Dataset/features.txt",sep = " ",header = F)[,2]
# make every name unique (there are duplicated)
colname<-make.names(colname,unique = T)
# add a colname for the activity stored in y_test
# "()" in the names are replaced by "..
# add the 2 columns manually for acrivity and subject
colname<-c(colname,"activity","subject")

# assign colname to both tables
names(test)<-colname
names(train)<-colname

#add a column to save the stage and subject (test or train)
#x_test<-as_tibble(x_test)
test<-mutate(test,status="test")

#x_train<-as_tibble(x_train)
train<-mutate(train,status="train")

######## Q1 Merges the training and the test sets to create one data set.
Q1_df<-bind_rows(train,test)
# QC that there are 2947 rows from test dataset and 7352 rows from train dataset
table(Q1_df$status)

######### Q2 Extracts only the measurements on the mean and standard deviation for each measurement. 
#search the names of columns containing "mean" or "std"
# manually add the column created "activity" "subject" "stage"
newcol<-grep("mean|std",colname,value = T) %>% c("activity","subject","status")

Q2_df<-select(Q1_df,all_of(newcol))

######### Q3 Uses descriptive activity names to name the activities in the data set
activity_label<-read.table("UCI HAR Dataset/activity_labels.txt")[,2]

#add activity_name column to dataset, where uses the activity label name 
Q3_df<-mutate(Q2_df,activity_name=activity_label[Q2_df$activity])

######### Q4 Appropriately labels the data set with descriptive variable names
# replace "..." by _
# remove ".."
# trim leading and triming white space (call library stringr)
Q4_df<-Q3_df
#replace "..."by "_"
names(Q4_df)<-gsub("\\.\\.\\.","_",names(Q4_df)) 
#replace ".."by "" and trim 
names(Q4_df)<-gsub("\\.\\.","",names(Q4_df)) %>% str_trim()

######### Q5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# As I understand the question, I will create a new dataset with average for each selected variables per activity and subject
# I need to create another variable that is unique for activity and subject called activity_subject
# There are 6 activities and 30 subjects. There will be 180 groups. 


newdf<-mutate(Q4_selectdf_label,activity_subject=activity*100+subject)
newdf_gr<-group_by(newdf,activity_subject)

#drop columns:activity, status and subject 
newdf2=subset(newdf,select=-c(activity,status,subject))

newdf<-mutate(Q4_selectdf_label,activity_subject=activity*100+subject)
newdf_gr<-group_by(newdf,activity_subject)

#drop columns:activity, status and subject 
newdf2=subset(newdf,select=-c(activity,status,subject))


#use activity_subject as flag to indicator activity and subject group 
newdf<-mutate(Q4_df,activity_subject=activity*100+subject)

newdf2=subset(newdf,select=-c(activity,status,subject))

#newdf_gr<-group_by(newdf2,activity_subject)

#calculate average by groups 
Q5_df_mean<-aggregate(newdf2[, 1:81], list(newdf2$activity_subject), mean)

write.table(Q5_df_mean,"peer_graded_tidyTable.txt")

