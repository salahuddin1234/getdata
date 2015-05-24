After reading the data and descriptive labels I used sapply to label the data

xtest=read.csv("test/X_test.txt", header=FALSE, sep="")
xtrain=read.csv("train/X_train.txt", header=FALSE, sep="")

ytest=read.csv("test/y_test.txt", col.names=c("activity"), header=FALSE, sep="")
ytrain=read.csv("train/y_train.txt",col.names=c("activity"), header=FALSE, sep="")

subjecttrain=read.csv("train/subject_train.txt", col.names=c("subject"), header=FALSE, sep="")
subjecttest=read.csv("test/subject_test.txt", col.names=c("subject"), header=FALSE, sep="")

xall = rbind(xtrain, xtest) 
yall = rbind(ytrain, ytest)
subjectall = rbind(subjecttrain, subjecttest)
#as the extraction simplifies the data set I decided to merge test and train without adding them the subject and the activity
#2 Extracts only the measurements on the mean and standard deviation for each measurement. 

features=read.csv("features.txt", header=FALSE, sep="")
names(features)=c("id","feature")

vectExtract = grep("mean\\(\\)|std\\(\\)",features$feature)

library("dplyr")
xallExtract = select(xall, one_of(paste0("V",as.character(vectExtract))))
#3 Uses descriptive activity names to name the activities in the data set

activity_labels=read.csv("activity_labels.txt", col.names=c("id","label"),header=FALSE, sep="")
yall$activity = sapply(yall$activity, function(x){as.character(activity_labels$label[x])})

xallExtract$activity = yall$activity
xallExtract$subject = subjectall$subject
#4 Appropriately labels the data set with descriptive variable names. 

#as features have labels containing the characters ()- I suppress them using the gsub function.
names(xallExtract) = c(gsub("[-\\(\\)\\,]","",grep("mean\\(\\)|std\\(\\)",features$feature,value=TRUE)),"activity","subject")
#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidySet = summarise_each(group_by(xallExtract, activity,subject),funs(mean), vars=-c(activity,subject))
# change the names of tidy set to reflect mean computation
names(tidySet)[3:length(tidySet)] = paste(names(tidySet)[3:length(tidySet)],"mean", sep=".")
write.table(tidySet, file="tidyset.txt", row.name=FALSE)