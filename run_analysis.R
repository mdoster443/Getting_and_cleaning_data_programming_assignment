file_url<-'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
#destination of data in a zip folder
download.file(file_url,'data.zip')
#downloads data and renames the zip folder to data
unzip('data.zip',list=TRUE)
#unzips data folder
newdir<-paste0(getwd(),'/','data/UCI HAR Dataset')
#creates new directory variable
setwd(newdir)
#sets working directory within the unzipped data folder
library(dplyr)
library(data.table)
#load necessary libraries
features<-read.table("features.txt",header = FALSE)
activity_names<-read.table("activity_labels.txt", header = FALSE)
features_train<-read.table("./train/X_train.txt",header = FALSE)
activitys_train<-read.table("./train/Y_train.txt",header=FALSE)
subjects_train<-read.table("./train/subject_train.txt",header=FALSE)
bodyAccX_train<-read.table("./train/Inertial Signals/body_acc_x_train.txt",header=FALSE)
bodyAccY_train<-read.table("./train/Inertial Signals/body_acc_y_train.txt",header=FALSE)
bodyAccZ_train<-read.table("./train/Inertial Signals/body_acc_z_train.txt",header=FALSE)
bodyGyroX_train<-read.table("./train/Inertial Signals/body_gyro_x_train.txt",header=FALSE)
bodyGyroY_train<-read.table("./train/Inertial Signals/body_gyro_y_train.txt",header=FALSE)
bodyGyroZ_train<-read.table("./train/Inertial Signals/body_gyro_z_train.txt",header=FALSE)
totalAccX_train<-read.table("./train/Inertial Signals/total_acc_x_train.txt",header=FALSE)
totalAccY_train<-read.table("./train/Inertial Signals/total_acc_y_train.txt",header=FALSE)
totalAccZ_train<-read.table("./train/Inertial Signals/total_acc_z_train.txt",header=FALSE)
features_test<-read.table("./test/X_test.txt",header = FALSE)
activitys_test<-read.table("./test/Y_test.txt",header=FALSE)
subjects_test<-read.table("./test/subject_test.txt",header=FALSE)
bodyAccX_test<-read.table("./test/Inertial Signals/body_acc_x_test.txt",header=FALSE)
bodyAccY_test<-read.table("./test/Inertial Signals/body_acc_y_test.txt",header=FALSE)
bodyAccZ_test<-read.table("./test/Inertial Signals/body_acc_z_test.txt",header=FALSE)
bodyGyroX_test<-read.table("./test/Inertial Signals/body_gyro_x_test.txt",header=FALSE)
bodyGyroY_test<-read.table("./test/Inertial Signals/body_gyro_y_test.txt",header=FALSE)
bodyGyroZ_test<-read.table("./test/Inertial Signals/body_gyro_z_test.txt",header=FALSE)
totalAccX_test<-read.table("./test/Inertial Signals/total_acc_x_test.txt",header=FALSE)
totalAccY_test<-read.table("./test/Inertial Signals/total_acc_y_test.txt",header=FALSE)
totalAccZ_test<-read.table("./test/Inertial Signals/total_acc_z_test.txt",header=FALSE)
#read each file with read.table() and assign it to the corresponding variable of file name
names(features_train)<-features$V2
names(features_test)<-features$V2
#renames column names with correct labels from features data frame
names(activitys_train)<-'activity_num'
names(activitys_test)<-'activity_num'
#labels the one column with 'activitys'
names(subjects_train)<-'subjects'
names(subjects_test)<-'subjects'
#labels the one column with 'subjects
sub_act_feat_train<-cbind(subjects_train,activitys_train,features_train)
sub_act_feat_test<-cbind(subjects_test,activitys_test,features_test)
#Combines subject, activities, and features for train and test data frames
sub_act_feat_all<-rbind(sub_act_feat_test,sub_act_feat_train)
#(Prompt 1) Combines both test and train dataframes
cols<-grep(pattern='[Mm]ean|std|subjects|activity_num',x=names(sub_act_feat_all))
#creates variable of columns with mean and std
spec_sub_act_feat_all<-sub_act_feat_all[,cols]
#(Prompt 2) creates new dataframe with only columns of subject,activity, mean,
# and std
names(activity_names) <- c('num','activitys')
#renamed columns to make it easier later
spec_sub_act_feat_all <- merge(spec_sub_act_feat_all,activity_names,by.x='activity_num',by.y='num')
#(Prompt 3) added character value for activity
spec_sub_act_feat_all <- spec_sub_act_feat_all[,c(2,89,3:88)]
#reordered character activity column to be after subject and removed numeric activity column
spec_sub_act_feat_all <- spec_sub_act_feat_all %>% arrange(spec_sub_act_feat_all[,1])
#arranged subjects column from 1 to 30
names(spec_sub_act_feat_all) <- gsub("tBodyAcc-",
                                    "body accelerometer signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("tGravityAcc-",
                                    "gravity accelerometer signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("tBodyAccJerk-",
                                    "jerk body accelerometer signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("tBodyGyro-",
                                    "body gyroscope signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("tBodyGyroJerk-",
                                    "body jerk gyroscope signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("tBodyAccMag-",
                                    "Magnitude of body accelerometer signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("tGravityAccMag-",
                                    "Magnitude of gravity accelerometer signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("tBodyAccJerkMag-",
                                    "Magnitude of body jerk accelerometer signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("tBodyGyroMag-",
                                    "Magnitude of body gyroscope signal . ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("tBodyGyroJerkMag-",
                                    "Magnitude of body jerk gyroscope signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("fBodyAcc-",
                                    "FFT applied to body accelerometer signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("fBodyAccJerk-",
                                    "FFT applied to body jerk accelerometer signal. ",
                                    names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("fBodyGyro-",
                                     "FFT applied to body gyroscope signal. ",
                                     names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("fBodyAccMag-",
                                     "Magnitude of FFT applied to body accelerometer signal. ",
                                     names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("fBodyBodyAccJerkMag-",
                                     "Magnitude of FFT applied to body jerk accelerometer signal. ",
                                     names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("fBodyBodyGyroMag-",
                                     "Magnitude of FFT applied to body gyroscope signal. ",
                                     names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("fBodyBodyGyroJerkMag-",
                                     "Magnitude of FFT applied to body jerk gyroscope signal. ",
                                     names(spec_sub_act_feat_all))
names(spec_sub_act_feat_all) <- gsub("angle(tBodyAccMean,gravity)",
                                     "Vector of mean body accelerometer signal and gravity",
                                     names(spec_sub_act_feat_all),fixed=TRUE)
names(spec_sub_act_feat_all) <- gsub("angle(tBodyAccJerkMean),gravityMean)",
                                     "Vector of mean body jerk accelerometer signal and gravity",
                                     names(spec_sub_act_feat_all),fixed=TRUE)
names(spec_sub_act_feat_all) <- gsub("angle(tBodyGyroMean,gravityMean)",
                                     "Vector of mean body gyroscope signal and gravity",
                                     names(spec_sub_act_feat_all),fixed=TRUE)
names(spec_sub_act_feat_all) <- gsub("angle(tBodyGyroJerkMean,gravityMean)",
                                     "Vector of mean body jerk gyroscope signal and gravity",
                                     names(spec_sub_act_feat_all),fixed=TRUE)
names(spec_sub_act_feat_all) <- gsub("angle(X,gravityMean)",
                                     "Vector of gravity mean in X direction",
                                     names(spec_sub_act_feat_all),fixed=TRUE)
names(spec_sub_act_feat_all) <- gsub("angle(Y,gravityMean)" ,
                                     "Vector of gravity mean in Y direction",
                                     names(spec_sub_act_feat_all),fixed=TRUE)
names(spec_sub_act_feat_all) <- gsub("angle(Z,gravityMean)" ,
                                     "Vector of gravity mean in Z direction",
                                     names(spec_sub_act_feat_all),fixed=TRUE)
#(Prompt 4) renames columns to more descriptive variable names derived from 
tidydata_avgs <- spec_sub_act_feat_all %>% group_by(subjects,activitys) %>% 
                        summarize_all(mean) %>% ungroup
#(Prompt 5) creates second tidy data set with the mean of each column value based
#on each subject's activity
