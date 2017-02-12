# Getting and Cleaning Data Course Project (Code Book)
#The data
We used the experiments of D. Anguita, A. Ghio, et al. "The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data." [1] 

We calculate the average of each variable related to the mean and std functions for each activity and each subject. 6(activities)x30(subjects)=180 rows. 

#The variables
the first two variables are: subject and activities, next we can find 45 mean variables  
 [1] "timeBodyAccelerationMEANY"                               
 [2] "timeBodyAccelerationMEANZ"                               
 [3] "timeGravityAccelerationMEANX"                            
 [4] "timeGravityAccelerationMEANY"                            
 [5] "timeGravityAccelerationMEANZ"                            
 [6] "timeBodyAccelerationJerkSignalMEANX"                     
 [7] "timeBodyAccelerationJerkSignalMEANY"                     
 [8] "timeBodyAccelerationJerkSignalMEANZ"                     
 [9] "timeBodyGyroMEANX"                                       
[10] "timeBodyGyroMEANY"                                       
[11] "timeBodyGyroMEANZ"                                       
[12] "timeBodyGyroJerkSignalMEANX"                             
[13] "timeBodyGyroJerkSignalMEANY"                             
[14] "timeBodyGyroJerkSignalMEANZ"                             
[15] "timeBodyAccelerationMagnitudeMEAN"                       
[16] "timeGravityAccelerationMagnitudeMEAN"                    
[17] "timeBodyAccelerationJerkSignalMagnitudeMEAN"             
[18] "timeBodyGyroMagnitudeMEAN"                               
[19] "timeBodyGyroJerkSignalMagnitudeMEAN"                     
[20] "frequencyBodyAccelerationMEANX"                          
[21] "frequencyBodyAccelerationMEANY"                          
[22] "frequencyBodyAccelerationMEANZ"                          
[23] "frequencyBodyAccelerationMEANFreqX"                      
[24] "frequencyBodyAccelerationMEANFreqY"                      
[25] "frequencyBodyAccelerationMEANFreqZ"                      
[26] "frequencyBodyAccelerationJerkSignalMEANX"                
[27] "frequencyBodyAccelerationJerkSignalMEANY"                
[28] "frequencyBodyAccelerationJerkSignalMEANZ"                
[29] "frequencyBodyAccelerationJerkSignalMEANFreqX"              
[30] "frequencyBodyAccelerationJerkSignalMEANFreqY"            
[31] "frequencyBodyAccelerationJerkSignalMEANFreqZ"            
[32] "frequencyBodyGyroMEANX"                                  
[33] "frequencyBodyGyroMEANY"                                  
[34] "frequencyBodyGyroMEANZ"                                  
[35] "frequencyBodyGyroMEANFreqX"                              
[36] "frequencyBodyGyroMEANFreqY"                              
[37] "frequencyBodyGyroMEANFreqZ"                              
[38] "frequencyBodyAccelerationMagnitudeMEAN"                  
[39] "frequencyBodyAccelerationMagnitudeMEANFreq"              
[40] "frequencyBodyBodyAccelerationJerkSignalMagnitudeMEAN"    
[41] "frequencyBodyBodyAccelerationJerkSignalMagnitudeMEANFreq"  
[42] "frequencyBodyBodyGyroMagnitudeMEAN"                      
[43] "frequencyBodyBodyGyroMagnitudeMEANFreq"                  
[44] "frequencyBodyBodyGyroJerkSignalMagnitudeMEAN"            
[45] "frequencyBodyBodyGyroJerkSignalMagnitudeMEANFreq"

and finally 33 std variables

 [1] "timeBodyAccelerationSTDX"                           
 [2] "timeBodyAccelerationSTDY"                           
 [3] "timeBodyAccelerationSTDZ"                           
 [4] "timeGravityAccelerationSTDX"                        
 [5] "timeGravityAccelerationSTDY"                        
 [6] "timeGravityAccelerationSTDZ"                        
 [7] "timeBodyAccelerationJerkSignalSTDX"                 
 [8] "timeBodyAccelerationJerkSignalSTDY"                 
 [9] "timeBodyAccelerationJerkSignalSTDZ"                 
[10] "timeBodyGyroSTDX"                                   
[11] "timeBodyGyroSTDY"                                   
[12] "timeBodyGyroSTDZ"                                   
[13] "timeBodyGyroJerkSignalSTDX"                         
[14] "timeBodyGyroJerkSignalSTDY"                         
[15] "timeBodyGyroJerkSignalSTDZ"                         
[16] "timeBodyAccelerationMagnitudeSTD"                   
[17] "timeGravityAccelerationMagnitudeSTD"                
[18] "timeBodyAccelerationJerkSignalMagnitudeSTD"         
[19] "timeBodyGyroMagnitudeSTD"                           
[20] "timeBodyGyroJerkSignalMagnitudeSTD"                 
[21] "frequencyBodyAccelerationSTDX"                      
[22] "frequencyBodyAccelerationSTDY"                      
[23] "frequencyBodyAccelerationSTDZ"                      
[24] "frequencyBodyAccelerationJerkSignalSTDX"            
[25] "frequencyBodyAccelerationJerkSignalSTDY"            
[26] "frequencyBodyAccelerationJerkSignalSTDZ"            
[27] "frequencyBodyGyroSTDX"                              
[28] "frequencyBodyGyroSTDY"                              
[29] "frequencyBodyGyroSTDZ"                              
[30] "frequencyBodyAccelerationMagnitudeSTD"              
[31] "frequencyBodyBodyAccelerationJerkSignalMagnitudeSTD"  
[32] "frequencyBodyBodyGyroMagnitudeSTD"                  
[33] "frequencyBodyBodyGyroJerkSignalMagnitudeSTD"  

##Transformations and work that you performed to clean up the data 

### 1. Merges the training and the test sets to create one data set.
I used a function in order to load both bases.   

```r
library("dplyr")

load.base <- function(mode){
        ## Load and working the a mode(train/test) base 
        X_mode <- read.table(paste0(mode,"/X_",mode,".txt"), header=FALSE) 
        subject_mode <- scan(paste0(mode,"/subject_",mode,".txt"))
        y_mode<- scan(paste0(mode,"/y_",mode,".txt")) 
        db <- cbind(subject  = subject_mode,  #bind subject, activity and the base
                       activity = y_mode, 
                       X_mode)
        tbl_df(db)
} 

base <- rbind(load.base("test"),load.base("train")) #merge test and train   
```
There is a problem with the column names, There are repeated names in the feature document, for solving the problem I added a letter for the repeated names in the variables. 


```r
#Load the features
features <- read.table("features.txt", header = FALSE, stringsAsFactors=FALSE )
#sort the features 
features <- features %>% arrange(V2)
#Use a x test variable to check repetitions
x<- rep(1,nrow(features)) 
for(k in 2:nrow(features)){
        if(features[k-1,2]==features[k,2]) x[k]<-x[k-1]+1
}
# clean the variables without problems  
for(k in 1:(nrow(features))){if(k==nrow(features)|(x[k+1]==1&x[k]==1)) x[k]<-0}
#Add a LETTER for the variables with repetitions
for(k in 1:nrow(features)){features[k,2]<- paste0(features[k,2],LETTERS[x[k]])}
#sorting the features as original
features <- (features %>% arrange(V1))$V2
names(base)<- c("subject", "activity", features)
```


### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

I selected all the columns with the strings "std" and "mean" without the names which start with angle, because the function angle used the mean as parameter. 

```r
base <- base%>% 
        select(1:3,contains("std"),contains("mean"),-starts_with("angle"))
```

### 3. Uses descriptive activity names to name the activities in the data set


```r
activities <- c("WALKING", "WALKING UPSTAIRS", "WALKING DOWNSTAIRS",
                "SITTING", "STANDING", "LAYING")
base <- base %>% mutate(activity=factor(activity,labels=activities))
```

### 4. Appropriately labels the data set with descriptive variable names.

```r
col_names <- names(base) 
names(base) <-gsub("std","STD",
                   gsub("mean","MEAN", 
                        gsub("^t","time",
                             gsub("^f","frequency",
                                  gsub("Mag","Magnitude",
                                       gsub("Jerk","JerkSignal",
                                            gsub("Acc","Acceleration",
                                                 gsub("-","",
                                                      gsub("[,]","",
                                                           gsub("[(][)]","",
                                                                col_names))))))))))
```

### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


```r
tidy.data <- aggregate(base[,4:length(base)],list(
        subject=base$subject, 
        activity=base$activity),mean)%>%
        arrange(subject,activity)
write.csv(tidy.data,"tidy_data.csv")
```

#Reference

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
