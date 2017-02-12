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

base <- base%>% 
        select(1:3,contains("mean"),contains("std"),-starts_with("angle"))


activities <- c("WALKING", "WALKING UPSTAIRS", "WALKING DOWNSTAIRS",
                "SITTING", "STANDING", "LAYING")
base <- base %>% mutate(activity=factor(activity,labels=activities))

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


tidy.data <- aggregate(base[,4:length(base)],list(
        subject=base$subject, 
        activity=base$activity),mean)%>%
        arrange(subject,activity)
write.csv(tidy.data,"tidy_data.csv")