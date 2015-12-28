runAnalysis <- function()
{
  library(dplyr)
  

  ##read x and y test obs.
  x.testObs <- read.table("UCI HAR Dataset/test/X_test.txt")
  y.testObs <- read.table("UCI HAR Dataset/test/y_test.txt")
  subject.testObs <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  ##read x and y train obs
  x.trainObs <- read.table("UCI HAR Dataset/train/X_train.txt")
  y.trainObs <- read.table("UCI HAR Dataset/train/y_train.txt")
  subject.trainObs <- read.table("UCI HAR Dataset/train/subject_train.txt")
  
  ##merge test data sets
  testObs <- data.frame(subject.testObs,y.testObs,x.testObs)
  
  ##merge train data sets
  trainObs <- data.frame(subject.trainObs,y.trainObs,x.trainObs)
  
  ##Combine Test and Train data sets
  mixObs <- rbind(testObs,trainObs)
  
  ##Read features
  featuresObs <- read.table("UCI HAR Dataset/features.txt")
  
  ##Find column names of measurement
  columnNames <- as.vector(featuresObs[,2])
  
  ##set column names of merged data set
  colnames(mixObs) <- c("subjectId","activityLbl",columnNames)
  
  ##removed duplicate column names from merge data sets
  mixObs <- mixObs[ , !duplicated(colnames(mixObs))]
  
  ##select only subject , activity and columns with mean or std dev measurments
  mixObs <- select(mixObs,contains("subjectId"),
                          contains("activityLbl"),
                          contains("std"),
                          contains("mean"),
                          -contains("freq"),
                          -contains("angle"))
  
  ##Read Activity Name and Activity Code
  activityLbls <- read.table("UCI HAR Dataset/activity_labels.txt")
  
  ##Replace Activity Code with Activity Name in data sets
  mixObs$activityLbl <- as.character(activityLbls[match(mixObs$activityLbl,activityLbls$V1),'V2'])                    
  
  ##Group rows by subject and Activity and summarize mean of each measuremennts
  data.summary <- mixObs %>% group_by(subjectId,activityLbl) 
                        %>% summarise_each(funs(mean))
  
 
 ##write summary to a file summary.txt
 write.table(data.summary,file="summary.txt",row.names = FALSE)
}