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
  testObs <- data.frame(subject.testObs,x.testObs,y.testObs)
  
  ##merge train data sets
  trainObs <- data.frame(subject.trainObs,x.trainObs,y.trainObs)
  
  ##Combine Test and Train data sets
  mixObs <- rbind(testObs,trainObs)
  
  ##Read features
  featuresObs <- read.table("UCI HAR Dataset/features.txt")
  
  ##Find column names of measurement
  columnNames <- as.vector(featuresObs[,2])
  
  ##set column names of merged data set
  colnames(mixObs) <- c("subjectId","activityLbl",columnNames)
  
  mixObs <- select(mixObs,contains("subjectId"),
                          contains("activityLbl"),
                          contains("-std"),
                          contains("-mean"),
                          -startswith("fBodyAcc-bandsEnergy"))
  ##select only subject , activity and columns with mean or std dev measurments
 return (mixObs)
              
  
  

}