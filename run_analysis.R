### It is assumed that "UCI HAR Dataset" folder with Samsung dataset is in the working directory ###
setwd("C:/Users/orojuan/Documents/Data Science/Getting data/UCI HAR Dataset")

# Read feature names
header <- read.table("./features.txt",header=F,comment.char="",as.is=T)[,2]
# Read activity labels
act <- read.table("./activity_labels.txt",header=F,comment.char="",as.is=T)[,2]

# Funtion defined to read either the train or test datasets
readSet <- function(type = "train"){
  # Read features' measurements
  x <- read.table(paste("./",type,"/X_",type,".txt",sep=""),header=F,comment.char="")
  # Read IDs of subjects involved in measurements
  ind <- as.factor(readLines(paste("./",type,"/subject_",type,".txt",sep="")))
  # Read activity labels assigned to each record
  y <- readLines(paste("./",type,"/Y_",type,".txt",sep=""))
  # Bind response, subject and features
  res <- cbind(y,ind,x)
  # Assign headers to dataframe
  dimnames(res)[[2]] <- c("Act","Subject",header)
  return(res)
}

# Read and merge train and test datasets (Activity #1 in peer assessment project)
dataset <- rbind(readSet("train"),readSet("test"))
# Re-label records with descriptive activity names (Activities #3 & #4 in peer assessment project)
dataset$Act <- as.factor(act[dataset$Act])
# Extract measurements on the mean and std dev for each measurement (Activity #2 in peer assessment project)
subset <- dataset[,c(1:2,grep("mean|std",header)+2)]
# Calculate average of measurements for each activity and each subject (Activity #5 in peer assessment project)
res <- by(subset[, 3:ncol(subset)], interaction(subset$Act, subset$Subject, drop = T), colMeans)
# Coerce "by" object to a dataframe that includes a column for activity-subject tuples
res <- cbind(names(res),as.data.frame(do.call(rbind,res)))
dimnames(res)[[2]][1] <- "ActSub"
# Write res with .txt extension in parent directory
write.table(res,"../output.txt",row.names=F)