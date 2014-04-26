setwd("C:/Users/orojuan/Documents/Data Science/Getting data/UCI HAR Dataset")
header <- read.table("./features.txt",header=F,comment.char="",as.is=T)[,2]
act <- read.table("./activity_labels.txt",header=F,comment.char="",as.is=T)[,2]

readSet <- function(type = "train"){
  x <- read.table(paste("./",type,"/X_",type,".txt",sep=""),header=F,comment.char="")
  ind <- as.factor(readLines(paste("./",type,"/subject_",type,".txt",sep="")))
  y <- readLines(paste("./",type,"/Y_",type,".txt",sep=""))
  res <- cbind(y,ind,x)
  dimnames(res)[[2]] <- c("Act","Subject",header)
  return(res)
}

dataset <- rbind(readSet("train"),readSet("test"))
dataset$Act <- as.factor(act[dataset$Act])

subset <- dataset[,grep("mean|std",header)+2]

res <- by(dataset[, 3:ncol(dataset)], interaction(dataset$Act, dataset$Subject, drop = T), colMeans)  #Split only on ocurring tuples
res <- do.call(rbind, res)
