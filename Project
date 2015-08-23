run_analysis <- function(){
    URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    file_name <- "getdata-projectfiles-UCI HAR Dataset.zip"
    if(file_name %in% list.files()){
        combineDATA()
    }else {
        WD <- getwd()
        download.file(URL,destfile = paste(WD,"getdata-projectfiles-UCI HAR Dataset.zip",sep="/"), method = "curl")
        unzip(file_name)
        combineDATA()
    }
}

combineDATA <- function(){
    OrigWD<-getwd()
    WD <- paste(OrigWD,"UCI HAR Dataset", sep="/")
    setwd(WD)
    features <- read.table("features.txt",col.names =c("ID","feature"))
    activity <- read.table("activity_labels.txt", col.names = c("activity_id","activity"))
    #test data
    testWD <- paste(WD,"test",sep="/")
    setwd(testWD)
    y <- read.table("y_test.txt", colClasses = "numeric", col.names = "activity_id")
    y$ID <- as.numeric(rownames(y))
    y <- merge(y,activity, by.x = "activity_id")
    x <- read.table("X_test.txt", col.names = features$feature, colClasses = "numeric")
    x$ID <- as.numeric(rownames(x))
    xmean <- x[,colnames(x)[grep("mean",colnames(x))]]
    xmean$ID <- as.numeric(rownames(x))
    xstd <- x[,colnames(x)[grep("std",colnames(x))]]
    xstd$ID <- as.numeric(rownames(x))
    xFinal <- merge(xmean,xstd,all=TRUE)
    test <- read.table("subject_test.txt", colClasses = "numeric", col.names = "subject")
    test$ID <- as.numeric(rownames(test))
    finalTEST <- merge(test,y,all=TRUE)
    finalTEST <- merge(finalTEST,xFinal,all=TRUE)
    #Training data
    trainWD <- paste(WD,"train", sep="/")
    setwd(trainWD)
    y <- read.table("y_train.txt", colClasses = "numeric", col.names = "activity_id")
    y$ID <- as.numeric(rownames(y))
    y <- merge(y,activity, by.x = "activity_id")
    x <- read.table("X_train.txt", col.names = features$feature, colClasses = "numeric")
    x$ID <- as.numeric(rownames(x))
    xmean <- x[,colnames(x)[grep("mean",colnames(x))]]
    xmean$ID <- as.numeric(rownames(x))
    xstd <- x[,colnames(x)[grep("std",colnames(x))]]
    xstd$ID <- as.numeric(rownames(x))
    xFinal <- merge(xmean,xstd,all=TRUE)
    train <- read.table("subject_train.txt", colClasses = "numeric", col.names = "subject")
    train$ID <- as.numeric(rownames(train))
    finalTRAIN <- merge(train,y,all=TRUE)
    finalTRAIN <- merge(finalTRAIN,xFinal,all=TRUE)
    #Combine both
    DATA1 <- rbind(finalTEST, finalTRAIN)
    setwd(OrigWD)
    DATA2 <- melt(DATA1,id=c("subject","activity"))
    AVG <- dcast(DATA2, formula = subject+activity~variable,mean)
    write.table(AVG,file = "avgDATA.txt",row.names = FALSE)
}
