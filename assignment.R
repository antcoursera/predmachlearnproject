filename <- "training.csv"
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
if(file.exists(filename) == FALSE) {
    download.file(url, filename, method="curl")
}
training <- read.csv(filename)

filename <- "testing.csv"
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if(file.exists(filename) == FALSE) {
    download.file(url, filename, method="curl")
}
testing <- read.csv(filename)


library(caret)

# Subsetting

colnames(testing[1,!is.na(testing[1,])])
predict_cols = grep("^(gyros|total_accel|accel|magnet|roll|pitch|yaw)",colnames(training),value=T)
training_clean = subset(training[testing$new_window=='no'],select=c(predict_cols,'classe'))
set.seed(111)

train = createDataPartition(training_clean$classe, p=0.5, list=F)

t = training_clean[train,]
v = training_clean[-train,]

cm = cor(subset(t,select=-classe))
diag(cm) = 0
cm = abs(cm)
library(lattice)
levelplot(cm)

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
fit = train(classe~., t, method="rf") 
stopCluster(cl)

p = predict(fit, v)
confusionMatrix(p,v$classe)


pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pfinal = predict(fit, testing)
pml_write_files(pfinal)
