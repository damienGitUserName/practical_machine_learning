
setwd("/Users/damienbenveniste/CourseOnline/Coursera/JHU data specialization/Machine learning/");

training    <- read.csv("pml-training.csv")
testing     <- read.csv("pml-testing.csv")

myvars      <- colnames(testing)[colSums(is.na(testing)) == nrow(testing)]

newtraining <- training[!colnames(testing) %in% myvars]
newtesting  <- testing[!colnames(testing) %in% myvars]

clas <- c("classe","cvtd_timestamp","X")
newtrainingSub <- droplevels(newtraining[!colnames(newtraining) %in% clas])
newtrainingSub$user_name <- as.factor(newtrainingSub$user_name)
newtrainingSub$new_window <- as.factor(newtrainingSub$new_window)


probId <- c("problem_id","cvtd_timestamp","X")
newtestingSub <- droplevels(newtesting[!colnames(newtesting) %in% probId])
newtestingSub$user_name <- as.factor(newtestingSub$user_name)
newtestingSub$new_window <- as.factor(newtestingSub$new_window)

levels(newtestingSub$new_window) <- c("yes","no")



library(randomForest)
modFit <- randomForest(newtrainingSub,newtraining$classe,do.trace=100)
pred        <- predict(modFit,newtestingSub)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}


