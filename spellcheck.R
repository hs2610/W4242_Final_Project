setwd("Data")
source("../Code/get.feature.R")

# load data
train <- read.csv("train.csv", stringsAsFactors=F)
test <- read.csv("test.csv", stringsAsFactors=F)

# Bookkeeping: there are five sets in the training data
training.set <- list()
for(i in 1:5){
  training.set[[i]] <- train[train$set == i,]
}

test.set <- list()
for(i in 1:5){
  test.set[[i]] <- test[test$set == i,]
}

# cleanup
rm(i,test,train)

# run spell check
training.files <- do.call(rbind,training.set)[,1]
test.files <- do.call(rbind,test.set)[,1]

training.misspell <- vapply(training.files, get.misspelling.count, setname="train", c(0))
test.misspell <- vapply(test.files, get.misspelling.count, setname="test", c(0))

# save output
test <- data.frame(id=test.files, misspell=test.misspell)
train <- data.frame(id=training.files, misspell=training.misspell)
write.csv(train,"train.misspell.csv", row.names=F)
write.csv(test,"test.misspell.csv", row.names=F)