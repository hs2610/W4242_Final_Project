setwd("Data")
library("kernlab")
library("class")
library("randomForest")
source("../Code/get.feature.R")
source("../Code/round.grade.R")
source("../Code/QuadraticWeightedKappa.R")

# the minimum and maximum score possible for each of the five sets
min.score <- c(2, 0, 0, 0, 0)
max.score <- c(12, 3, 3, 3, 4)

# Load the CSV data sets
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

##
# Features
##

# Character length
for(i in 1:5){
  training.set[[i]]$char.count <- 
    vapply(training.set[[i]]$essay, get.char.count, c(0), USE.NAMES=F)
  test.set[[i]]$char.count <- 
    vapply(test.set[[i]]$essay, get.char.count, c(0), USE.NAMES=F)
}

# Word count
for(i in 1:5){
  training.set[[i]]$word.count <- 
    vapply(training.set[[i]]$essay, get.word.count, c(0), USE.NAMES=F)
  test.set[[i]]$word.count <- 
    vapply(test.set[[i]]$essay, get.word.count, c(0), USE.NAMES=F)
}

# Number of adjectives and adverbs
adjadv.list <- read.table("adjadv.txt", header=F, stringsAsFactors=F)[,1]

for(i in 1:5){
  training.set[[i]]$adjadv.count <- vapply(
    training.set[[i]]$essay,
    function(essay){get.identical.word.counts(essay, adjadv.list)}, c(0))
  test.set[[i]]$adjadv.count <- vapply(
    test.set[[i]]$essay,
    function(essay){get.identical.word.counts(essay, adjadv.list)}, c(0))
}

# Number of transitioning words
trans.list <- read.table("transitions.txt",
                         header=F, stringsAsFactors=F, sep= "")[,1]

for(i in 1:5){
  training.set[[i]]$trans.count <- vapply(
    training.set[[i]]$essay,
    function(essay){get.transition.counts(trans.list, essay)}, c(0))
  test.set[[i]]$trans.count <- vapply(
    test.set[[i]]$essay,
    function(essay){get.transition.counts(trans.list, essay)}, c(0))
}

# Number of Unique words
for(i in 1:5){
  training.set[[i]]$unique.count <- vapply(
    training.set[[i]]$essay, function(essay){get.unique.words(essay)}, c(0))
  test.set[[i]]$unique.count <- vapply(
    test.set[[i]]$essay, function(essay){get.unique.words(essay)}, c(0))
}

# Number of sentences
for(i in 1:5){
  training.set[[i]]$sentence.count <- vapply(
    training.set[[i]]$essay, function(essay){get.sentence.count(essay)}, c(0))
  test.set[[i]]$sentence.count <- vapply(
    test.set[[i]]$essay, function(essay){get.sentence.count(essay)}, c(0))
}

# Average character count for words in essay
for(i in 1:5){
  training.set[[i]]$avg.char.words <- vapply(
    training.set[[i]]$essay, function(essay){get.avg.char.count.words(essay)}, c(0))
  test.set[[i]]$avg.char.words <- vapply(
    test.set[[i]]$essay, function(essay){get.avg.char.count.words(essay)}, c(0))
}

# Number of words 8+ char long
for(i in 1:5){
  training.set[[i]]$words.long <- vapply(
    training.set[[i]]$essay,
    function(essay){get.word.count.greater.than.length(essay, 8)}, c(0))
  test.set[[i]]$words.long <- vapply(
    test.set[[i]]$essay,
    function(essay){get.word.count.greater.than.length(essay, 8)}, c(0))
}

# Number of words 5- char long
for(i in 1:5){
  training.set[[i]]$words.short <- vapply(
    training.set[[i]]$essay,
    function(essay){get.word.count.less.than.length(essay, 5)}, c(0))
  test.set[[i]]$words.short <- vapply(
    test.set[[i]]$essay,
    function(essay){get.word.count.less.than.length(essay, 5)}, c(0))
}

# Number of misspelled words
train.mistakes <- read.csv("train.misspell.csv")
test.mistakes <- read.csv("test.misspell.csv")

for(i in 1:5){
  training.set[[i]] <- merge(training.set[[i]], train.mistakes)
  test.set[[i]] <- merge(test.set[[i]], test.mistakes)
}

##
# LEARN FROM TRAIN AND APPLY ON TRAIN (CROSS-VALIDATION)
##

avg.score <- list()
for(i in 1:5){
  # 10-fold cross validation
  df <- training.set[[i]]
  df$rand <- runif(nrow(df))
  df$grp <- findInterval(df$rand, quantile(df$rand, seq(0, 1, by=1/10)), rightmost.closed=T)
  
  scores <- list()
  
  for(j in 1:10){
    #LEARN
    t.set <- df[df$grp != j,]
    d.set <- df[df$grp == j,]
    
    rf <- randomForest(grade ~ word.count + adjadv.count + trans.count +
      unique.count + sentence.count + avg.char.words + words.long +
      words.short + misspell, data=t.set)
    
    
    
    #PREDICT
    pred <- predict(rf, d.set)
    pred <- round.grade(pred, min.score[i], max.score[i])
    
    #SCORE
    scores[[j]] <- ScoreQuadraticWeightedKappa(d.set$grade, pred, min.score[i], max.score[i])
  }
  # create a matrix out of all these scores
  scores <- do.call(rbind, scores)
  avg.score[[i]] <- apply(scores, 2, mean)
}

avg.score.combined <- as.data.frame(do.call(rbind, avg.score))
names(avg.score.combined) <- c("score")
avg.score.combined

##
# LEARN FROM TRAIN AND APPLY ON TEST
##

pred <- list()
for(i in 1:5){
  rf <- randomForest(grade ~ word.count + adjadv.count + trans.count +
    unique.count + sentence.count + avg.char.words + words.long +
    words.short + misspell, data=training.set[[i]])
  pred[[i]] <- predict(rf, test.set[[i]])
  pred[[i]] <- round.grade(pred[[i]], min.score[i], max.score[i])
}

pred <- unlist(pred)
test <- do.call(rbind,test.set)
submission <- cbind(test[,c("id", "set")], rep(1, length(pred)), pred)
names(submission) <- c("id", "set", "weight", "grade")

# save the output as CSV
write.csv(submission, "submission.csv", row.names=F)