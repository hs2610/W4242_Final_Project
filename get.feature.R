get.char.count <- function(essay){
  Sys.setlocale(category = "LC_ALL", locale = "C")
  # calculate the char count
  char.count <- nchar(essay)
  # return the character count
  char.count
}

get.word.count <- function(essay){
  Sys.setlocale(category = "LC_ALL", locale = "C")
  # calculate the word count
  # TODO: Split by space might not understand tabs at the end of lines
  # TODO: Use get.words
  word.count <- length(get.words(essay))
  # return the character count
  word.count
}

get.words <- function(essay){
  e <- gsub("[,;\\.\\?!\"]", "", essay)
  # return a character vector of all words
 strsplit(essay, " ")[[1]]
}

get.sentences <- function(essay){
  lines <- gsub("[.!?]\\s*", "\n", essay)
  strsplit(lines, "\n")[[1]]
}

get.sentence.count <- function(essay){
  length(get.sentences(essay))
}

get.identical.word.count <- function(keyword, words){
  length(words[words==keyword])
}

get.identical.word.counts <- function(essay, keywords){
  words <- get.words(essay)
  identical.counts <- vapply(keywords, get.identical.word.count, words=words, c(0))
  
  sum(identical.counts)
}

get.matching.word.count <- function(essay, pattern){
  words <- get.words(essay)
  length(grep(pattern,words))
}

get.transition.count <- function(lookup, essay){
  match.counts.trans <- grep(lookup,essay,ignore.case = TRUE)
  # return a character vector of all words
  sum(match.counts.trans)
}

get.transition.counts <- function(keywords, essay){
  sum(vapply(keywords, get.transition.count, essay=essay, c(0)))
}

get.unique.words <- function(essay){
  words <-get.words(essay)
  unique.words <- unique(words)
  length(unique.words)
}

get.avg.char.count.words <-function(essay){
  words<-get.words(essay)
  char.count.words <-get.char.count(words)
  mean(char.count.words)
}

get.misspelling.count <- function(filename, setname){
  Sys.setlocale(category = "LC_ALL", locale = "C")
  mistakes <- aspell(c(paste(setname,"/",filename,sep="")), encoding="latin1")
  length(mistakes$Original)
}

get.word.count.exact.length <- function(essay, exact.length){
  words.list <- get.words(essay)
  lengths <- vapply(words.list, nchar, c(0))
  sum(lengths==exact.length)
}

get.word.count.less.than.length <- function(essay, max.length){
  words.list <- get.words(essay)
  lengths <- vapply(words.list, nchar, c(0))
  sum(lengths<=max.length)
}

get.word.count.greater.than.length <- function(essay, min.length){
  words.list <- get.words(essay)
  lengths <- vapply(words.list, nchar, c(0))
  sum(lengths>=min.length)
}
