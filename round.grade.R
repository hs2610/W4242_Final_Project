round.grade <- function(grade, min, max){
  grade <- round(grade)
  if (min(grade) < min){
    grade[grade < min] <- min
  }
  if (max(grade) > max){
    grade[grade > max] <- max
  }
  grade
}