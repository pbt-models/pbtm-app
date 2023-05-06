# ---- checkModelReady ---- #

# returns false if colStatus is false for that column
checkModelReady <- function(requirements, statuses) {
  compare <- sapply(1:nCols, function(i) {
    test <- (requirements[i] == T & statuses[i] == T) | (requirements[i] == F)
    if (length(test) == 0) {F} else {test}
  })
  !(F %in% compare)
}
