
#' @description checks that all required columns are validated for each model
#' @param requirements boolean vector of column requirements
#' @param statuses boolean vector of current column readiness statuses
#' @returns T/F if statuses satisfy requirements

checkModelReady <- function(requirements, statuses) {
  compare <- sapply(1:nCols, function(i) {
    test <-
      (requirements[i] == TRUE & statuses[i] == TRUE) |
      (requirements[i] == FALSE)
    ifelse(length(test) == 0, FALSE, test)
  })
  all(compare)
}
