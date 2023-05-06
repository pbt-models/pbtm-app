# ---- validateCol ---- #

#' @param col data vector to evaluate
#' @param expectedType checks that `col` is this type
#' @param minValue if `col` is numeric, checks this constraint
#' @param maxValue if `col` is numeric, checks this constraint
#' @returns boolean

validateCol <- function(col, expectedType, minValue, maxValue) {
  info <- NULL
  msg <- NULL
  valid <- FALSE
  
  if (anyNA(col)) {
    msg <- list(span("Warning: Missing value in data", style = "color:red"), br())
  }
  
  colType <- class(col)
  info <- list(paste("Type:", colType), br())
  
  # Numeric value checks
  if (colType == "numeric") {
    info <- append(info,
      list(paste0("Range: ", round(min(col), 2), " => ", round(max(col), 2)), br())
    )
    
    # column type mismatch
    if (!is.na(expectedType) & colType != expectedType) {
      msg <- append(msg,
        list(span("Error: Incorrect column type, expected", expectedType, style = "color:red"), br())
      )
    }
    
    # min check
    if (!is.na(minValue) & min(col) < minValue) {
      msg <- append(msg, 
        list(span("Error: Min value less than", minValue, style = "color:red"), br())
      )
    }
    
    # max check
    if (!is.na(maxValue) & max(col) > maxValue) {
      msg <- append(msg,
        list(span("Error: Max value greater than ", maxValue, style = "color:red"), br())
      )
    }
    
  } else if (!is.na(expectedType) & colType != expectedType) {
    newmsg <- list(span("Error: Incorrect column type, expected", expectedType, style = "color:red"), br())
    msg <- append(msg, newmsg)
  }
  
  # valid if no messages yet
  if (is.null(msg)) {
    msg <- list(span(strong("OK"), style = "color:blue"))
    valid <- TRUE
  } else {
    msg <- head(msg, -1) # remove the last br()
  }
  
  list(
    valid = valid,
    ui = tagList(info, msg)
  )
}
