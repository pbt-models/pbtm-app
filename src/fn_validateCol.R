
#' @description checks a data column and returns whether it's valid and a message about it
#' @param col data vector to evaluate
#' @param expectedType checks that `col` is this type
#' @param minValue if `col` is numeric, checks this constraint
#' @param maxValue if `col` is numeric, checks this constraint
#' @returns boolean

validateCol <- function(col, expectedType, minValue, maxValue) {
  span_red <- \(...) span(style = "color: red;", ...)
  info <- NULL
  msg <- NULL
  valid <- FALSE
  
  if (anyNA(col)) {
    msg <- list(
      span_red("Warning: Missing value in data"),
      br()
    )
  }
  
  colType <- class(col)
  info <- list(paste("Type:", colType), br())
  
  # column type mismatch
  if (!is.na(expectedType) & colType != expectedType) {
    msg <- append(msg, list(
      span_red("Error: Incorrect column type, expected", expectedType),
      br()
    ))
  }
  
  # Numeric value checks
  if (colType == "numeric") {
    info <- append(info, list(
      paste0("Range: ", round(min(col), 2), " 🡒 ", round(max(col), 2)),
      br()
    ))

    # min check
    if (!is.na(minValue) & min(col) < minValue) {
      msg <- append(msg, list(
        span_red("Error: Min value less than", minValue),
        br()
      ))
    }
    
    # max check
    if (!is.na(maxValue) & max(col) > maxValue) {
      msg <- append(msg, list(
        span_red("Error: Max value greater than ", maxValue),
        br()
      ))
    }
  }
  
  # valid if no messages yet
  if (is.null(msg)) {
    msg <- list(span(strong("OK"), style = "color: blue;"))
    valid <- TRUE
  } else {
    msg <- head(msg, -1) # remove the last br()
  }
  
  list(
    valid = valid,
    ui = tagList(info, msg)
  )
}
