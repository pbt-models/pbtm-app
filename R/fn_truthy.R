# ---- truthy ---- #

#' @param x a value to be evaluated as `TRUE` or `FALSE`
#' @returns boolean

truthy <- function(x) {
  # null values are false
  if (is.null(x)) return(FALSE)
  
  # errors are false
  if (inherits(x, "try-error")) return(FALSE)
  
  # functions are true
  if (is.function(x)) return(TRUE)
  
  # empty vectors, lists, or other are false
  if (length(x) == 0) return(FALSE)
  
  # data frames are truthy if nrows > 0
  if (is.data.frame(x)) return(nrow(x) > 0)
  
  # non-atomic objects are true
  if (!is.atomic(x)) return(TRUE)

  # vectors containing only NA are false
  if (all(is.na(x))) return(FALSE)
  
  # empty strings are false
  if (is.character(x) && !any(nzchar(stats::na.omit(x)))) return(FALSE)
  
  # all-false vectors are false
  if (is.logical(x) && !any(stats::na.omit(x))) return(FALSE)
  
  # anything with length > 1 is true
  if (length(x) > 1) return(TRUE)

  # these special cases are false
  if (x %in% c(NA, "NA", "", 0, F)) return(FALSE)
  
  # anything else
  TRUE
}
