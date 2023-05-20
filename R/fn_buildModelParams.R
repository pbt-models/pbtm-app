
#' @description sets up lists for model run in the calling environment
#' @param setParams named list of params with value or `NA`
#' @param paramRanges named list of (lower, start, upper) param constraints
#' @returns no return value, instead it directly manipulates variables in the calling environment

buildModelParams <- function(setParams, paramRanges) {
  env <- parent.frame()
  defined <- lower <- start <- upper <- list()
  for (p in names(paramRanges)) {
    paramValue <- setParams[[p]]
    if (truthy(paramValue)) {
      defined[[p]] <- paramValue
      assign(p, paramValue, envir = env)
    } else {
      if (exists(c(p))) remove(list = c(p), envir = env)
      ranges <- paramRanges[[p]]
      lower[[p]] <- ranges[1]
      start[[p]] <- ranges[2]
      upper[[p]] <- ranges[3]
    }
  }
  assign("defined", defined, envir = env)
  assign("lower", lower, envir = env)
  assign("start", start, envir = env)
  assign("upper", upper, envir = env)
}
