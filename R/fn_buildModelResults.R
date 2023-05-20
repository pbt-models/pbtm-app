
#' @description builds the model results list
#' @param model the nls model object
#' @param params list of param names
#' @param defined named list of user-defined param values
#' @param CumFraction vector of cumulative fraction values to correlate the model against
#' @returns a named list of model results

buildModelResults <- function(model, params, defined, CumFraction) {
  coefs <- getModelCoefs(model)
  results <- list()
  for (p in params) results[[p]] <- c(defined[[p]], coefs[[p]])[1]
  results$Correlation <- cor(CumFraction, predict(model)) ^ 2
  results
}
