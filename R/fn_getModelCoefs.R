
#' @description gets the coefficient estimates from an nls model
#' @param model the nls model
#' @returns a named list

getModelCoefs <- function(model) {
  summary(model)$coefficients %>%
    as_tibble(rownames = "Param") %>%
    select(1:2) %>%
    deframe() %>%
    round(4) %>%
    as.list()
}
