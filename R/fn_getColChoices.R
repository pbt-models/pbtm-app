
#' @description generates a named list of unique values in a column and adds the count
#' @param df the data frame
#' @param col string specifying column name
#' @returns named list

getColChoices <- function(df, col) {
  df %>%
    count(.data[[col]]) %>%
    select(n, everything()) %>%
    mutate(n = sprintf("%s (n = %s)", .data[[col]], n)) %>%
    deframe()
}
