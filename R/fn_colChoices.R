colChoices <- function(df, col) {
  df %>%
    count(.data[[col]]) %>%
    select(n, everything()) %>%
    mutate(n = sprintf("%s (n = %s)", .data[[col]], n)) %>%
    deframe()
}
