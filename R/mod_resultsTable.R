# ---- resultsTable ---- #

#' @param ns namespace function from calling server
#' @param results reactive model results from calling server

resultsTable <- function(ns, results) {
  box(
    width = 6,
    title = "Model results",
    renderTable({
      res <- results()
      
      validate(need(is.list(res), paste("Model failed to compute, try adjusting parameters. Reason:", res)))
      
      # convert list to simple data frame
      res %>%
        enframe() %>%
        unnest(value) %>%
        rename(
          Parameter = name,
          Value = value
        )
    },
      digits = 4,
      width = "100%"
    )
  )
}
