
#' @description a shared ui component
#' @param ns namespace function from calling server
#' @param results reactive model results from calling server

modelResultsUI <- function(ns, results, held) {
  renderUI({
    namedWell(
      title = "Model results",
      renderTable({
        res <- results()
        
        validate(need(res != "No data", "No data selected."))
        validate(need(is.list(res), sprintf("Unable to compute model, try adjusting parameters. Reason: %s", res)), errorClass = "model-results")
        
        # convert list to simple data frame
        res %>%
          enframe() %>%
          unnest(value) %>%
          rename(
            Parameter = name,
            Value = value
          ) %>%
          mutate(Hold = lapply(Parameter, function(p) {
            if (p == "Correlation") {
              ""
            } else {
              HTML(sprintf(
                "<input type='checkbox' %s onclick=\"Shiny.setInputValue('%s-hold', this.checked, {priority: 'event'})\">",
                ifelse(held()[[p]], "checked", ""),
                ns(p)
              ))
            }
          }))
      },
        digits = 4,
        width = "100%",
        striped = FALSE,
        hover = TRUE,
        sanitize.text.function = function(x) x,
        align = "llc"
      )
    )
  })
}
