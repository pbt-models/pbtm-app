# ---- trtIdSelect ---- #

#' @param ns namespace function from calling server
#' @param data reactive dataset from which to pull TrtID values

trtIdSelect <- function(ns, data) {
  renderUI({
    if ("TrtDesc" %in% names(data())) {
      choices <- data() %>%
        mutate(Label = sprintf("%s: %s", TrtID, TrtDesc)) %>%
        distinct(Label, TrtID) %>%
        mutate(Label = str_trunc(Label, 30)) %>%
        deframe()
    } else {
      choices <- unique(data()$TrtID)
    }
    
    div(
      div(
        class = "well-title",
        "Additional treatment filters"
      ),
      div(
        class = "well",
        checkboxGroupInput(
          inputId = ns("trtIdSelect"),
          label = "Treatment ID:",
          choices = choices,
          selected = choices,
          inline = TRUE
        )
      )
    )
  })
}
