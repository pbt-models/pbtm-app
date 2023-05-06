# ---- trtIdSelect ---- #

#' @param ns namespace function from calling server
#' @param data reactive dataset from which to pull TrtID values

trtIdSelect <- function(ns, data) {
  renderUI({
    if ("TrtDesc" %in% names(data())) {
      choices <- data() %>%
        mutate(Label = paste(TrtID, TrtDesc, sep = ": ")) %>%
        distinct(Label, TrtID) %>%
        mutate(Label = str_trunc(Label, 30)) %>%
        deframe()
    } else {
      choices <- unique(data()$TrtID)
    }
    
    box(
      width = 6,
      title = "Additional treatment filters",
      checkboxGroupInput(
        inputId = ns("trtIdSelect"),
        label = "Treatment ID:",
        choices = choices,
        selected = choices
      )
    )
  })
}
