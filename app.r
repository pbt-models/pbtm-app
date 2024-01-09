# Population-based threshold models dashboard
# Ben Bradford, UW-Madison
# Pedro Bello, UC Davis


shiny::shinyApp(ui, server)


#- Dependencies -#

#' app.R
#' - global.R
#' - ui.R
#' - server.R
#' - modules in ./R


#- Renv for pkg management -#

# renv::init()         # initiate renv if not already
# renv::dependencies() # show project dependencies
# renv::update()       # update project libraries
# renv::clean()        # remove unused packages
# renv::snapshot()     # save updated lock file to project
# renv::restore()      # restore versions from lockfile


#- Testing -#

# shiny::devmode(TRUE)
# shiny::devmode(FALSE)
