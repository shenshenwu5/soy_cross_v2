library(shiny)
source("R/mod_parent.R")
ui <- parent_admin_ui("admin")
server <- function(input, output, session) {
  parent_admin_server("admin", db_path = "data/db/soy_cross.db")
}
shiny::runApp(shinyApp(ui = ui, server = server))

