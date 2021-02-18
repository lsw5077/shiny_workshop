# wrapper for shiny::shinyApp()

launchApp <- function() {
  shinyApp(ui = ui, server = server)
}