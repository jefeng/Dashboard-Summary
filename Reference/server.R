library(shiny)


shinyServer(function(input, output,session) {
  days <-reactive({
    return(input$days)
  })
}) 