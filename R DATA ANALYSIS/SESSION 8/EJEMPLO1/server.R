#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) { 
    output$output_text <- renderText(paste("Grafico de mpg ~ ", input$x))   # input$x es la selección que se hizo en la UI
    output$output_plot <- renderPlot(plot(as.formula(paste("mpg~", input$x)), 
                                          data = mtcars))
}
)