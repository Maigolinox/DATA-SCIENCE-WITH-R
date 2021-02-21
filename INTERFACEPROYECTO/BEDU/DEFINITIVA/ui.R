
library(shiny)
library(shinyjs)
library(shinyBS)

# source("LECTURADATA.R")


shinyUI(navbarPage(title = "EQUIPO 4-BEDU",
                   theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                   fluid = TRUE, 
                   collapsible = TRUE,
                   tabPanel(
                       "INICIO",
                       includeHTML("home.html"),
                       tags$script(src = "plugins/scripts.js"),
                       tags$head(
                           tags$link(rel = "stylesheet", 
                                     type = "text/css", 
                                     href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                           tags$link(rel = "icon", 
                                     type = "image/png", 
                                     href = "images/2.png")
                       )
                   ),
                   
                   tabPanel("ANÁLISIS",
                            includeHTML("ANALISIS.html"),
                            shinyjs::useShinyjs(),
                            tags$head(
                                tags$link(rel = "stylesheet", 
                                          type = "text/css", 
                                          href = "plugins/carousel.css"),
                                tags$script(src = "plugins/holder.js")
                            ),
                            
                   ),
                   
                   
                   tabPanel("INTEGRANTES DEL EQUIPO 4",
                            includeHTML("about.html"),
                            shinyjs::useShinyjs(),
                            tags$head(
                                tags$link(rel = "stylesheet", 
                                          type = "text/css", 
                                          href = "plugins/carousel.css"),
                                tags$script(src = "plugins/holder.js")
                            ),
                            
                   )
                   )
    
    
)
