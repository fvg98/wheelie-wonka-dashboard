## ui.R ##
library(shiny)
library(shinydashboard)
library(shinythemes)
library (leaflet)
library(shinyTime)

###### PRINCIPAL ###### 
navbarPage(theme = shinytheme("united"),id = "navibar",
           title = tags$a(tags$img(src='W.png', 
                                   height=28,style=
                                     "display: block; 
                                   margin-left: auto; 
                                   margin-right: auto; 
                                   display: flex; 
                                   align-items: center; 
                                   justify-content: center;")),
           windowTitle = HTML("Wheelie Wonka"),
           ###################################################
           # Panel: Mapa
           tabPanel("Mapa interactivo", 
                    titlePanel(div(
                      windowTitle = "Landing page",
                      htmlOutput("header_1"))),
                    tags$br(),
                    fluidPage(
                      sidebarLayout(
                        # Controles
                        sidebarPanel(
                          width = 3,
                          
                          dateInput("date_input", 
                                    h4("Fecha"), 
                                    value = "2021-05-01",
                                    language = "es"
                                    ),
                          
                          actionButton("current_date", "Hoy", width = 125),
                          
                          timeInput("time_input", 
                                    h4("Hora"), 
                                    value = strptime("07:00:00", "%T"), minute.steps = 10, seconds = F),
                          
                          actionButton("current_time", "Hora actual", width = 125),
                          
                          
                          ),
                        # Mapa
                        mainPanel(
                          column(width = 12, leafletOutput("bikemap", width = "100%"))#,
                          #textOutput("time_output")
                          )
                        )
                      )
                    ),
           navbarMenu("Contacto",
                      tabPanel(title=HTML("</a></li><li><a href='https://github.com/marcoyel21/economia_computacional' target='_blank'>Repositorio en GitHub")),        
                      tabPanel(title=HTML("</a></li><li><a href='https://github.com/marcoyel21/economia_computacional' target='_blank'>Correo"))
           )
                    
           ###################################################
           )

              