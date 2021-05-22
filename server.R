library(shiny)

##########################################
# Data loading
datos <- read.csv("datos_fake.csv")
estaciones <- read.csv("hubway_stations.csv")
colnames(estaciones)[6] <- 'long'

##########################################
# Server
server <- function(session, input, output) {
  ###########################################
  #Texto a mostrar en el landing page
  output$header_1 <- renderText({
    paste("<center><b>Consulta de disponibilidad por estaciones</b></center>")
  })
  
  ###########################################
  # Hora del usuario
  output$user_time <- reactive(as.numeric(input$client_time) / 1000) # in s

  ###########################################
  # Mapa
  
  bikeIcon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/5/55/BicycleMarkerSymbol.png",
    iconWidth = 31.5, iconHeight = 49.6, # original: 315*496
    iconAnchorX = 20, iconAnchorY = 50
  )
  
  output$bikemap <- renderLeaflet({
    selec_day <- weekdays(input$date_input)
    #selec_hr <- format(strptime(input$time_input,"%H:%M:%S"),'%H')
    #selec_mn <- as.integer(format(strptime(input$time_input,"%H:%M:%S"),'%M'))
    #output$time_output <- renderText(strftime(input$time_input, "%M"))
    
    selec_hr <- strftime(input$time_input, "%H")
    selec_mn <- as.integer(strftime(input$time_input, "%M"))
    if (selec_mn <= 10) {
      selec_mn <- '1'
    } else if (selec_mn <= 20) {
      selec_mn <- '2'
    } else if (selec_mn <= 30) {
      selec_mn <- '3'
    } else if (selec_mn <= 40) {
      selec_mn <- '4'
    } else if (selec_mn <= 50) {
      selec_mn <- '5'
    } else {
      selec_mn <- '6'
    }
    aux <- datos[datos$day==selec_day, ]
    aux <- datos[datos$hour==selec_hr, ]
    aux <- datos[datos$min_group==selec_mn, ]
    
    estaciones$predic <- t(datos[1,c(4:145)])
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
                       ) %>%
      addMarkers(data = estaciones, 
                 clusterOptions = markerClusterOptions(),
                 label = ~as.character(predic),
                 popup = paste(
                   "<b>",estaciones$municipal,", MA","</b>","<br>",
                   "<b>Estación: </b>",estaciones$station,"<br>",
                   "<b>Terminal: </b>",estaciones$terminal,"<br>",
                   "<b>Estatus: </b>",estaciones$status,"<br>",
                   "<b>Bicicletas disponibles: </b>",estaciones$predic, 
                   sep=""),
                 icon = bikeIcon
                 )
  })
  
  ###########################################
  # Botones
  observeEvent(input$current_time, {
    updateTimeInput(session, "time_input", value = Sys.time())
  })
  
  observeEvent(input$current_date, {
    updateDateInput(session, "date_input", value = Sys.time())
  })
  
}

