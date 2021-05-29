library(shiny)

##########################################
# Data loading
load('dashPredicts.RData')
colnames(datos)[c(2,3,8)] <- c('day','hour','min_group')
colnames(datos)[c(4:7)] <- c('real','rf','lasso','xgb')
datos[, c(4:7)] <- round(datos[, c(4:7)])
datos$hour <- as.integer(datos$hour)
datos$min_group <- as.integer(datos$min_group)
#datos <- read.csv("datos_fake.csv") # Para pruebas
estaciones <- read.csv("hubway_stations.csv")
estaciones <- estaciones[estaciones$id!=44, ] # Sin predicciones posibles
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
  
  # Íconos
  bikeIcon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/5/55/BicycleMarkerSymbol.png",
    iconWidth = 31.5, iconHeight = 49.6, # original: 315*496
    iconAnchorX = 20, iconAnchorY = 50
  )
  # Render
  output$bikemap <- renderLeaflet({
    selec_day <- weekdays(input$date_input)
    #selec_hr <- format(strptime(input$time_input,"%H:%M:%S"),'%H')
    #selec_mn <- as.integer(format(strptime(input$time_input,"%H:%M:%S"),'%M'))
    #output$time_output <- renderText(strftime(input$time_input, "%M"))
    
    selec_hr <- as.integer(strftime(input$time_input, "%H"))
    selec_mn <- as.integer(strftime(input$time_input, "%M"))
    if (selec_mn < 10) {
      selec_mn <- '1'
    } else if (selec_mn < 20) {
      selec_mn <- '2'
    } else if (selec_mn < 30) {
      selec_mn <- '3'
    } else if (selec_mn < 40) {
      selec_mn <- '4'
    } else if (selec_mn < 50) {
      selec_mn <- '5'
    } else {
      selec_mn <- '6'
    }
    aux <- datos[datos$day==selec_day, ]
    aux <- aux[aux$hour==selec_hr, ]
    aux <- aux[aux$min_group==selec_mn, ]
    aux <- aux[order(aux$id),]
    
    estaciones <- merge(estaciones, aux[, c(1, 4:7)], by = 'id')
    estaciones[,c(8:11)] <- ifelse(estaciones[,7]=="Removed",0, estaciones[,c(8:11)])
    # a partir de 7 empieza lo merged
    # estaciones$predic <- aux$
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
                       ) %>%
      addMarkers(data = estaciones, 
                 clusterOptions = markerClusterOptions(),
                 label = ~as.character(xgb),
                 popup = paste(
                   "<b>",estaciones$municipal,", MA","</b>","<br>",
                   "<b>Estación: </b>",estaciones$station,"<br>",
                   "<b>Terminal: </b>",estaciones$terminal,"<br>",
                   "<b>Estatus: </b>",estaciones$status,"<br>",
                   "<b>Promedio real: </b>",estaciones$real,"<br>",
                   "<b>Predicción XGB: </b>",estaciones$xgb,"<br>",
                   "<b>Predicción Random Forest: </b>",estaciones$rf,"<br>",
                   "<b>Predicción Lasso: </b>",estaciones$lasso, 
                   sep=""),
                 icon = bikeIcon
                 )
  })
  
  ###########################################
  # Botones
  observeEvent(input$current_time, {
    updateTimeInput(session, "time_input", value = (Sys.time()-5*3600))
  })
  
  observeEvent(input$current_date, {
    updateDateInput(session, "date_input", value = Sys.time())
  })
  
}

