
# Data loading
data <- read.csv("plantilla_llena.csv")
# Borrar estación con id 44 de la existencia xd 
estaciones <- read.csv("hubway_stations.csv")
colnames(estaciones)[6] <- 'long'

datos <- data[, c(2:4,11:14)]

datos$date <- as.Date(datos$date)

datos$weekday <- weekdays(datos$date)

datos <- aggregate(cbind(acum_llegada_real, 
                         acum_llegada_pred_rf, acum_llegada_pred_lasso,
                         acum_llegada_pred_xgb) ~ strt_statn + weekday + hour, 
                   data = datos, FUN = mean)

datos$minutes <- format(strptime(datos$hour,"%H:%M:%S"),'%M')
datos$minutes <- ifelse(datos$minutes=='00', 1, 
                        ifelse(datos$minutes=='10', 2, 
                               ifelse(datos$minutes=='20', 3,
                                      ifelse(datos$minutes=='30', 4,
                                             ifelse(datos$minutes=='40', 5, 6))))) 

datos$hour <- format(strptime(datos$hour,"%H:%M:%S"),'%H')

colnames(datos)[1] <- 'id'

save(datos, file = "dashPredicts.RData")

load('dashPredicts.RData')






