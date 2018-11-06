library(readr)
library(tidyr)
raw <- data.frame(read_csv("dirtyDataEmpresa.csv"))
View(raw)
#para buscar el valor especifico de Q-
str(raw)

#subset para ekiminar los Q - del camuion_5  para todos los valores, luego lo hago para todos, y lo convierto a NA. 
raw$Camion_5[raw$Camion_5 == "Q-"] <- NA
raw$Pickup[raw$Pickup == "Q-"] <- NA
raw$Moto[raw$Moto == "Q-"] <- NA
raw$directoCamion_5[raw$directoCamion_5 == "Q-"] <- NA
raw$directoPickup[raw$directoPickup == "Q-"] <- NA
raw$directoMoto[raw$directoMoto == "Q-"]<- NA
raw$fijoCamion_5[raw$fijoCamion_5 == "Q-"]<- NA
raw$fijoPickup[raw$fijoPickup == "Q-"]<-NA
raw$fijoMoto[raw$fijoMoto == "Q-"]<-NA

raw$X5.30[raw$X5.30 == "x"] <- 1
raw$X30.45[raw$X30.45 == "x"] <- 1
raw$X45.75[raw$X45.75 == "x"] <-1
raw$X75.120[raw$X75.120 == "x"] <- 1
raw$X120.[raw$X120.=="x"] <- 1

raw$X23 <- NULL
raw$X24 <- NULL
raw$X25 <- NULL
raw$X26 <- NULL
raw$X27 <- NULL
raw$X28 <- NULL

options(digits = 6)

# dejar solo los valores que no tienen NA. 
raw[is.na(raw$Camion_5)== FALSE, ]
raw[is.na(raw$Pickup)== FALSE, ]
raw[is.na(raw$X5.30)== FALSE, ]
# dos columnas transporte y valor

#MELTS para resumir transporte, tipo_costo, tiempo

rawm <-  raw %>%
  gather(Transporte, Costo_Total, Camion_5:Moto, na.rm = TRUE)
head(rawm)
View(rawm)


#rawm_TipoCostoDirecto <- rawm %>%
  #gather(Tipo_Costo, Costo_Directo, directoCamion_5:fijoMoto, na.rm = TRUE)

rawm_TipoCostoDirecto <- rawm %>%
  gather(Costo_Directo, Q_Costo_Directo, directoCamion_5:directoMoto, na.rm = TRUE)
rawm_TipoCostoFijo <- rawm_TipoCostoDirecto %>%
  gather(Costo_Fijo, Q_Costo_Fijo, fijoCamion_5:fijoMoto, na.rm = TRUE)

raw_final <- rawm_TipoCostoFijo %>%
  gather(Rango_Tiempo, SI_NO,X5.30:X120., na.rm = TRUE)
raw_final$SI_NO<- NULL

#Quitar las Qs de dinero y convertir los valores a numericos
#variable temporal para almacenar la data
raw_final$Factura <- as.numeric(gsub("Q", "", raw_final$factura))
raw_final$CostoFijo <- as.numeric(gsub("Q","", raw_final$Q_Costo_Fijo))
raw_final$CostoDirecto <- as.numeric(gsub("Q", "", raw_final$Q_Costo_Directo))
raw_final$CostoTotal <- as.numeric(gsub("Q", "", raw_final$Costo_Total))


raw_final$factura <- raw_final$Factura
raw_final$Factura <- NULL

raw_final$Q_Costo_Fijo <- raw_final$CostoFijo
raw_final$CostoFijo<- NULL

raw_final$Q_Costo_Directo <- raw_final$CostoDirecto
raw_final$CostoDirecto <- NULL

raw_final$Costo_Total <- raw_final$CostoTotal
raw_final$CostoTotal <- NULL

#voy a eliminar las variables Costo_Fijo y Costo_Directo porque solo me interesa el total y ese dato esta en la varibale creada en los melts de Q_Costo_Directo y fijo
raw_final$Costo_Directo <- NULL
raw_final$Costo_Fijo <- NULL

#solo para ver si podia cambiar los valores de Rango Tiempo
raw_final2 <- raw_final
raw_final2$Rango_Tiempo<- gsub("X5.30", "5-30", raw_final2$Rango_Tiempo)
View(raw_final2)
rm(raw_final2)


str(raw_final)
head(rawm_CostoDirecto)
View(rawm_TipoCosto)
View(raw_final)
rm(rawm_TipoCostoFijo, rawm_TipoCostoDirecto,rawm, raw_final, raw)

write.table(raw_final,file="dataLimpia.csv", sep=";", dec=".", row.names = FALSE)

raw_final[raw_final$factura < raw_final$Costo_Total,]
