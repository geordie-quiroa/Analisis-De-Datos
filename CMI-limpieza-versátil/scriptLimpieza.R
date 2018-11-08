library(dplyr)
library(stringr)
library(lubridate)

############################################ CORRER BLOQUE DE HASTA ABAJO #####################################################

################################### PARA USARLO, CAMBIAR EL PATH DE LOS ARCHIVOS QUE VAN A CARGAR (HASTA ABAJO) ###############


##################################### Funcion a partir del código correcto

##################################################################################################################################

#                              CORRER ESTOS ÚLTIMOS BLOQUES DE CÓDIGO PARA QUE TODO FUNCIONE 

##################################################################################################################################


files <- list.files(path = "D:/geord/Docs/Data Wrangling/Github/CMI-limpieza/versatil/files") ###### cambiar este path al de ustedes
length(files)

bloqueIndividual <- function (archivoCSV){
  dataSample <- read.csv(archivoCSV, header = FALSE, stringsAsFactors=FALSE )
  tmpFile <- dataSample
  tmpFile[8:ncol(dataSample)]<- NULL
  names(tmpFile)<-c("FECHA", "TIENDA", "CODPROD", "DESC", "MEDIDA", "UNI","VENTA")
  tmpFile$CODPROD <- as.factor(tmpFile$CODPROD)
  contador <- ncol(dataSample)-9 # el nueve puede ser z 
  
  for(i in 1:contador) { # FALLA en el segundo archivo
    if ( isTRUE(str_detect(dataSample[1,i], "VENTAS POR PRODUCTO DEL"))) { # la columna i contiene la FECHA 
      tmpFile["FECHA"] <- str_extract(dataSample[,i], "([0-9]{2}\\/[0-9]{2}\\/[0-9]{4})+")
      tmpFile$FECHA <- dmy(tmpFile$FECHA)
    } else if (isTRUE(str_detect(dataSample[1,i], "BODEGA O TIENDA:"))) { # la columna i contiene la Tienda 
      dataSample[,i] <-str_extract(dataSample[,i], "Z[\\w]+") #RegEx para la zona de la tienda
      tmpFile["TIENDA"] <- dataSample[i]
    } else if (isTRUE(str_detect(dataSample[1,i], "CODPROD"))) { # la columna i contiene la FECHA 
      tmpFile["CODPROD"] <- dataSample[i+9]
    } else if(isTRUE(str_detect(dataSample[1,i], "DESCRIPC"))) { # la columna i contiene la FECHA 
      tmpFile["DESC"] <- dataSample[i+9]
    } else if (isTRUE(str_detect(dataSample[1,i],"MEDIDA"))){ # la columna i contiene la FECHA 
      tmpFile["MEDIDA"]<- dataSample[i+9]
    }else if (isTRUE(str_detect(dataSample[1,i],"UNI")) && str_length(dataSample[1,i])==3){ # la columna i contiene la FECHA 
      tmpFile["UNI"] <-dataSample[i+9]
    } else if (isTRUE(str_detect(dataSample[1,i], "VENTA"))){ # la columna i contiene la FECHA 
      tmpFile["VENTA"] <-dataSample[i+9]
    }
  }
  return(tmpFile)
}
f=1
while (f <=length(files)){
  if (f == 1){
    tablaMaestra <- bloqueIndividual(files[f])
  } else {
    tablaLimpia <- bloqueIndividual(files[f])
    tablaMaestra<- rbind(tablaMaestra, tablaLimpia)
  }
  f = f+1
}
tablaMaestra <- tablaMaestra %>%
  mutate(precioUnitario = VENTA/UNI)%>%
  filter(!is.na(precioUnitario))

#test <- bloqueIndividual("080118-12.csv")
#View(test)
View(tablaMaestra)






