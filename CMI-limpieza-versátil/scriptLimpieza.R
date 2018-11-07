library(dplyr)
library(stringr)
library(lubridate)

############################################ CORRER BLOQUE DE HASTA ABAJO #####################################################

################################### PARA USARLO, CAMBIAR EL PATH DE LOS ARCHIVOS QUE VAN A CARGAR (HASTA ABAJO) ###############

dataSample <- read.csv("060118-12.csv", header = FALSE )
View(dataSample)

dataSample$select <- complete.cases(dataSample[1:10])

?ncol()
tmpFile <- dataSample
tmpFile[8:ncol(dataSample)]<- NULL
names(tmpFile)<-c("FECHA", "TIENDA", "CODPROD", "DESC", "MEDIDA", "UNI","VENTA")
View(tmpFile)

example <- "BODEGA O TIENDA: Z12 TIENDA 05/08/2018"
zona  <- "([0-9]{2}\\/[0-9]{2}\\/[0-9]{4})+"

str_detect(example, zona)
example<-str_extract(example, zona)
example

?str_extract()
str_detect(dataSample[1,1], "VENTAS POR PRODUCTO DEL")
?which()

for(i in 1:ncol(dataSample)) { # FALLA en el segundo archivo
  contador <- 0
  if(contador < 7){
    if ( str_detect(dataSample[1,i], "VENTAS POR PRODUCTO DEL") == TRUE) { # la columna i contiene la FECHA 
      tmpFile["FECHA"] <- str_extract(dataSample[,i], "([0-9]{2}\\/[0-9]{2}\\/[0-9]{4})+")
      tmpFile$FECHA <- dmy(tmpFile$FECHA)
      contador <- contador+1
      
    } else if ( str_detect(dataSample[1,i], "BODEGA O TIENDA:") == TRUE) { # la columna i contiene la Tienda 
      dataSample[,i] <-str_extract(dataSample[,i], "Z[\\w]+") #RegEx para la zona de la tienda
      tmpFile["TIENDA"] <- dataSample[i]
      contador <- contador+1
      #print(dataSample[,i])
      #print(tmpFile$TIENDA)
    } else if (str_detect(dataSample[1,i], "CODPROD") == TRUE) { # la columna i contiene la FECHA 
      tmpFile["CODPROD"] <- dataSample[i+9]
      contador <- contador+1
    } else if(str_detect(dataSample[1,i], "DESCRIPC") == TRUE) { # la columna i contiene la FECHA 
      tmpFile["DESC"] <- dataSample[i+9]
      contador <- contador+1
    } else if (str_detect(dataSample[1,i],"MEDIDA")==TRUE){ # la columna i contiene la FECHA 
      tmpFile["MEDIDA"]<- dataSample[i+9]
      contador <- contador+1
    }else if (str_detect(dataSample[1,i],"UNI")==TRUE){ # la columna i contiene la FECHA 
      tmpFile["UNI"] <-dataSample[i+9]
      contador <- contador+1
    } else if (str_detect(dataSample[1,i], "VENTA")==TRUE ){ # la columna i contiene la FECHA 
      tmpFile["VENTA"] <-dataSample[i+9]
      contador <- contador+1
    }
  } else {
    contadorArchivosProcesados <- contadorArchivosProcesados+1
    break
  }
}


### CORREGIR FECHASSSS

tmpFile <- tmpFile  %>%
  mutate(costoUnit = VENTA/UNI)
#ArchivoFuncionalPrimero <- tmpFile
# testing for loop -------------------------------------------------------------
for(i in 1:58) {
  if ( str_detect(dataSample[1,i], "VENTAS POR PRODUCTO DEL") == TRUE) { # la columna i contiene la FECHA 
    print(dataSample[i])
    tmpFile["FECHA"] <- str_extract(dataSample[,i], "([0-9]{2}\\/[0-9]{2}\\/[0-9]{4})+")
    #dataSample[,i] <-str_replace(dataSample[,i], "FECHA ", "")
    #tmpFile["FECHA"] <- dataSample[i]
    #tmpFile$FECHA <- dmy(tmpFile$FECHA)
    #contador <- contador+1
  } else {
    
  }
}
?paste
View(dataSample)

files <- list.files(path = "D:/geord/Docs/Data Wrangling/final/files")
contadorArchivosProcesados <- 1

for (f in 1:length(files)){  
  dataSample <- read.csv(files[f], header = FALSE )
  tmpFile <- dataSample
  tmpFile[8:ncol(dataSample)]<- NULL
  names(tmpFile)<-c("FECHA", "TIENDA", "CODPROD", "DESC", "MEDIDA", "UNI","VENTA")
  for(i in 1:ncol(dataSample)) { # FALLA en el segundo archivo
    contador <- 0
    if(contador < 7){
      if ( isTRUE(str_detect(dataSample[1,i], "VENTAS POR PRODUCTO DEL"))) { # la columna i contiene la FECHA 
        tmpFile["FECHA"] <- str_extract(dataSample[,i], "([0-9]{2}\\/[0-9]{2}\\/[0-9]{4})+")
        tmpFile$FECHA <- dmy(tmpFile$FECHA)
        contador <- contador+1
        
      } else if ( str_detect(str_detect(dataSample[1,i], "BODEGA O TIENDA:"))) { # la columna i contiene la Tienda 
        dataSample[,i] <-str_extract(dataSample[,i], "Z[\\w]+") #RegEx para la zona de la tienda
        tmpFile["TIENDA"] <- dataSample[i]
        contador <- contador+1
        #print(dataSample[,i])
        #print(tmpFile$TIENDA)
      } else if (str_detect(str_detect(dataSample[1,i], "CODPROD"))) { # la columna i contiene la FECHA 
        tmpFile["CODPROD"] <- dataSample[i+9]
        contador <- contador+1
      } else if(str_detect(str_detect(dataSample[1,i], "DESCRIPC"))) { # la columna i contiene la FECHA 
        tmpFile["DESC"] <- dataSample[i+9]
        contador <- contador+1
      } else if (str_detect(str_detect(dataSample[1,i],"MEDIDA"))){ # la columna i contiene la FECHA 
        tmpFile["MEDIDA"]<- dataSample[i+9]
        contador <- contador+1
      }else if (str_detect(str_detect(dataSample[1,i],"UNI"))){ # la columna i contiene la FECHA 
        tmpFile["UNI"] <-dataSample[i+9]
        contador <- contador+1
      } else if (str_detect(str_detect(dataSample[1,i], "VENTA"))){ # la columna i contiene la FECHA 
        tmpFile["VENTA"] <-dataSample[i+9]
        contador <- contador+1
      }
    } else {
      break
    }
  }
  if (contadorArchivosProcesados == 1){
    tablaMaestra <- tmpFile
  } else {
    rbind(tablaMaestra, tmpFile)
  }
  contadorArchivosProcesados <- contadorArchivosProcesados+1
}



hola <- function(){
  files <- list.files(path = "D:/geord/Docs/Data Wrangling/final/files")
  contadorArchivosProcesados <- 1
  
  for (f in 1:length(files)){  
    dataSample <- read.csv(files[f], header = FALSE )
    tmpFile <- dataSample
    tmpFile[8:ncol(dataSample)]<- NULL
    names(tmpFile)<-c("FECHA", "TIENDA", "CODPROD", "DESC", "MEDIDA", "UNI","VENTA")
    for(i in 1:ncol(dataSample)) { # FALLA en el segundo archivo
      contador <- 0
      if(contador < 7){
        if ( isTRUE(str_detect(dataSample[1,i], "VENTAS POR PRODUCTO DEL"))) { # la columna i contiene la FECHA 
          tmpFile["FECHA"] <- str_extract(dataSample[,i], "([0-9]{2}\\/[0-9]{2}\\/[0-9]{4})+")
          tmpFile$FECHA <- dmy(tmpFile$FECHA)
          contador <- contador+1
          
        } else if (isTRUE(str_detect(dataSample[1,i], "BODEGA O TIENDA:"))) { # la columna i contiene la Tienda 
          dataSample[,i] <-str_extract(dataSample[,i], "Z[\\w]+") #RegEx para la zona de la tienda
          tmpFile["TIENDA"] <- dataSample[i]
          contador <- contador+1
          #print(dataSample[,i])
          #print(tmpFile$TIENDA)
        } else if (isTRUE(str_detect(dataSample[1,i], "CODPROD"))) { # la columna i contiene la FECHA 
          tmpFile["CODPROD"] <- dataSample[i+9]
          contador <- contador+1
        } else if(isTRUE(str_detect(dataSample[1,i], "DESCRIPC"))) { # la columna i contiene la FECHA 
          tmpFile["DESC"] <- dataSample[i+9]
          contador <- contador+1
        } else if (isTRUE(str_detect(dataSample[1,i],"MEDIDA"))){ # la columna i contiene la FECHA 
          tmpFile["MEDIDA"]<- dataSample[i+9]
          contador <- contador+1
        }else if (isTRUE(str_detect(dataSample[1,i],"UNI"))){ # la columna i contiene la FECHA 
          tmpFile["UNI"] <-dataSample[i+9]
          contador <- contador+1
        } else if (isTRUE(str_detect(dataSample[1,i], "VENTA"))){ # la columna i contiene la FECHA 
          tmpFile["VENTA"] <-dataSample[i+9]
          contador <- contador+1
        }
      } else {
        break
      }
    }
    if (contadorArchivosProcesados == 1){
      tablaMaestra <- tmpFile
    } else {
      rbind(tablaMaestra, tmpFile)
    }
    contadorArchivosProcesados <- contadorArchivosProcesados+1
  }
  
  
}


View(tablaMaestra)


rm(ArchivoFuncionalPrimero, dataSample, file1, tmpFile)
rm(contador, contadorArchivosProcesados, example, files, i,r,zona)
rm(tablaMaestra,f, hola)
dataSample[,4]

hola()
####################### Codigo correcto
dataSample <- read.csv("060118-12.csv", header = FALSE )
tmpFile <- dataSample
tmpFile[8:ncol(dataSample)]<- NULL
names(tmpFile)<-c("FECHA", "TIENDA", "CODPROD", "DESC", "MEDIDA", "UNI","VENTA")

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

View(tmpFile)
############### agregar codigo correcto a funcion ESTE TODAVIA DA PROBLEMAS
files <- list.files(path = "D:/geord/Docs/Data Wrangling/final/files")
contadorArchivosProcesados <- 0
for (f in 1:length(files)){
  contador <- 0
  dataSample <- read.csv(files[f], header = FALSE )
  tmpFile <- dataSample
  tmpFile[8:ncol(dataSample)]<- NULL
  names(tmpFile)<-c("FECHA", "TIENDA", "CODPROD", "DESC", "MEDIDA", "UNI","VENTA")
  
  for(i in 1:ncol(dataSample)) { 
    if(contador <= 7){
      if ( isTRUE(str_detect(dataSample[1,i], "VENTAS POR PRODUCTO DEL"))) { # la columna i contiene la FECHA 
        tmpFile["FECHA"] <- str_extract(dataSample[,i], "([0-9]{2}\\/[0-9]{2}\\/[0-9]{4})+")
        tmpFile$FECHA <- dmy(tmpFile$FECHA)
        contador <- contador+1
        
      } else if (isTRUE(str_detect(dataSample[1,i], "BODEGA O TIENDA:"))) { # la columna i contiene la Tienda 
        dataSample[,i] <-str_extract(dataSample[,i], "Z[\\w]+") #RegEx para la zona de la tienda
        tmpFile["TIENDA"] <- dataSample[i]
        contador <- contador+1
        #print(dataSample[,i])
        #print(tmpFile$TIENDA)
      } else if (isTRUE(str_detect(dataSample[1,i], "CODPROD"))) { # la columna i contiene la FECHA 
        tmpFile["CODPROD"] <- dataSample[i+9]
        contador <- contador+1
      } else if(isTRUE(str_detect(dataSample[1,i], "DESCRIPC"))) { # la columna i contiene la FECHA 
        tmpFile["DESC"] <- dataSample[i+9]
        contador <- contador+1
      } else if (isTRUE(str_detect(dataSample[1,i],"MEDIDA"))){ # la columna i contiene la FECHA 
        tmpFile["MEDIDA"]<- dataSample[i+9]
        contador <- contador+1
      }else if (isTRUE(str_detect(dataSample[1,i],"UNI")) && str_length(dataSample[1,i])==3){ # la columna i contiene la FECHA 
        tmpFile["UNI"] <-dataSample[i+9]
        contador <- contador+1
      } else if (isTRUE(str_detect(dataSample[1,i], "VENTA"))){ # la columna i contiene la FECHA 
        tmpFile["VENTA"] <-dataSample[i+9]
        contador <- contador+1
      }
    } 
    if (contador == 7){
      contadorArchivosProcesados <- contadorArchivosProcesados+1
      break
    }
  }
  if (contadorArchivosProcesados == 1){
    tablaMaestra <- tmpFile
  } else {
    #tablaMaestra <- rbind(tablaMaestra, tmpFile)
  }

}
View(tmpFile)
View(tablaMaestra)

rm(ArchivoFuncionalPrimero, dataSample, file1, tmpFile)
rm(contador, contadorArchivosProcesados, example, files, i,r,zona)
rm(tablaMaestra,f, hola, tablaLimpiaIndividual, bloqueIndividual, test, tablaLimpia)

##################################### Funcion a partir del código correcto

##################################################################################################################################

#                              CORRER ESTOS ÚLTIMOS BLOQUES DE CÓDIGO PARA QUE TODO FUNCIONE 

##################################################################################################################################

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


files <- list.files(path = "D:/geord/Docs/Data Wrangling/Github/CMI-limpieza/versatil/files") ###### cambiar este path al de ustedes
length(files)
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






