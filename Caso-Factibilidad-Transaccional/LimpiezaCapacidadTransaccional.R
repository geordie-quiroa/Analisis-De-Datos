library(lubridate)
library(tidyr)
library(dplyr)
library(tidyverse)

df <- read.csv("registrosPlataformaBancaria.csv")

View(df)

df$anio = 2018

names(df) <- c("MES", "DIA", "HORA", "MINUTO", "SEGUNDO", "ANIO")
names(df)

df <- df %>%  # variable_funcion = variable_dataset
  mutate(FECHA = make_datetime(year=ANIO, month = MES, day = DIA, hour = HORA, min = MINUTO, sec = SEGUNDO))

wday(df$FECHA, label = TRUE) %>% head()

df <- df %>%  # variable_funcion = variable_dataset
  mutate(DiaSemana = wday(df$FECHA, label = TRUE))

df <- df %>%
  mutate(FechaSinHora = make_date(year = ANIO, month = MES, day = DIA))


DiasMayorFlujo <- df %>% group_by(DiaSemana) %>% summarise(total = n())
HoraMayorFlujo <- df %>% group_by(HORA) %>% summarise (totalTransacciones = n())
View (DiasMayorFlujo)
View (HoraMayorFlujo)
View(df)


#df$DiaSemana<-str_extract_all(df$DiaSemana,"\ \.", "")


#agrupar por hora y luego por datetime,
#el promedio es la sobre los segundos usados. 

df_TransactionsAtSecondPerMinuteAndHourWithFechaCompleta <- df %>%
  select(HORA, MINUTO, SEGUNDO, FechaSinHora, FECHA) %>%
  group_by(HORA, MINUTO, SEGUNDO, FechaSinHora, FECHA) %>%
  summarise(total_transacciones = n()) %>%
  arrange(SEGUNDO)

View(df_TransactionsAtSecondPerMinuteAndHourWithFechaCompleta)

df_TransactionsAtSecondPerMinuteAndHour <- df %>%
  select(HORA, MINUTO, SEGUNDO, FechaSinHora) %>%
  group_by(HORA, MINUTO, SEGUNDO, FechaSinHora) %>%
  summarise(total_transacciones = n()) %>%
  arrange(SEGUNDO)

#df_TransactionsAtSecondPerMinuteAndHour <- df_TransactionsAtSecondPerMinuteAndHour %>%
 # mutate(FechaSinHora = make_date(year = year(FECHA), month = month(FECHA), day = day(FECHA)))

View (df)
View(df_TransactionsAtSecondPerMinuteAndHour)

?n_distinct()

df_UsedSecondsPerMinuteAndDate <- df_TransactionsAtSecondPerMinuteAndHour %>%
  select(FECHA, FechaSinHora) %>%
  group_by(hour(FECHA), minute(FECHA), FechaSinHora) %>%
  summarise(SegundosUsados = n_distinct(second(FECHA))) %>%
  arrange(FechaSinHora)
names(df_UsedSecondsPerMinuteAndDate) <- c("HORA", "MINUTO", "FechaSinHora", "SegundosUsados")

head(df_UsedSecondsPerMinuteAndDate)
View(df_UsedSecondsPerMinuteAndDate)

#rm(df_TransactionsAtSecondPerMinuteAndHour)


# para calcular el total de transacciones por minuto segun cada hora y fecha
df_TransactionsPerMinuteAndDate <- df_TransactionsAtSecondPerMinuteAndHour %>%
  select(HORA, MINUTO, FechaSinHora, total_transacciones) %>%
  group_by(HORA, MINUTO, FechaSinHora) %>%
  summarise(TransPorMinuto = sum(total_transacciones))%>%
  arrange(FechaSinHora)
#summarise(SegundosUsados = n_distinct(second(FECHA)))
#arrange(HORA)

df_TransactionsPerMinuteAndDate <- transform(df_TransactionsPerMinuteAndDate, SegundosUsados = df_UsedSecondsPerMinuteAndDate$SegundosUsados)

View(df_TransactionsPerMinuteAndDate)

#promedioTPS <- sum(df_TransactionsAtSecondPerMinuteAndHour$total_transacciones)/sum(df_UsedSecondsPerMinuteAndDate$SegundosUsados)

totalOperaciones <- sum(df_TransactionsPerMinuteAndDate$TransPorMinuto)
totalSegundos<- sum(df_TransactionsPerMinuteAndDate$SegundosUsados)

promedioTPS <- sum(df_TransactionsPerMinuteAndDate$TransPorMinuto)/sum(df_TransactionsPerMinuteAndDate$SegundosUsados)
meanTPS <- mean()

desvEst <- sd(df_TransactionsAtSecondPerMinuteAndHour$total_transacciones)


write.table(DiasMayorFlujo,file="DiasMayorFlujo.csv", sep=";", dec=".", row.names = FALSE)
write.table(HoraMayorFlujo,file="HoraMayorFlujo.csv", sep=";", dec=".", row.names = FALSE)
write.table(df_TransactionsAtSecondPerMinuteAndHour,file="TransAtSeconds.csv", sep=";", dec=".", row.names = FALSE)
write.table(df_UsedSecondsPerMinuteAndDate,file="UsedSeconds.csv", sep=";", dec=".", row.names = FALSE)


df_CapacityOverflow <- df %>%
  select(SEGUNDO, FECHA) %>%
  group_by(SEGUNDO, FECHA) %>%
  summarise(total_transacciones = n()) %>%
  filter(total_transacciones>50) %>%
  arrange(FECHA)

df %>%
  select(DiaSemana, FECHA) %>%
  group_by(DiaSemana) %>%
  summarize(Total = n_distinct(FECHA)) %>%
  filter(DiaSemana == "vie\\.") 
  
View(df_CapacityOverflow)

ReceptionMinute <- 60 #segundos
TPScapacity <- 50 #transacciones por segundo
MaxCapacityAtReceptionMinute <- TPScapacity * ReceptionMinute 
