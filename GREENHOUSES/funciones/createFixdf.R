#Para relizar esta funci?n me inspire en el siguiente post de stackOverflow:
##<https://stackoverflow.com/questions/22203493/aggregate-1-minute-data-into-5-minute-average-data>

#Recibe
#   x: timeSerie
#   n: Intervalo en el que se va a alinear la ts
#Retorna
#   TimeSeries alineada hacia 'abajo', en intervalos 'n'
#Funcionamiento
# Se resta al indice de la timeSerie (fechas) 'n' segundos
# Y se reemplaza el inicio de cada periodo de redondeando 'n' segundos
align.time.down = function(x, n) {
  index(x) = index(x) - n
  align.time(x, n)
}


#Recibe
#   ts_raw: TimeStamps en crudo, tal y como fueron extraidas del sensor
#   val_raw: Valores en crudo asociados a 'ts_raw', tal y como fueron extraidos del sensor
#   breakt: Intervalo por el que se rompe la timeSeries para apliar func
#   func: Funcion que se aplica a cada intervalo
#   factor: Indica si val_raw debe tratarse como un factor o como numerico, por defecto se trata como 
#           num?rico
#   interval: Indica el intervalo de la muestra (ts_raw, val_raw) debe tratarse

#Retorna
#   Un dataframe formado por ts_raw y val_raw agregados en un factor 'breakT', con columnas:
# V1: TimeStamps
# V2: Valores

#Funcionamiento
#   Se transforman ts_raw a formato 'POSIXct' y val_raw a 'numeric'
#   Se convierte ts_raw y val_raw a un objeto 'xts', una timeSerie, que hereda de 'zoo'
#   Se agrega con la media la timeSerie en intervalos 'breakT'
#   Se redondea hacia el principio de cada intervalo con 'align.time.down()', por defecto se hace
#   el redondeo hacia el ?ltimo valor que entra en el intervalo, lo cual es incorrecto.
#   Por ultimo se convierte la timeSerie en un dataframe.

#Consideraciones importantes:
#   Una vez que se ha realizado la agregaci?n se crea un dataframe llamado dummy.df, este df tiene 
#   una referencia temporal 'perfecta' que define el intervalo te?rico, se utiliza para explicitar los datos
#   perdidos en el dataframe agregado, este efecto se consigue haciendo un merge de df con dummy.df .
#   Si se desea reproducir un ejemplo de prueba se puede consultar el siguiente post de stackOverflow en el 
#   cual yo solucion? un problema similar aplicando un merge, alli queda m?s claro este procedimiento : 
#   https://stackoverflow.com/questions/55854335/interpolating-data-in-third-column-by-a-regular-posixct-series
create.fix.df<-function(ts_raw, val_raw, breakt,func=mean,
                        factor=F, interval){
  if (factor == F){
    val_raw<-as.numeric(val_raw)
  }
  ts_raw<-as.POSIXct(ts_raw, format = "%Y-%m-%d %H:%M")
  df.xts <- xts(val_raw, ts_raw)

  means.xts <- period.apply(df.xts, endpoints(df.xts, "mins", k=breakt), func)
  means.rounded <- align.time(means.xts, breakt*60)
  df<-data.frame(date=index(means.rounded), coredata(means.rounded))
  colnames(df)<-c("V1", "V2")
  
  #---------------------------------fortification---------------------------------#
  df <- df[df$V1 > interval[1] & df$V1 < interval[2],]
  
  dummy.df  <- seq(as.POSIXct(interval[1]), as.POSIXct(interval[2]), breakt*60) 
  dummy.df  <- as.data.frame(dummy.df)
  dummy.df$V2 <-0 
  colnames(dummy.df) <- c("V1", "V2")
  df<-merge(df, dummy.df, by = "V1", all = T,)
  colnames(df) <- c("V1", "V2","x")

  return(df[,c("V1", "V2")])
}