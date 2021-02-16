#Recibe:
#   ldailytS: Lista de dias(ldaily), en los que cada item es una timeSerie(ts)
#   i: Lista numeros que indexan los elementos que se pretenden extraer de ldailytS.
#Retorna: 
#   Un mapeo de i en ldailytS
#Justificación:
#   Es una simple operación de acceso multi-indexado en forma de función infija, que se necesita
#   como funcion-argumento para la funcion map2 utilizada en maxRad.daily.
`%i%`<-function(ldailytS, i){
  return(index(ldailytS[i]))
}

#Recibe:
#     time: time
#     value: valores asociados a time
#Retorna:
#   Un vector de timeStamps conteniendo los instantes en los que 'value' es máxmo

max.daily.time<-function(time, value){
  #Creacion de objeto xts
  rad.xts <- xts(value, time)
  
  #Dividir fisicamente en dias
  rad.xts.daily<-split(rad.xts, f = "days")
  
  #Indice del maximo diario
  rad.xts.daily.max.indexs<-lapply(rad.xts.daily, which.max)
  
  #Extraer de cada dia la fecha correspondiente al indice
  rad.maxs<-map2(rad.xts.daily, rad.xts.daily.max.indexs, `%i%`)
  rad.maxs<-do.call(c, rad.maxs)
  
  #Transformar en dataframe rellenando a 0.
  rad.maxs.filled<-as.data.frame(time)
  rad.maxs.filled[,2]<-0
  colnames(rad.maxs.filled)<-c("V1", "V2")
  
  #Marcar con '1' los timeStamps calculados como maximos
  for (ts in rad.maxs) {
    mask<-rad.maxs.filled$V1==ts
    rad.maxs.filled$V2[mask]<-1
  }
  return (rad.maxs.filled)
}
