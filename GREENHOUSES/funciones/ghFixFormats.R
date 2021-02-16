#Recibe:
#   gh: Dataframe representando un greenhouse
#   Se asume que la primera columna es la referencia temporal
#   Y las demas contiene valores numéricos asociados
#Retorna:
#   Dataframe cuya primera está columna formateada a POSIXct y las restantes a numeric.
gh.fix.formats<-function(gh, mask.numeric, mask.factor, multiclass = F){
  if (multiclass == T){
    gh[mask.numeric] <- sapply(gh[mask.numeric], as.numeric)
    gh[mask.factor] <- lapply(gh[mask.factor], as.factor)
  }else{
    cs<-c(2,3:ncol(gh))
    gh[cs] <- sapply(gh[cs], as.numeric)
  }
  gh$time<- as.POSIXct(gh$time, format = "%Y-%m-%d %H:%M:%S")
  #gh<-gh[complete.cases(gh), ]
  return(gh)
}