
#df: un megadataframe
#mask.V: una mascara booleana para obtener aquellas columnas que contegan observaciones (valores)
#        mask.T es su complementario, es decir aquellas columnas que contienen ref. temporales          
summary.tS<-function(df, mask.V){
  library(purrr)
  
  mask.T<-!mask.V 
  t1<-df[mask.T][1,]
  t2<-df[mask.T][2,]
  infoDesfase<-t(t2-t1)
  
  NAs <-sapply(df[mask.T], function(y) sum(is.na(y)))
  valid <-sapply(df[mask.T], function(y) sum(!is.na(y)))
  total <-sapply(df[mask.T], function(y) length(y))
  
  first<-t(df[mask.T][1,])
  last<-sapply(df[mask.T], function(x) {length(x)-sum(is.na(x))})
  last<-map2(df[mask.T], last, function(x,y) {x[y]})
  last<-do.call("c", last)
  
  infoDesfase <- as.data.frame(infoDesfase)
  infoDesfase <- cbind(infoDesfase, valid, NAs, total, first, last)
  colnames(infoDesfase) <- c("Frequency", "Valid", "NAs", "Total", "First", "Last")
  return(infoDesfase)
}