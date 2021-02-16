#Recibe:
#   vA, vB: vectores numéricos de longitudes iguales o distintas
#Retorna:
#   La media de los dos
#Funcionamiento:
#   Si la longitud de vA y vB es igual se calcula la media.
#   Si la longiud difiere en un factor 'diff', se recortan las 'diff' ultimas filas
#   tanto de vA como de vB y se calcula la media con normalidad.
#Justificación:
#   Esta función existe porque pueden haber sensores hermanos tal que por diferentes 
#   causas no tengan las mismas mediciones.
mean.safe<-function(vA, vB){
  nrA<-length(vA)
  nrB<-length(vB)
  diff<- abs(nrA - nrB)
  if (diff > 0){
    if (nrA > nrB){
      vA<-head(vA, -diff) #Cut "diff" rows from A
    }else{
      vB<-head(vB, -diff) #Cut "diff" rows from B
    }
  }
  return((vA + vB) / 2)
}