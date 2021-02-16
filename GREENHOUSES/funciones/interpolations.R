na.interp.df<-function(df){
  colsInterp<- sapply(df[2:ncol(df)], na.interp)
  df.interp <- as.data.frame(df$time)
  df.interp <- cbind(df.interp, colsInterp)
  colnames(df.interp)[1] <- "time"
  return(df.interp)
}
na.interp2.df<-function(df, fun = na.seasplit){
  df.copy <- df
  for (i in seq(2,ncol(df.copy),1)){
    df.copy[,i] <- ts(df.copy[,i], frequency = 48, start = c(1))  %>% fun %>% as.numeric()
  }
  return (df.copy)
}