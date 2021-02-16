
#Retorna una lista numerada con todos los conjuntos que pueden extraerse de una determinada SERIE DE TIEMPO.
#Los parameteros gap y k determinan el nº de conjuntos que se pueden extraer.
#Esta funcion esta pensada para utilizarse como parte de una familia de funciones de validación.

#ts  : timeSerie 
#k: tamaño del cjto de entramiento más grande (en días)
#gap : tamaño del salto de un cjto al siguiente

#Consideraciones importantes:
# Si gap == k :
#    no se crea solapamiento
# Si gap < k:
#    se crea solapamiento, esta opción es deseable cuando hay pocos datos y es preciso reutilizar una parte del cjto anterior
create_sequential_sets_ts<-function(k, gap, ts, start){
  start = start
  timeL = end(ts)[1]
  end = start + k
  i = 1
  listedSets = list()
  while(end < timeL){
    listedSets[[toString(i)]] <- window(ts, start = start, end = end)
    i = i + 1
    start = start + gap
    end = end + gap
  }
  return(listedSets) 
}

#Retorna una lista numerada con todos los conjuntos que pueden extraerse de un determinado DATAFRAME
#Los parameteros gap y k determinan el nº de conjuntos que se pueden extraer.
#Esta funcion esta pensada para utilizarse como parte de una familia de funciones de validación.

#ts  : timeSerie 
#k: tamaño del cjto de entramiento más grande (en días)
#gap : tamaño del salto de un cjto al siguiente

#Consideraciones importantes:
# Si gap == k :
#    no se crea solapamiento
# Si gap < k:
#    se crea solapamiento, esta opción es deseable cuando hay pocos datos y es preciso reutilizar una parte del cjto anterior
create_sequential_sets_df<-function(k, gap, df){
  start = 1
  timeL = nrow(df)
  end = start + k
  i = 1
  listedSets = list()
  while(end < timeL){
    listedSets[[toString(i)]] <- df[seq(start,end,1),]
    i = i + 1
    start = start + gap
    end = end + gap
  }
  return(listedSets) 
}

# Primero obtengo la parte final que consta de : training + test
# Por último obtengo la parte de training.
create_custom_set_prophet<-function(df.extra, freq = 48, h = 7, optTL){
  return (head(tail(df.extra, h*freq + optTL*freq), optTL*freq))
}

#Dado un dataframe, construye a partir de el una lista de conjuntos de entramiento para cada modelo indicado en 'optTL', 
#del tamaño preciso indicado en . Primero se determina el conjunto de TEST, que siempre serán los ultimos 'h' días y con los dias 
#anteriores se proceden a crear los conjuntos para los modelos. El resultado es una lista de series de tiempo con una frecuencia

#'freq', todas los cjtos parten de una serie común TS con inicio en 'start', por lo tanto tienen una ref. temporal común. 
#df: dataframe
#optTL: optimunTL, lista con la informacion necesaria sobre los periodos de entramiento optimos para cada modelo
#freq: frecuencia de las ts
#h: forecast horizont
#start: instante de tiempo por el que van a empezar las timeSeries creadas
sets_creator <- function(df, optTL, freq, h, start){
  myts <- ts(df, frequency = freq, start = start)
  myts.test  <-  tail(myts, h*freq)
  myts.train <- window(myts, end = start(myts.test))
  
  myts.train.nnetar <- window(myts.train, start = end(myts.train)[1] - optTL$NNETAR)
  myts.train.arima <- window(myts.train, start = end(myts.train)[1] - optTL$ARIMA)
  myts.train.snaive <- window(myts.train, start = end(myts.train)[1] - optTL$SNAIVE)
  myts.train.tbats <- window(myts.train, start = end(myts.train)[1] - optTL$TBATS)
  myts.prophet <- create_custom_set_prophet(df, freq, h, optTL$PROPHET)
  
  return(list(
    TS = myts,
    TRAIN = myts.train,
    TEST = myts.test,
    TRAIN.NNETAR = myts.train.nnetar,
    TRAIN.ARIMA  = myts.train.arima,
    TRAIN.SNAIVE = myts.train.snaive,
    TRAIN.TBATS  = myts.train.tbats,
    TRAIN.PROPHET = myts.prophet
  )
  )
}

#Esta función en realidad es un invocador de una funcion 'fun' que se ha parametrizado para que sea independiente, 
#'fun' debe ser equivalente o igual a 'sets_creator'. Para cada elemento de una lista de dataframes 'dfs', se aplica 
#'fun' y se almacena en una lista que finalmente se devuelve.

#dfs: Lista de dataframes, se asume que forman parte una intervalo que ha sido descompuesto de una forma determinada (con 
#     solapamiento o sin solapamiento) y almacenado en una lista.
#fun: función con la que se va a tratar cada elemento de la lista dfs, se presupone que será 'sets_creator'
#optTL: parámetro para fun
#freq: parámetro para fun
#h: parámetro para fun
#gap: tamaño del salto de un cjto al siguiente. Necesario para mantener el inicio de la serie temporal correspondiente 
#      a cada conjunto transformado por fun 

#Consideraciones importantes:
#En el proceso de invocación a 'fun' se le calcula cual debe ser el instante en el que debe comenzar cada lote de series de tiempo
#o conjuntos de entreamiento, para que al ensamblar la parte correspondiente a la prediccion de cada lote se pueda formar una
#timeSeries continua en el tiempo sin incoherencias.

create_custom_sets<-function(dfs, fun, optTL, freq = 48, h = 7, gap, start){
  lista = list()
  i = 1
  start = start
  for(df in dfs){
    lista[[toString(i)]] <- fun(df, optTL, freq, h, start)
    start = (lista[[toString(i)]]$TS %>% start())[1] + gap
    i = i + 1
  }
  return(lista)
}


#setsList: lista de 'conjuntos' de entrenamiento, uno especializado para cada modelo. 
#predictor: la variable especifica que se quiere predecir, dado que cada cjto de entrenamiento de setsList 
#           es una serie de tiempo multivariable
#freq: frecuencia e la serie de tiempo
#h: forecast horizont, en días.
train_models<-function(setsList, predictor, freq = 48, h = 7){
  start.time.snaive <- Sys.time()
  SNAIVE  <- snaive(setsList$TRAIN.SNAIVE[, predictor], h = freq*h)
  end.time.snaive <- Sys.time()
  snaive.time <- end.time.snaive - start.time.snaive
  print(paste("SNAIVE:", snaive.time, sep = " "))
  
  ARIMA   <- auto.arima(setsList$TRAIN.ARIMA[, predictor], 
                        seasonal = T,approximation = T, stepwise = T)
  end.time.arima <- Sys.time()
  arima.time <- end.time.arima-end.time.snaive
  print(paste("ARIMA:", arima.time, sep = " "))
  
  NNETAR  <- nnetar(setsList$TRAIN.NNETAR[, predictor], lambda=0)
  end.time.nnetar <- Sys.time()
  nnetar.time <- end.time.nnetar-end.time.arima
  print(paste("NNETAR:", nnetar.time, sep = " "))
  
  TBATS <- tbats(setsList$TRAIN.TBATS[, predictor])
  end.time.tbats <- Sys.time()
  tbats.time <- end.time.tbats-end.time.nnetar
  print(paste("TBATS:", tbats.time, sep = " "))
  
  train.prophet <- setsList$TRAIN.PROPHET[, c("time", predictor)]
  colnames(train.prophet) <- c("ds", "y")
  PROPHET <- prophet(train.prophet)
  future <- make_future_dataframe(PROPHET, periods = freq*h, freq = 60*30)
  end.time.prophet <- Sys.time()
  prophet.time <- end.time.prophet - end.time.tbats
  print(paste("PROPHET:", prophet.time, sep = " "))
  
  return(list(
    SNAIVE  = list(MODEL = SNAIVE,
                   TIME = snaive.time),
    
    ARIMA   = list(MODEL = ARIMA,
                   TIME = arima.time),
    
    NNETAR = list(MODEL = NNETAR,
                  TIME  = nnetar.time),
    
    TBATS   = list(MODEL = TBATS, 
                   TIME  = tbats.time),
    
    PROPHET = list(MODEL = list(PROPHET  = PROPHET,
                                FUTURE  = future),
                   TIME  =  prophet.time),
    TOTTIME =  Sys.time() - start.time.snaive) 
  )
} 

train_models_exogenous<-function(setsList, predictor, freq = 48, h = 7, exoge_names){
  
  start.time.snaive <- Sys.time()
  SNAIVE  <- snaive(setsList$TRAIN.SNAIVE[, predictor], h = freq*h)
  end.time.snaive <- Sys.time()
  snaive.time <- end.time.snaive - start.time.snaive
  print(paste("SNAIVE:", snaive.time, sep = " "))
  
  ARIMA   <- auto.arima(setsList$TRAIN.ARIMA[, predictor], 
                        seasonal = T,approximation = T, stepwise = T,
                        xreg = setsList$TRAIN.ARIMA[,exoge_names] %>%
                          matrix(ncol = 
                                   ifelse(is.null(ncol(setsList$TRAIN.ARIMA[,exoge_names])), 1,
                                          ncol(setsList$TRAIN.ARIMA[,exoge_names]))))
  
  end.time.arima <- Sys.time()
  arima.time <- end.time.arima-end.time.snaive
  print(paste("ARIMA:", arima.time, sep = " "))
  
  NNETAR  <- nnetar(setsList$TRAIN.NNETAR[, predictor], lambda=0,
                    xreg = setsList$TRAIN.NNETAR[,exoge_names] %>% 
                      matrix(ncol = 
                               ifelse(is.null(ncol(setsList$TRAIN.NNETAR[,exoge_names])), 1,
                                      ncol(setsList$TRAIN.NNETAR[,exoge_names]))))
  
  end.time.nnetar <- Sys.time()
  nnetar.time <- end.time.nnetar-end.time.arima
  print(paste("NNETAR:", nnetar.time, sep = " "))
  
  TBATS <- tbats(setsList$TRAIN.TBATS[, predictor])
  end.time.tbats <- Sys.time()
  tbats.time <- end.time.tbats-end.time.nnetar
  print(paste("TBATS:", tbats.time, sep = " "))
  
  train.prophet <- setsList$TRAIN.PROPHET[, c("time", predictor)]
  train.prophet<-cbind(train.prophet, setsList$TRAIN.PROPHET[, exoge_names])
  colnames(train.prophet) <- c(c("ds", "y"), exoge_names)
  PROPHET <- prophet()
  
  for(reg in exoge_names){
    PROPHET <- add_regressor(PROPHET,reg)
  }
  
  PROPHET <- fit.prophet(PROPHET, train.prophet)
  future <- make_future_dataframe(PROPHET, periods = freq*h, freq = 60*30)
  
  future <- cbind(future, rbind(setsList$TRAIN.PROPHET[,exoge_names] %>% as.matrix(),
                                setsList$TEST[,exoge_names] %>% as.matrix()))
  colnames(future) <- c(c("ds", exoge_names))
  end.time.prophet <- Sys.time()
  prophet.time <- end.time.prophet - end.time.tbats
  print(paste("PROPHET:", prophet.time, sep = " "))
  
  
  return(list(
    SNAIVE  = list(MODEL = SNAIVE,
                   TIME = snaive.time),
    
    ARIMA   = list(MODEL = ARIMA,
                   TIME = arima.time),
    
    NNETAR = list(MODEL = NNETAR,
                  TIME  = nnetar.time),
    
    TBATS   = list(MODEL = TBATS, 
                   TIME  = tbats.time),
    
    PROPHET = list(MODEL = list(PROPHET  = PROPHET,
                                FUTURE  = future),
                   TIME  =  prophet.time),
    TOTTIME =  Sys.time() - start.time.snaive) 
  )
} 

train_models_list<-function(fun, setsList, pred, ...){
  result <- list()
  i <- 1
  for (setList in setsList){
    result[[toString(i)]] <- fun(setList, pred, ...)
    i = i+1
  }
  return (result)
}


save_models <- function(models, route, sufix){
  print(paste(paste(route, "SNAIVE", sep=""),sufix, sep=""))
  saveRDS(models$SNAIVE, paste(paste(route, "SNAIVE", sep=""),sufix, sep=""))
  saveRDS(models$ARIMA,  paste(paste(route, "ARIMA", sep=""),sufix, sep=""))
  saveRDS(models$NNETAR, paste(paste(route, "NNETAR", sep=""),sufix, sep=""))
  saveRDS(models$TBATS,  paste(paste(route, "TBATS", sep=""),sufix, sep=""))
  saveRDS(models$PROPHET,  paste(paste(route, "PROPHET", sep=""),sufix, sep=""))
}

get_forecasts <- function(trainedModels, sets, h=7, freq=48){
  SNAIVE.fcast  <- trainedModels$SNAIVE$MODEL
  ARIMA.fcast   <- forecast(trainedModels$ARIMA$MODEL, h = h*freq)
  NNETAR.fcast  <- forecast(trainedModels$NNETAR$MODEL, h = h*freq)
  TBATS.fcast   <- forecast(trainedModels$TBATS$MODEL, h = h*freq)
  PROPHET.fcast <- predict(trainedModels$PROPHET$MODEL$PROPHET, 
                           trainedModels$PROPHET$MODEL$FUTURE)$yhat %>% tail(h*freq) %>%
    ts(frequency = freq, start(TBATS.fcast$mean))
  
  MEAN.fcast = (SNAIVE.fcast$mean + ARIMA.fcast$mean + NNETAR.fcast$mean + TBATS.fcast$mean + PROPHET.fcast)/5
  
  return(list(SNAIVE.fcast = SNAIVE.fcast,
              ARIMA.fcast  = ARIMA.fcast,
              NNETAR.fcast = NNETAR.fcast,
              TBATS.fcast  = TBATS.fcast,
              PROPHET.fcast = PROPHET.fcast,
              MEAN.fcast = MEAN.fcast))
}


get_forecasts_exogenous <- function(trainedModels, setsList, h=7, freq=48, exoge_names){
  
  SNAIVE.fcast  <- trainedModels$SNAIVE$MODEL
  
  ARIMA.fcast   <- forecast(trainedModels$ARIMA$MODEL, h = h*freq,
                            xreg = setsList$TEST[,exoge_names] %>% 
                              matrix(ncol = 
                                       ifelse(is.null(ncol(setsList$TEST[,exoge_names])), 1,
                                              ncol(setsList$TEST[,exoge_names]))))
  
  NNETAR.fcast  <- forecast(trainedModels$NNETAR$MODEL, h = h*freq,
                            xreg = setsList$TEST[,exoge_names] %>% 
                              matrix(ncol = 
                                       ifelse(is.null(ncol(setsList$TEST[,exoge_names])), 1,
                                              ncol(setsList$TEST[,exoge_names]))))
  
  TBATS.fcast   <- forecast(trainedModels$TBATS$MODEL, h = h*freq)
  
  PROPHET.fcast <- predict(trainedModels$PROPHET$MODEL$PROPHET, 
                           trainedModels$PROPHET$MODEL$FUTURE)$yhat %>% tail(h*freq) %>%
    ts(frequency = freq, start(TBATS.fcast$mean))
  
  MEAN.fcast = (ARIMA.fcast$mean + NNETAR.fcast$mean  + PROPHET.fcast)/3
  
  return(list(SNAIVE.fcast = SNAIVE.fcast,
              ARIMA.fcast  = ARIMA.fcast,
              NNETAR.fcast = NNETAR.fcast,
              TBATS.fcast  = TBATS.fcast,
              PROPHET.fcast = PROPHET.fcast,
              MEAN.fcast = MEAN.fcast))
}

get_forecasts_list<-function(fun, trainedModelsLists, setsLists, ...){
  result <- list()
  i <- 1
  for (trainedModelsList in trainedModelsLists){
    result[[toString(i)]] <- fun(trainedModelsList, setsLists[[i]], ...)
    i = i + 1
  }
  return (result)
}

get_intervals<-function(fcasts){
  df<-cbind(fcasts$TBATS.fcast$mean %>% as.numeric,
            fcasts$ARIMA.fcast$mean %>% as.numeric,
            fcasts$SNAIVE.fcast$mean %>% as.numeric,
            fcasts$NNETAR.fcast$mean %>% as.numeric,
            fcasts$PROPHET.fcast%>% as.numeric)
  
  df %>% apply(1,max)  %>%
    ts(frequency = freq.daily, start = start(fcasts$SNAIVE.fcast$mean)) -> max_bound
  
  df %>% apply(1,min)  %>%
    ts(frequency = freq.daily, start = start(fcasts$SNAIVE.fcast$mean)) -> min_bound
  
  return (list(MIN=min_bound,
               MAX=max_bound))
}

get_intervals_list<-function(fun, intervalLists){
  result <- list()
  i <- 1
  for (interval in intervalLists){
    result[[toString(i)]] <- fun(interval)
    i = i + 1
  }
  return (result)
}

get_accs<-function(fcasts, ts.test, predictor, models){
  rbind(accuracy(fcasts$SNAIVE.fcast$mean, ts.test[, predictor]) %>% as.numeric(),
        accuracy(fcasts$ARIMA.fcast$mean, ts.test[, predictor]) %>% as.numeric(),
        accuracy(fcasts$NNETAR.fcast$mean, ts.test[, predictor]) %>% as.numeric(),
        accuracy(fcasts$TBATS.fcast$mean, ts.test[, predictor]) %>% as.numeric(),
        accuracy(fcasts$PROPHET.fcast, ts.test[, predictor]) %>% as.numeric(),
        accuracy(fcasts$MEAN.fcast, ts.test[, predictor]) %>% as.numeric()) %>% as.data.frame() -> x
  
  cbind(c("SNAIVE", "ARIMA", "NNETAR", "TBATS", "PROPHET", "MEAN"),x) -> x
  colnames(x) <-c("METHODS", "ME","RMSE", "MAE", "MPE", "MAPE", "ACF1", "THEILSU")
  
  list(models$SNAIVE$TIME, models$ARIMA$TIME,
       models$TBATS$TIME, models$NNETAR$TIME,
       models$PROPHET$TIME, models$SNAIVE$TIME) %>% 
    lapply(function(x){
      if (units(x) == "mins") 
        return (as.numeric(x)*60)
      else if (units(x) == "hours")
        return (as.numeric(x) * 60*60)
      else return (as.numeric(x))} ) -> secs
  secs[[6]] <- secs[[1]] + secs[[2]] +  secs[[3]] +  secs[[4]]  + secs[[5]]
  
  ref <- secs[[1]]
  secs %>% unlist / ref -> CC
  CC %>% round() -> CC
  return(cbind(x, CC))
}

get_accsExog<-function(fcasts, ts.test, predictor, models){
  rbind(accuracy(fcasts$SNAIVE.fcast$mean, ts.test[, predictor]) %>% as.numeric(),
        accuracy(fcasts$ARIMA.fcast$mean, ts.test[, predictor]) %>% as.numeric(),
        accuracy(fcasts$NNETAR.fcast$mean, ts.test[, predictor]) %>% as.numeric(),
        accuracy(fcasts$PROPHET.fcast, ts.test[, predictor]) %>% as.numeric(),
        accuracy(fcasts$MEAN.fcast, ts.test[, predictor]) %>% as.numeric()) %>% as.data.frame() -> x
  
  cbind(c("SNAIVE", "ARIMA", "NNETAR", "PROPHET", "MEAN"),x) -> x
  colnames(x) <-c("METHODS", "ME","RMSE", "MAE", "MPE", "MAPE", "ACF1", "THEILSU")
  
  list(models$SNAIVE$TIME, models$ARIMA$TIME, models$NNETAR$TIME,
       models$PROPHET$TIME, models$SNAIVE$TIME) %>% 
    lapply(function(x){
      if (units(x) == "mins") 
        return (as.numeric(x)*60)
      else if (units(x) == "hours")
        return (as.numeric(x) * 60*60)
      else return (as.numeric(x))} ) -> secs
  secs[[5]] <- secs[[1]] + secs[[2]] +  secs[[3]] +  secs[[4]]
  
  ref <- secs[[1]]
  secs %>% unlist / ref -> CC
  CC %>% round() -> CC
  return(cbind(x, CC))
}

get_accs_list<-function(fun, fcasts, sets, predictor, models){
  result <- list()
  for (i in seq(1,length(fcasts),1)){
    result[[toString(i)]] <- fun(fcasts[[i]], sets[[i]]$TEST, predictor, models[[i]])
  }
  return (result)
}

# ggdisplay_forecasts<-function(test, fcast, predictor, title){
#   result<- autoplot(test[, predictor], series = "test", color = "black")+
#     autolayer(fcast$TBATS.fcast$mean, series = "TBATS")+
#     autolayer(fcast$ARIMA.fcast$mean, series = "ARIMA") + 
#     autolayer(fcast$SNAIVE.fcast$mean, series = "SNAIVE") +
#     autolayer(fcast$NNETAR.fcast$mean, series = "NNETAR") +
#     autolayer(fcast$PROPHET.fcast, series = "PROPHET") +
#     labs(x = "time(nº days)", y = "ºC") +
#     ggtitle(title)
#   
#   intervals <-  get_intervals(fcast)
#   
#   interv<-autoplot(test[, predictor], series = "TEST") + 
#     autolayer(intervals$MIN, series = "INTERVAL") + 
#     autolayer(intervals$MAX, series = "INTERVAL") +
#     labs(x = "time(nº days)", y = "ºC") +
#     ggtitle(title)
#   
#   return (list(FORECASTS = result,
#                INTERVALS = interv))
# }
# 
# ggtsdisplay_best <- function(accs, fcasts, train, test, predictor){
#   best<-accs[which.min(accs[, "RMSE"]), "METHODS"] %>% paste(".fcast",sep = "")
#   fcasts[[best]]$method -> method
#   fcasts[[best]] %>% residuals -> residuals
#   fcasts[[best]]$mean - test[, predictor] -> error
#   
#   # Residuals 
#   residuals %>% autoplot() + ggtitle("Residuals") -> resid
#   residuals %>% gghistogram(add.normal = TRUE, add.rug = TRUE, bins = 130) +
#     ggtitle("Residuals histogram") + 
#     theme_gray() -> resid.hist
#   residuals %>% ggAcf(lag.max = 48*7) +
#     ggtitle("Residuals ACF") + 
#     theme_gray()-> resid.acf
#   residuals %>% as.numeric %>% as.data.frame() %>% 
#     ggplot(aes(sample = .))+
#     stat_qq() + stat_qq_line() +
#     ggtitle("Residuals QQ") -> resid.qq
#   
#   extVars <- c("tempExt", "radExt", "humExt.E", "humExt.R", "vientoVelo")
#   train <- train[,extVars]
#   resid.corr<-function(){ts.union(train, residuals) %>%
#       as.data.frame() %>%
#       chart.Correlation(dpi = "retina",
#                         histogram = F, pch = 19)}
#   
#   # Forecast error
#   error %>% autoplot() + ggtitle("Forecast error")-> err
#   error %>% gghistogram(add.normal = TRUE, add.rug = TRUE, bins = 50) +
#     ggtitle("Forecast error histogram") + 
#     theme_gray() -> err.hist
#   error %>% ggAcf(lag.max = 48*7) +
#     ggtitle("Forecast error ACF") + 
#     theme_gray()-> err.acf
#   error %>% as.numeric %>% as.data.frame() %>% 
#     ggplot(aes(sample = .))+
#     stat_qq() + stat_qq_line() +
#     ggtitle("Forecast error QQ") -> err.qq
#   
#   test <- test[, extVars]
#   error.corr<-function(){ts.union(test, error) %>%
#       as.data.frame() %>%
#       chart.Correlation(dpi = "retina",
#                         histogram = F, pch = 19)}
#   
#   list(BEST       = method, 
#        RESIDERROR = function(){grid.arrange(resid, err, nrow = 1)},
#        HISTO      = function(){grid.arrange(resid.hist, err.hist, nrow = 1)},
#        ACF        = function(){grid.arrange(resid.acf, err.acf, nrow = 1)},
#        QQ         = function(){grid.arrange(resid.qq, err.qq, nrow = 1)},
#        CORR_RES   = resid.corr,
#        CORR_ERR   = error.corr)
# }


get_sequential_forecasts<-function(fcastLists, ts = T, freq = 48, sets, pred){
  df_1<-cbind( SNAIVE = fcastLists[[1]]$SNAIVE.fcast$mean %>% as.numeric(),
               ARIMA  = fcastLists[[1]]$ARIMA.fcast$mean %>% as.numeric(),
               TBATS  = fcastLists[[1]]$TBATS.fcast$mean %>% as.numeric(),
               NNETAR = fcastLists[[1]]$NNETAR.fcast$mean %>% as.numeric(),
               PROPHET= fcastLists[[1]]$PROPHET.fcast %>% as.numeric(),
               MEAN   = fcastLists[[1]]$MEAN.fcast %>% as.numeric(),
               REAL   = sets[[1]]$TEST[,pred] %>% as.numeric())
  
  for (i in seq(2,length(fcastLists),1)){
    df_2<-cbind( SNAIVE = fcastLists[[i]]$SNAIVE.fcast$mean %>% as.numeric(),
                 ARIMA  = fcastLists[[i]]$ARIMA.fcast$mean %>% as.numeric(),
                 TBATS  = fcastLists[[i]]$TBATS.fcast$mean %>% as.numeric(),
                 NNETAR = fcastLists[[i]]$NNETAR.fcast$mean %>% as.numeric(),
                 PROPHET= fcastLists[[i]]$PROPHET.fcast %>% as.numeric(),
                 MEAN   = fcastLists[[i]]$MEAN.fcast %>% as.numeric(),
                 REAL   = sets[[i]]$TEST[,pred] %>% as.numeric()
    )
    df_1<-rbind(df_1, df_2)
  }
  colnames(df_1) <- c("SNAIVE", "ARIMA", "TBATS",  "NNETAR", "PROPHET","MEAN", "REAL")
  if (ts == T){
    return (df_1 %>% ts(frequency = freq,
                        start =  c((sets[[1]]$TEST[,pred] %>% start)[1],
                                   (sets[[1]]$TEST[,pred] %>% start)[2])))
  }else{
    return (df_1 %>% as.data.frame())
  }
}

get_sequential_intervals<-function(intervLists, ts = T, freq = 48, sets, pred){
  df_1<-cbind(MAX = intervLists[[1]]$MAX,
              MIN = intervLists[[1]]$MIN,
              REAL = sets[[1]]$TEST[,pred] %>% as.numeric())
  
  for (i in seq(2,length(intervLists),1)){
    df_2<-cbind(MAX = intervLists[[i]]$MAX,
                MIN = intervLists[[i]]$MIN,
                REAL = sets[[i]]$TEST[,pred] %>% as.numeric())
    
    df_1<-rbind(df_1, df_2)
  }
  
  if (ts == T){
    return (df_1 %>% ts(frequency = freq,
                        start = c((sets[[1]]$TEST[,pred] %>% start)[1],
                                  (sets[[1]]$TEST[,pred] %>% start)[2])))
  }else{
    return (df_1 %>% as.data.frame())
  }
}

get_sequential_accs<-function(accsList, vars){
}