
train_NNs_exog<-function(lTrains, pred, exog_var){
  lista<-list()
  i=1
  for(lTrain in lTrains){
    print(i)
    lista[[toString(i)]] <- nnetar(lTrain[,pred], lambda = 0,
                                   xreg = lTrain[,exog_var] %>% 
                                     matrix(ncol = ifelse(is.null(lTrain[,exog_var]),1,
                                                          ncol(lTrain[,exog_var]))))
    i=i+1
  }
  return(lista)
}

get_multiFcasts_exog<-function(models, testSet, h, freq, exog_var){
  lista<-list()
  i=1
  for(model in models){
    print(i)
    lista[[toString(i)]]<- forecast(model, h = h*freq,
                                    xreg = testSet[,exog_var] %>% 
                                      matrix(ncol = 
                                               ifelse(is.null(ncol(testSet[,exog_var])), 1,
                                                      ncol(testSet[,exog_var]))))
    i=i+1
  }
  return(lista)
}

train_ARIMAs_exog<-function(lTrains, pred, exog_var){
  lista <- list()
  i = 1
  for(lTrain in lTrains){
    print(i)
    lista[[toString(i)]] <- auto.arima(lTrain[, pred],
                                       trace =T, seasonal = T, 
                                       approximation = T, stepwise = T,
                                       xreg = lTrain[,exog_var] %>% 
                                         matrix(ncol = ifelse(is.null(lTrain[,exog_var]),1,
                                                              ncol(lTrain[,exog_var]))))
    i = i + 1
  }
  return(lista)
}

make_prophet_exog<-function(lTrains.TRAIN, TEST, pred, exoge_names, freq = 48, h = 7){
  
  train.prophet<-cbind(pleiadesGH.v2.interp$time[1:length((lTrains.TRAIN[,pred]))] %>% as.data.frame(),
                       as.numeric(lTrains.TRAIN[,pred]))
  colnames(train.prophet) <- c("ds", "y")
  
  train.prophet<-cbind(train.prophet,
                       lTrains.TRAIN[, exoge_names] %>% matrix( ncol = ifelse(is.null(lTrains.TRAIN[, exoge_names]),1,
                                                                              ncol(lTrains.TRAIN[, exoge_names]))))
  colnames(train.prophet) <- c(c("ds", "y"), exoge_names)
  PROPHET <- prophet()
  
  for(reg in exoge_names){
    PROPHET <- add_regressor(PROPHET,reg)
  }
  
  PROPHET <- fit.prophet(PROPHET, train.prophet)
  future <- make_future_dataframe(PROPHET, periods = freq*h, freq = 60*30)
  
  future <- cbind(future, rbind(lTrains.TRAIN[,exoge_names] %>% as.matrix(),
                                TEST[,exoge_names] %>% as.matrix()))
  
  return(list(PROPHET = PROPHET,
              FUTURE = future))
}

train_PROPHETs_exog<-function(lTrains, tests, pred, exog_var){
  
  lista<-list()
  i=1
  for(lTrain in lTrains){
    print(i)
    lista[[toString(i)]] <- make_prophet_exog(lTrain, tests, pred, exog_var)
    i=i+1
  }
  return(lista)
}


forecast_prophet<-function(prophet, h){
  return(predict(prophet$PROPHET,prophet$FUTURE)$yhat %>% tail(h) %>%
           ts(frequency = 48, start(1)))
}

get_prophetFcasts<-function(prophets, h){
  lista = list()
  i = 1
  for(prophet in prophets){
    print(i)
    lista[[as.character(i)]] <- forecast_prophet(prophet, h = h)
    i = i +1
  }
  return(lista)
}