train_NNs<-function(lTrains, pred){
  lista <- list()
  i = 1
  for(lTrain in lTrains){
    print(i)
    lista[[toString(i)]] <- nnetar(lTrain[,pred], lambda = 0)
    i = i + 1
  }
  return(lista)
}

train_ARIMAs<-function(lTrains, pred){
  lista <- list()
  i = 1
  for(lTrain in lTrains){
    print(i)
    lista[[toString(i)]] <- auto.arima(lTrain[, pred],
                                       trace =T, seasonal = T, 
                                       approximation = T, stepwise = T)
    i = i + 1
  }
  return(lista)
}

make_prophet<-function(lTrains.TRAIN, pred){
  cbind(pleiadesGH.v2.interp$time[1:length((lTrains.TRAIN[,pred]))] %>% as.data.frame(),
        as.numeric(lTrains.TRAIN[,pred])) -> df
  colnames(df) <- c("ds", "y")
  return(prophet(df))
}

train_PROPHETs<-function(lTrains, pred){
  
  lista <- list()
  i = 1
  for(lTrain in lTrains){
    print(i)
    lista[[toString(i)]] <- make_prophet(lTrain, pred)
    i = i + 1
  }
  return(lista)
}

train_SNAIVEs<-function(lTrains, pred, h){
  lista <- list()
  i = 1
  for(lTrain in lTrains){
    print(i)
    lista[[toString(i)]] <- snaive(lTrain[, pred], h)
    i = i + 1
  }
  return(lista)
}


train_TBATSs<-function(lTrains, pred){
  
  lista <- list()
  i = 1
  for(lTrain in lTrains){
    print(i)
    lista[[toString(i)]] <-  tbats(lTrain[, pred])
    i = i + 1
  }
  return(lista)
}

get_multiFcasts<-function(models, h, freq){
  lista<-list()
  i=1
  for(model in models){
    print(i)
    lista[[toString(i)]]<- forecast(model, h = h*freq)
    i=i+1
  }
  return(lista)
}

forecast_prophet<-function(prophet, h){
  future <- make_future_dataframe(prophet, periods = h, freq = 60*30)
  return(predict(prophet,future)$yhat %>% tail(h) %>%
           ts(frequency = 48, start(1)))
}

get_prophetFcasts<-function(prophets, h){
  lista <- list()
  i = 1
  for(prophet in prophets){
    print(i)
    lista[[toString(i)]] <- forecast_prophet(prophet,  h = h)
    i = i + 1
  }
  return(lista)
}

get_multiAccs<-function(test, fcasts, pred){
  cbind(c(seq(1:16) * 15),
        cbind(accuracy(test[,pred], fcasts[[1]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[2]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[3]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[4]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[5]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[6]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[7]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[8]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[9]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[10]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[11]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[12]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[13]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[14]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[15]]$mean)[c(2,5)],
              accuracy(test[,pred], fcasts[[16]]$mean)[c(2,5)])  %>% t %>%
          as.data.frame()) -> accs
  accs <- cbind(c("TRAIN0", "TRAIN1", "TRAIN2", "TRAIN3", "TRAIN4", "TRAIN5",
                  "TRAIN6", "TRAIN7", "TRAIN8", "TRAIN9", "TRAIN10",
                  "TRAIN11","TRAIN12","TRAIN13","TRAIN14","TRAIN15"), accs)
  colnames(accs) <- c("TRAINS", "NDIAS", "RMSE","MAPE")
  return (accs)
}

get_multiAccs_prophet<-function(test, fcasts, pred){
  cbind(c(seq(1:16) * 15),
        cbind(accuracy(test[,pred], fcasts[[1]])[c(2,5)],
              accuracy(test[,pred], fcasts[[2]])[c(2,5)],
              accuracy(test[,pred], fcasts[[3]])[c(2,5)],
              accuracy(test[,pred], fcasts[[4]])[c(2,5)],
              accuracy(test[,pred], fcasts[[5]])[c(2,5)],
              accuracy(test[,pred], fcasts[[6]])[c(2,5)],
              accuracy(test[,pred], fcasts[[7]])[c(2,5)],
              accuracy(test[,pred], fcasts[[8]])[c(2,5)],
              accuracy(test[,pred], fcasts[[9]])[c(2,5)],
              accuracy(test[,pred], fcasts[[10]])[c(2,5)],
              accuracy(test[,pred], fcasts[[11]])[c(2,5)],
              accuracy(test[,pred], fcasts[[12]])[c(2,5)],
              accuracy(test[,pred], fcasts[[13]])[c(2,5)],
              accuracy(test[,pred], fcasts[[14]])[c(2,5)],
              accuracy(test[,pred], fcasts[[15]])[c(2,5)],
              accuracy(test[,pred], fcasts[[16]])[c(2,5)])  %>% t %>%
          as.data.frame()) -> accs
  accs <- cbind(c("TRAIN0", "TRAIN1", "TRAIN2", "TRAIN3", "TRAIN4", "TRAIN5",
                  "TRAIN6", "TRAIN7", "TRAIN8", "TRAIN9", "TRAIN10",
                  "TRAIN11","TRAIN12","TRAIN13","TRAIN14","TRAIN15"), accs)
  colnames(accs) <- c("TRAINS", "NDIAS", "RMSE","MAPE")
  return (accs)
}