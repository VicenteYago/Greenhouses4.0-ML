# Greenhouse4.0-ML

![Hidroleaf interiors](https://github.com/VicenteYago/invernaderos4.0/blob/master/fotos/HIDROLEAF/hidro.jpg)



#### In this work, two 4.0 greenhouses have been analyzed from the *data-driven* point of view:
  - Pleiades: http://ecoproyecta.es/invernadero-bioclimatico/
  - Hidroleaf: Experimental greenhouse container developed by several companies in Murcia in conjunction with CEBAS-CESIC and DIIC-FIUM, saves up to 80% of water consumption compared to a traditional greenhouse.

#### With emphasis on interior temperatures:
  - internal air temperature
  - inside temperature of the pots
  
 ![globaltemp](https://github.com/VicenteYago/Greenhouses4.0-ML/blob/main/images/temperaturasGlobal.jpeg)

 ![Pleiades interiors](https://github.com/VicenteYago/Greenhouses4.0-ML/blob/main/images/tempIntExtFacet.jpeg)

  
 #### Additionally for the Pleiades greenhouse, a prediction system for the indoor air temperature has been developed, based on automatic statistical models
  - auto.arima
  - tbats
  - snaive
  - prophet
  - nnetar
  - comb (arithmetic mean of the predictions obtained with the previous models)

In order to improve this system, a brief study has been carried out to determine the number of days to be provided to each model in order to obtain a balance between accuracy and consumption of computational resources.

 ![Pleiades interiors](https://github.com/VicenteYago/Greenhouses4.0-ML/blob/main/images/testsAlineadosdual.jpeg)



#### In addition, two scenarios have been developed to make the predictions:
  - Un-informed scenario: Only indoor temperature history is available
  - Informed scenario: Historicals of the inner temperature are available. In addition, for the following variables, both the historical and the **future** values are available for 7 days:
    - outside temperature
    - solar radiation
    - external relative humidity
    
    
#### After modifying the cross-validation algorithm proposed by *Hyndman & Athanasopoulos, 2018 [Chapter 3.4]* the following results were obtained after predicting the indoor temperature at 7 days (horizon forecast = 7, frequency = 30 min):
  - Un-informed scenario (crossvalidation *ex ante* with last 196 days of 2018, 28 weeks)
  
  |MODEL   | MAPE | RMSE | MAE | CC | DAYS |
  | :-------: | :----: | :----: | :---: |:----:| :-----:|
  | tbats   | 9.13  | 2.95  | 2.22 | 15052| 15    |
  | comb    | 9.61  | 3.06  | 2.31 | 140553| 165    | 
  | snaive  | 10.30 | 3.44  | 2.53 | 1| 1    |
  | prophet | 12.11 | 3.45  | 2.79 | 155 |   165  |
  | auto.arima   | 10.71 | 3.53  | 2.70 | 119777 | 30    |
  | nnetar  | 14.51 | 4.63  | 3.50 | 4166 |   106  |

  - Informed scenario (crossvalidation *ex-post* with last 210 days of 2018, 30 weeks)
  
  |MODEL   | MAPE | RMSE | MAE | CC | DAYS |
  | :-------: | :----: | :----: | :---: |:----:| :-----:|
  | comb    | 6.20   |  2.06  |  1.53  | 281475   |  150   | 
  | prophet | 6.78   |  2.12  |  1.62  | 368   |  60   |
  | nnetar  | 6.83   |  2.16  |  1.60  | 12568   |  60   |
  | auto.arima   | 8.66   |  3.08  |  2.24  | 268538   |  150   |
  | snaive  | 9.77   |  3.19  |  2.38  |  1  |  1   |
  
 ![Pleiades interiors](https://github.com/VicenteYago/Greenhouses4.0-ML/blob/main/images/boxplotsEscenarios.jpeg)


#### Finally, as a practical case, an alert system has been developed, defined as follows
  - An alarm is triggered if in a week the predicted temperature exceeds the threshold 40 º C for 6 hours in a row.

  |           | NO ALARM | ALARM |  
  | :-------: | :----: | :----: |
  | **NO ALARM**| TNR =  0.82  | FPR = 0.17
  | **ALARM**   | FNR =  0  | TPR = 1

ACC = 0.89  

 ![Pleiades interiors](https://github.com/VicenteYago/Greenhouses4.0-ML/blob/main/images/ROCnonInf.jpeg)


