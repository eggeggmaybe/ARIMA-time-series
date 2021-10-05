#This is used to call back the actual data forecast when you difference the time series data before using ARIMA.
#Note that the difference can be first, seasonal, or both

library(readr)
library(fpp3)
total_train=read_csv("Clean data train.csv")
total_test=read_csv("Clean data test.csv")

testset=total_test
Tout<-nrow(total_test)
y_f1<-ts(numeric(Tout),start=c(2021,1),frequency=17568) #define testing period
Demand_test_ts<- ts(testset[,1],frequency =48*7,start=decimal_date(as.POSIXct("2021-01-01 00:00:00")))

#call back to level prediction, the function assume data is yearly
#step is how many data points needed call back
#season is the how many data points in a day
call_back = function (step,season) {
  y_f1<-ts(numeric(Tout),start=c(2021,1),frequency=season*366) #366 means yearly data
  step=step
  season=season 
  data=total_train[,1]
  datats=ts(data,frequency=season*366) #366 means yearly data
  datats=tail(datats,season+1)
  fcast=as.numeric(y_f1)
  level_fcast=c()

  for (i in seq(1,step)){
    y_ti=fcast[i]+(datats[season+i]-datats[i])+datats[i+1]
    datats=append(datats,y_ti)
    level_fcast=append(level_fcast,y_ti)
    if (i%%7==0) {
      datats=head(datats,-1)
      datats=append(datats,Demand_test_ts[i])
    }
  }
  level_fcast
}

call_back(9648,48)
