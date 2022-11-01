# R-Time-Series-Sales
Wine sales

### Import wine data 
Wine_Zin2_1 <- read_dta("Downloads/Wine_Zin2-1.dta")

View(Wine_Zin2_1)

### view scatter
plot(Wine_Zin2_1$cases)
![image](https://user-images.githubusercontent.com/114650133/199144659-e46e5763-3cf5-4789-b540-7e7b4602a4e6.png)

### View Time Series line graph

![image](https://user-images.githubusercontent.com/114650133/199144792-46f9716d-0ee7-44bb-b2e2-150a488ee174.png)


### attach variables 
attach(Wine_Zin2_1)

### linear regression on wine cases
cases_trend<-lm(cases~time(cases))
summary(cases_trend)

### add line of regression 
abline(cases_trend)

![image](https://user-images.githubusercontent.com/114650133/199145493-f5c2583f-1501-4340-a7c3-79e9a2fdd9c9.png)


### add monthly dummies
class(date)
date<-as.Date(date,format = "%m/%d/%y")
class(date)
month<-format(date, "%m")
class(month)
month<-format(date, "%m")
class(month)
month<-as.factor(month)

### linear regression on wine cases plus months
cases_hat<-lm(cases~time(cases)+month)
summary(cases_hat)

### get fitted values and view 
fitted_values<-fitted(cases_hat)

![image](https://user-images.githubusercontent.com/114650133/199149381-44e0d461-10b5-48db-b470-fcff3b61172a.png)


### graph fitted values to actual

ts.plot(ts(cases),ts(fitted_values),col = c("black", "blue"))
legend("topleft", bty="n", lty=c(1,1), col=c("black","blue"),
       legend=c("Actual ", "Fitted "))
![image](https://user-images.githubusercontent.com/114650133/199151272-cd963e5d-f584-4ef5-9c49-33337750547d.png)


### tset the data begin 12/18/04 end 12/07/13
start(Wine_Zin2_1)
end(Wine_Zin2_1)

### missing obs
sum(is.na(Wine_Zin2_1))

### install forcast
install.packages("forecast")
library(forecast)

cases <-ts(cases, start = c(2004,12,18),end = c(2013,12,07), frequency = 13)
ddata<-decompose(cases,"additive")
plot(ddata)

![image](https://user-images.githubusercontent.com/114650133/199151669-dd4d36a5-c377-4c7d-885d-de69a0ad95e4.png)

plot(ddata$trend)

![image](https://user-images.githubusercontent.com/114650133/199158068-d5c7fbf4-84e8-47ef-b414-70ed9abec415.png)

boxplot(cases~cycle(cases))
![image](https://user-images.githubusercontent.com/114650133/199158092-43f544a6-db79-4a2c-beca-35df1961edb0.png)



### auto correlation coefficent 
![image](https://user-images.githubusercontent.com/114650133/199158226-1326b6fe-e5ce-4225-9390-292aa129d5f9.png)


### PAC and PACF graphs
par(mfrow=c(1,2))
acf(dcases,lag.max = 48)
pacf(dcases,lag.max = 48)

![image](https://user-images.githubusercontent.com/114650133/199158871-22e39e95-aef0-4dd1-b407-b9134af2e0a7.png)

### arima models

sarima011<- arima011<-arima(cases, order = c(0,1,1),seasonal = list(order = c(1,1,1),period = 13),method = "ML")
summary(sarima011)
coeftest(sarima011)





