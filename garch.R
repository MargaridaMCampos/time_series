library(fgarch)
library(rugarch)
library(tidyverse)
library(PerformanceAnalytics)
library(skewt)
data_garch<-read.csv("~/MECD-1/2S/Series_Temporais/Project/data/2015-2019 Nasdaq.txt", stringsAsFactors=FALSE)

log_return<-data_garch %>% 
  mutate(Date = as.Date(Date),
    log_return = log(1+(Close-lag(Close))/lag(Close))) %>% 
  filter(!is.na(log_return))

plot_time_series(log_return,Date,log_return,.title = "Log Returns Nasdaq")

plot_time_series(log_return,Date,Close,.title = "Closing Value Nasdaq")

log_ret<-log_return$log_return
ts_ret<-xts(log_ret,order.by =log_return$Date,frequency = 252)

par(mfrow = c(2,1))
acf(log_ret,plot = FALSE) %>% plot(xlab = "Lag",ylab = "ACF")
acf(abs(log_ret),plot = FALSE) %>% plot(xlab = "Lag",ylab = "ACF")

mean(log_ret)
var(log_ret)
ann_daily_volat<-sqrt(252)*sd(log_ret)

par(mfrow = c(2,1))
chart.RollingPerformance(R = ts_ret,width = 22,FUN = "sd.annualized",scale = 252,
                         main = "1 Month Rolling Volatility")
plot(abs(ts_ret-mean(log_ret)),type = "l",main = "Absolute Mean Deviance")


normal_garch_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                           variance.model = list(model = "sGARCH"),
                           distribution.model = "norm")

normal_garch_fit<-ugarchfit(data = ts_ret,spec = normal_garch_spec)
coef(normal_garch_fit)

plot(abs(ts_ret-mean(log_ret)),type = "l",main = "Absolute Mean Deviance")
plot(sigma(normal_garch_fit), main = "Predicted Volatility Normal GARCH(1,1)")

par(mfrow = c(1,1))

chart.Histogram(ts_ret,methods = c("add.normal","add.density"),
                colorset = c('gray','red','blue'))
dskt(seq(-5,5,0.1),8,0.8) %>%lines() 
legend("topright", c("Density", "Normal Dist"), fill=c("red", "blue"))

arch_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                variance.model = list(model = "sGARCH",garchOrder = c(1,0)),
                                distribution.model = "sstd")

arch_fit<-ugarchfit(data = ts_ret,spec = arch_spec)
coef(arch_fit)

tst_garch_spec<- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                            variance.model = list(model = "sGARCH"),
                            distribution.model = "sstd")

tst_garch_fit<-ugarchfit(data = ts_ret,spec = tst_garch_spec)
coef(tst_garch_fit)

gjr_garch_spec<- ugarchspec(mean.model = list(armaOrder = c(0,0),archm = TRUE,archpow = 2),
                            variance.model = list(model = "gjrGARCH"),
                            distribution.model = "sstd")

gjr_garch_fit<-ugarchfit(data = ts_ret,spec = gjr_garch_spec)
gjr_coef<-coef(gjr_garch_fit)

newsimpact_gjr<-newsimpact(gjr_garch_fit)

plot(newsimpact_gjr$zx,newsimpact_gjr$zy,xlab=  "Pred Error", ylab = "Pred Var")

newsimpact_aparch<-newsimpact(aparch_fit)

plot(newsimpact_aparch$zx,newsimpact_aparch$zy,xlab=  "Pred Error", ylab = "Pred Var")

arma_garch_spec<- ugarchspec(mean.model = list(armaOrder = c(1,1)),
                             variance.model = list(model = "sGARCH"),
                             distribution.model = "sstd")
arma_garch_fit<-ugarchfit(data = ts_ret,spec =arma_garch_spec)
coef(arma_garch_fit)

aparch_spec<- ugarchspec(mean.model = list(armaOrder = c(1,1)),
                             variance.model = list(model = "apARCH"),
                             distribution.model = "sstd")
aparch_fit<-ugarchfit(data = ts_ret,spec =aparch_spec)
coef(aparch_fit)

arma_garch_fit@fit$matcoef

stand_res_gjr<-residuals(gjr_garch_fit,standardize = TRUE)
acf(stand_res_gjr)
Box.test(abs(stand_res_gjr),22,type = "Ljung-Box")

par(mfrow = c(2,1))
plot(gjr_coef['mu']+gjr_coef['archm']*fitted(gjr_garch_fit))
plot(abs(ts_ret-mean(ts_ret)))

data.frame("a"=gjr_garch_fit@fit$sigma^2,"b"=gjr_garch_fit@fit$fitted.values*sqrt(252)) %>% View()

infocriteria(normal_garch_fit) %>% 
  as.data.frame() %>% 
  rename("normal" = V1) %>% 
  rownames_to_column() %>% 
cbind(infocriteria(arch_fit) %>% 
        as.data.frame() %>% 
        rename("arch" = V1))%>% 
  cbind(infocriteria(tst_garch_fit) %>% 
          as.data.frame() %>% 
          rename("tst" = V1))%>% 
  cbind(infocriteria(gjr_garch_fit) %>% 
          as.data.frame() %>% 
          rename("gjr" = V1))%>% 
  cbind(infocriteria(arma_garch_fit) %>% 
          as.data.frame() %>% 
          rename("arma" = V1))%>% 
  cbind(infocriteria(aparch_fit) %>% 
          as.data.frame() %>% 
          rename("aparch" = V1)) %>% t() %>% 
  cbind(data.frame("p" = c("Ljung",
          Box.test(normal_garch_fit %>% residuals(standardize=TRUE),22,"Ljung-Box")['p.value'] %>% as.numeric,
          Box.test(arch_fit %>% residuals(standardize=TRUE),22,"Ljung-Box")['p.value']%>% as.numeric,
          Box.test(tst_garch_fit %>% residuals(standardize=TRUE),22,"Ljung-Box")['p.value']%>% as.numeric,
          Box.test(gjr_garch_fit %>% residuals(standardize=TRUE),22,"Ljung-Box")['p.value']%>% as.numeric,
          Box.test(arma_garch_fit %>% residuals(standardize=TRUE),22,"Ljung-Box")['p.value']%>% as.numeric,
          Box.test(aparch_fit %>% residuals(standardize=TRUE),22,"Ljung-Box")['p.value']%>% as.numeric))) %>% View()

save(ts_ret,file = "./data/garch_data.RData")
