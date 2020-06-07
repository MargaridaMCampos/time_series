library(MASS)
library(tseries)
library(forecast)
library(dplyr)
library(ggplot2)
library(imputeTS)
library(readxl)
library(plotly)
library(lubridate)
library(urca)
library(highcharter)

raw_data<-read_excel('./data/2014-2018 PM10 LisAvLib.xlsx') %>% 
  rename("date" = Data,
         "y" = `Av.da Lib. (µg/m3)`)

means<-raw_data %>%
  group_by("day" = as.Date(date)) %>% 
  mutate(daily_mean = mean(y,na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by("week" = floor_date(as.Date(date), unit="week")) %>% 
  mutate(weekly_mean = mean(y,na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by("month" = floor_date(as.Date(date), unit="month") + 15) %>% 
  mutate(monthly_mean = mean(y,na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by("year" = floor_date(as.Date(date), unit="year") + 180) %>% 
  mutate(yearly_mean = mean(y,na.rm = TRUE)) %>% 
  ungroup()

train<-means %>% 
  distinct("date" = day, "y" = daily_mean) %>% 
  head(1805)

test<-means %>% 
  distinct("date" = day, "y" = daily_mean) %>% 
  tail(21)
  

plot_means<-highchart() %>% 
  hc_xAxis(type = "datetime") %>% 
  hc_yAxis(title = list(text = "PM10 (μg/m3)")) %>% 
  hc_chart(zoomType = "x") %>% 
  hc_add_series(means %>% 
                  distinct("x" = day,"y" = daily_mean),
                "line",hcaes(x = x,y = y), name = "Daily") %>% 
  hc_add_series(means %>% 
                  distinct("x" = week,"y" = weekly_mean),
                "line",hcaes(x = x,y = y),name = "Weekly") %>%
  hc_add_series(means %>% 
                  distinct("x" = month,"y" = monthly_mean),
                "line",hcaes(x = x,y = y),name = "Monthly") %>%
  hc_add_series(means %>% 
                  distinct("x" = year,"y" = yearly_mean),
                "line",hcaes(x = x,y = y), name = "Yearly") %>%
  hc_plotOptions(series = list(showInLegend = TRUE,
                               marker = list(radius = 1)))
  
get_init_eval<-function(df,freq){

raw_plot<-ggplotly(ggplot(df, aes(x = date, y = y)) +
                     geom_line() + 
                     xlab(""))

ts_data<-ts(df$y,frequency = freq)

ts_fill<-na_interpolation(ts_data)

ts_diff<-ts_fill %>% 
  diff()

log_data<-log(ts_fill)


acf_log<-Acf(log_data,
             plot = FALSE,
             lag.max = Inf)

pacf_log<-Pacf(log_data,
               plot = FALSE,
               lag.max = Inf)

box_data<-BoxCox(ts_fill,
                 lambda = BoxCox.lambda(ts_fill))

acf<-Acf(ts_diff,
         plot = FALSE,
         lag.max = Inf)

pacf<-Pacf(ts_diff,
           plot = FALSE,
           lag.max = Inf)

acf_box<-Acf(box_data,
         plot = FALSE,
         lag.max = Inf)

pacf_box<-Pacf(box_data,
           plot = FALSE,
           lag.max = Inf)

adf.test(log_data)

log_decomp<-decompose(log_data)

decomp<-decompose(ts_diff)

box_decomp<-decompose(box_data)

return(list("raw_plot" = raw_plot,"log_data" = log_data,"acf_log" = acf_log,
            "pacf_log" = pacf_log,"log_decomp" = log_decomp,"ts_data" = ts_fill,
            "acf" = acf, "pacf" = pacf, "decomp" = decomp, "box_data" = box_data,
            "acf_box" = acf_box, "pacf_box" = pacf_box, "box_decomp" = box_decomp))

}

daily_metrics<-get_init_eval(train,7)

daily_metrics_2014<-get_init_eval(raw_data %>% 
                                    filter(year(as.Date(date)) == 2014),
                                  freq = 24)
monthly_metrics<-get_init_eval(means %>% 
                                 distinct("date" = month, "y" = monthly_mean),
                               freq = 12)
auto_arima_month<-auto.arima(monthly_metrics$ts_data,stationary = TRUE)


save(daily_metrics,
     daily_metrics_2014,
     plot_means,
     monthly_metrics,
     file = './data/plots.Rdata')

ur.kpss(daily_metrics$ts_data %>% diff()) %>% summary()
ndiffs(daily_metrics$ts_data)
ndiffs(daily_metrics$log_data)
ndiffs(daily_metrics$box_data)
nsdiffs(daily_metrics$box_data)
autoplot(mstl(daily_metrics$log_data))

ma_data<-ma(daily_metrics$ts_data,order = 10)
ma_plot<-highchart() %>% 
  hc_chart(zoomType = "x",type = "line") %>%
  hc_xAxis(categories = as.Date(unique(means$day))) %>% 
  hc_add_series(daily_metrics$ts_data %>% as.numeric, name = "Original") %>% 
  hc_add_series(ma_data %>% as.numeric(), name = "MA 10",dashStyle = "Dash")

exp_smooth<-ses(daily_metrics$log_data,h = 5)
accuracy(exp_smooth)

autoplot(exp_smooth) +
  autolayer(fitted(exp_smooth), series="Fitted")

holt <- holt(daily_metrics$log_data, h=15)
holt_damped <- holt(daily_metrics$log_data, damped=TRUE, phi = 0.9, h=15)
autoplot(daily_metrics$log_data) +
  autolayer(holt, series="Holt's method", PI=FALSE) +
  autolayer(holt_damped, series="Damped Holt's method", PI=FALSE) 

auto_d1<-auto.arima(daily_metrics$ts_data,d = 1,max.p = 20, max.q = 3)
auto_log<-auto.arima(daily_metrics$log_data,max.p = 20, max.q = 3)
ma_log<-arima(daily_metrics$log_data,order = c(4,0,3))

pred_auto_log<-predict(auto_log,n.ahead = 26)


plot(Acf(ma_log$residuals))

Pacf(daily_metrics$log_data %>% diff(lag = 7),lag.max = 30)

seasonal_log <- auto.arima(daily_metrics$log_data,D=1)
seasonal_log %>% plot()

forecast_seasonal_log<-forecast(seasonal_log,h = 21)

highchart() %>% 
  hc_chart(zoomType = "x") %>% 
  hc_add_series(forecast_seasonal_log$mean%>% as.numeric()) %>% 
  hc_add_series(test$y %>% log %>% as.numeric())

### RUBEN IDEAS -----------------------
# x=arima.sim(list(order=c(1,1,0), ar=.80), n=500)
# plot(x)
# xacf=acf(x, lag.max=100)
# xpacf=acf(x, lag.max=100, type = c("partial"))
# par(mfrow=c(1,2))
# plot(xacf, type="h", xlab="lag", ylim=c(-.8,1))
# abline(h=0)
# plot(xpacf, type="h", xlab="lag", ylim=c(-.8,1))
# abline(h=0)
