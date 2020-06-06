library(MASS)
library(tseries)
library(forecast)
library(dplyr)
library(ggplot2)
library(imputeTS)
library(readxl)
library(plotly)
library(lubridate)
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
  group_by("month" = floor_date(as.Date(date), unit="month")) %>% 
  mutate(monthly_mean = mean(y,na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by("year" = floor_date(as.Date(date), unit="year")) %>% 
  mutate(yearly_mean = mean(y,na.rm = TRUE)) %>% 
  ungroup()

plot_means<-highchart() %>% 
  hc_yAxis(title = list(text = "PM10 (μg/m3)")) %>% 
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
  


ggplot(daily_mean,aes(x = day, y = y)) +
  geom_line() + 
  xlab("")


get_init_eval<-function(df,freq){

raw_plot<-ggplotly(ggplot(df, aes(x = date, y = y)) +
                     geom_line() + 
                     xlab(""))

ts_data<-ts(df$y,frequency = freq)

ts_fill<-na_interpolation(ts_data)


log_data<-log(ts_fill)


acf_log<-acf(log_data,
             plot = FALSE,
             lag.max = 52)

pacf_log<-pacf(log_kalman,
               plot = FALSE)

acf<-acf(ts_fill,
         plot = FALSE,
         lag.max = 52)

pacf<-pacf(ts_fill,
           plot = FALSE)

adf.test(log_data)

log_decomp<-decompose(log_data)

decomp<-decompose(ts_fill)

return(list("raw_plot" = raw_plot,"log_data" = log_data,"acf_log" = acf_log,
            "pacf_log" = pacf_log,"log_decomp" = log_decomp,"ts_data" = ts_fill,
            "acf" = acf, "pacf" = pacf, "decomp" = decomp))

}

daily_metrics<-get_init_eval(raw_data,24*365)

daily_metrics_2014<-get_init_eval(raw_data %>% 
                                    filter(year(as.Date(date)) == 2014),
                                  freq = 24)
save(daily_metrics,
     daily_metrics_2014,
     plot_means,
     file = './data/plots.Rdata')
