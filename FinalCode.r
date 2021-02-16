#Set working directory
setwd("D:/Developer/YouTube_SubsGrowth")

#Dependencies
#install.package("package_name")
library(lubridate)
library(zoo)
library(forecast)
library(ggrepel)
library(ggthemes)
library(ggplot2) 


#Read raw subscriber growth data and check head, tail and dimension
dat = read.csv('Totals.csv')
head(dat)
tail(dat)
dim(dat)

#Make the first observation as 12 as my starting subscriber count
dat[1,2] <- 12

#Generate a cumulative sum and store it in a new column
dat['TotalSubscribers'] <- cumsum(dat['Subscribers'])
tail(dat)
#Change the format of date column from string to date
dat$Date <- as.Date(dat$Date,"%Y-%m-%d")

#Create time series zoo object
date_range = seq(from = as.Date("2020-01-04"), to = as.Date("2021-01-02"), by = 1)
y<-zoo(dat['TotalSubscribers'], date_range)


#Store last day's Subscriber count in a variable to show on the plot
dat_ends <- tail(dat,n=1)


#Plot existing subscriber growth
ggplot(data = y) +
geom_line(mapping = aes(x=date_range, y=TotalSubscribers),color='black',size=2) +
  ggtitle("Total Subscribers for Data Science With Raghav") +
  theme_economist() +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  geom_text_repel(aes(x=Date,y=TotalSubscribers,label=TotalSubscribers),
                  data=dat_ends,fontface="plain",color="red",size=6) 


#MODEL1:- Using method rwf - Random walk with drift model - Equivalent to ARIMA(0,1,0)
z <- ts(dat['TotalSubscribers'],start=min(dat$Date),end=max(dat$Date),frequency=1)
autoplot(z) + aes(x=date_range) +
  autolayer(rwf(z, h=30,drift=TRUE),
            series="Naïve with drift", PI=FALSE) +
  ggtitle("Forecasts for Youtube Subscriber growth") +
  xlab("Date") + ylab("Subscribers") +
  guides(colour=guide_legend(title="Forecast")) +
  theme_economist()

#Store rwf point forecasts in a variable 
point_forecasts <- rwf(z, h=30,drift=TRUE)$mean


#MODEL2:- ARIMA modeling (Auto Regressive Integrated Moving Average)
lambda_subs <- BoxCox.lambda(y)
lambda_subs
fit_subs <- auto.arima(y,lambda=lambda_subs,seasonal = FALSE, approximation = FALSE,
                       stepwise = FALSE)
summary(fit_subs)

#Plot forecasts
fit_subs %>% forecast(h=30) %>% autoplot()  + 
    ggtitle("Forecasts for Youtube channel growth") +
    ylab('Subscriber Count') +
    theme_economist()

#Store ARIMA forecasts in a variable
fcast <- forecast(fit_subs,h=30)
fcast_df <- as.data.frame(fcast$mean)
colnames(fcast_df) <- "a"


#Model Evaluation:- Comparison with Actual Data for next 10 day forecasts
dat_actual <- read.csv('Totals_Actual.csv')
head(dat_actual)
tail(dat_actual)

dat_actual[1,2] <- 219 #Set first day in New data to last day's total subscriber count
dat_actual['TotalSubscribers'] <- cumsum(dat_actual['Subscribers'])

#Sum of Square Errors - RWF with drift model
sum((dat_actual[1:10,'TotalSubscribers'] - point_forecasts[1:10])**2)

#SUm of Square Errors - ARIMA (2,2,3) model
sum((dat_actual[1:10,'TotalSubscribers'] - fcast_df[1:10,'a'])**2)

#Sum of Square Errors - RWF with drift model
sum((dat_actual[1:30,'TotalSubscribers'] - point_forecasts[1:30])**2)

#SUm of Square Errors - ARIMA (2,2,3) model
sum((dat_actual[1:30,'TotalSubscribers'] - fcast_df[1:30,'a'])**2)
    
#How long to get to 1000 subscribers
fit_subs %>% forecast(h=480) %>% autoplot() +ylab('Subscriber Count') +   theme_economist()
