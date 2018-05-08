library(ggplot2)
library(dplyr)
library(TSA)
library(plotly)
library(tsoutliers)

df <- read.csv("raw_dataset.csv")

df<-df%>%
  filter(CO2.Class==0,CO2<800)

minutes<-nrow(df)

#make datasets with different timescales
#####
#mean value hourly
hourly_df <- df%>%
  group_by(indx=gl(ceiling(minutes/60),60,minutes))%>%
  summarise_each(funs(mean))%>%
  select(-time)

hourly_df$indx <- as.numeric(as.character(hourly_df$indx))



#mean value daily
daily_df <- df%>%
  group_by(indx=gl(ceiling(minutes/(60*24)),(60*24),minutes))%>%
  summarise_each(funs(mean))%>%
  select(-time)

daily_df$indx <- as.numeric(as.character(daily_df$indx))

#plot data
#####
#plot raw data
ggplot(df , aes(x=time , y=CO2))+
  geom_line()+
  labs(title="CO2 data by minute")

#plot hourly data
ggplot(hourly_df , aes(x=indx , y=CO2))+
  geom_line()+
  labs(title="hourly CO2 Data")

#plot daily data
ggplot(daily_df , aes(x=indx , y=CO2))+
  geom_line()+
  labs(title="daily CO2 Data")


#analyse data
#####
#FFT

minute_spec <- spectrum(df$CO2 , span=500 , plot=F)
hourly_spec <- spectrum(hourly_df$CO2 , span=50 , plot=F)
daily_spec <- spectrum(daily_df$CO2 , span=5 , plot=F)

plot_spectrograph <- function(fft_spec , period , x_label){
fft_spec <- as.data.frame(cbind(fft_spec$freq , fft_spec$spec))
names(fft_spec) <- c("freq" , "spec")

fft_spec <- fft_spec %>%
  filter(freq > 0.02)

ggplot(fft_spec , aes(x=1/freq , y=log(spec)))+
  geom_line()+
  labs(title=paste("Smoothed Spectrogram of", period , "CO2 data"))+
  xlab(paste0("period(" , x_label , ")"))+
  ylab("spectral density")
}

p1<-plot_spectrograph(minute_spec , "raw","minutes")
p2<-plot_spectrograph(hourly_spec , "hourly","hours")
p3<-plot_spectrograph(daily_spec , "daily","days")

#ggplotly(p1,p2,p3)


#plot outliers (ARIMA)
#####

plot_outliers <- function(DF){
  daily_ts <- ts(DF$CO2)
  minute.outlier <- tso(y=daily_ts,types=c("AO","LS","TC"))
  #plot(minute.outlier)

  outlier_df <- as.data.frame(cbind(minute.outlier$y , minute.outlier$yadj  , c(1:length(minute.outlier$yadj ))))
  names(outlier_df) <- c("actual", "adjusted", "x")
  
  outlier_points <- minute.outlier$outliers$time
  
  outlier_points <- outlier_df%>%
    filter(x %in% outlier_points)
  
  ggplot(outlier_df , aes(x=x , y=actual))+
    geom_line(col="blue")+
    geom_line(aes(x=x , y=adjusted) , col="grey")+
    geom_point(data=outlier_points , aes(x=x, y=actual) , col="red")+
    labs(title="daily CO2 concentration with outliers shown")
  
}


#plot_outliers(df)
#plot_outliers(hourly_df)
plot_outliers(daily_df)
