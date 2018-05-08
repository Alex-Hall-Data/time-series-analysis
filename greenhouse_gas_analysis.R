library(ggplot2)
library(dplyr)
library(TSA)
library(plotly)
library(zoo)
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
# ggplot(df , aes(x=time , y=CO2))+
#   geom_line()+
#   labs(title="CO2 data by minute")
# 
# #plot hourly data
# ggplot(hourly_df , aes(x=indx , y=CO2))+
#   geom_line()+
#   labs(title="hourly CO2 Data")
# 
# #plot daily data
# ggplot(daily_df , aes(x=indx , y=CO2))+
#   geom_line()+
#   labs(title="daily CO2 Data")





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


#ARIMA to find outliers
#####


  daily_ts <- ts(daily_df$CO2)
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



#plot with plotly
#####
#need to make a single df
daily_df$indx <- daily_df$indx *(60*24)
hourly_df$indx <- hourly_df$indx *(60)
df$indx <- df$time

daily_df <-daily_df%>%
  rename(daily_CO2 = CO2)

hourly_df <-hourly_df%>%
  rename(hourly_CO2 = CO2)

all_data <- left_join(df , hourly_df , by = "indx")
all_data <- left_join(all_data , daily_df , by="indx")%>%
  select(time,CO2,hourly_CO2 , daily_CO2)

all_data <- na.locf(all_data)
all_data[is.na(all_data)] <- 0
all_data <- all_data [-c(1:1500),]



# font style
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "black")


#plot concentration
dataplot <- plot_ly(data=all_data , x=~time)%>%
  add_lines( y=~hourly_CO2 , name="by-hour",visible=T)%>%
  add_lines(y=~CO2 , name = "by-minute",visible=F)%>%
  add_lines( y=~daily_CO2 , name="by-day",visible=F)%>%
  
 
  layout(
    annotations=list(
    text= "CO2 concentration data \n \n CO2 concentration",
    font = f,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE),
    xaxis = list(domain=c(0,43199)),
    yaxis= list(title = 'Concentration (ppm)'),
     updatemenus = list(
 
       list(
         y=0.5,
         buttons=list(
           
           list(method = "restyle",
                args = list("visible", list(T,F,F)),
                label = "by-hour"),
           list(method = "restyle",
               args = list("visible", list(F,T,F)),
                label = "by-minute"),
         list(method = "restyle",
             args = list("visible", list(F,F,T)),
              label = "by-day"))
       )
     )
  )


#plot FFT
#make single df
minute_fft <- data.frame(period = (1/minute_spec$freq) , minute_spec=log(minute_spec$spec))%>%
  filter(period>1 , period < 50)

hourly_fft <- data.frame(period = (1/hourly_spec$freq) , hourly_spec=log(hourly_spec$spec)) %>%
  filter(period>1 , period < 50)

daily_fft <- data.frame(period = (1/daily_spec$freq) , daily_spec=log(daily_spec$spec)) %>%
  filter(period>1 , period < 50)

fft_plot <- plot_ly(data=minute_fft , x=~period)%>%
  add_lines(data=hourly_fft , y=~hourly_spec , name="by-hour",visible=T)%>%
  add_lines(data = minute_fft , y=~minute_spec , name = "by-minute",visible=F)%>%
  add_lines( data = daily_fft , y=~daily_spec , name="by-day",visible=F)%>%
  
  layout(
    annotations=list(
    text= "Spectral plot of data",
    font = f,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE),
    xaxis = list(domain=c(0,50)),
    yaxis= list(title = 'Concentration (ppm)'),
    updatemenus = list(
      
      list(
        y=0.5,
        buttons=list(
          
          list(method = "restyle",
               args = list("visible", list(T,F,F)),
               label = "by-hour"),
          
          list(method = "restyle",
               args = list("visible", list(F,T,F)),
               label = "by-minute"),
          

          
          list(method = "restyle",
               args = list("visible", list(F,F,T)),
               label = "by-day"))
      )
    )
  )


#plot outliers
outlier_plot <- plot_ly(data=outlier_df , x=~x)%>%
  add_lines(data = outlier_df , y=~actual , name = "actual",visible=T)%>%
  add_lines(data=outlier_df , y=~adjusted , name="adjusted",visible=F )%>%
  add_trace(data=outlier_points , y=~actual , x=~x, name="outlier", mode='markers' , visible=F)%>%
  layout(
    annotations=list(
      text= "Outlier plot",
      font = f,
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE),
    xaxis = list(domain=c(0,50)),
    yaxis= list(title = 'concentration (ppm)'),
    updatemenus = list(
      
      list(
        y=0.5,
        x=1.5,
        buttons=list(
          list(method = "restyle",
               args = list("visible", list(T,F,F)),
               label = "raw data"),
          
          list(method = "restyle",
               args = list("visible", list(T,T,T)),
               label = "outliers shown"))
      )
    )
  )


 subplot(dataplot , fft_plot , nrows=2 , titleX=T , titleY=T  , margin=0.1)%>%
   layout(
          showlegend=FALSE,showlegend2=FALSE)
 
 outlier_plot

