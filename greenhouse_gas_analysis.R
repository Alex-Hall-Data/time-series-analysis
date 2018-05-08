library(ggplot2)
library(dplyr)

df <- read.csv("raw_dataset.csv")

df<-df%>%
  filter(CO2.Class==0,CO2<800)

minutes<-nrow(df)

#mean value hourly
hourly_df <- df%>%
  group_by(indx=gl(ceiling(minutes/60),60,minutes))%>%
  summarise_each(funs(mean))%>%
  select(-time)

hourly_df$indx <- as.numeric(as.character(hourly_df$indx))

#plot raw data
ggplot(df , aes(x=time , y=CO2))+
  geom_line()+
  labs(title="CO2 data by minute")

#plot hourly data
ggplot(hourly_df , aes(x=indx , y=CO2))+
  geom_line()+
  labs(title="hourly CO2 Data")

