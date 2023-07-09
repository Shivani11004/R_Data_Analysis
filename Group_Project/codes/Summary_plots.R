# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(patchwork)

rm(list = ls()) ### cleaning environment
setwd("C:\\R") ### setting working directory

patan_data<-read.csv("cleaned_data_2.csv") ### reading data

patan_data$Date<-as.Date(patan_data$Date,format = "%Y-%m-%d") ### changing date class

#################### Annual Streamflow data of Patan ######################

Q_annual<-aggregate(patan_data$Q,list(patan_data$year),mean) ### Strmflw annual mean
Q_annual<-round(Q_annual,3) ### setting decimal place upto 3 places
colnames(Q_annual)<-c("Year","Strm.flw") ### changing column names

############# Monthly PPT,PET & Streamflow data of Patan ################

Monthly<-read.csv("Time_Series_Monthly.csv")
Monthly<-round(Monthly,3)### setting decimal place upto 3 place
colnames(Monthly)<-c("Year","Month","P","E","Q") ### changing column name

###### Plotting of Monthly PPT,PET,Streamflow ######

Monthly$yy_mm<-as.Date(with(Monthly,paste(Year,Month,"01",sep='-')),"%Y-%m-%d")

#######PPT#########

p_plot <- ggplot(Monthly, aes(x=yy_mm, y=P)) +
  geom_line(color="steelblue",lwd=0.8)  +
  geom_point(size=1)+
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Precipitation", 
       x="Time",
       title="Monthly Precipitation(mm)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p_plot
ggsave("Monthly_Precipitation.png",width=8,height=4)

###########PET#########

e_plot <- ggplot(Monthly, aes(x=yy_mm, y=E)) +
  geom_line( color="purple",lwd=0.8)  + 
  geom_point(size=1) +
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Evapotranspiration", 
       x="Time", 
       title="Monthly Evapotranspiration(mm)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
e_plot
ggsave("Monthly_Evapotransipiration.png",width=8,height=4)

########## Streamflow ########

q_plot <- ggplot(Monthly, aes(x=yy_mm, y=Q)) +
  geom_line( color="red",lwd=0.8)  + 
  geom_point(size=1) +
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Streamflow", 
       x="Time", 
       title="Monthly Streamflow(cm^3/s)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
q_plot
ggsave("Monthly_Streamflow.png",width=8,height=4)

########### Monthly Boxplot #########

###### PPT ######

p_boxplot<-ggplot(Monthly,aes(x=Month,y=P,group=Month))+
  geom_boxplot(varwidth=T, fill="plum")+xlim(month.abb[1:12])+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Precipitation", 
       x="Months", 
       title="Monthly Precipitation(mm)")
p_boxplot
ggsave("Boxplot_Precipitation.png",width=8,height=4)

####### PET #########

e_boxplot<-ggplot(Monthly,aes(x=Month,y=E,group=Month))+
  geom_boxplot(varwidth=T, fill="brown1")+xlim(month.abb[1:12])+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Evapotranspiration", 
       x="Months", 
       title="Monthly Evapotranspiration(mm)")
e_boxplot
ggsave("Boxplot_Evapotranspiration.png",width=8,height=4)

######## Streamflow #######

q_boxplot<-ggplot(Monthly,aes(x=Month,y=Q,group=Month))+
  geom_boxplot(varwidth=T, fill="darkolivegreen3")+xlim(month.abb[1:12])+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Streamflow", 
       x="Months", 
       title="Monthly Streamflow(cm^3/s)")
q_boxplot
ggsave("Boxplot_Streamflow.png",width=8,height=4)

############ Seasonal data ###########

summer_data<-read.csv("summer.csv")
winter_data<-read.csv("winter.csv")
monsoon_data<-read.csv("monsoon.csv")
post_monsoon_data<-read.csv("post_monsoon.csv")

######## Seasonal Scatter plot #######

######### PPT #########

options(scipen=999)
theme_set(theme_bw())

p_summer<-ggplot(summer_data,aes(x=year,y=P,group=month))+
  geom_point(shape=18, color="darkred")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Precipitation", 
       x="", 
       title="Summer")
p_summer
ggsave("p_summer.png",width=8,height=4)

p_monsoon<-ggplot(monsoon_data,aes(x=year,y=P,group=month))+
  geom_point(shape=17, color="steelblue")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Precipitation", 
       x="", 
       title="Monsoon")
p_monsoon
ggsave("p_monsoon.png",width=8,height=4)

p_postmonsoon<-ggplot(post_monsoon_data,aes(x=year,y=P,group=month))+
  geom_point(shape=19, color="blue4")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Precipitation", 
       x="", 
       title="Post-Monsoon")
p_postmonsoon
ggsave("p_postmonsoon.png",width=8,height=4)

p_winter<-ggplot(winter_data,aes(x=year,y=P,group=month))+
  geom_point(shape=15, color="darkmagenta")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Precipitation", 
       x="", 
       title="Winter")
p_winter
ggsave("p_winter.png",width=8,height=4)

######## PET ##########

e_summer<-ggplot(summer_data,aes(x=year,y=E,group=month))+
  geom_point(shape=18, color="darkred")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Evapotranspiration", 
       x="", 
       title="Summer")
e_summer
ggsave("e_summer.png",width=8,height=4)

e_monsoon<-ggplot(monsoon_data,aes(x=year,y=E,group=month))+
  geom_point(shape=17, color="steelblue")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Evapotranspiration", 
       x="", 
       title="Monsoon")
e_monsoon
ggsave("e_monsoon.png",width=8,height=4)

e_postmonsoon<-ggplot(post_monsoon_data,aes(x=year,y=E,group=month))+
  geom_point(shape=19, color="blue4")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Evapotranspiration", 
       x="", 
       title="Post-Monsoon")
e_postmonsoon
ggsave("e_postmonsoon.png",width=8,height=4)

e_winter<-ggplot(winter_data,aes(x=year,y=E,group=month))+
  geom_point(shape=15, color="darkmagenta")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Evapotranspiration", 
       x="", 
       title="Winter")
e_winter
ggsave("e_winter.png",width=8,height=4)

######### Streamflow ############

q_summer<-ggplot(summer_data,aes(x=year,y=Q,group=month))+
  geom_point(shape=18, color="darkred")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Streamflow", 
       x="", 
       title="Summer")
q_summer
ggsave("q_summer.png",width=8,height=4)

q_monsoon<-ggplot(monsoon_data,aes(x=year,y=Q,group=month))+
  geom_point(shape=17, color="steelblue")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Streamflow", 
       x="", 
       title="Monsoon")
q_monsoon
ggsave("q_monsoon.png",width=8,height=4)

q_postmonsoon<-ggplot(post_monsoon_data,aes(x=year,y=Q,group=month))+
  geom_point(shape=19, color="blue4")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Streamflow", 
       x="", 
       title="Post-Monsoon")
q_postmonsoon
ggsave("q_postmonsoon.png",width=8,height=4)

q_winter<-ggplot(winter_data,aes(x=year,y=Q,group=month))+
  geom_point(shape=15, color="darkmagenta")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Streamflow", 
       x="", 
       title="Winter")
q_winter
ggsave("q_winter.png",width=8,height=4)

######### Patchwork plots ############

p_total<-p_winter+p_postmonsoon+p_monsoon+p_summer
p_total
ggsave("p_all.png",width=16,height=8)

e_total<-e_winter+e_postmonsoon+e_monsoon+e_summer
e_total
ggsave("e_all.png",width=16,height=8)

q_total<-q_winter+q_postmonsoon+q_monsoon+q_summer
q_total
ggsave("q_all.png",width=16,height=8)

######## Yearly Streamflow ##########

gg <- ggplot(Q_annual, aes(x=Year, y=Strm.flw)) +
geom_point(shape=17, color="darkred",size=2)+geom_smooth(method = 'loess', formula = 'y ~ x')+
  xlim(c(1980,2015 )) +
   ylim(c(0, 120)) +
     labs(
          y="Streamflow",
          x="Year",
          title="Average Yearly Streamflow(cm^3/s)")
   
   plot(gg)
   ggsave("yearly_streamflow.png",width=8,height=4)
