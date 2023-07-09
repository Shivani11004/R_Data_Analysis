## Code Till Monthly sorted data with dataframes contaning values like mean mode etc for yearly and monthly##

#Getting required libraries
library('psych')
library('moments')
#setting directory
setwd("C:\\Users\\utkar\\OneDrive\\Documents\\RProject")
#Reading dataset
patan_data <- read.csv("Patan.csv")

#setting up date as our required formates and making new col with value of year and month
patan_data$Date <- as.Date(patan_data$Date, format = "%d-%m-%Y") 
patan_data$Year <- as.numeric(format(patan_data$Date, "%Y"))
patan_data$Month <- as.numeric(format(patan_data$Date, "%m"))
patan_data$Day <- as.numeric(format(patan_data$Date, "%d"))
patan_data$Month_Year <- (format(patan_data$Date, "%m/%Y"))

any(is.na(patan_data)) #to find if data has NA
mean(patan_data$Q) # find the mean, this will result in NA
x<- c(which(is.na(patan_data$Q))) # saving all locations which has NA
########
for (i in x) {
  year = patan_data$Year[i] #taking year od point having NA
  month = patan_data$Month[i] # storng month of point having NA
  # making a subset data of that whole month of the year which has NA
  sub = subset(patan_data$Q, patan_data$Year == year & patan_data$Month == month) 
  No_NA = sum(is.na(sub)) # findign total number of NA in that month
  sub[is.na(sub)] = 0 # replacing NA with zero
  No = length(sub) # total length or total day in that month
  avg = sum(sub)/(No - No_NA) #finding average flow of that month
  patan_data$Q[i] = avg # replacing NA in original dataset with month avg
}

#function to make monthly dataset of P E and Q
sort_month<-function(df,x,func,name){
  yy<-unique(df$Year) # Unique years
  mm<-unique(df$Month) #unique months
  vec_val<-c() #empty vector to store the aggregates
  vec_yy<-c() # empty vector to store year for each iteration
  vec_mm<-c() # empty vector to store month for each iteration
  for(i in yy){
    for(j in mm){
      val<-sapply(df[df$Year==i & df$Month==j,][x],func) ### Monthly Aggregate based on the Variable
      vec_val<-append(vec_val,val) # append the aggregated value
      vec_yy<-append(vec_yy,i) #append the year
      vec_mm<-append(vec_mm,j) #append the month
    }
  }
  YM <- paste(vec_yy, "-", vec_mm)
  Monthly<-data.frame(vec_yy,vec_mm,YM,vec_val) # create a dataframe using year, month and variable
  colnames(Monthly)<-c("Year","Month","Year_Month", name) #renaming the columns
  rm(YM)
  return(Monthly)
}

Month_P<-sort_month(patan_data,2,sum,colnames(patan_data[2]))
Month_E<-sort_month(patan_data,3,sum,colnames(patan_data[3]))
Month_Q<-sort_month(patan_data,4,mean,colnames(patan_data[4]))
Monthly<-Month_P
Monthly$E<-Month_E$E
Monthly$Q<-Month_Q$Q
rm(Month_E, Month_P, Month_Q, avg, i, month, No, No_NA, sub, x, year, sort_month)
write.csv(Monthly,"Monthly.csv", row.names = FALSE)

#Getting Stats for Month P
Monthly_stat<-function(df,mon){
  set <- subset(df, df$Month == mon)
  set_mean <- mean(set$P)
  set_median <- median(set$P)
  set_sd <- sd(set$P)
  set_MAD <- mad(set$P)
  set_CV <- (set_sd/set_mean)*100
  set_skew <- skewness(set$P)
  Monthly_stats<-data.frame(set_mean, set_median, set_sd, set_MAD, set_CV, set_skew) # create a dataframe using year, month and variable
  return(Monthly_stats)
}
Month_Stat_P <- data.frame()
for(i in 1:12){
  month_ <- Monthly_stat(Monthly,i)
  Month_Stat_P <- rbind(Month_Stat_P, month_)
}
rownames(Month_Stat_P) <- c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames(Month_Stat_P) <- c("Mean_P", "Median_P", "SD_P", "MAD_P", "CV_P", "Skew_P")
rm(month_, i, Monthly_stat,x)

#Summary Stat for Month Q

Monthly_stat<-function(df,mon){
  set <- subset(df, df$Month == mon)
  set_mean <- mean(set$Q)
  set_median <- median(set$Q)
  set_sd <- sd(set$Q)
  set_MAD <- mad(set$Q)
  set_CV <- (set_sd/set_mean)*100
  set_skew <- skewness(set$Q)
  Monthly_stats<-data.frame(set_mean, set_median, set_sd, set_MAD, set_CV, set_skew) # create a dataframe using year, month and variable
  return(Monthly_stats)
}
Month_Stat_Q <- data.frame()
for(i in 1:12){
  month_ <- Monthly_stat(Monthly,i)
  Month_Stat_Q <- rbind(Month_Stat_Q, month_)
}

rownames(Month_Stat_Q) <- c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames(Month_Stat_Q) <- c("Mean_Q", "Median_Q", "SD_Q", "MAD_Q", "CV_Q", "Skew_Q")
rm(month_, i, Monthly_stat)

#Summary Stat for Month E

Monthly_stat<-function(df,mon){
  set <- subset(df, df$Month == mon)
  set_mean <- mean(set$E)
  set_median <- median(set$E)
  set_sd <- sd(set$E)
  set_MAD <- mad(set$E)
  set_CV <- (set_sd/set_mean)*100
  set_skew <- skewness(set$E)
  Monthly_stats<-data.frame(set_mean, set_median, set_sd, set_MAD, set_CV, set_skew) # create a dataframe using year, month and variable
  return(Monthly_stats)
}

Month_Stat_E <- data.frame()
for(i in 1:12){
  month_ <- Monthly_stat(Monthly,i)
  Month_Stat_E <- rbind(Month_Stat_E, month_)
}
rm(month_, i, Monthly_stat)
rownames(Month_Stat_E)<-c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames(Month_Stat_E) <- c("Mean_E", "Median_E", "SD_E", "MAD_E", "CV_E", "Skew_E")
rm(month_, i, Monthly_stat,x)

#Combining Monthly Summary Stats for Month_Stat_E,Month_Stat_P,Month_Stat_Q and deleting the non essential data
x <- c(1:12)
Month_Stat = cbind(Month_Stat_E,Month_Stat_P,Month_Stat_Q,x)
rm(Month_Stat_E,Month_Stat_P,Month_Stat_Q,x)


#Yearly Summary Stats for Yearly P

Yearly_Stat<-function(df, yr){
  set <- subset(df, df$Year == yr)
  set_mean <- mean(set$P)
  set_median <- median(set$P)
  set_sd <- sd(set$P)
  set_MAD <- mad(set$P)
  set_CV <- (set_sd/set_mean)*100
  set_skew <- skewness(set$P)
  Year_stats<-data.frame(set_mean, set_median, set_sd, set_MAD, set_CV, set_skew) # create a dataframe using year, month and variable

  return(Year_stats)
}
Year_Stat_P <- data.frame()
for(i in 1980:2015){
  year_ <- Yearly_Stat(Monthly,i)
  Year_Stat_P <- rbind(Year_Stat_P, year_)
}
rm(year_, i, Yearly_Stat)
rownames(Year_Stat_P)<-c(1980:2015)
colnames(Year_Stat_P) <- c("Mean_P", "Median_P", "SD_P", "MAD_P", "CV_P", "Skew_P")



#Yearly Summary Stats for Yearly Q

Yearly_Stat<-function(df, yr){
  set <- subset(df, df$Year == yr)
  set_mean <- mean(set$Q)
  set_median <- median(set$Q)
  set_sd <- sd(set$Q)
  set_MAD <- mad(set$Q)
  set_CV <- (set_sd/set_mean)*100
  set_skew <- skewness(set$Q)
  Year_stats<-data.frame(set_mean, set_median, set_sd, set_MAD, set_CV, set_skew) # create a dataframe using year, month and variable
  
  return(Year_stats)
}
Year_Stat_Q <- data.frame()
for(i in 1980:2015){
  year_ <- Yearly_Stat(Monthly,i)
  Year_Stat_Q <- rbind(Year_Stat_Q, year_)
}
rm(year_, i, Yearly_Stat)
rownames(Year_Stat_Q)<-c(1980:2015)
colnames(Year_Stat_Q) <- c("Mean_Q", "Median_Q", "SD_Q", "MAD_Q", "CV_Q", "Skew_Q")


# Yearly Summary Stats for Yearly E

Yearly_Stat<-function(df, yr){
  set <- subset(df, df$Year == yr)
  set_mean <- mean(set$E)
  set_median <- median(set$E)
  set_sd <- sd(set$E)
  set_MAD <- mad(set$E)
  set_CV <- (set_sd/set_mean)*100
  set_skew <- skewness(set$E)
  Year_stats<-data.frame(set_mean, set_median, set_sd, set_MAD, set_CV, set_skew) # create a dataframe using year, month and variable
  
  return(Year_stats)
}
Year_Stat_E <- data.frame()
for(i in 1980:2015){
  year_ <- Yearly_Stat(Monthly,i)
  Year_Stat_E <- rbind(Year_Stat_E, year_)
}
rm(year_, i, Yearly_Stat)
rownames(Year_Stat_E)<-c(1980:2015)
colnames(Year_Stat_E) <- c("Mean_E", "Median_E", "SD_E", "MAD_E", "CV_E", "Skew_E")

#Combining Yearly Summary Stats for Year_Stat_E,Year_Stat_P,Year_Stat_Q and deleting the non essential data
x <- c(1980:2015)
Year_Stat = cbind(Year_Stat_E,Year_Stat_P,Year_Stat_Q,x)
rm(Year_Stat_E,Year_Stat_P,Year_Stat_Q,x)

library(ggplot2)
library(tidyverse)
library(lubridate)
library(tseries)

#Plotting the autocorrelation Graphs

acf(patan_data$Q, lag.max=1000, main="Autocorrelation of Streamflow", col = "Orange", ylab = "Autocorrelation")
acf(patan_data$E, lag.max=1000, main="Autocorrelation of Streamflow", col = "Green", ylab = "Autocorrelation")
acf(patan_data$P, lag.max=1000, main="Autocorrelation of Streamflow", col = "Red", ylab = "Autocorrelation")
acf(patan_data$Q, lag.max=90, main="Autocorrelation of Streamflow", col = "Orange", ylab = "Autocorrelation")
acf(patan_data$E, lag.max=90, main="Autocorrelation of Streamflow", col = "Green", ylab = "Autocorrelation")
acf(patan_data$P, lag.max=90, main="Autocorrelation of Streamflow", col = "Red", ylab = "Autocorrelation")


### Plotting the Monthly Summary Stats

mean_plot <- ggplot(Month_Stat, aes(x=x)) +
  geom_line(aes(y = Mean_P,colour="Mean P")) +
  geom_line(aes(y = Mean_E,colour="Mean E")) +
  geom_line(aes(y = Mean_Q,colour="Mean Q"))+
  xlim(month.abb[1:12])+
  labs(
    y="Mean",
    x="",
    title = "Comparison of Mean")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank()# Center title position and size
  )
mean_plot


median_plot <- ggplot(Month_Stat, aes(x=x)) +
  geom_line(aes(y = Median_P,colour="Median P")) +
  geom_line(aes(y = Median_E,colour="Median E")) +
  geom_line(aes(y = Median_Q,colour="Median Q"))+
  xlim(month.abb[1:12])+
  labs(
    y="Median",
    x="",
    title = "Comparison of Median")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank()# Center title position and size
  )
median_plot


SD_plot <- ggplot(Month_Stat, aes(x=x)) +
  geom_line(aes(y = SD_P,colour="SD P")) +
  geom_line(aes(y = SD_E,colour="SD E")) +
  geom_line(aes(y = SD_Q,colour="SD Q"))+
  xlim(month.abb[1:12])+
  labs(
    y="Standard Deviation",
    x="",
    title = "Comparison of Standard Deviation")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank()# Center title position and size
  )
SD_plot


MAD_plot <- ggplot(Month_Stat, aes(x=x)) +
  geom_line(aes(y = MAD_P,colour="MAD P")) +
  geom_line(aes(y = MAD_E,colour="MAD E")) +
  geom_line(aes(y = MAD_Q,colour="MAD Q"))+
  xlim(month.abb[1:12])+
  labs(
    y="Median Absolute Deviation",
    x="",
    title = "Comparison of Median Absolute Deviation")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank()# Center title position and size
  )
MAD_plot


CV_plot <- ggplot(Month_Stat, aes(x=x)) +
  geom_line(aes(y = CV_P,colour="CV P")) +
  geom_line(aes(y = CV_E,colour="CV E")) +
  geom_line(aes(y = CV_Q,colour="CV Q"))+
  xlim(month.abb[1:12])+
  labs(
    y="Coefficient of Variation",
    x="",
    title = "Comparison of CV")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank()# Center title position and size
  )
CV_plot


Skew_plot <- ggplot(Month_Stat, aes(x=x)) +
  geom_line(aes(y = Skew_P,colour="Skew P")) +
  geom_line(aes(y = Skew_E,colour="Skew E")) +
  geom_line(aes(y = Skew_Q,colour="Skew Q"))+
  xlim(month.abb[1:12])+
  labs(
    y="Skewness",
    x="",
    title = "Comparison of Skewness")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank()# Center title position and size
  )
Skew_plot


mean_plot + median_plot + SD_plot + MAD_plot + CV_plot + Skew_plot
ggsave("Monthly_stat.png",width = 16, height = 8)
rm(mean_plot,median_plot,SD_plot,MAD_plot,CV_plot,Skew_plot)





##Plotting Yearly Stat Plots

mean_plot <- ggplot(Year_Stat, aes(x=x)) +
  geom_line(aes(y = Mean_P,colour="Mean P")) +
  geom_line(aes(y = Mean_E,colour="Mean E")) +
  geom_line(aes(y = Mean_Q,colour="Mean Q"))+
  xlim(1980,2015)+
  labs(
    y="Mean",
    x="",
    title = "Comparison of Mean")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(-1,-1),legend.title=element_blank()# Center title position and size
  )
mean_plot


median_plot <- ggplot(Year_Stat, aes(x=x)) +
  geom_line(aes(y = Median_P,colour="Median P")) +
  geom_line(aes(y = Median_E,colour="Median E")) +
  geom_line(aes(y = Median_Q,colour="Median Q"))+
  xlim(1980,2015)+
  labs(
    y="Median",
    x="",
    title = "Comparison of Median")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(-1,-1),legend.title=element_blank()# Center title position and size
  )
median_plot


SD_plot <- ggplot(Year_Stat, aes(x=x)) +
  geom_line(aes(y = SD_P,colour="SD P")) +
  geom_line(aes(y = SD_E,colour="SD E")) +
  geom_line(aes(y = SD_Q,colour="SD Q"))+
  xlim(1980,2015)+
  labs(
    y="Standard Deviation",
    x="",
    title = "Comparison of Standard Deviation")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(-1,-1),legend.title=element_blank()# Center title position and size
  )
SD_plot


MAD_plot <- ggplot(Year_Stat, aes(x=x)) +
  geom_line(aes(y = MAD_P,colour="MAD P")) +
  geom_line(aes(y = MAD_E,colour="MAD E")) +
  geom_line(aes(y = MAD_Q,colour="MAD Q"))+
  xlim(1980,2015)+
  labs(
    y="Median Absolute Deviation",
    x="",
    title = "Comparison of Median Absolute Deviation")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(-1,-1),legend.title=element_blank()# Center title position and size
  )
MAD_plot


CV_plot <- ggplot(Year_Stat, aes(x=x)) +
  geom_line(aes(y = CV_P,colour="CV P")) +
  geom_line(aes(y = CV_E,colour="CV E")) +
  geom_line(aes(y = CV_Q,colour="CV Q"))+
  xlim(1980,2015)+
  labs(
    y="Coefficient of Variation",
    x="",
    title = "Comparison of CV")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(-1,-1),legend.title=element_blank()# Center title position and size
  )
CV_plot


Skew_plot <- ggplot(Year_Stat, aes(x=x)) +
  geom_line(aes(y = Skew_P,colour="P")) +
  geom_line(aes(y = Skew_E,colour="E")) +
  geom_line(aes(y = Skew_Q,colour="Q"))+
  xlim(1980,2015)+
  labs(
    y="Skewness",
    x="",
    title = "Comparison of Skewness")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text=element_text(size=12),
    legend.justification=c(1,1),legend.position=c(-1,-1),legend.title=element_blank(),
    # Center title position and size
  )
Skew_plot


mean_plot + median_plot + SD_plot + MAD_plot + CV_plot + Skew_plot
ggsave("Year_stat.png",width = 16, height = 8)
