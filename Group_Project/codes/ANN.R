library(neuralnet)
library(forecast)
############ Setting the Directory and Reading the Data Sets ###########
setwd("D:\\IITH\\Semester 1\\CE6501\\Project\\data\\")
df <- read.csv("D://IITH/Semester 1/CE6501/Project/data/cleaned_data_1.csv")
######################### Data Scaling Function ########################
scaler <- function(vx){ 
  vals <- (vx - min(vx))/(max(vx) - min(vx))
  return(vals)
}
##################### Data De-Scaling Function ########################
descaler <- function(xv, max, min){ 
  dvals <- xv * (max - min) + min
  dvals <- round(dvals, digits = 5)
  return(dvals)
}
############ Normalization ###########################################
df_scaled<-data.frame(df$Date,scaler(df$P),scaler(df$E),scaler(df$Q))
colnames(df_scaled)<-c("Date","P","E","Q")

############ Split Data  66.6% Train and 33.3% Test #################
train_data<-df_scaled[1:8769,2:4]
test_data<-df_scaled[8770:13149,2:4]
set.seed(1)
############ Neural Network ########################################
fnet<-neuralnet(formula=Q~P+E,
                data=train_data,
                hidden=c(8,4,4),learningrate = 0.1,
                threshold = 0.0184,lifesign = "full",
                lifesign.step = 10,
                linear.output=FALSE)
print(fnet)
plot(fnet)
############## Compute Value ######################################
Predict<-compute(fnet,test_data[,-3])
result<-data.frame(test_data$Q,Predict$net.result)
colnames(result)<-c("Actual","Predict")
############ Descaling ##############
result$Actual<-round(descaler(result$Actual,max(df$Q),min(df$Q)),3)
result$Predict<-round(descaler(result$Predict,max(df$Q),min(df$Q)),3)
############## Metrics evaluation ############
accuracy(result$Predict,result$Actual)
result$Date<-df_scaled[8770:13149,1]
result$year<-as.numeric(strftime(result$Date,"%Y")) #year
annual<-aggregate(Predict~year,result,sum) 
annual$Actual<-aggregate(Actual~year,result,sum)$Actual
########################### Actual vs Fitted Plots #############################
library(ggplot2)
ggplot(result,aes(Actual,Predict))+geom_point(color='darkmagenta')+stat_smooth(method=lm,se=TRUE,color='steelblue',lwd=2)+
  labs(x="Actual StreamFlow",y="Predicted StreamFlow",
       title="Daily StreamFlow Prediction for 2015: Patan")+
  theme_bw()

reg <- read.csv("D://IITH/Semester 1/CE6501/Project/data/test_regression.csv") # Regression Plot Summaries
accuracy(round(reg$.fitted,3),round(reg$y,3)) # Accuracy for the Regression Model


