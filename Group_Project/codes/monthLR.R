#library(tidyverse)
library(readr)
library(broom)
library(ggplot2)
scaled_monthly <- read_csv("Downloads/scaled_monthly.csv")
x <- c(scaled_monthly$P)
y <- c(scaled_monthly$Q)
model <- lm(y~x)
print(summary(model))
##################################################
model.diag.metrics <- augment(model)
head(model.diag.metrics)
par(mfrow = c(2, 2),mar=c(2, 2, 2, 2))
plot(model,1,main="",col = "blue")
plot(model,2,main="",col = "blue")
plot(model,3,main="",col = "blue")
plot(model,5,main="",col = "blue")
x0<-as.data.frame(model.diag.metrics)
data1 <- as.data.frame(cbind(x0$y,x0$.fitted ))
colnames(data1)
colnames(data1)<-c("y",".fitted")
x1 <- c(data1$y)
y1 <- c(data1$.fitted)
ggplot(model.diag.metrics, aes(x1, y1)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)+labs(y = "Fitted Q", x= "Actual Q")+ ggtitle("Actual vs Fitted")



##################################################################################################################################################################################################################################################################


x1 <- c(scaled_monthly$E)
y1 <- c(scaled_monthly$Q)
model1 <- lm(y1~x1)
print(summary(model1))
##################################################
model.diag.metrics <- augment(model1)
head(model.diag.metrics)
par(mfrow = c(2, 2),mar=c(2, 2, 2, 2))
plot(model1,1,main="",col = "blue")
plot(model1,2,main="",col = "blue")
plot(model1,3,main="",col = "blue")
plot(model1,5,main="",col = "blue")

##############################################################################################################################################################################################################

x1 <- c(scaled_monthly$E)
x2 <- c(scaled_monthly$P)
y1 <- c(scaled_monthly$Q)
model2 <- lm(y1~x1+x2)
print(summary(model2)) 
#plot(model2)
##################################################
model.diag.metrics <- augment(model2)
head(model.diag.metrics)
model.diag.metrics <- augment(model2)
x0 <- c(model.diag.metrics)
data1 <- as.data.frame(cbind(x0$y1,x0$.fitted ))
colnames(data1)
colnames(data1)<-c("y",".fitted")
p1 <- c(data1$y)
p2 <- c(data1$.fitted)
ggplot(model.diag.metrics, aes(x = p1,y = p2)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)+labs(y = "Fitted Q", x= "Actual Q")+ ggtitle("Actual vs Fitted")
#######################################################################################################
par(mfrow = c(2, 2),mar=c(2, 2, 2, 2))
plot(model2,1,main="",col = "blue")
plot(model2,2,main="",col = "blue")
plot(model2,3,main="",col = "blue")
plot(model2,5,main="",col = "blue")




