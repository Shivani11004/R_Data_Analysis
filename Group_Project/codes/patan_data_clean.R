setwd("D:\\IITH\\Semester 1\\CE6501\\Project\\data\\")
##### read the dataframe#############
df<-read.csv("Patan.csv")

# set the datetime###########
df$Date<-as.Date(df$Date,format="%d-%m-%Y")  ## Setting the Date Format
df$year<-as.numeric(strftime(df$Date,"%Y")) #year
df$month<-as.numeric(strftime(df$Date,"%m")) #month
df$day<-as.numeric(strftime(df$Date,"%d"))# day
# Data Cleaning ##############
# count total missing value in P, E ,Q
sum(is.na(df$P))
sum(is.na(df$E))
sum(is.na(df$Q))
mean(df$Q)
# Replace missing values for column Q ###########
monthly<-aggregate(Q~year+month,df,mean)
Q_mat<-matrix(monthly$Q,nrow=12,ncol=36)
Q_mat<-data.frame(Q_mat)
rownames(Q_mat)<-c(1:12)
colnames(Q_mat)<-c(1980:2015)
missing<-which(is.na(df$Q))
for(i in missing){
  yy<-as.character(df$year[i])
  mm<-as.character(df$month[i])
  df$Q[i]<-Q_mat[mm,yy]
}
################################################################################
################## Monthly Data ##########################
#function to make monthly dataset of P E and Q
sort_month<-function(df,x,func,name){
  yy<-unique(df$year) # Unique years
  mm<-unique(df$month) #unique months
  vec_val<-c() #empty vector to store the aggregates
  vec_yy<-c() # empty vector to store year for each iteration
  vec_mm<-c() # empty vector to store month for each iteration
  for(i in yy){
    for(j in mm){
      val<-sapply(df[df$year==i & df$month==j,][x],func) ### Monthly Aggregate based on the Variable
      vec_val<-append(vec_val,val) # append the aggregated value
      vec_yy<-append(vec_yy,i) #append the year
      vec_mm<-append(vec_mm,j) #append the month
    }
  }
  Monthly<-data.frame(vec_yy,vec_mm,vec_val) # create a dataframe using year, month and variable
  colnames(Monthly)<-c("year","month",name) #renaming the columns
  return(Monthly)
}

Month_P<-sort_month(df,2,sum,names(df[2]))
Month_E<-sort_month(df,3,sum,colnames(df[3]))
Month_Q<-sort_month(df,4,mean,colnames(df[4]))
Monthly<-Month_P
Monthly$E<-Month_E$E
Monthly$Q<-Month_Q$Q
write.csv(Monthly,"Monthly.csv",row.names=FALSE)
df1<-Monthly[,3:5] # monthly 3:5
process <- preProcess(as.data.frame(df1), method=c("range"))
norm_scale <- predict(process, as.data.frame(df1))
Monthly[,3:5]<-norm_scale # monthly 3:5
#write.csv(df,"scaled_monthly.csv",row.names = FALSE)
##################################################################
############### Seasonal Data #######################
season<-function(df,x,y){
  a<-subset(df,df$month %in% c(x:y))
  b<-aggregate(P~year+month,a,sum)
  b$E<-aggregate(E~year+month,a,sum)$E
  b$Q<-aggregate(Q~year+month,a,mean)$Q# segregating the months and year
  return(b)
}
winter<-season(df,1,2)
summer<-season(df,3,5)
monsoon<-season(df,6,9)
post_monsoon<-season(df,10,12)
write.csv(winter,"winter.csv",row.names=FALSE)
write.csv(summer,"summer.csv",row.names=FALSE)
write.csv(monsoon,"monsoon.csv",row.names=FALSE)
write.csv(post_monsoon,"post_monsoon.csv",row.names=FALSE)
##################################################################
############### Yearly Data #######################
annual<-aggregate(P~year,df,sum) 
annual$E<-aggregate(E~year,df,sum)$E
annual$Q<-aggregate(Q~year,df,mean)$Q
write.csv(annual,"annual.csv",row.names=FALSE)
