library(ggplot2)
library(grid)
library(patchwork)
library(cowplot)
setwd("D:\\IITH\\Semester 1\\CE6501\\Project\\data\\")
df <- read.csv("D://IITH/Semester 1/CE6501/Project/data/Monthly.csv")
df1<-df[,3:5] # monthly 3:5
process <- preProcess(as.data.frame(df1), method=c("range"))
norm_scale <- predict(process, as.data.frame(df1))
df[,3:5]<-norm_scale # monthly 3:5
#ax<-matrix(df$P,nrow=12,ncol = 36)
#################################################################################
plot_methods_EQ<-function(df,mm){
  title<-paste(month.abb[mm])
  t<-ggplot(df[df$month==mm,],aes(P,Q))+geom_point(color='black',shape=21,size=3,fill='blue')
  t<-t+ labs(x="PPT",title=title)+ theme_bw()
  t<-t+ geom_smooth(method = lm,color='red')
  return(t)
}
plots<-list() # Saving Each Month plots to a list
for(i in 1:12){
  plots[[i]]<-plot_methods_EQ(df,i)+theme(axis.title.y = element_blank())
}
y_lab_big <- 
  ggplot() + 
  annotate(geom = "text", x = 1, y = 1, label = "StreamFlow (in cms)", angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()

(y_lab_big |wrap_plots(plots, nrow = 3,ncol=4,)) +
  plot_layout(widths = c(.1,3))

