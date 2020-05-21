rm(list=ls())
#Import and prepare the data
setwd("C:/Mason/Course/DAEN 690/Code/Modeling")
df=read.csv("act_arr.csv")
mydata=df[,-1]
library(dplyr)

m=mydata %>%
  group_by(ACT_ARR_HOUR,ACT_ARR_DATE) %>%
  summarize(Del=sum(ArrDelayMinutes))
m=data.frame(m)
m$ACT_ARR_DATE=strptime(m$ACT_ARR_DATE,format='%Y-%m-%d')

#### polts the delays
plot(m$ACT_ARR_HOUR,m$Del)
RTT=unique(predict(loess(Del ~ ACT_ARR_HOUR , data=m, span=.5)))
lines(RTT,col='red')
plot(m$ACT_ARR_HOUR,m$Del,type = 'l')
jan18=m[m$ACT_ARR_DATE$mon==0 & m$ACT_ARR_DATE$year==118,]

time0=as.POSIXct("2019-01-01 01:00:00")
time24=as.POSIXct("2019-01-02 00:00:00")
xaxis=seq(time0,time24,3600)
xaxis=strptime(xaxis,format='%Y-%m-%d  %H:%M:%S')

###Plot for Jan 18####
library(randomcoloR)
color=distinctColorPalette(31)
par(xpd = T, mar = par()$mar + c(0,0,0,4))
plot(x=xaxis,y=jan18$Del[jan18$ACT_ARR_DATE$mday==1],type='l',ylim=c(0,60000),xlab="Time of Day",lwd=1,
     ylab ="Hourly Delay (Minute)",col=color[1],main = "Hourly Delay vs Recurrent Hourly Delay Pattern (RHDP)")
for (i in 2:31){
  lines(y=jan18$Del[jan18$ACT_ARR_DATE$mday==i],x=xaxis,col=color[i],lwd=1)
}
lines(RTT,col=2,lwd=3,x=xaxis)
tag=c("1-Jan","2-Jan","3-Jan","4-Jan","5-Jan","6-Jan","7-Jan","8-Jan","9-Jan","10-Jan","11-Jan","12-Jan","13-Jan",
      "14-Jan","15-Jan","16-Jan","17-Jan","18-Jan","19-Jan","20-Jan","21-Jan","22-Jan","23-Jan","24-Jan","25-Jan","26-Jan",
      "27-Jan","28-Jan","29-Jan","30-Jan","31-Jan")
legend("topleft",
       legend=tag,
       fill=color, cex=0.55, title="Date", horiz=F, inset=c(1.025,0),xpd=T)

legend("topright",
       legend="RHDP",
       fill='red', cex=0.85, horiz=T, inset=c(0,0), lwd = 2,col='red',border='red')
par(mar=c(5, 4, 4, 2) + 0.1)


 ### Cumm. Del. Plot ####
par(xpd = T, mar = par()$mar + c(0,0,0,4))
plot(x=xaxis,y=cumsum(jan18$Del[jan18$ACT_ARR_DATE$mday==1]),type='l',ylim=c(0,600000),xlab="Time of Day",lwd=1,ylab="Cumulative Delay (Minute)",
     main = "Cumulative Delay vs Recurrent Cumulative Delay Pattern (RCDP)")
for (i in 2:31){
  lines(cumsum(jan18$Del[jan18$ACT_ARR_DATE$mday==i]),col=color[i],lwd=1,x=xaxis)
}
lines(cumsum(RTT),col=2,lwd=3,x=xaxis)
tag=c("1-Jan","2-Jan","3-Jan","4-Jan","5-Jan","6-Jan","7-Jan","8-Jan","9-Jan","10-Jan","11-Jan","12-Jan","13-Jan",
      "14-Jan","15-Jan","16-Jan","17-Jan","18-Jan","19-Jan","20-Jan","21-Jan","22-Jan","23-Jan","24-Jan","25-Jan","26-Jan",
      "27-Jan","28-Jan","29-Jan","30-Jan","31-Jan")
legend("topleft",
       legend=tag,
       fill=color, cex=0.55, title="Date", horiz=F, inset=c(1.025,0),xpd=T)

legend("topright",
       legend="RCDP",
       fill='red', cex=0.85, horiz=T, inset=c(0,0), lwd = 2,col='red',border='red')
par(mar=c(5, 4, 4, 2) + 0.1)


############# Plots of market shares of Airlines
Airlines=read.csv("Airlines.csv")
Airlines=Airlines[order(Airlines$IATA_CODE),]
marketShare <- df %>% 
  group_by(Marketing_Airline_Network) %>%
  dplyr::summarise(Count = n(),
                   mean_ARRIVAL_DELAY = mean(ArrDelayMinutes),
                   sum_ARRIVAL_DELAY = sum(ArrDelayMinutes),
                   min_ARRIVAL_DELAY = min(ArrDelayMinutes),
                   max_ARRIVAL_DELAY = max(ArrDelayMinutes)
  ) %>% arrange(desc(Count))
x=unique(marketShare$Marketing_Airline_Network)
x=x[order(x)]
marketShare$Marketing_Airline_Network=as.character(marketShare$Marketing_Airline_Network)
for (i in 1:11){
  marketShare$Marketing_Airline_Network[marketShare$Marketing_Airline_Network==as.character(x[i])]=as.character(Airlines$AIRLINE[i])}
library(plotly)
plot_ly(marketShare, labels = ~Marketing_Airline_Network, values = ~Count, type = 'pie',
        textposition = 'outside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        marker = list(
          line = list(color = '#FFFFFF', width = 1)),
        showlegend = F)

### Box plots of airlines delays
newairline=subset(df,ArrDelayMinutes>0)
newairline$Marketing_Airline_Network=as.character(newairline$Marketing_Airline_Network)
for (i in 1:11){
  newairline$Marketing_Airline_Network[newairline$Marketing_Airline_Network==as.character(x[i])]=as.character(Airlines$AIRLINE[i])}
ggplot(newairline, aes(x=reorder(Marketing_Airline_Network, ArrDelayMinutes, FUN=median), y=ArrDelayMinutes, fill=Marketing_Airline_Network)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0,70)) +
  labs(x = 'Airline', y = 'Arrival Delay',
       title = 'Arrival Delay vs. Airlines')




########### Heat map of delays
library(tidyverse)
library(lubridate)
df$ACT_ARR_DATE=strptime(df$ACT_ARR_DATE,format='%Y-%m-%d')
df$wday=df$ACT_ARR_DATE$wday+1
df$day=wday(df$wday,label=T)
df$ACT_ARR_DATE=as.character(df$ACT_ARR_DATE)
dt2=df %>%
  group_by(day,ACT_ARR_HOUR ) %>%
  summarize(Del=mean(ArrDelayMinutes),count=n())
dt2=data.frame(dt2)
dt2  %>%
  ggplot(aes(ACT_ARR_HOUR ,day,fill=Del))+
  geom_tile()+
  labs(x="Hour of the day",y="Day of the week")+
  scale_fill_distiller(palette = "Spectral")


####### Plots of delays based on their calss
newdf=df
newdf$Min15=0
newdf$Min15[newdf$ArrDelayMinutes<=15]=1
newdf$Min45=0
newdf$Min45[newdf$ArrDelayMinutes>15 & newdf$ArrDelayMinutes<=45 ]=1
newdf$More=0
newdf$More[newdf$ArrDelayMinutes>45 ]=1
newdf$type[newdf$Min15==1]="On Time (Delay < 15 Mins)"
newdf$type[newdf$Min45==1]="Small Delay (15 < Delay < 45 Mins)"
newdf$type[newdf$More==1]="Large Delay (Delay > 45 Mins)"
delay <- newdf %>% 
  group_by(Marketing_Airline_Network,type) %>%
  dplyr::summarise(Count = n())
delay=data.frame(delay)
for (i in 1:11){
  delay$Percentage[((i-1)*3+1):((i-1)*3+3)]=delay$Count[((i-1)*3+1):((i-1)*3+3)]/sum(delay$Count[((i-1)*3+1):((i-1)*3+3)])
}
x=unique(delay$Marketing_Airline_Network)
x=x[order(x)]
delay$Marketing_Airline_Network=as.character(delay$Marketing_Airline_Network)
for (i in 1:11){
  delay$Marketing_Airline_Network[delay$Marketing_Airline_Network==as.character(x[i])]=as.character(Airlines$AIRLINE[i])}
## Count of flights of each delay class
ggplot(data=delay, aes(x=Marketing_Airline_Network,y=Count,fill=factor(type))) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Delay Class of Airlines")+
  theme(legend.title = element_text(color = "black"),
   legend.text = element_text(color = "black"))+
  scale_fill_discrete(name = "Delay Class")+
  labs(x="Airline",y="Count")

## Percentage of flights of each delay class
ggplot(data=delay, aes(x=Marketing_Airline_Network,y=Percentage,fill=factor(type))) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Delay Class of Airlines")+
  theme(legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black"))+
  scale_fill_discrete(name = "Delay Class")+
  labs(x="Airline",y="Percentage")





  