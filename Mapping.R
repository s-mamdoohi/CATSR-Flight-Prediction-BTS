rm(list=ls())
#Import and prepare the data
#set working directory
#setwd("C:/Mason/Course/DAEN 690/Mapping")
#read csv
df=read.csv("crs_arr.csv")
df$CRS_ARR_DATETIME_EST=as.POSIXlt(df$CRS_ARR_DATETIME_EST,tz="EST")
df$wday=df$CRS_ARR_DATETIME_EST$wday
df$wday=df$wday+1
df$day=wday(df$wday,label=T)
del=read.csv("act_arr.csv")

# Impoart data of airports
#read csv
airport=read.csv("Airport.csv")
airport=airport[airport[,5]=="USA",c(2,16,15)]
colnames(airport)=c("Code","lon","lat")
tags=c("LAX","JFK","ORD","DEN","IAH","PHX","SEA","DFW","CLT","BWI","DCA","IAD","MDW","EWR","FLL","SFO","ATL","DTW","HOU","MIA","MSP","SLC","BOS",
       "DAL","SNA","BUR","ONT","LGB","SJC","ANC")
airport$Code %in% tags


#import libraries for use
library(ggplot2)
library(dyplr)

#### plots the delays
class(del$ACT_ARR_DATETIME_EST)
del$ACT_ARR_DATETIME_EST=as.character(del$ACT_ARR_DATETIME_EST)
jan=del[grep("2019-01-", del$ACT_ARR_DATETIME_EST),]
jan$ACT_ARR_DATETIME_EST=as.POSIXlt(jan$ACT_ARR_DATETIME_EST,tz="EST")
jan$DAY_OF_MONTH=jan$ACT_ARR_DATETIME_EST$mday
m=jan %>%
  group_by(ACT_ARR_HOUR,DAY_OF_MONTH) %>%
  summarize(Del=sum(ArrDelayMinutes))
m=data.frame(m)
plot(m$ACT_ARR_HOUR,m$Del)
lines(RTT)
plot(m$ACT_ARR_HOUR,m$Del,type = 'l')
RTT=unique(predict(loess(Del ~ ACT_ARR_HOUR , data=m, span=.5)))


#hourly plots for Jan 18
library(RColorBrewer)
library(randomcoloR)
color=distinctColorPalette(31)
plot(x=c(0:23),y=m$Del[m$DAY_OF_MONTH==1],type='l',ylim=c(0,48000),xlab="Time of Day (EST)",lwd=1.5,ylab ="Arrival Delay (Minute)",col=color[1],xlim=c(0,28),
     main = "Hourly Delay vs Recurrent Delay Pattern (RDP)")
for (i in 2:31){
  lines(m$Del[m$DAY_OF_MONTH==i],x=c(0:23),col=color[i],lwd=1.5)
}
lines(RTT,col=2,lwd=4,x=c(0:23))
tag=c("1-Jan","2-Jan","3-Jan","4-Jan","5-Jan","6-Jan","7-Jan","8-Jan","9-Jan","10-Jan","11-Jan","12-Jan","13-Jan",
      "14-Jan","15-Jan","16-Jan","17-Jan","18-Jan","19-Jan","20-Jan","21-Jan","22-Jan","23-Jan","24-Jan","25-Jan","26-Jan",
      "27-Jan","28-Jan","29-Jan","30-Jan","31-Jan")

legend("topright",
       legend=tag,
       fill=color, cex=0.55, title="Date", horiz=F, inset=0)

legend("topright",
       legend="RDP",
       fill='red', cex=0.85, horiz=T, inset=c(0.105,0), lwd = 2)



#cumulative plots for Jan 18
plot(x=c(0:23),y=cumsum(m$Del[m$DAY_OF_MONTH==1]),type='l',ylim=c(0,600000),xlab="Time of Day (EST)",lwd=1.5,ylab="Cumulative Arrival Delay (Minute)",
     main = "Cumulative Delay vs Recurrent Cumulative Delay Pattern (RCDP)",xlim = c(0,28))
for (i in 2:31){
  lines(cumsum(m$Del[m$DAY_OF_MONTH==i]),col=color[i],lwd=1.5,x=c(0:23))
}
lines(cumsum(RTT),col=2,lwd=4,x=c(0:23))
tag=c("1-Jan","2-Jan","3-Jan","4-Jan","5-Jan","6-Jan","7-Jan","8-Jan","9-Jan","10-Jan","11-Jan","12-Jan","13-Jan",
      "14-Jan","15-Jan","16-Jan","17-Jan","18-Jan","19-Jan","20-Jan","21-Jan","22-Jan","23-Jan","24-Jan","25-Jan","26-Jan",
      "27-Jan","28-Jan","29-Jan","30-Jan","31-Jan")

legend("topright",
       legend=tag,
       fill=color, cex=0.55, title="Date", horiz=F, inset=0)

legend("topright",
       legend="RCDP",
       fill='red', cex=0.85, horiz=T, inset=c(0.105,0), lwd = 2)




library(magrittr)
library(dplyr)

############################
############################
############################

dt=df %>%
  group_by(Origin) %>%
  summarize(Del=mean(ArrDelayMinutes),count=n())
dt=data.frame(dt)
dt=dt[order(dt$Del,decreasing =T),]



hotspot=airport[airport$Code %in% tags,]
for (i in 1:nrow(hotspot)){
hotspot$Del[i]=dt$Del[as.character(dt$Origin)==as.character(hotspot$Code[i])]
}
hotspot=as_tibble(hotspot[,c(2,3,4,1)])



library(usmap)
library(ggplot2)
library(ggrepel)

## Plot delays of airports
plot_usmap(fill = "yellow", alpha = 0.25) +
  ggrepel::geom_label_repel(data = ap_transformed,
                            aes(x = lon.1, y = lat.1, label = Code),
                            size = 3, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "red", segment.size = 1,
                            seed = 1002) +
  geom_point(data = ap_transformed,
             aes(x = lon.1, y = lat.1, size = Del),
             color = "purple", alpha = 0.5) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(title = "Airports Average Arrival Delay in 2018 and 2019",
       subtitle = "Source: Bureau of Transportation Statistics, 2018 and 2019",
       size = "Avergae Arrival Delay (Minutes)") +
  theme(legend.position = "right")



###################
###heatmap of delays
###################
library(tidyverse)
library(lubridate)


dt2=df %>%
  group_by(day,CRS_ARR_HOUR) %>%
  summarize(Del=mean(ArrDelayMinutes),count=n())
dt2=data.frame(dt2)

dt2  %>%
  ggplot(aes(CRS_ARR_HOUR,day,fill=Del))+
  geom_tile()+
  labs(x="Hour of the day",y="Day of the week")+
  scale_fill_distiller(palette = "Spectral")


###############
##Flights map with their delays
###############

tagsnew=c("LAX","JFK","ORD","DEN","IAH","PHX","SEA","DFW","CLT","BWI","DCA","IAD","EWR","FLL","SFO","ATL","DTW","HOU","MIA","MSP","SLC","BOS",
       "DAL")
df2=df[df$Origin %in% tagsnew & df$Dest %in% tagsnew,]
hotspotnew=airport[airport$Code %in% tagsnew,]
dt3=df2 %>%
  group_by(Origin,Dest) %>%
  summarize(Del=mean(ArrDelayMinutes),count=n())
dt3=data.frame(dt3)

for (i in 1:nrow(dt3)){
  dt3$O.lon[i]=ap_transformed$lon[as.character(ap_transformed$Code)==as.character(dt3$Origin[i])]
  dt3$O.lat[i]=ap_transformed$lat[as.character(ap_transformed$Code)==as.character(dt3$Origin[i])]
  dt3$D.lon[i]=ap_transformed$lon[as.character(ap_transformed$Code)==as.character(dt3$Dest[i])]
  dt3$D.lat[i]=ap_transformed$lat[as.character(ap_transformed$Code)==as.character(dt3$Dest[i])]
}

tpick <- dt3 %>%
  select(lon = O.lon, lat = O.lat)
tdrop <- dt3 %>%
  select(lon = D.lon, lat = D.lat)

library(geosphere)
library(ggplot2)
library(ggrepel)
worldmap <- borders("usa", colour="#efede1", fill="#efede1") # create a layer of borders
ggplot() + worldmap + 
  geom_curve(data=dt3, aes(x = O.lon, y = O.lat, xend = D.lon, yend = D.lat), col = "#b29e7d", size = 1, curvature = .2) + 
  geom_point(data=hotspotnew, aes(x = lon, y = lat), col = "#970027") + 
  geom_text_repel(data=hotspotnew, aes(x = lon, y = lat, label = Code), col = "black", size = 2, segment.color = NA) + 
  theme(panel.background = element_rect(fill="white"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )


worldmap <- borders("usa", colour="#efede1", fill="#efede1") # create a layer of borders
ggplot() + worldmap + 
  geom_curve(data=dt3, aes(x = O.lon, y = O.lat, xend = D.lon, yend = D.lat, color=Del) ,size = .6, curvature = .2,
             arrow = arrow(length = unit(0.4,"cm"))) +
  #scale_color_gradient(low="lightgreen", high="red") +
  #scale_color_gradientn(colours = c(rainbow(2)[2],rainbow(2)[1]))+
  scale_color_gradientn(colours = rainbow(3,rev = T)) +
  
  geom_point(data=hotspotnew, aes(x = lon, y = lat), col = "#970027",size=2) + 
  geom_text_repel(data=hotspotnew, aes(x = lon, y = lat, label = Code), col = "black", size = 3, segment.color = NA) + 
  theme(panel.background = element_rect(fill="white"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )
