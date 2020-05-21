# Import the raw data
setwd("C:/Mason/Course/DAEN 690/Code/Modeling")
df=read.csv("act_arr.csv")
# add canceled flights
cancel=read.csv("cancellations_dep.csv")
mydata=df[,-1]
library(dplyr)

#Creating a required dataframe
mydata=mydata %>%
  group_by(ACT_ARR_DATE,ACT_ARR_HOUR) %>%
  summarize(Del=sum(ArrDelayMinutes),Count=n())
mydata=data.frame(mydata)
mydata=mydata[-c(1:5,17524:175346),]

#Adding binary variables of temporal features (e.g. Day of week)
mydata$Date=strptime(mydata$ACT_ARR_DATE,format='%Y-%m-%d')
mydata=mydata[order(mydata$Date,mydata$ACT_ARR_DATE),]
mydata$M[mydata$Date$wday==1]=1
mydata$T[mydata$Date$wday==2]=1
mydata$W[mydata$Date$wday==3]=1
mydata$R[mydata$Date$wday==4]=1
mydata$F[mydata$Date$wday==5]=1
mydata$ST[mydata$Date$wday==6]=1
mydata$SN[mydata$Date$wday==0]=1
mydata$Weekend[mydata$Date$wday==6|mydata$Date$wday==0]=1


# Listing the federal holdiays 
holidays=as.POSIXct(c("2017-12-25","2017-12-24","2017-12-31","2018-12-25","2018-12-24","2018-12-31",
                      "2019-12-25","2019-12-24","2019-12-31", "2018-01-01","2018-01-02","2019-01-01","2019-01-02",
                      "2018-01-15","2019-01-21","2018-02-19","2019-02-18","2018-05-28","2019-05-27","2018-07-04","2019-07-04","2018-09-03",
                      "2019-09-02","2018-10-08","2019-10-14","2018-11-12","2019-11-11","2018-11-21","2018-11-22","2018-11-23",
                      "2019-11-27","2019-11-28","2019-11-29"))

# Add a new variable (Holiday) to data
mydata$Holiday=as.character(mydata$Date) %in% as.character(holidays)*1

# Seasonal dummy varibales
mydata$Winter[mydata$Date$mon==11|mydata$Date$mon==0|mydata$Date$mon==1]=1
mydata$Spring[mydata$Date$mon==2|mydata$Date$mon==3|mydata$Date$mon==4]=1
mydata$Summer[mydata$Date$mon==5|mydata$Date$mon==6|mydata$Date$mon==7]=1
mydata$Fall[mydata$Date$mon==8|mydata$Date$mon==9|mydata$Date$mon==10]=1
mydata[is.na(mydata[,])]=0


#Adding canceled flights
cancel=cancel[-c(1659,10395),]
mydata$Cancel=cancel$cancellations

# Save the dataset
write.csv(mydata, file = "Processed_Data.csv", row.names = TRUE) 

