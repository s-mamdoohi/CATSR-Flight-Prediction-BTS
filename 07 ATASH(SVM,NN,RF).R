rm(list=ls())
# Importing the required datasets
#set working directory
#setwd("C:/Mason/Course/DAEN 690/Code/Modeling")
#read csv
m=read.csv("Processed_Data.csv")
# Adding Canceled Flights
m$Count=m$Cancel+m$Count



#Creating Modeling Matrix
#Hourly Matrix
hourly=matrix(data=NA,ncol=24,nrow=730)
colnames(hourly)=as.character(c(1:24))
for (i in c(1:69,71:433,435:730)){
  hourly[i,]=(m$Del[m$ACT_ARR_DATE ==unique(as.character(m$ACT_ARR_DATE ))[i]])}
hourly=data.frame(hourly)
hourly = hourly[complete.cases(hourly), ]

#Cumulative Matrix
cumul=matrix(data=NA,ncol=24,nrow=nrow(hourly))
colnames(cumul)=as.character(c(1:24))
for (i in 1:nrow(cumul)){
  cumul[i,]=cumsum(t(hourly[i,]))
}
cumul=data.frame(cumul)




### Flight frequency matrix
# Hourly
counthourly=matrix(data=NA,ncol=24,nrow=730)
for (i in c(1:69,71:433,435:730)){
  counthourly[i,]=m$Count[m$ACT_ARR_DATE==unique(as.character(m$ACT_ARR_DATE ))[i]]}
counthourly=data.frame(counthourly)
counthourly = counthourly[complete.cases(counthourly), ]
#Cumulative
countcum=matrix(data=NA,ncol=24,nrow=nrow(counthourly))
for (i in 1:nrow(countcum)){
  countcum[i,]=cumsum(t(counthourly[i,]))
}
countcum=data.frame(countcum)

### Canceled Flight frequency matrix
# Hourly
cancelhourly=matrix(data=NA,ncol=24,nrow=730)
for (i in c(1:69,71:433,435:730)){
  cancelhourly[i,]=m$Cancel[m$ACT_ARR_DATE==unique(as.character(m$ACT_ARR_DATE ))[i]]}
cancelhourly=data.frame(cancelhourly)
cancelhourly = cancelhourly[complete.cases(cancelhourly), ]
#Cumulative
cancelcum=matrix(data=NA,ncol=24,nrow=nrow(cancelhourly))
for (i in 1:nrow(cancelcum)){
  cancelcum[i,]=cumsum(t(cancelhourly[i,]))
}
cancelcum=data.frame(cancelcum)
###
#Cancelation rate
cancelhourlyper=cancelhourly/counthourly
cancelcumper=cancelcum/countcum



# Adding Temporal Dummy Variables
#import libraries
library(dplyr)
n=m %>%
  group_by(ACT_ARR_DATE) %>%
  summarise(M=unique(M),T=unique(T),W=unique(W),R=unique(R),F=unique(F),ST=unique(ST),SN=unique(SN),Weekend=unique(Weekend),
            Holiday=unique(Holiday),Winter=unique(Winter),Spring=unique(Spring), Summer=unique(Summer),Fall=unique(Fall))
n=data.frame(n)
n=n[-c(70,434),]


# Combining Final Data with Temporal Dummy Variable (Both Hourly and Cumulative)
#No 70 and 434 are having problem (missed values)
final=cbind(n[,1],hourly,n[,-1])
finalcum=cbind(n[,1],cumul,n[,-1])
finalcum$C6=cancelcumper$X6
finalcum$C12=cancelcumper$X12
finalcum$C18=cancelcumper$X18
finalcum$C24=cancelcumper$X24
colnames(final)[1]=c("Date")
colnames(finalcum)[1]=c("Date")


##########################
#######DATA SPLITTING#####
##########################
train=final[1:364,]
val=final[365:728,]
traincum=finalcum[1:364,]
valcum=finalcum[365:728,]


###### Sequntial Regression Models#####
#############################

#### SVR (creating 12 models for 12 sequences of apprach ATASH)
library(e1071)
n1=svm(X13~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n2=svm(X14~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n3=svm(X15~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n4=svm(X16~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n5=svm(X17~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n6=svm(X18~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n7=svm(X19~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n8=svm(X20~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n9=svm(X21~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n10=svm(X22~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+X21+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n11=svm(X23~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+X21+X22+Holiday+Winter+Spring+Summer+Fall,data=traincum)
n12=svm(X24~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+X21+X22+X23+Holiday+Winter+Spring+Summer+Fall,data=traincum)


N=4
#testing the result
test=valcum[,-c(14:25,39)]
test['X13']=predict(n1,test)
test['X14']=predict(n2,test)
test['X15']=predict(n3,test)
test['X16']=predict(n4,test)
test['X17']=predict(n5,test)
test['X18']=predict(n6,test)
test['X19']=predict(n7,test)
test['X20']=predict(n8,test)
test['X21']=predict(n9,test)
test['X22']=predict(n10,test)
test['X23']=predict(n11,test)
test['X24']=predict(n12,test)

# Ploting the results
time0=as.POSIXct("2019-01-01 01:00:00")
time24=as.POSIXct("2019-01-02 00:00:00")
xaxis=seq(time0,time24,3600)
xaxis=strptime(xaxis,format='%Y-%m-%d  %H:%M:%S')

plot(y=valcum[N,2:25],x=xaxis,type='l',xlab="Time of Day",lwd=2,ylab="Cumulative Delay (Minute)",
     main = "Predicted vs Actual Cumulative Delay",col=4, ylim=c(0,max(valcum[N,2:25],test[N,c(13,30:41)])) )
lines(test[N,c(13,30:41)],col=2,x=xaxis[12:24],type='b' ,lwd=2)
legend("topleft",
       legend=c("Actual Cumulative Delay","Predicted Cumulative Delay"),
       fill=c(4,2), cex=0.85, horiz=F, inset=0, lwd = 1,bty='n')


### Computing RMSE
RMSE=sqrt(sum((valcum[,c(14:25)]-test[,c(30:41)])^2)/(nrow(test)*12))
RMSE



############ Neural Network ##########
#scaling

max=apply(finalcum[,-c(1)],2,max)
min=apply(finalcum[,-c(1)],2,min)

Df=as.data.frame(scale(finalcum[,-c(1)],center = min,scale=max-min))
#data spliting
trainDf=Df[1:346,]
testDf=Df[365:728,]

# Modeling (12 sequences like SVR)
set.seed(1234)
library(neuralnet)
n1=neuralnet(X13~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(8,4,2))
n2=neuralnet(X14~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(9,5,2))
n3=neuralnet(X15~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(10,6,3))
n4=neuralnet(X16~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(11,7,4))
n5=neuralnet(X17~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(12,8,4))
n6=neuralnet(X18~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(13,9,4))
n7=neuralnet(X19~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(14,10,5))
n8=neuralnet(X20~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(15,11,5))
n9=neuralnet(X21~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(16,12,5))
n10=neuralnet(X22~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+X21+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(17,13,6))
n11=neuralnet(X23~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+X21+X22+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(18,14,6))
n12=neuralnet(X24~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+X21+X22+X23+Holiday+Winter+Spring+Summer+Fall,data=trainDf,linear.output = T,hidden = c(19,15,6))

#Visualizing the NN
library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(n12)


#Test the results
test=testDf[,-c(13:24)]
test['X13']=predict(n1,test)
test['X14']=predict(n2,test)
test['X15']=predict(n3,test)
test['X16']=predict(n4,test)
test['X17']=predict(n5,test)
test['X18']=predict(n6,test)
test['X19']=predict(n7,test)
test['X20']=predict(n8,test)
test['X21']=predict(n9,test)
test['X22']=predict(n10,test)
test['X23']=predict(n11,test)
test['X24']=predict(n12,test)

#re-scaling
test$X12=(test$X12)*(max(finalcum$X12)-min(finalcum$X12))+min(finalcum$X12)
test$X13=(test$X13)*(max(finalcum$X13)-min(finalcum$X13))+min(finalcum$X13)
test$X14=(test$X14)*(max(finalcum$X14)-min(finalcum$X14))+min(finalcum$X14)
test$X15=(test$X15)*(max(finalcum$X15)-min(finalcum$X15))+min(finalcum$X15)
test$X16=(test$X16)*(max(finalcum$X16)-min(finalcum$X16))+min(finalcum$X16)
test$X17=(test$X17)*(max(finalcum$X17)-min(finalcum$X17))+min(finalcum$X17)
test$X18=(test$X18)*(max(finalcum$X18)-min(finalcum$X18))+min(finalcum$X18)
test$X19=(test$X19)*(max(finalcum$X19)-min(finalcum$X19))+min(finalcum$X19)
test$X20=(test$X20)*(max(finalcum$X20)-min(finalcum$X20))+min(finalcum$X20)
test$X21=(test$X21)*(max(finalcum$X21)-min(finalcum$X21))+min(finalcum$X21)
test$X22=(test$X22)*(max(finalcum$X22)-min(finalcum$X22))+min(finalcum$X22)
test$X23=(test$X23)*(max(finalcum$X23)-min(finalcum$X23))+min(finalcum$X23)
test$X24=(test$X24)*(max(finalcum$X24)-min(finalcum$X24))+min(finalcum$X24)



#Ploting results
N=60 #1 March
plot(y=valcum[N,2:25],x=xaxis,type='l',xlab="Time of Day",lwd=2,ylab="Cumulative Delay (Minute)",col=4, ylim=c(0,max(valcum[N,2:25],test[N,c(12,30:41)])))
lines(test[N,c(12,30:41)],col=2,x=xaxis[12:24],type='b' ,lwd=2, bty='n')
legend("topleft",
       legend=c("Actual Cumulative Delay","Predicted Cumulative Delay"),
       fill=c(4,2), cex=0.85, horiz=F, inset=0, lwd = 1)

###RMSE
RMSE=sqrt(sum((valcum[,c(14:25)]-test[,c(30:41)])^2)/(nrow(test)*12))
RMSE

##### Random Forest
library(randomForest)
set.seed(1234)
n1=randomForest(X13~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n2=randomForest(X14~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n3=randomForest(X15~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n4=randomForest(X16~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n5=randomForest(X17~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n6=randomForest(X18~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n7=randomForest(X19~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n8=randomForest(X20~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n9=randomForest(X21~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n10=randomForest(X22~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+X21+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n11=randomForest(X23~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+X21+X22+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)
n12=randomForest(X24~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X15+X15+X16+X17+X18+X19+X20+X21+X22+X23+Holiday+Winter+Spring+Summer+Fall,data=traincum,ntree=200)



N=4
#test
test=valcum[,-c(14:25,39)]
test['X13']=predict(n1,test)
test['X14']=predict(n2,test)
test['X15']=predict(n3,test)
test['X16']=predict(n4,test)
test['X17']=predict(n5,test)
test['X18']=predict(n6,test)
test['X19']=predict(n7,test)
test['X20']=predict(n8,test)
test['X21']=predict(n9,test)
test['X22']=predict(n10,test)
test['X23']=predict(n11,test)
test['X24']=predict(n12,test)

#Plot the results
plot(y=valcum[N,2:25],x=xaxis,type='l',xlab="Time of Day",lwd=2,ylab="Cumulative Delay (Minute)",
     main = "Predicted vs Actual Cumulative Delay",col=4, ylim=c(0,max(valcum[N,2:25],test[N,c(13,30:41)])) )
lines(test[N,c(13,30:41)],col=2,x=xaxis[12:24],type='b' ,lwd=2)
legend("topleft",
       legend=c("Actual Cumulative Delay","Predicted Cumulative Delay"),
       fill=c(4,2), cex=0.85, horiz=F, inset=0, lwd = 1,bty='n')


### RMSE
RMSE=sqrt(sum((valcum[,c(14:25)]-test[,c(30:41)])^2)/(nrow(test)*12))
RMSE






########################
####CATEGORICAL#########
########################
########################
############################
### Delay severity calsses##
############################
str(finalcum)
finalcum$M=as.factor(finalcum$M)
finalcum$T=as.factor(finalcum$T)
finalcum$W=as.factor(finalcum$W)
finalcum$R=as.factor(finalcum$R)
finalcum$F=as.factor(finalcum$F)
finalcum$ST=as.factor(finalcum$ST)
finalcum$SN=as.factor(finalcum$SN)
finalcum$Weekend=as.factor(finalcum$Weekend)
finalcum$Holiday=as.factor(finalcum$Holiday)
finalcum$Winter=as.factor(finalcum$Winter)
finalcum$Spring=as.factor(finalcum$Spring)
finalcum$Summer=as.factor(finalcum$Summer)
finalcum$Fall=as.factor(finalcum$Fall)

#Develop histograms of end-of-the-day delays (5 classes)
# Add cut off values to the histogram
X24=finalcum$X24/1000
library(fitdistrplus)
fit.gamma <- fitdist(X24, distr = "gamma",method = "mle")
par=fit.gamma$estimate
summary(fit.gamma)
xfit<-seq(min(X24),max(X24),length=50)
yfit=dgamma(xfit,shape=par[1],rate=par[2])
#yfit=yfit*30000*length(X24)
h<-hist(finalcum$X24, breaks=30, col="lightblue", xlab="Cumulative Delay")
        #main="Histogram with Gamma Curve (Shape= 6.255, Rate = 0.0213) "
yhat <- yfit*diff(h$mids[1:2])*length(finalcum$X24)
lines(xfit*1000, yhat/1000, col="red", lwd=2)
abline(v = c(1.15*mean(finalcum$X24),1.85*mean(finalcum$X24),0.65*mean(finalcum$X24),1.5*mean(finalcum$X24)),
       ,col=c("blue","darkgreen","darkorange1","purple"), lwd=3, lty=2)
legend("right",
       legend=c("Gamma Distribution","0.65 Mean","1.15 Mean","1.50 Mean","1.85 Mean"),
       fill=c("red","darkorange1","blue","purple","darkgreen"), cex=.8, title="Threshold", horiz=F,xpd=T,
       lty=c(1,2,2,2,2), lwd=2,border = NA,col=c("red","darkorange1","blue","purple","darkgreen"), inset = c(-0.03,0))#,bty = "n")

##histograms of end-of-the-day delays (4 classes)
h<-hist(finalcum$X24, breaks=30, col="lightblue", xlab="Cumulative Delay")
#main="Histogram with Gamma Curve (Shape= 6.255, Rate = 0.0213) "
yhat <- yfit*diff(h$mids[1:2])*length(finalcum$X24)
lines(xfit*1000, yhat/1000, col="red", lwd=2)
abline(v = c(1.15*mean(finalcum$X24),1.85*mean(finalcum$X24),0.65*mean(finalcum$X24)),
       col=c("blue","darkgreen","darkorange1"), lwd=3, lty=2)
legend("right",
       legend=c("Gamma Distribution","0.65 Mean","1.15 Mean","1.85 Mean"),
       fill=c("red","darkorange1","blue","darkgreen"), cex=.8, title="Threshold", horiz=F,xpd=T,
       lty=c(1,2,2,2), lwd=2,border = NA,col=c("red","darkorange1","blue","darkgreen"), inset = c(-0.03,0))#,bty = "n")



##histograms of end-of-the-day delays (3 classes)
h<-hist(finalcum$X24, breaks=30, col="lightblue", xlab="Cumulative Delay")
#main="Histogram with Gamma Curve (Shape= 6.255, Rate = 0.0213) "
yhat <- yfit*diff(h$mids[1:2])*length(finalcum$X24)
lines(xfit*1000, yhat/1000, col="red", lwd=2)
abline(v = c(1.15*mean(finalcum$X24),1.85*mean(finalcum$X24)),
       col=c("blue","darkgreen"), lwd=3, lty=2)
legend("right",
       legend=c("Gamma Distribution","1.15 Mean","1.85 Mean"),
       fill=c("red","blue","darkgreen"), cex=.8, title="Threshold", horiz=F,xpd=T,
       lty=c(1,2,2), lwd=2,border = NA,col=c("red","blue","darkgreen"), inset = c(-0.03,0))#,bty = "n")





#Classify Based on 4 Classes
final4=finalcum
final4$Class[final4$X24<0.65*mean(final4$X24)]="Good"
final4$Class[final4$X24<1.15*mean(final4$X24)& final4$X24>0.65*mean(final4$X24)]="Normal"
final4$Class[final4$X24>1.15*mean(final4$X24) & final4$X24<1.85*mean(final4$X24)]="Semi-Normal"
final4$Class[final4$X24>1.85*mean(final4$X24)]="Abnormal"
table(final4$Class)
table(final4$Class)/sum(table(final4$Class))*100


#Classify Based on 3 Classes
final3=finalcum
final3$Class[final3$X24<1.15*mean(final3$X24)]="Normal"
final3$Class[final3$X24>1.15*mean(final3$X24) & final3$X24<1.85*mean(final3$X24)]="Semi-Normal"
final3$Class[final3$X24>1.85*mean(final3$X24)]="Abnormal"
table(final3$Class)
table(final3$Class)/sum(table(final3$Class))*100



#Classify Based on 5 Classes
final5=finalcum
final5$Class[final5$X24<0.65*mean(final5$X24)]="Good"
final5$Class[final5$X24<1.15*mean(final5$X24)& final5$X24>0.65*mean(final5$X24)]="Normal"
final5$Class[final5$X24>1.15*mean(final5$X24) & final5$X24<1.5*mean(final5$X24)]="Semi-Normal"
final5$Class[final5$X24>1.5*mean(final5$X24) & final5$X24<1.85*mean(final5$X24)]="Semi-Abnormal"
final5$Class[final5$X24>1.85*mean(final5$X24)]="Abnormal"
table(final5$Class)
table(final5$Class)/sum(table(final5$Class))*100





######Visualizing the classes
#3Class
library(ggplot2)
library(viridis)
final3$Class <- factor(final3$Class, levels = c("Normal", "Semi-Normal", "Abnormal"))
ggplot(final3,aes(x=X24,fill=Class))+
  geom_histogram(aes(y = ..count..), bins = 30, alpha=1)+
  #scale_fill_manual(values=rev(rainbow(3,start=0,end=0.35)))+
  #scale_fill_manual(values=c(hue_pal()(3)[c(2,3,1)]))+
  scale_fill_viridis(discrete=TRUE)+#,option = "magma")
  geom_rug(aes(col=Class),sides="t",alpha=1,length = unit(0.02, "npc"),size=.5)+
  labs(x="Cumulative Delay", y="Frequency")+
  scale_color_viridis(discrete=TRUE)+
  theme(legend.position = c(0.85, 0.5),legend.text=element_text(size=15),legend.title=element_text(size=12))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

#4Class
library(ggplot2)
library(viridis)
final4$Class <- factor(final4$Class, levels = c("Good","Normal", "Semi-Normal", "Abnormal"))
ggplot(final4,aes(x=X24,fill=Class))+
  geom_histogram(aes(y = ..count..), bins = 30, position = "stack")+
  scale_fill_viridis(discrete=TRUE)+
  geom_rug(aes(col=Class),sides="t",alpha=1,length = unit(0.02, "npc"),size=.5)+
  labs(x="Cumulative Delay", y="Frequency")+
  scale_color_viridis(discrete=TRUE)+
  theme(legend.position = c(0.85, 0.5),legend.text=element_text(size=15),legend.title=element_text(size=12))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
#5Class
library(ggplot2)
library(viridis)
final5$Class <- factor(final5$Class, levels = c("Good","Normal", "Semi-Normal","Semi-Abnormal", "Abnormal"))
ggplot(final5,aes(x=X24,fill=Class))+
  geom_histogram(aes(y = ..count..), bins = 30)+
  scale_fill_viridis(discrete=TRUE)+
  geom_rug(aes(col=Class),sides="t",alpha=1,length = unit(0.02, "npc"),size=.5)+
  labs(x="Cumulative Delay", y="Frequency")+
  scale_color_viridis(discrete=TRUE)+
  theme(legend.position = c(0.825, 0.5),legend.text=element_text(size=15),legend.title=element_text(size=12))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))



################## DATA Balancing
train3=final3[1:364,]
val3=final3[365:728,]
train4=final4[1:364,]
val4=final4[365:728,]
train5=final5[1:364,]
val5=final5[365:728,]
library(DMwR)

table(val3$Class)
table(val3$Class)/sum(table(val3$Class))
### Balancing the data with 3 Classs
set.seed(264)
table(train3$Class)
Ab <- SMOTE(Class ~ . , train3, perc.over = 2100, k=5, perc.under=100)
table(Ab$Class)
#per.over: new number for minority category= (perc.over/100+1)*old number of minority, new majority=perc.over/100*old number of minority*perc.under/100+ old minority
Semi=rbind(Ab[Ab$Class=="Abnormal",],train3[train3$Class!="Abnormal",])
Semi$Class=as.character(Semi$Class)
Semi$Class=as.factor(Semi$Class)
table(Semi$Class)
NewSemi <- SMOTE(Class ~ . , Semi, perc.over =300, k=5, perc.under=100)
table(NewSemi$Class)
Bal.train3=rbind(train3[train3$Class=="Normal",],Ab[Ab$Class=="Abnormal",],NewSemi[NewSemi$Class=="Semi-Normal",])
class(Bal.train3$Class)
table(Bal.train3$Class)
write.csv(Bal.train3, file = "Bal.train3.csv", row.names = TRUE)

### With 4 Class
table(train4$Class)
Ab <- SMOTE(Class ~ . , train4, perc.over = 1500, k=5, perc.under=100)
table(Ab$Class)
#per.over: new number for minority category= (perc.over/100+1)*old number of minority, new majority=perc.over/100*old number of minority*perc.under/100+ old minority
Semi=train4[train4$Class!="Abnormal" & train4$Class!="Good",]
Semi$Class=as.character(Semi$Class)
Semi$Class=as.factor(Semi$Class)
table(Semi$Class)
NewSemi <- SMOTE(Class ~ . , Semi, perc.over =170, k=5, perc.under=250)
table(NewSemi$Class)

Good=train4[train4$Class!="Abnormal" & train4$Class!="Semi-Normal",]
Good$Class=as.character(Good$Class)
Good$Class=as.factor(Good$Class)
table(Good$Class)
NewGood <- SMOTE(Class ~ . , Good, perc.over =150, k=5, perc.under=100)
table(NewGood$Class)
Bal.train4=rbind(train4[train4$Class=="Normal",],Ab[Ab$Class=="Abnormal",],NewSemi[NewSemi$Class=="Semi-Normal",],NewGood[NewGood$Class=="Good",])
class(Bal.train4$Class)
table(Bal.train4$Class)



### With 5 Classe
table(train5$Class)
Ab <- SMOTE(Class ~ . , train5, perc.over = 1500, k=5, perc.under=100)
table(Ab$Class)
#per.over: new number for minority category= (perc.over/100+1)*old number of minority, new majority=perc.over/100*old number of minority*perc.under/100+ old minority
SemiAb=train5[train5$Class!="Abnormal" & train5$Class!="Good" & train5$Class!="Semi-Normal",]
SemiAb$Class=as.character(SemiAb$Class)
SemiAb$Class=as.factor(SemiAb$Class)
table(SemiAb$Class)
NewSemiAb <- SMOTE(Class ~ . , SemiAb, perc.over =500, k=5, perc.under=250)
table(NewSemiAb$Class)
SemiNo=train5[train5$Class!="Abnormal" & train5$Class!="Good" & train5$Class!="Semi-Abnormal",]
SemiNo$Class=as.character(SemiNo$Class)
SemiNo$Class=as.factor(SemiNo$Class)
table(SemiNo$Class)
NewSemiNo <- SMOTE(Class ~ . , SemiNo, perc.over =300, k=5, perc.under=250)
table(NewSemiNo$Class)
Good=train5[train5$Class!="Abnormal" & train5$Class!="Semi-Normal"& train5$Class!="Semi-Abnormal",]
Good$Class=as.character(Good$Class)
Good$Class=as.factor(Good$Class)
table(Good$Class)
NewGood <- SMOTE(Class ~ . , Good, perc.over =150, k=5, perc.under=100)
table(NewGood$Class)
Bal.train5=rbind(train5[train5$Class=="Normal",],Ab[Ab$Class=="Abnormal",],NewSemiNo[NewSemiNo$Class=="Semi-Normal",],NewGood[NewGood$Class=="Good",],
                 NewSemiAb[NewSemiAb$Class=="Semi-Abnormal",])
table(Bal.train5$Class)


#Saving the actual classes
actual3 <- as.factor(val3$Class)
actual4 <- as.factor(val4$Class)
actual5 <- as.factor(val5$Class)



#Modeling
final3$Class <- factor(final3$Class, levels = c("Normal", "Semi-Normal", "Abnormal"))
train3=final3[1:364,]
val3=final3[365:728,]
val3$Class=as.factor(val3$Class)
actual3 <- as.factor(val3$Class)
## Importing and preparing the data for modeling
Bal.train3=read.csv("Bal.train3.csv")
Bal.train3$Class=as.factor(Bal.train3$Class)
Bal.train3$Class <- factor(Bal.train3$Class, levels = c("Normal", "Semi-Normal", "Abnormal"))
Bal.train3=Bal.train3[,-1]
Bal.train3$M=as.factor(Bal.train3$M)
Bal.train3$T=as.factor(Bal.train3$T)
Bal.train3$W=as.factor(Bal.train3$W)
Bal.train3$R=as.factor(Bal.train3$R)
Bal.train3$F=as.factor(Bal.train3$F)
Bal.train3$ST=as.factor(Bal.train3$ST)
Bal.train3$SN=as.factor(Bal.train3$SN)
Bal.train3$Weekend=as.factor(Bal.train3$Weekend)
Bal.train3$Holiday=as.factor(Bal.train3$Holiday)
Bal.train3$Winter=as.factor(Bal.train3$Winter)
Bal.train3$Spring=as.factor(Bal.train3$Spring)
Bal.train3$Summer=as.factor(Bal.train3$Summer)
Bal.train3$Fall=as.factor(Bal.train3$Fall)
Bal.train3=data.frame(Bal.train3)




library(caret)
library(e1071)
library(randomForest)
library(neuralnet)
set.seed(1234)
#SVM
svm.cat=svm(Class~C18+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+Holiday+Winter+Spring+Summer+Fall,data=Bal.train3,kernel="linear")
p1=unname(predict(svm.cat,val3))
confusionMatrix(p1,actual3)

#RF
set.seed(1234)
rf.cat=randomForest(Class~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+Holiday+Winter+Spring+Summer+Fall,data=Bal.train3, ntree=250,mtry=12)
p2=unname(predict(rf.cat,val3))
confusionMatrix(p2,actual3)



# Saving missclassified days
har=data.frame(cbind(p1,actual3))
missdays=factor(val3[har$p1==3 & har$actual3==2,1])
missdays2=factor(val3[har$p1==2 & har$actual3==3,1])
write.csv(missdays, file = "miss(semi-ab).csv", row.names = TRUE) ### seminomal days misclassed as abnormal
write.csv(missdays2, file = "miss(ab-semi).csv", row.names = TRUE) ### abnorma days misclassed as seminormal


# Saving a matrix consisting the delay profiles, acutual class of delay, and predicted delay
shekl=cbind(val3[,1:25],p1,actual3)
shekl$p1=as.numeric(shekl$p1)
shekl$actual3=as.numeric(shekl$actual3)
shekl$missed=0
shekl$missed[shekl$Date %in%missdays]=1
shekl$rang=c("#E8E8E8")
shekl$rang[shekl$missed==1]=c("#69b3a2")

## Plot the misslcassified days
time0=as.POSIXct("2019-01-01 01:00:00")
time24=as.POSIXct("2019-01-02 00:00:00")
xaxis=seq(time0,time24,3600)
xaxis=strptime(xaxis,format='%Y-%m-%d  %H:%M:%S')
#par(xpd = T, mar = par()$mar + c(0,0,0,0))
plot(x=xaxis,y=shekl[1,2:25],type='l',ylim=c(0,800000),xlab="Time of Day",lwd=1,
     ylab ="Hourly Delay (Minute)",col=shekl$rang[1],main = "Misclasiffied Days Plot")
for (i in 2:364){
  lines(y=shekl[i,2:25],x=xaxis,col=shekl$rang[i],lwd=(0.5+shekl$missed[i]*1.1))
}
abline(h=1.85*mean(finalcum$X24),col="slateblue4",lty=2)
abline(h=1.15*mean(finalcum$X24),col="slateblue4",lty=2)
abno=shekl[shekl$actual3==3,]
for (i in 1:nrow(abno)){
  lines(y=abno[i,2:25],x=xaxis,col="lightcoral",lwd=1.5)
}
text(10, 1.95*mean(finalcum$X24), "Abnormality Threshold", col = 1)
text(10, 1.05*mean(finalcum$X24), "Normality Threshold", col = 1)
tag=c("MisDays (Semi/Abnormal)","Abnormal Days","Other Days", "Class Thresholds")
legend("topleft",
       legend=tag,
       fill=c("#69b3a2","lightcoral","#E8E8E8","slateblue4"), cex=.85, horiz=F, inset=c(0,0),xpd=T, lty=c(1,1,1,2),
       border = NA ,col=c("#69b3a2","lightcoral","#E8E8E8","slateblue4"),bty = "n")
#par(mar=c(5, 4, 4, 2) + 0.1)




# Saving a matrix consisting the delay profiles, acutual class of delay, and predicted delay
shekl=cbind(val3[,1:25],p1,actual3)
shekl$p1=as.numeric(shekl$p1)
shekl$actual3=as.numeric(shekl$actual3)
shekl$missed=0
shekl$missed[shekl$Date %in%missdays2]=1
shekl$rang=c("#E8E8E8")
shekl$rang[shekl$missed==1]=c("#69b3a2")


## Plot the other type of misslcassified days
time0=as.POSIXct("2019-01-01 01:00:00")
time24=as.POSIXct("2019-01-02 00:00:00")
xaxis=seq(time0,time24,3600)
xaxis=strptime(xaxis,format='%Y-%m-%d  %H:%M:%S')
#par(xpd = T, mar = par()$mar + c(0,0,0,0))
plot(x=xaxis,y=shekl[1,2:25],type='l',ylim=c(0,800000),xlab="Time of Day",lwd=1,
     ylab ="Hourly Delay (Minute)",col=shekl$rang[1],main = "Misclasiffied Days Plot")
for (i in 2:364){
  lines(y=shekl[i,2:25],x=xaxis,col="#E8E8E8",lwd=(0.5+shekl$missed[i]*1))
}
abline(h=1.85*mean(finalcum$X24),col="slateblue4",lty=2)
abline(h=1.15*mean(finalcum$X24),col="slateblue4",lty=2)
abno=shekl[shekl$actual3==2,]
for (i in 1:nrow(abno)){
  lines(y=abno[i,2:25],x=xaxis,col="#69b3a2",lwd=.3)
}
veleshkon=shekl[shekl$missed==1,2:25]
for (i in 1:nrow(veleshkon)){
  lines(y=veleshkon[i,1:24],x=xaxis,col="red",lwd=2)
}
text(10, 1.95*mean(finalcum$X24), "Abnormality Threshold", col = 1)
text(10, 1.05*mean(finalcum$X24), "Normality Threshold", col = 1)
tag=c("MisDays (Semi/Abnormal)","Abnormal Days","Other Days", "Class Thresholds")
legend("topleft",
       legend=tag,
       fill=c("#69b3a2","lightcoral","#E8E8E8","slateblue4"), cex=.85, horiz=F, inset=c(0,0),xpd=T, lty=c(1,1,1,2),
       border = NA ,col=c("#69b3a2","lightcoral","#E8E8E8","slateblue4"),bty = "n")
#par(mar=c(5, 4, 4, 2) + 0.1)






# Saving a matrix consisting the delay profiles, acutual class of delay, and predicted delay
shekl=cbind(val3[,1:25],p1,actual3)
shekl$p1=as.numeric(shekl$p1)
shekl$actual3=as.numeric(shekl$actual3)
shekl$missed=0
shekl$missed[shekl$Date %in%missdays2]=1
shekl$rang=c("#E8E8E8")
shekl$rang[shekl$missed==1]=c("#69b3a2")
rain=rainbow(3,start=0,end=0.25,rev=T)

######Visualization of classification results
#par(xpd = T, mar = par()$mar + c(0,0,0,0))
plot(x=xaxis,y=shekl[1,2:25],type='l',ylim=c(0,800000),xlab="Time of Day",lwd=1,
     ylab ="Hourly Delay (Minute)",col=rain[shekl$p1[1]],main = "Misclasiffied Days Plot")
for (i in 2:364){
  lines(y=shekl[i,2:25],x=xaxis,col=rain[shekl$p1[i]])
}
abline(h=1.85*mean(finalcum$X24),col="slateblue4",lty=2)
abline(h=1.15*mean(finalcum$X24),col="slateblue4",lty=2)



### Create a dataframe of misclassified days and cancelation ratios associated with that days
nemoodar=valcum[,c(1,42)]
nemoodar$missed=0
nemoodar$missed[nemoodar$Date %in%missdays]=1
nemoodar$missed[nemoodar$Date %in%missdays2]=2
nemoodar$missed=factor(nemoodar$missed)
nemoodar$Date=as.POSIXct(nemoodar$Date)

### Plot misclassified days and cancelation ratios associated with that days
ggplot(data=nemoodar, aes(x=Date, y=C24,fill=missed)) +
  geom_bar(stat="identity", color="gray")+
  theme_minimal()+
  scale_fill_manual(values = c("white","red3","darkgreen"))+
  theme_minimal()+
  geom_hline(yintercept=mean(nemoodar$C24[nemoodar$missed==0]), linetype="dashed", color = "darkgray")+
  geom_hline(yintercept=mean(nemoodar$C24[nemoodar$missed==1]), linetype="dashed", color = "red3")+
  geom_hline(yintercept=mean(nemoodar$C24[nemoodar$missed==2]), linetype="dashed", color = "darkgreen")+
  ggpubr::rotate_x_text()

## If the mean of cancelation ratio of misclassified days are equla with other days
t.test(nemoodar$C24[nemoodar$missed!=2],nemoodar$C24[nemoodar$missed==2])



##########Equity Index (EI) Analytics
#############
#############
#############
#Importing and preparing
#read csv
EQ=read.csv("EQ.csv")
EQ=EQ[,-1]
colnames(EQ)=c("Airline","Date","Noon","Midnight")
EQ$Ratio=EQ$Midnight/EQ$Noon
EQ$Increased=0
EQ$Increased[EQ$Ratio>1]=1
EQ$Increased=as.factor(EQ$Increased)
table(EQ$Airline)
for (i in 1:nrow(final3)){
EQ$Class[as.character(EQ$Date)==as.character(final3$Date[i])]=as.character(final3$Class[i])}
EQ = EQ[complete.cases(EQ), ]
EQ$Class=as.factor(EQ$Class)
EQ$misclass=0
EQ$misclass[as.character(EQ$Date)%in% as.character(missdays2)]=1
EQ$misclass=as.factor(EQ$misclass)



### Plot EI of all Ailines
ggplot(data=EQ, aes(x=Noon,y=Midnight))+
  geom_point(aes(colour=Airline))+
  xlim(0, max(EQ$Noon)) + ylim(0, max(EQ$Midnight))+
  coord_fixed(ratio = 1, expand = T, clip = "on")

ggplot(data=EQ[EQ$misclass==1,], aes(x=Noon,y=Midnight))+
  geom_point(aes(colour=Airline),size=1.5)+
  coord_fixed(ratio = 1, expand = T, clip = "on")+
  geom_abline(intercept = 0)
  



library(viridis)
### Plot EI of Specific Airlines (Southwest) based on the class of delays
ggplot(data=EQ[EQ$Airline=="WN",], aes(x=Noon,y=Midnight))+
  geom_point(aes(colour=Class))+
  geom_abline(intercept = 0)+
  scale_color_viridis(discrete=TRUE)+
  #scale_color_manual(values=rainbow(3,start=0,end=0.25, rev=T))+
  coord_fixed(ratio = 1, expand = T, clip = "on")+
  labs(title="WN")+
  xlim(0, max(EQ$Noon[EQ$Airline=="WN"])) + ylim(0, max(EQ$Midnight[EQ$Airline=="WN"])) 

##Plot EI of Specific Airlines (Southwest) based on if their EI ratio is greater than 1 or not
ggplot(data=EQ[EQ$Airline=="WN",], aes(x=Noon,y=Midnight))+
  geom_point(aes(colour=Increased))+
  geom_abline(intercept = 0)+
  #scale_color_viridis(discrete=TRUE)+
  scale_color_manual(values=c("blue","red"))+
  coord_fixed(ratio = 1, expand = T, clip = "on")+
  labs(title="WN")+
  xlim(0, max(EQ$Noon[EQ$Airline=="WN"])) + ylim(0, max(EQ$Midnight[EQ$Airline=="WN"])) 



#### Finding correlation of EI with delay class
EQ.m=EQ[EQ$Airline=="AA"|EQ$Airline=="UA"|EQ$Airline=="DL"|EQ$Airline=="WN",c(1,2,3,7)]
library(reshape2)
EQ.wide <- dcast(EQ.m, Date+Class ~Airline, value.var="Noon")




library(hrbrthemes)
library(GGally)
library(viridis)
## Plot parallel coordinate to check if there is a pattern
ggparcoord(EQ.wide,
           columns = 3:6, groupColumn = 2, order = "anyClass",
           scale="uniminmax",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for Equity Index",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )




########Check the correlation of EI and delay classes
cor=EQ.wide[,-1]
cor$Normal[cor$Class=="Normal"]=1
cor$'Semi-Normal'[cor$Class=="Semi-Normal"]=1
cor$Abnormal[cor$Class=="Abnormal"]=1
cor[is.na(cor)]=0

# Plot the correlation matrix
library(corrplot)
title <- "Correlation"
SetoCorr = cor(cor[,-1])
corrplot(SetoCorr, method="number", title=title, mar=c(0,0,1,0))
