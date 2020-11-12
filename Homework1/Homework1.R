df_all=read.csv("E0.csv")
str(df_all)
df=df_all[,1:56]
str(df)
summary(df)
library(ggplot2)

#TASK 1
###Part 1
hist(df$FTHG, ylab="Number of Games", xlab="Home Goals",main="Histogram of Home Goals")
hist(df$FTAG, ylab="Number of Games", xlab="Away Goals",main="Histogram of Away Goals")
hist(df$FTHG-df$FTAG, ylab="Number of Games", xlab="HHome goals – Away Goals",
     main="Histogram of Difference")
#######################

###Part 2
hist1<-hist(df$FTHG, ylab="Number of Games", xlab="Home Goals",main="Histogram of Home Goals")
x <- df$FTHG
xfit<-seq(min(x),max(x),length=78) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(hist1$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

yfite<-dexp(xfit,rate=1/mean(x),log = FALSE) 
yfite <- yfite*diff(hist1$mids[1:2])*length(x) 
lines(xfit, yfite, col="blue", lwd=2)


###########################

hist2<-hist(df$FTAG, ylab="Number of Games", xlab="Away Goals",main="Histogram of Away Goals")
x1 <- df$FTAG
xfit1<-seq(min(x1),max(x1),length=78) 
yfit1<-dnorm(xfit1,mean=mean(x1),sd=sd(x1)) 
yfit1 <- yfit1*diff(hist2$mids[1:2])*length(x1) 
lines(xfit1, yfit1, col="blue", lwd=2)
###########################

hist3<-hist(df$FTHG-df$FTAG, ylab="Number of Games", xlab="HHome goals – Away Goals",
     main="Histogram of Difference")
x2 <- df$FTHG-df$FTAG
xfit2<-seq(min(x2),max(x2),length=78) 
yfit2<-dnorm(xfit2,mean=mean(x2),sd=sd(x2)) 
yfit2 <- yfit2*diff(hist2$mids[1:2])*length(x2) 
lines(xfit2, yfit2, col="blue", lwd=2)

#TASK 2




