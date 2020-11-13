df_all=fread("E0.csv")
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

df_Bet365=df[,c(8,25,26,27)]
df_BetAndWin=df[,c(8,28,29,30)]
df_Pinnacle=df[,c(8,34,35,36)]
df_IW=df[,c(8,31,32,33)]

###PART 1
df_Bet365$home_prob=1/df_Bet365$B365H
df_Bet365$draw_prob=1/df_Bet365$B365D
df_Bet365$away_prob=1/df_Bet365$B365A

df_BetAndWin$home_prob=1/df_BetAndWin$BWH
df_BetAndWin$draw_prob=1/df_BetAndWin$BWD
df_BetAndWin$away_prob=1/df_BetAndWin$BWA

df_Pinnacle$home_prob=1/df_Pinnacle$PSH
df_Pinnacle$draw_prob=1/df_Pinnacle$PSD
df_Pinnacle$away_prob=1/df_Pinnacle$PSA

df_IW$home_prob=1/df_IW$IWH
df_IW$draw_prob=1/df_IW$IWD
df_IW$away_prob=1/df_IW$IWA

###PART 2
df_Bet365$total_prob=df_Bet365$home_prob+df_Bet365$draw_prob+df_Bet365$away_prob
df_Bet365$normalized_home_prob=(1/df_Bet365$B365H)/df_Bet365$total_prob
df_Bet365$normalized_draw_prob=(1/df_Bet365$B365D)/df_Bet365$total_prob
df_Bet365$normalized_away_prob=(1/df_Bet365$B365A)/df_Bet365$total_prob
#sum(df_Bet365[,c(8,9,10)])

df_BetAndWin$total_prob=df_BetAndWin$home_prob+df_BetAndWin$draw_prob+df_BetAndWin$away_prob
df_BetAndWin$normalized_home_prob=(1/df_BetAndWin$BWH)/df_BetAndWin$total_prob
df_BetAndWin$normalized_draw_prob=(1/df_BetAndWin$BWD)/df_BetAndWin$total_prob
df_BetAndWin$normalized_away_prob=(1/df_BetAndWin$BWA)/df_BetAndWin$total_prob
#sum(df_BetAndWin[,c(8,9,10)])

df_Pinnacle$total_prob=df_Pinnacle$home_prob+df_Pinnacle$draw_prob+df_Pinnacle$away_prob
df_Pinnacle$normalized_home_prob=(1/df_Pinnacle$PSH)/df_Pinnacle$total_prob
df_Pinnacle$normalized_draw_prob=(1/df_Pinnacle$PSD)/df_Pinnacle$total_prob
df_Pinnacle$normalized_away_prob=(1/df_Pinnacle$PSA)/df_Pinnacle$total_prob
#sum(df_Pinnacle[,c(8,9,10)])

df_IW$total_prob=df_IW$home_prob+df_IW$draw_prob+df_IW$away_prob
df_IW$normalized_home_prob=(1/df_IW$IWH)/df_IW$total_prob
df_IW$normalized_draw_prob=(1/df_IW$IWD)/df_IW$total_prob
df_IW$normalized_away_prob=(1/df_IW$IWA)/df_IW$total_prob
#sum(df_IW[,c(8,9,10)])

str(df_Bet365)
###PART 3

ggplot(df_BetAndWin,aes(x=home_prob-away_prob,y=draw_prob))+ geom_bar(stat='identity', width  = 0.2)+xlim(-1,1)

ggplot(df_Pinnacle,aes(x=home_prob-away_prob,y=draw_prob))+ geom_bar(stat='identity', width  = 0.2)+xlim(-1,1)

ggplot(df_IW,aes(x=home_prob-away_prob,y=draw_prob))+ geom_bar(stat='identity', width  = 0.2)+xlim(-1,1)


#################3
a<-seq(-1,1,by=0.2)
#df_Bet365, draw:=FALSE][FTR=="D", draw:=TRUE]

install.packages("comprehenr")
library(comprehenr)
min_int=min(df_Bet365$categ)
max_int=max(df_Bet365$categ)

table(df_Bet365$categ)
df_Bet365$categ=to_vec(for(posi in 1:78) for(i in a) 
  if(i>(df_Bet365$home_prob[posi]-df_Bet365$away_prob[posi]) && 
     i<(df_Bet365$home_prob[posi]-df_Bet365$away_prob[posi]+0.2)) i) 

df<-tapply(df_Bet365$draw, df_Bet365$categ, mean)


df_Bet365[,tot_cat_per:=NULL]

str(df_Bet365)
ggplot(df_Bet365,aes(x=categ,y=draw_prob))+ geom_bar(stat='identity',fill="black")+
  scale_x_discrete(limits=seq(-1,1,0.2))+labs(x="Draw",y="Draw Prob")+
  geom_bar(aes(x=categ, y=draw),stat="identity",fill="yellow",alpha=.3)


table(df_Bet365$categ,df_Bet365$draw)

table(df_Bet365$categ)

    

    
    
  
  
