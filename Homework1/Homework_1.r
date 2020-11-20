library(data.table)
df_1=fread("E0.csv")
df_2=fread("E0-2.csv")
df_3=fread("E0-3.csv")
joints <- intersect(colnames(df_3), colnames(df_1[,1:56]))
df<-rbind(subset(df_1, select = joints), subset(df_2, select = joints),subset(df_3, select=joints))
library(ggplot2)

hist(df$FTHG, ylab="Number of Games", xlab="Home Goals",main="Histogram of Home Goals")

hist(df$FTAG, ylab="Number of Games", xlab="Away Goals",main="Histogram of Away Goals")

hist(df$FTHG-df$FTAG, ylab="Number of Games", xlab="HHome goals – Away Goals",
     main="Histogram of Difference")

hist1<-hist(df$FTHG, ylab="Number of Games", xlab="Home Goals",main="Histogram of Home Goals")
x <- df$FTHG
xfit<-seq(min(x),max(x),length=78) 
yfit_pois<-dpois(as.integer(xfit),lambda=1/mean(x),log = FALSE)
yfit_pois<- yfit_pois*diff(hist1$mids[1:2])*length(x) 
lines(xfit, yfit_pois, col="green", lwd=2)

yfit_normal<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit_normal<- yfit_normal*diff(hist1$mids[1:2])*length(x) 
lines(xfit, yfit_normal, col="blue", lwd=2)

yfit_exponential<-dexp(xfit,rate=1/mean(x),log = FALSE) 
yfit_exponential <- yfit_exponential*diff(hist1$mids[1:2])*length(x) 
lines(xfit, yfit_exponential, col="red", lwd=2)

hist2<-hist(df$FTAG, ylab="Number of Games", xlab="Away Goals",main="Histogram of Away Goals")
x1 <- df$FTAG
xfit1<-seq(min(x1),max(x1),length=78) 
yfit1_pois<-dpois(as.integer(xfit1),lambda=1/mean(x1),log = FALSE)
yfit1_pois<- yfit1_pois*diff(hist2$mids[1:2])*length(x1) 
lines(xfit1, yfit1_pois, col="green", lwd=2)

yfit1_normal<-dnorm(xfit1,mean=mean(x1),sd=sd(x1)) 
yfit1_normal <- yfit1_normal*diff(hist2$mids[1:2])*length(x1) 
lines(xfit1, yfit1_normal, col="blue", lwd=2)

yfit1_exponential<-dexp(xfit1,rate=1/mean(x1),log = FALSE) 
yfit1_exponential <- yfit1_exponential*diff(hist2$mids[1:2])*length(x1) 
lines(xfit1, yfit1_exponential, col="red", lwd=2)

df_Bet365=df[,c(7,22,23,24,25,26)]
df_BetAndWin=df[,c(7,22,23,27,28,29)]
df_Pinnacle=df[,c(7,22,23,33,34,35)]
df_IW=df[,c(7,22,23,30,31,32)]

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

df_Bet365$total_prob=df_Bet365$home_prob+df_Bet365$draw_prob+df_Bet365$away_prob
df_Bet365$normalized_home_prob=(1/df_Bet365$B365H)/df_Bet365$total_prob
df_Bet365$normalized_draw_prob=(1/df_Bet365$B365D)/df_Bet365$total_prob
df_Bet365$normalized_away_prob=(1/df_Bet365$B365A)/df_Bet365$total_prob

df_BetAndWin$total_prob=df_BetAndWin$home_prob+df_BetAndWin$draw_prob+df_BetAndWin$away_prob
df_BetAndWin$normalized_home_prob=(1/df_BetAndWin$BWH)/df_BetAndWin$total_prob
df_BetAndWin$normalized_draw_prob=(1/df_BetAndWin$BWD)/df_BetAndWin$total_prob
df_BetAndWin$normalized_away_prob=(1/df_BetAndWin$BWA)/df_BetAndWin$total_prob

df_Pinnacle$total_prob=df_Pinnacle$home_prob+df_Pinnacle$draw_prob+df_Pinnacle$away_prob
df_Pinnacle$normalized_home_prob=(1/df_Pinnacle$PSH)/df_Pinnacle$total_prob
df_Pinnacle$normalized_draw_prob=(1/df_Pinnacle$PSD)/df_Pinnacle$total_prob
df_Pinnacle$normalized_away_prob=(1/df_Pinnacle$PSA)/df_Pinnacle$total_prob

df_IW$total_prob=df_IW$home_prob+df_IW$draw_prob+df_IW$away_prob
df_IW$normalized_home_prob=(1/df_IW$IWH)/df_IW$total_prob
df_IW$normalized_draw_prob=(1/df_IW$IWD)/df_IW$total_prob
df_IW$normalized_away_prob=(1/df_IW$IWA)/df_IW$total_prob

library(comprehenr)

a<-seq(-1,1,by=0.2)
df_Bet365$diff=df_Bet365$home_prob-df_Bet365$away_prob
df_Bet365$categ<-to_vec(for(posi in 1:838) for(i in a) 
  if(i>=(df_Bet365$home_prob[posi]-df_Bet365$away_prob[posi]) && 
     i<(df_Bet365$home_prob[posi]-df_Bet365$away_prob[posi]+0.2)) as.integer((i+1)/0.2)-1)

df_Bet365[categ==0, bins:="(-1, -0.8]"]
df_Bet365[categ==1, bins:="(-0.8, -0.6]"]
df_Bet365[categ==2, bins:="(-0.6, -0.4]"]
df_Bet365[categ==3, bins:="(-0.4, -0.2]"]
df_Bet365[categ==4, bins:="(-0.2, 0]"]
df_Bet365[categ==5, bins:="(0, 0.2]"]
df_Bet365[categ==6, bins:="(0.2, 0.4]"]
df_Bet365[categ==7, bins:="(0.4, 0.6]"]
df_Bet365[categ==8, bins:="(0.6, 0.8]"]
df_Bet365[categ==9, bins:="(0.8, 1]"]

df_Bet365[FTR=="D",draw:=1]
df_Bet365[FTR=="H",draw:=0]
df_Bet365[FTR=="A",draw:=0]
#determined_bins <- factor(c( "(-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]","(-0.4, -0.2]",
    #              "(-0.2, 0]","(0, 0.2]","(0.2, 0.4]","(0.4, 0.6]","(0.6, 0.8]", "(0.8, 1]"),
     #           levels = c( "(-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]","(-0.4, -0.2]",
      #                     "(-0.2, 0]","(0, 0.2]","(0.2, 0.4]","(0.4, 0.6]","(0.6, 0.8]", "(0.8, 1]"),
       #         ordered = T)
df_Bet365[,real_draw_prob:=mean(draw),by=categ]
#table(df_Bet365$bins,df_Bet365$draw)
ggplot(df_Bet365,aes(x=diff,y=draw_prob))+geom_point(color="red")+                        
  labs(x="Difference between Probabilities",y="Probability of Draw")+ggtitle("Bet 365")+ 
  geom_point(aes(x=diff, y=real_draw_prob),color="blue") + theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-1,1))

ggplot(df_Bet365,aes(x=bins,y=draw_prob))+ stat_summary(fun.y="mean", geom="bar")+
  labs(x="Bins",y="Probability of Draw")+ggtitle("Bet 365")+
  geom_bar(aes(x=bins, y=real_draw_prob),stat = "summary", fun.y = "mean",
           fill="yellow",alpha=.3) + theme(axis.text.x = element_text(angle = 45,hjust = 1))

df_BetAndWin$diff=df_BetAndWin$home_prob-df_BetAndWin$away_prob
df_BetAndWin$categ<-to_vec(for(posi in 1:838) for(i in a) 
  if(i>=(df_BetAndWin$home_prob[posi]-df_BetAndWin$away_prob[posi]) && 
     i<(df_BetAndWin$home_prob[posi]-df_BetAndWin$away_prob[posi]+0.2)) as.integer((i+1)/0.2)-1)

df_BetAndWin[categ==0, bins:="(-1, -0.8]"]
df_BetAndWin[categ==1, bins:="(-0.8, -0.6]"]
df_BetAndWin[categ==2, bins:="(-0.6, -0.4]"]
df_BetAndWin[categ==3, bins:="(-0.4, -0.2]"]
df_BetAndWin[categ==4, bins:="(-0.2, 0]"]
df_BetAndWin[categ==5, bins:="(0, 0.2]"]
df_BetAndWin[categ==6, bins:="(0.2, 0.4]"]
df_BetAndWin[categ==7, bins:="(0.4, 0.6]"]
df_BetAndWin[categ==8, bins:="(0.6, 0.8]"]
df_BetAndWin[categ==9, bins:="(0.8, 1]"]

df_BetAndWin[FTR=="D",draw:=1]
df_BetAndWin[FTR=="H",draw:=0]
df_BetAndWin[FTR=="A",draw:=0]
determined_bins <- factor(c( "(-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]","(-0.4, -0.2]",
                             "(-0.2, 0]","(0, 0.2]","(0.2, 0.4]","(0.4, 0.6]","(0.6, 0.8]", "(0.8, 1]"),
                          levels = c( "(-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]","(-0.4, -0.2]",
                                      "(-0.2, 0]","(0, 0.2]","(0.2, 0.4]","(0.4, 0.6]","(0.6, 0.8]", "(0.8, 1]"),
                          ordered = T)
df_BetAndWin[,real_draw_prob:=mean(draw),by=categ]
ggplot(df_BetAndWin,aes(x=diff,y=draw_prob))+geom_point(color="red")+                        
  labs(x="Difference between Probabilities",y="Probability of Draw")+ggtitle("Bet and Win")+
  geom_point(aes(x=diff, y=real_draw_prob),color="blue") + theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-1,1))

ggplot(df_BetAndWin,aes(x=bins,y=draw_prob))+ stat_summary(fun.y="mean", geom="bar")+
  labs(x="Bins",y="Probability of Draw")+ggtitle("Bet and Win")+
  geom_bar(aes(x=bins, y=real_draw_prob),stat = "summary", fun.y = "mean",
           fill="yellow",alpha=.3) + theme(axis.text.x = element_text(angle = 45,hjust = 1))

df_IW$diff=df_IW$home_prob-df_IW$away_prob
df_IW$categ<-to_vec(for(posi in 1:838) for(i in a) 
  if(i>=(df_IW$home_prob[posi]-df_IW$away_prob[posi]) && 
     i<(df_IW$home_prob[posi]-df_IW$away_prob[posi]+0.2)) as.integer((i+1)/0.2)-1)

df_IW[categ==0, bins:="(-1, -0.8]"]
df_IW[categ==1, bins:="(-0.8, -0.6]"]
df_IW[categ==2, bins:="(-0.6, -0.4]"]
df_IW[categ==3, bins:="(-0.4, -0.2]"]
df_IW[categ==4, bins:="(-0.2, 0]"]
df_IW[categ==5, bins:="(0, 0.2]"]
df_IW[categ==6, bins:="(0.2, 0.4]"]
df_IW[categ==7, bins:="(0.4, 0.6]"]
df_IW[categ==8, bins:="(0.6, 0.8]"]
df_IW[categ==9, bins:="(0.8, 1]"]

df_IW[FTR=="D",draw:=1]
df_IW[FTR=="H",draw:=0]
df_IW[FTR=="A",draw:=0]
determined_bins <- factor(c( "(-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]","(-0.4, -0.2]",
                             "(-0.2, 0]","(0, 0.2]","(0.2, 0.4]","(0.4, 0.6]","(0.6, 0.8]", "(0.8, 1]"),
                          levels = c( "(-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]","(-0.4, -0.2]",
                                      "(-0.2, 0]","(0, 0.2]","(0.2, 0.4]","(0.4, 0.6]","(0.6, 0.8]", "(0.8, 1]"),
                          ordered = T)
df_IW[,real_draw_prob:=mean(draw),by=categ]
#table(df_IW$bins,df_IW$draw)
ggplot(df_IW,aes(x=diff,y=draw_prob))+geom_point(color="red")+                        
  labs(x="Difference between Probabilities",y="Probability of Draw")+ggtitle("IW")+ 
  geom_point(aes(x=diff, y=real_draw_prob),color="blue") + theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-1,1))

ggplot(df_IW,aes(x=bins,y=draw_prob))+ stat_summary(fun.y="mean", geom="bar")+
  labs(x="Bins",y="Probability of Draw")+ggtitle("IW")+
  geom_bar(aes(x=bins, y=real_draw_prob),stat = "summary", fun.y = "mean",
           fill="yellow",alpha=.3) + theme(axis.text.x = element_text(angle = 45,hjust = 1))

df_Pinnacle$diff=df_Pinnacle$home_prob-df_Pinnacle$away_prob
df_Pinnacle$categ<-to_vec(for(posi in 1:838) for(i in a) 
  if(i>=(df_Pinnacle$home_prob[posi]-df_Pinnacle$away_prob[posi]) && 
     i<=(df_Pinnacle$home_prob[posi]-df_Pinnacle$away_prob[posi]+0.2)) as.integer((i+1)/0.2)-1)

df_Pinnacle[categ==0, bins:=determined_bins[1]]
df_Pinnacle[categ==1, bins:=determined_bins[2]]
df_Pinnacle[categ==2, bins:=determined_bins[3]]
df_Pinnacle[categ==3, bins:=determined_bins[4]]
df_Pinnacle[categ==4, bins:=determined_bins[5]]
df_Pinnacle[categ==5, bins:=determined_bins[6]]
df_Pinnacle[categ==6, bins:=determined_bins[7]]
df_Pinnacle[categ==7, bins:=determined_bins[8]]
df_Pinnacle[categ==8, bins:=determined_bins[9]]
df_Pinnacle[categ==9, bins:=determined_bins[10]]

df_Pinnacle[FTR=="D",draw:=1]
df_Pinnacle[FTR=="H",draw:=0]
df_Pinnacle[FTR=="A",draw:=0]
determined_bins <- factor(c( "(-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]","(-0.4, -0.2]",
                             "(-0.2, 0]","(0, 0.2]","(0.2, 0.4]","(0.4, 0.6]","(0.6, 0.8]", "(0.8, 1]"),
                          levels = c( "(-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]","(-0.4, -0.2]",
                                      "(-0.2, 0]","(0, 0.2]","(0.2, 0.4]","(0.4, 0.6]","(0.6, 0.8]", "(0.8, 1]"),
                          ordered = T)
df_Pinnacle[,real_draw_prob:=mean(draw),by=categ]
#table(df_Pinnacle$bins,df_Pinnacle$draw)
ggplot(df_Pinnacle,aes(x=diff,y=draw_prob))+geom_point(color="red")+                        
  labs(x="Difference between Probabilities",y="Probability of Draw")+ggtitle("Pinnacle")+ 
  geom_point(aes(x=diff, y=real_draw_prob),color="blue") + theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-1,1))

ggplot(df_Pinnacle,aes(x=bins,y=draw_prob))+ stat_summary(fun.y="mean", geom="bar")+
  labs(x="Bins",y="Probability of Draw")+ggtitle("Pinnaccle")+
  geom_bar(aes(x=bins, y=real_draw_prob),stat = "summary", fun.y = "mean",
           fill="yellow",alpha=.3) + theme(axis.text.x = element_text(angle = 45,hjust = 1))

df_Bet365_wt_red=df_Bet365[HR!=1&AR!=1]
df_Bet365_wt_red[,real_draw_prob:=mean(draw),by=categ]
ggplot(df_Bet365_wt_red,aes(x=diff,y=draw_prob))+geom_point(color="red")+                        
  labs(x="Difference between Probabilities",y="Probability of Draw")+ggtitle("Bet 365(without red card)")+
  geom_point(aes(x=diff, y=real_draw_prob),color="blue") + theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-1,1))

ggplot(df_Bet365_wt_red,aes(x=bins,y=draw_prob))+ stat_summary(fun.y="mean", geom="bar")+
  labs(x="Bins",y="Probability of Draw")+ggtitle("Bet 365(without red card)")+
  geom_bar(aes(x=bins, y=real_draw_prob),stat = "summary", fun.y = "mean",
           fill="yellow",alpha=.3) + theme(axis.text.x = element_text(angle = 45,hjust = 1))

df_BetAndWin_wt_red=df_BetAndWin[HR!=1&AR!=1]
df_BetAndWin_wt_red[,real_draw_prob:=mean(draw),by=categ]
ggplot(df_BetAndWin_wt_red,aes(x=diff,y=draw_prob))+geom_point(color="red")+                        
  labs(x="Difference between Probabilities",y="Probability of Draw")+ggtitle("Bet and Win(without red card)")+ 
  geom_point(aes(x=diff, y=real_draw_prob),color="blue") + theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-1,1))

ggplot(df_BetAndWin_wt_red,aes(x=bins,y=draw_prob))+ stat_summary(fun.y="mean", geom="bar")+
  labs(x="Bins",y="Probability of Draw")+ggtitle("Bet and Win(without red card)")+
  geom_bar(aes(x=bins, y=real_draw_prob),stat = "summary", fun.y = "mean",
           fill="yellow",alpha=.3) + theme(axis.text.x = element_text(angle = 45,hjust = 1))

df_IW_wt_red=df_IW[HR!=1&AR!=1]
df_IW_wt_red[,real_draw_prob:=mean(draw),by=categ]
ggplot(df_IW_wt_red,aes(x=diff,y=draw_prob))+geom_point(color="red")+                        
  labs(x="Difference between Probabilities",y="Probability of Draw")+ggtitle("IW(without red card)")+ 
  geom_point(aes(x=diff, y=real_draw_prob),color="blue") + theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-1,1))

ggplot(df_IW_wt_red,aes(x=bins,y=draw_prob))+ stat_summary(fun.y="mean", geom="bar")+
  labs(x="Bins",y="Probability of Draw")+ggtitle("IW (without red card)")+
  geom_bar(aes(x=bins, y=real_draw_prob),stat = "summary", fun.y = "mean",
           fill="yellow",alpha=.3) + theme(axis.text.x = element_text(angle = 45,hjust = 1))

df_Pinnacle_wt_red=df_Pinnacle[HR!=1&AR!=1]
df_Pinnacle_wt_red[,real_draw_prob:=mean(draw),by=categ]
ggplot(df_Pinnacle_wt_red,aes(x=diff,y=draw_prob))+geom_point(color="red")+                        
  labs(x="Difference between Probabilities",y="Probability of Draw")+ggtitle("Pinnacle(without red card)")+ 
  geom_point(aes(x=diff, y=real_draw_prob),color="blue") + theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-1,1))

ggplot(df_Pinnacle_wt_red,aes(x=bins,y=draw_prob))+ stat_summary(fun.y="mean", geom="bar")+
  labs(x="Bins",y="Probability of Draw")+ggtitle("Pinnacle (without red card)")+
  geom_bar(aes(x=bins, y=real_draw_prob),stat = "summary", fun.y = "mean",
           fill="yellow",alpha=.3) + theme(axis.text.x = element_text(angle = 45,hjust = 1))


