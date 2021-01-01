getwd()

library(data.table)
library(lubridate, quietly=TRUE)
library(zoo, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(glmnet)
library(MLmetrics)
library("mvtnorm") 
library(tidyr)
library(ggplot2)

dt=fread("GercekZamanliTuketim-01012016-01122020.csv")
dt$Tarih=dmy(dt$Tarih)

setnames(dt, "Tüketim Miktarı (MWh)", "consumption")
setnames(dt, "Tarih", "date")
setnames(dt, "Saat", "hour")
dt$consumption=gsub('\\.', '', dt$consumption)
dt$consumption=gsub('\\,', '.', dt$consumption)
dt$hour=gsub(':00', '', dt$hour)

dt$consumption=as.numeric(dt$consumption)
dt$hour=as.numeric(dt$hour)
str(dt)

dt<-dt[-2068]

str(dt)

dt_lagged1=copy(dt)
dt_lagged1[, Lag_48:=shift(consumption,type="lag",n=48)]
dt_lagged1[, Lag_168:=shift(consumption,type="lag",n=168)]

train=filter(dt_lagged1,date<="2020-10-31")
#train

test=filter(dt_lagged1,date>"2020-10-31")
#test

MAPE_48=MAPE(test$consumption,test$Lag_48)
MAPE_48_values=abs(((test$consumption-test$Lag_48)/test$Lag_48))
print(paste("MAPE value for lag48 equals to:", MAPE_48))
MAPE_168=MAPE(test$consumption,test$Lag_168)
MAPE_168_values=abs(((test$consumption-test$Lag_168)/test$Lag_168))
print(paste("MAPE value for lag168 equals to:", MAPE_168))

summary(MAPE_48_values)

summary(MAPE_168_values)

str(train)

Reg=lm(data=train, consumption~Lag_48+Lag_168)
summary(Reg)

predictions=predict(Reg,newdata = test)
MAPE_reg=MAPE(predictions,test$consumption)
MAPE_reg_values=as.vector(abs(((test$consumption-predictions)/test$consumption)))
print(paste("MAPE value for autoregression model equals to:", MAPE_reg))

summary(MAPE_reg_values)

names_train<-paste0("hour",0:23,"_train")
names_test<-paste0("hour",0:23,"_test")

train_split <- split(train, train$hour)
new_names_train <- names_train
for (i in 1:length(train_split)) {
  assign(new_names_train[i], train_split[[i]])
}

test_split <- split(test, test$hour)
new_names_test <- names_test
for (i in 1:length(test_split)) {
  assign(new_names_test[i], test_split[[i]])
}

mape_result_c=rep(0,24)

Reg0=lm(data=hour0_train, consumption~Lag_48+Lag_168)
summary(Reg0)
predictions0=predict(Reg0,newdata = hour0_test)
MAPE_0=MAPE(predictions0,hour0_test$consumption)
MAPE_0_values=as.vector(abs((hour0_test$consumption-predictions0)/length(test$consumption)))
MAPE0c_reg_values=as.vector(abs(((hour0_test$consumption-predictions0)/hour0_test$consumption)))
mape_result_c[1]<-MAPE_0
MAPE_0

summary(MAPE0c_reg_values)

Reg1=lm(data=hour1_train, consumption~Lag_48+Lag_168)
summary(Reg1)
predictions1=predict(Reg1,newdata = hour1_test)
MAPE_1=MAPE(predictions1,hour1_test$consumption)
MAPE_1_values=as.vector(abs((hour1_test$consumption-predictions1)/length(test$consumption)))
MAPE1c_reg_values=as.vector(abs(((hour1_test$consumption-predictions1)/hour1_test$consumption)))
mape_result_c[2]<-MAPE_1
MAPE_1

summary(MAPE1c_reg_values)

Reg2=lm(data=hour2_train, consumption~Lag_48+Lag_168)
summary(Reg2)
predictions2=predict(Reg2,newdata = hour2_test)
MAPE_2=MAPE(predictions2,hour2_test$consumption)
MAPE_2_values=as.vector(abs((hour2_test$consumption-predictions2)/length(test$consumption)))
MAPE2c_reg_values=as.vector(abs(((hour2_test$consumption-predictions2)/hour2_test$consumption)))
mape_result_c[3]<-MAPE_2
MAPE_2

summary(MAPE2c_reg_values)

Reg3=lm(data=hour3_train, consumption~Lag_48+Lag_168)
summary(Reg3)
predictions3=predict(Reg3,newdata = hour3_test)
MAPE_3=MAPE(predictions3,hour3_test$consumption)
MAPE_3_values=as.vector(abs((hour3_test$consumption-predictions3)/length(test$consumption)))
MAPE3c_reg_values=as.vector(abs(((hour3_test$consumption-predictions3)/hour3_test$consumption)))
mape_result_c[4]<-MAPE_3
MAPE_3

summary(MAPE3c_reg_values)

Reg4=lm(data=hour4_train, consumption~Lag_48+Lag_168)
summary(Reg4)
predictions4=predict(Reg4,newdata = hour4_test)
MAPE_4=MAPE(predictions4,hour4_test$consumption)
MAPE_4_values=as.vector(abs((hour4_test$consumption-predictions4)/length(test$consumption)))
MAPE4c_reg_values=as.vector(abs(((hour4_test$consumption-predictions4)/hour4_test$consumption)))
mape_result_c[5]<-MAPE_4
MAPE_4

summary(MAPE4c_reg_values)

Reg5=lm(data=hour5_train, consumption~Lag_48+Lag_168)
summary(Reg5)
predictions5=predict(Reg5,newdata = hour5_test)
MAPE_5=MAPE(predictions5,hour5_test$consumption)
MAPE_5_values=as.vector(abs((hour5_test$consumption-predictions5)/length(test$consumption)))
MAPE5c_reg_values=as.vector(abs(((hour5_test$consumption-predictions5)/hour5_test$consumption)))
mape_result_c[6]<-MAPE_5
MAPE_5

summary(MAPE5c_reg_values)

Reg6=lm(data=hour6_train, consumption~Lag_48+Lag_168)
summary(Reg6)
predictions6=predict(Reg6,newdata = hour6_test)
MAPE_6=MAPE(predictions6,hour6_test$consumption)
MAPE_6_values=as.vector(abs((hour6_test$consumption-predictions6)/length(test$consumption)))
MAPE6c_reg_values=as.vector(abs(((hour6_test$consumption-predictions6)/hour6_test$consumption)))
mape_result_c[7]<-MAPE_6
MAPE_6

summary(MAPE6c_reg_values)

Reg7=lm(data=hour7_train, consumption~Lag_48+Lag_168)
summary(Reg7)
predictions7=predict(Reg7,newdata = hour7_test)
MAPE_7=MAPE(predictions7,hour7_test$consumption)
MAPE_7_values=as.vector(abs((hour7_test$consumption-predictions7)/length(test$consumption)))
MAPE7c_reg_values=as.vector(abs(((hour7_test$consumption-predictions7)/hour7_test$consumption)))
mape_result_c[8]<-MAPE_7
MAPE_7

summary(MAPE7c_reg_values)

Reg8=lm(data=hour8_train, consumption~Lag_48+Lag_168)
summary(Reg8)
predictions8=predict(Reg8,newdata = hour8_test)
MAPE_8=MAPE(predictions8,hour8_test$consumption)
MAPE_8_values=as.vector(abs((hour8_test$consumption-predictions8)/length(test$consumption)))
MAPE8c_reg_values=as.vector(abs(((hour8_test$consumption-predictions8)/hour8_test$consumption)))
mape_result_c[9]<-MAPE_8
MAPE_8

summary(MAPE8c_reg_values)

Reg9=lm(data=hour9_train, consumption~Lag_48+Lag_168)
summary(Reg9)
predictions9=predict(Reg9,newdata = hour9_test)
MAPE_9=MAPE(predictions9,hour9_test$consumption)
MAPE_9_values=as.vector(abs((hour9_test$consumption-predictions9)/length(test$consumption)))
MAPE9c_reg_values=as.vector(abs(((hour9_test$consumption-predictions9)/hour9_test$consumption)))
mape_result_c[10]<-MAPE_9
MAPE_9

summary(MAPE9c_reg_values)

Reg10=lm(data=hour10_train, consumption~Lag_48+Lag_168)
summary(Reg10)
predictions10=predict(Reg10,newdata = hour10_test)
MAPE_10=MAPE(predictions10,hour10_test$consumption)
MAPE_10_values=as.vector(abs((hour10_test$consumption-predictions10)/length(test$consumption)))
MAPE10c_reg_values=as.vector(abs(((hour10_test$consumption-predictions10)/hour10_test$consumption)))
mape_result_c[11]<-MAPE_10
MAPE_10

summary(MAPE10c_reg_values)

Reg11=lm(data=hour11_train, consumption~Lag_48+Lag_168)
summary(Reg11)
predictions11=predict(Reg11,newdata = hour11_test)
MAPE_11=MAPE(predictions11,hour11_test$consumption)
MAPE_11_values=as.vector(abs((hour11_test$consumption-predictions11)/length(test$consumption)))
MAPE11c_reg_values=as.vector(abs(((hour11_test$consumption-predictions11)/hour11_test$consumption)))
mape_result_c[12]<-MAPE_11
MAPE_11

summary(MAPE11c_reg_values)

Reg12=lm(data=hour12_train, consumption~Lag_48+Lag_168)
summary(Reg12)
predictions12=predict(Reg12,newdata = hour12_test)
MAPE_12=MAPE(predictions12,hour12_test$consumption)
MAPE_12_values=as.vector(abs((hour12_test$consumption-predictions12)/length(test$consumption)))
MAPE12c_reg_values=as.vector(abs(((hour12_test$consumption-predictions12)/hour12_test$consumption)))
mape_result_c[13]<-MAPE_12
MAPE_12

summary(MAPE12c_reg_values)

Reg13=lm(data=hour13_train, consumption~Lag_48+Lag_168)
summary(Reg13)
predictions13=predict(Reg13,newdata = hour13_test)
MAPE_13=MAPE(predictions13,hour13_test$consumption)
MAPE_13_values=as.vector(abs((hour13_test$consumption-predictions13)/length(test$consumption)))
MAPE13c_reg_values=as.vector(abs(((hour13_test$consumption-predictions13)/hour13_test$consumption)))
mape_result_c[14]<-MAPE_13
MAPE_13

summary(MAPE13c_reg_values)

Reg14=lm(data=hour14_train, consumption~Lag_48+Lag_168)
summary(Reg14)
predictions14=predict(Reg14,newdata = hour14_test)
MAPE_14=MAPE(predictions14,hour14_test$consumption)
MAPE_14_values=as.vector(abs((hour14_test$consumption-predictions14)/length(test$consumption)))
MAPE14c_reg_values=as.vector(abs(((hour14_test$consumption-predictions14)/hour14_test$consumption)))
mape_result_c[15]<-MAPE_14
MAPE_14

summary(MAPE14c_reg_values)

Reg15=lm(data=hour15_train, consumption~Lag_48+Lag_168)
summary(Reg15)
predictions15=predict(Reg15,newdata = hour15_test)
MAPE_15=MAPE(predictions15,hour15_test$consumption)
MAPE_15_values=as.vector(abs((hour15_test$consumption-predictions15)/length(test$consumption)))
MAPE15c_reg_values=as.vector(abs(((hour15_test$consumption-predictions15)/hour15_test$consumption)))
mape_result_c[16]<-MAPE_15
MAPE_15

summary(MAPE15c_reg_values)

Reg16=lm(data=hour16_train, consumption~Lag_48+Lag_168)
summary(Reg16)
predictions16=predict(Reg16,newdata = hour16_test)
MAPE_16=MAPE(predictions16,hour16_test$consumption)
MAPE_16_values=as.vector(abs((hour16_test$consumption-predictions16)/length(test$consumption)))
MAPE16c_reg_values=as.vector(abs(((hour16_test$consumption-predictions16)/hour16_test$consumption)))
mape_result_c[17]<-MAPE_16
MAPE_16

summary(MAPE16c_reg_values)

Reg17=lm(data=hour17_train, consumption~Lag_48+Lag_168)
summary(Reg17)
predictions17=predict(Reg17,newdata = hour17_test)
MAPE_17=MAPE(predictions17,hour17_test$consumption)
MAPE_17_values=as.vector(abs((hour17_test$consumption-predictions17)/length(test$consumption)))
MAPE17c_reg_values=as.vector(abs(((hour17_test$consumption-predictions17)/hour17_test$consumption)))
mape_result_c[18]<-MAPE_17
MAPE_17

summary(MAPE17c_reg_values)

Reg18=lm(data=hour18_train, consumption~Lag_48+Lag_168)
summary(Reg18)
predictions18=predict(Reg18,newdata = hour18_test)
MAPE_18=MAPE(predictions18,hour18_test$consumption)
MAPE_18_values=as.vector(abs((hour18_test$consumption-predictions18)/length(test$consumption)))
MAPE18c_reg_values=as.vector(abs(((hour18_test$consumption-predictions18)/hour18_test$consumption)))
mape_result_c[19]<-MAPE_18
MAPE_18

summary(MAPE18c_reg_values)

Reg19=lm(data=hour19_train, consumption~Lag_48+Lag_168)
summary(Reg19)
predictions19=predict(Reg19,newdata = hour19_test)
MAPE_19=MAPE(predictions19,hour19_test$consumption)
MAPE_19_values=as.vector(abs((hour19_test$consumption-predictions19)/length(test$consumption)))
MAPE19c_reg_values=as.vector(abs(((hour19_test$consumption-predictions19)/hour19_test$consumption)))
mape_result_c[20]<-MAPE_19
MAPE_19

summary(MAPE19c_reg_values)

Reg20=lm(data=hour20_train, consumption~Lag_48+Lag_168)
summary(Reg20)
predictions20=predict(Reg20,newdata = hour20_test)
MAPE_20=MAPE(predictions20,hour20_test$consumption)
MAPE_20_values=as.vector(abs((hour20_test$consumption-predictions20)/length(test$consumption)))
MAPE20c_reg_values=as.vector(abs(((hour20_test$consumption-predictions20)/hour20_test$consumption)))
mape_result_c[21]<-MAPE_20
MAPE_20

summary(MAPE20c_reg_values)

Reg21=lm(data=hour21_train, consumption~Lag_48+Lag_168)
summary(Reg21)
predictions21=predict(Reg21,newdata = hour21_test)
MAPE_21=MAPE(predictions21,hour21_test$consumption)
MAPE_21_values=as.vector(abs((hour21_test$consumption-predictions21)/length(test$consumption)))
MAPE21c_reg_values=as.vector(abs(((hour21_test$consumption-predictions21)/hour21_test$consumption)))
mape_result_c[22]<-MAPE_21
MAPE_21

summary(MAPE21c_reg_values)

Reg22=lm(data=hour22_train, consumption~Lag_48+Lag_168)
summary(Reg22)
predictions22=predict(Reg22,newdata = hour22_test)
MAPE_22=MAPE(predictions22,hour22_test$consumption)
MAPE_22_values=as.vector(abs((hour22_test$consumption-predictions22)/length(test$consumption)))
MAPE22c_reg_values=as.vector(abs(((hour22_test$consumption-predictions22)/hour22_test$consumption)))
mape_result_c[23]<-MAPE_22
MAPE_22

summary(MAPE22c_reg_values)

Reg23=lm(data=hour23_train, consumption~Lag_48+Lag_168)
summary(Reg23)
predictions23=predict(Reg23,newdata = hour23_test)
MAPE_23=MAPE(predictions23,hour23_test$consumption)
MAPE_23_values=as.vector(abs((hour23_test$consumption-predictions23))/length(test$consumption))
MAPE23c_reg_values=as.vector(abs(((hour23_test$consumption-predictions23)/hour23_test$consumption)))
mape_result_c[24]<-MAPE_23
MAPE_23

summary(MAPE23c_reg_values)

plot(mape_result_c,type="l",ylab="MAPE Values (Quantity)",xlab="Hours",main="MAPE Values for Hourly Regression Model")

C_all_val=c(MAPE0c_reg_values,MAPE1c_reg_values,MAPE2c_reg_values,MAPE3c_reg_values,MAPE4c_reg_values,MAPE5c_reg_values,
           MAPE6c_reg_values,MAPE7c_reg_values,MAPE8c_reg_values,MAPE9c_reg_values,MAPE10c_reg_values,MAPE11c_reg_values,
           MAPE12c_reg_values,MAPE13c_reg_values,MAPE14c_reg_values,MAPE15c_reg_values,MAPE16c_reg_values,MAPE17c_reg_values,
           MAPE18c_reg_values,MAPE19c_reg_values,MAPE20c_reg_values,MAPE21c_reg_values,MAPE22c_reg_values,MAPE23c_reg_values)

summary(C_all_val)

cont=copy(dt)

lag_creator<-function(DT, names){   
    hours=c(0:23)
    for (which_hour in hours){
        new_col<-paste0(names,which_hour)
        which_hour=which_hour+1
        DT[,(new_col):=shift(DT[,2],type="lag",n=which_hour)]                
  
        }  
    return(DT[])
}

feature_mat<-function(DT,names){
    feat_mat=data.table(date=seq.POSIXt(from = as.POSIXct("2016-01-01"),to = as.POSIXct("2020-12-01"),by="day"))
    hours=c(0:23)    
    for (which_hour in hours){
    new_col<-paste0(names,which_hour)
    feat_mat[,(new_col):=0]                
    } 
    for(whic_date in seq.POSIXt(from = as.POSIXct("2016-01-01"),to = as.POSIXct("2020-12-01"),by="day")){
        current_DT=filter(DT,date==as.POSIXct(whic_date, origin = "1970-01-01"))
        print(current_DT)
        for(which_hour in hours){
            feat_mat[date==as.POSIXct(whic_date, origin = "1970-01-01"), 
                     feat_mat[,which_hour+1]:=current_DT[hour==which_hour,consumption]] 
        }   
    }
    return(feat_mat[])
}

col_name<-function(DT, names){   
    hours=c(0:23)
    for (which_hour in hours){
        new_col<-paste0(names,which_hour)
        setnames(DT,old=as.character(which_hour),new=as.character(new_col))              
        }  
}

data_wide <- spread(cont, hour, consumption)
lag_2=copy(data_wide[,date:=date+2])
col_name(lag_2,"Lag_day2_hour_")
#lag_2

data_wide <- spread(cont, hour, consumption)
lag_7=copy(data_wide[,date:=date+7])
col_name(lag_7,"Lag_day7_hour_")
#lag_7

dt_with_lag2=merge(cont,lag_2,by.x="date",by.y="date",all.x=TRUE)

full_dt=merge(dt_with_lag2,lag_7,by.x="date",by.y="date",all.x=TRUE)

full_dt %>% relocate(consumption, .after = last_col())
#full_dt

train_d=filter(full_dt,date<="2020-10-31")
#train

test_d=filter(full_dt,date>"2020-10-31")
#test

names_train<-paste0("train_d",0:23)
names_test<-paste0("test_d",0:23)

train_split <- split(train_d, train_d$hour)
new_names_train <- names_train
for (i in 1:length(train_split)) {
  assign(new_names_train[i], train_split[[i]])
}

test_split <- split(test_d, test_d$hour)
new_names_test <- names_test
for (i in 1:length(test_split)) {
  assign(new_names_test[i], test_split[[i]])
}

#print(cvfit$name)
#print(cvfit$glmnet.fit)
#print(cvfit$lambda.1se)
#print(cvfit$lambda.min)
#plot(cvfit)
#coef(cvfit,s="lambda.min")
#coef(cvfit,s="lambda.1se")

mape_result_d=rep(0,24)

set.seed(1)

train_mat0=as.matrix(train_d0[complete.cases(train_d0),-c('date',"hour","consumption"),with=F])
train_mat1=as.matrix(train_d1[complete.cases(train_d1),-c('date',"hour","consumption"),with=F])
train_mat2=as.matrix(train_d2[complete.cases(train_d2),-c('date',"hour","consumption"),with=F])
train_mat3=as.matrix(train_d3[complete.cases(train_d3),-c('date',"hour","consumption"),with=F])
train_mat4=as.matrix(train_d4[complete.cases(train_d4),-c('date',"hour","consumption"),with=F])
train_mat5=as.matrix(train_d5[complete.cases(train_d5),-c('date',"hour","consumption"),with=F])
train_mat6=as.matrix(train_d6[complete.cases(train_d6),-c('date',"hour","consumption"),with=F])
train_mat7=as.matrix(train_d7[complete.cases(train_d7),-c('date',"hour","consumption"),with=F])
train_mat8=as.matrix(train_d8[complete.cases(train_d8),-c('date',"hour","consumption"),with=F])
train_mat9=as.matrix(train_d9[complete.cases(train_d9),-c('date',"hour","consumption"),with=F])
train_mat10=as.matrix(train_d10[complete.cases(train_d10),-c('date',"hour","consumption"),with=F])
train_mat11=as.matrix(train_d11[complete.cases(train_d11),-c('date',"hour","consumption"),with=F])
train_mat12=as.matrix(train_d12[complete.cases(train_d12),-c('date',"hour","consumption"),with=F])
train_mat13=as.matrix(train_d13[complete.cases(train_d13),-c('date',"hour","consumption"),with=F])
train_mat14=as.matrix(train_d14[complete.cases(train_d14),-c('date',"hour","consumption"),with=F])
train_mat15=as.matrix(train_d15[complete.cases(train_d15),-c('date',"hour","consumption"),with=F])
train_mat16=as.matrix(train_d16[complete.cases(train_d16),-c('date',"hour","consumption"),with=F])
train_mat17=as.matrix(train_d17[complete.cases(train_d17),-c('date',"hour","consumption"),with=F])
train_mat18=as.matrix(train_d18[complete.cases(train_d18),-c('date',"hour","consumption"),with=F])
train_mat19=as.matrix(train_d19[complete.cases(train_d19),-c('date',"hour","consumption"),with=F])
train_mat20=as.matrix(train_d20[complete.cases(train_d20),-c('date',"hour","consumption"),with=F])
train_mat21=as.matrix(train_d21[complete.cases(train_d21),-c('date',"hour","consumption"),with=F])
train_mat22=as.matrix(train_d22[complete.cases(train_d22),-c('date',"hour","consumption"),with=F])
train_mat23=as.matrix(train_d23[complete.cases(train_d23),-c('date',"hour","consumption"),with=F])

result_vec0=as.vector(t(train_d0[complete.cases(train_d0),"consumption"]))
result_vec1=as.vector(t(train_d1[complete.cases(train_d1),"consumption"]))
result_vec2=as.vector(t(train_d2[complete.cases(train_d2),"consumption"]))
result_vec3=as.vector(t(train_d3[complete.cases(train_d3),"consumption"]))
result_vec4=as.vector(t(train_d4[complete.cases(train_d4),"consumption"]))
result_vec5=as.vector(t(train_d5[complete.cases(train_d5),"consumption"]))
result_vec6=as.vector(t(train_d6[complete.cases(train_d6),"consumption"]))
result_vec7=as.vector(t(train_d7[complete.cases(train_d7),"consumption"]))
result_vec8=as.vector(t(train_d8[complete.cases(train_d8),"consumption"]))
result_vec9=as.vector(t(train_d9[complete.cases(train_d9),"consumption"]))
result_vec10=as.vector(t(train_d10[complete.cases(train_d10),"consumption"]))
result_vec11=as.vector(t(train_d11[complete.cases(train_d11),"consumption"]))
result_vec12=as.vector(t(train_d12[complete.cases(train_d12),"consumption"]))
result_vec13=as.vector(t(train_d13[complete.cases(train_d13),"consumption"]))
result_vec14=as.vector(t(train_d14[complete.cases(train_d14),"consumption"]))
result_vec15=as.vector(t(train_d15[complete.cases(train_d15),"consumption"]))
result_vec16=as.vector(t(train_d16[complete.cases(train_d16),"consumption"]))
result_vec17=as.vector(t(train_d17[complete.cases(train_d17),"consumption"]))
result_vec18=as.vector(t(train_d18[complete.cases(train_d18),"consumption"]))
result_vec19=as.vector(t(train_d19[complete.cases(train_d19),"consumption"]))
result_vec20=as.vector(t(train_d20[complete.cases(train_d20),"consumption"]))
result_vec21=as.vector(t(train_d21[complete.cases(train_d21),"consumption"]))
result_vec22=as.vector(t(train_d22[complete.cases(train_d22),"consumption"]))
result_vec23=as.vector(t(train_d23[complete.cases(train_d23),"consumption"]))

cvfit0=cv.glmnet(train_mat0,result_vec0,family='gaussian',nfolds = 10,type.measure="mae")
cvfit1=cv.glmnet(train_mat1,result_vec1,family='gaussian',nfolds = 10,type.measure="mae")
cvfit2=cv.glmnet(train_mat2,result_vec2,family='gaussian',nfolds = 10,type.measure="mae")
cvfit3=cv.glmnet(train_mat3,result_vec3,family='gaussian',nfolds = 10,type.measure="mae")
cvfit4=cv.glmnet(train_mat4,result_vec4,family='gaussian',nfolds = 10,type.measure="mae")
cvfit5=cv.glmnet(train_mat5,result_vec5,family='gaussian',nfolds = 10,type.measure="mae")
cvfit6=cv.glmnet(train_mat6,result_vec6,family='gaussian',nfolds = 10,type.measure="mae")
cvfit7=cv.glmnet(train_mat7,result_vec7,family='gaussian',nfolds = 10,type.measure="mae")
cvfit8=cv.glmnet(train_mat8,result_vec8,family='gaussian',nfolds = 10,type.measure="mae")
cvfit9=cv.glmnet(train_mat9,result_vec9,family='gaussian',nfolds = 10,type.measure="mae")
cvfit10=cv.glmnet(train_mat10,result_vec10,family='gaussian',nfolds = 10,type.measure="mae")
cvfit11=cv.glmnet(train_mat11,result_vec11,family='gaussian',nfolds = 10,type.measure="mae")
cvfit12=cv.glmnet(train_mat12,result_vec12,family='gaussian',nfolds = 10,type.measure="mae")
cvfit13=cv.glmnet(train_mat13,result_vec13,family='gaussian',nfolds = 10,type.measure="mae")
cvfit14=cv.glmnet(train_mat14,result_vec14,family='gaussian',nfolds = 10,type.measure="mae")
cvfit15=cv.glmnet(train_mat15,result_vec15,family='gaussian',nfolds = 10,type.measure="mae")
cvfit16=cv.glmnet(train_mat16,result_vec16,family='gaussian',nfolds = 10,type.measure="mae")
cvfit17=cv.glmnet(train_mat17,result_vec17,family='gaussian',nfolds = 10,type.measure="mae")
cvfit18=cv.glmnet(train_mat18,result_vec18,family='gaussian',nfolds = 10,type.measure="mae")
cvfit19=cv.glmnet(train_mat19,result_vec19,family='gaussian',nfolds = 10,type.measure="mae")
cvfit20=cv.glmnet(train_mat20,result_vec20,family='gaussian',nfolds = 10,type.measure="mae")
cvfit21=cv.glmnet(train_mat21,result_vec21,family='gaussian',nfolds = 10,type.measure="mae")
cvfit22=cv.glmnet(train_mat22,result_vec22,family='gaussian',nfolds = 10,type.measure="mae")
cvfit23=cv.glmnet(train_mat23,result_vec23,family='gaussian',nfolds = 10,type.measure="mae")

for(i in seq(0,23,by=1)){    
    plot(get(paste0("cvfit",i)))
    print(paste("Hour",i))
    print(paste("Min value for Lambda=",get(paste0("cvfit",i))$lambda.min))
    print(paste("1se value for Lambda=",get(paste0("cvfit",i))$lambda.1se))    
    print(coef(get(paste0("cvfit",i)), s = 'lambda.min'))
    print(paste0("Plot for ",i,"'th hour's Cross Validation"))
}

test_mat0=as.matrix(test_d0[complete.cases(test_d0),-c('date',"hour","consumption")])
test_mat1=as.matrix(test_d1[complete.cases(test_d1),-c('date',"hour","consumption")])
test_mat2=as.matrix(test_d2[complete.cases(test_d2),-c('date',"hour","consumption")])
test_mat3=as.matrix(test_d3[complete.cases(test_d3),-c('date',"hour","consumption")])
test_mat4=as.matrix(test_d4[complete.cases(test_d4),-c('date',"hour","consumption")])
test_mat5=as.matrix(test_d5[complete.cases(test_d5),-c('date',"hour","consumption")])
test_mat6=as.matrix(test_d6[complete.cases(test_d6),-c('date',"hour","consumption")])
test_mat7=as.matrix(test_d7[complete.cases(test_d7),-c('date',"hour","consumption")])
test_mat8=as.matrix(test_d8[complete.cases(test_d8),-c('date',"hour","consumption")])
test_mat9=as.matrix(test_d9[complete.cases(test_d9),-c('date',"hour","consumption")])
test_mat10=as.matrix(test_d10[complete.cases(test_d10),-c('date',"hour","consumption")])
test_mat11=as.matrix(test_d11[complete.cases(test_d11),-c('date',"hour","consumption")])
test_mat12=as.matrix(test_d12[complete.cases(test_d12),-c('date',"hour","consumption")])
test_mat13=as.matrix(test_d13[complete.cases(test_d13),-c('date',"hour","consumption")])
test_mat14=as.matrix(test_d14[complete.cases(test_d14),-c('date',"hour","consumption")])
test_mat15=as.matrix(test_d15[complete.cases(test_d15),-c('date',"hour","consumption")])
test_mat16=as.matrix(test_d16[complete.cases(test_d16),-c('date',"hour","consumption")])
test_mat17=as.matrix(test_d17[complete.cases(test_d17),-c('date',"hour","consumption")])
test_mat18=as.matrix(test_d18[complete.cases(test_d18),-c('date',"hour","consumption")])
test_mat19=as.matrix(test_d19[complete.cases(test_d19),-c('date',"hour","consumption")])
test_mat20=as.matrix(test_d20[complete.cases(test_d20),-c('date',"hour","consumption")])
test_mat21=as.matrix(test_d21[complete.cases(test_d21),-c('date',"hour","consumption")])
test_mat22=as.matrix(test_d22[complete.cases(test_d22),-c('date',"hour","consumption")])
test_mat23=as.matrix(test_d23[complete.cases(test_d23),-c('date',"hour","consumption")])

lasso_model0 <- glmnet(train_mat0,result_vec0, alpha = 1, lambda = cvfit0$lambda.min, standardize = FALSE)
lasso_model1 <- glmnet(train_mat1,result_vec1, alpha = 1, lambda = cvfit1$lambda.min, standardize = FALSE)
lasso_model2 <- glmnet(train_mat2,result_vec2, alpha = 1, lambda = cvfit2$lambda.min, standardize = FALSE)
lasso_model3 <- glmnet(train_mat3,result_vec3, alpha = 1, lambda = cvfit3$lambda.min, standardize = FALSE)
lasso_model4 <- glmnet(train_mat4,result_vec4, alpha = 1, lambda = cvfit4$lambda.min, standardize = FALSE)
lasso_model5 <- glmnet(train_mat5,result_vec5, alpha = 1, lambda = cvfit5$lambda.min, standardize = FALSE)
lasso_model6 <- glmnet(train_mat6,result_vec6, alpha = 1, lambda = cvfit6$lambda.min, standardize = FALSE)
lasso_model7 <- glmnet(train_mat7,result_vec7, alpha = 1, lambda = cvfit7$lambda.min, standardize = FALSE)
lasso_model8 <- glmnet(train_mat8,result_vec8, alpha = 1, lambda = cvfit8$lambda.min, standardize = FALSE)
lasso_model9 <- glmnet(train_mat9,result_vec9, alpha = 1, lambda = cvfit9$lambda.min, standardize = FALSE)
lasso_model10 <- glmnet(train_mat10,result_vec10, alpha = 1, lambda = cvfit10$lambda.min, standardize = FALSE)
lasso_model11 <- glmnet(train_mat11,result_vec11, alpha = 1, lambda = cvfit11$lambda.min, standardize = FALSE)
lasso_model12 <- glmnet(train_mat12,result_vec12, alpha = 1, lambda = cvfit12$lambda.min, standardize = FALSE)
lasso_model13 <- glmnet(train_mat13,result_vec13, alpha = 1, lambda = cvfit13$lambda.min, standardize = FALSE)
lasso_model14 <- glmnet(train_mat14,result_vec14, alpha = 1, lambda = cvfit14$lambda.min, standardize = FALSE)
lasso_model15 <- glmnet(train_mat15,result_vec15, alpha = 1, lambda = cvfit15$lambda.min, standardize = FALSE)
lasso_model16 <- glmnet(train_mat16,result_vec16, alpha = 1, lambda = cvfit16$lambda.min, standardize = FALSE)
lasso_model17 <- glmnet(train_mat17,result_vec17, alpha = 1, lambda = cvfit17$lambda.min, standardize = FALSE)
lasso_model18 <- glmnet(train_mat18,result_vec18, alpha = 1, lambda = cvfit18$lambda.min, standardize = FALSE)
lasso_model19 <- glmnet(train_mat19,result_vec19, alpha = 1, lambda = cvfit19$lambda.min, standardize = FALSE)
lasso_model20 <- glmnet(train_mat20,result_vec20, alpha = 1, lambda = cvfit20$lambda.min, standardize = FALSE)
lasso_model21 <- glmnet(train_mat21,result_vec21, alpha = 1, lambda = cvfit21$lambda.min, standardize = FALSE)
lasso_model22 <- glmnet(train_mat22,result_vec22, alpha = 1, lambda = cvfit22$lambda.min, standardize = FALSE)
lasso_model23 <- glmnet(train_mat23,result_vec23, alpha = 1, lambda = cvfit23$lambda.min, standardize = FALSE)

predicts_hour0 <- predict(lasso_model0, s = cvfit0$lambda.min, newx = test_mat0)
predicts_hour1 <- predict(lasso_model1, s = cvfit1$lambda.min, newx = test_mat1)
predicts_hour2 <- predict(lasso_model2, s = cvfit2$lambda.min, newx = test_mat2)
predicts_hour3 <- predict(lasso_model3, s = cvfit3$lambda.min, newx = test_mat3)
predicts_hour4 <- predict(lasso_model4, s = cvfit4$lambda.min, newx = test_mat4)
predicts_hour5 <- predict(lasso_model5, s = cvfit5$lambda.min, newx = test_mat5)
predicts_hour6 <- predict(lasso_model6, s = cvfit6$lambda.min, newx = test_mat6)
predicts_hour7 <- predict(lasso_model7, s = cvfit7$lambda.min, newx = test_mat7)
predicts_hour8 <- predict(lasso_model8, s = cvfit8$lambda.min, newx = test_mat8)
predicts_hour9 <- predict(lasso_model9, s = cvfit9$lambda.min, newx = test_mat9)
predicts_hour10 <- predict(lasso_model10, s = cvfit10$lambda.min, newx = test_mat10)
predicts_hour11 <- predict(lasso_model11, s = cvfit11$lambda.min, newx = test_mat11)
predicts_hour12 <- predict(lasso_model12, s = cvfit12$lambda.min, newx = test_mat12)
predicts_hour13 <- predict(lasso_model13, s = cvfit13$lambda.min, newx = test_mat13)
predicts_hour14 <- predict(lasso_model14, s = cvfit14$lambda.min, newx = test_mat14)
predicts_hour15 <- predict(lasso_model15, s = cvfit15$lambda.min, newx = test_mat15)
predicts_hour16 <- predict(lasso_model16, s = cvfit16$lambda.min, newx = test_mat16)
predicts_hour17 <- predict(lasso_model17, s = cvfit17$lambda.min, newx = test_mat17)
predicts_hour18 <- predict(lasso_model18, s = cvfit18$lambda.min, newx = test_mat18)
predicts_hour19 <- predict(lasso_model19, s = cvfit19$lambda.min, newx = test_mat19)
predicts_hour20 <- predict(lasso_model20, s = cvfit20$lambda.min, newx = test_mat20)
predicts_hour21 <- predict(lasso_model21, s = cvfit21$lambda.min, newx = test_mat21)
predicts_hour22 <- predict(lasso_model22, s = cvfit22$lambda.min, newx = test_mat22)
predicts_hour23 <- predict(lasso_model23, s = cvfit23$lambda.min, newx = test_mat23)

mape_result_d[1] <- MAPE(test_d0$consumption, predicts_hour0)
mape_result_d[2] <- MAPE(test_d1$consumption, predicts_hour1)
mape_result_d[3] <- MAPE(test_d2$consumption, predicts_hour2)
mape_result_d[4] <- MAPE(test_d3$consumption, predicts_hour3)
mape_result_d[5] <- MAPE(test_d4$consumption, predicts_hour4)
mape_result_d[6] <- MAPE(test_d5$consumption, predicts_hour5)
mape_result_d[7] <- MAPE(test_d6$consumption, predicts_hour6)
mape_result_d[8] <- MAPE(test_d7$consumption, predicts_hour7)
mape_result_d[9] <- MAPE(test_d8$consumption, predicts_hour8)
mape_result_d[10] <- MAPE(test_d9$consumption, predicts_hour9)
mape_result_d[11] <- MAPE(test_d10$consumption, predicts_hour10)
mape_result_d[12] <- MAPE(test_d11$consumption, predicts_hour11)
mape_result_d[13] <- MAPE(test_d12$consumption, predicts_hour12)
mape_result_d[14] <- MAPE(test_d13$consumption, predicts_hour13)
mape_result_d[15] <- MAPE(test_d14$consumption, predicts_hour14)
mape_result_d[16] <- MAPE(test_d15$consumption, predicts_hour15)
mape_result_d[17] <- MAPE(test_d16$consumption, predicts_hour16)
mape_result_d[18] <- MAPE(test_d17$consumption, predicts_hour17)
mape_result_d[19] <- MAPE(test_d18$consumption, predicts_hour18)
mape_result_d[20] <- MAPE(test_d19$consumption, predicts_hour19)
mape_result_d[21] <- MAPE(test_d20$consumption, predicts_hour20)
mape_result_d[22] <- MAPE(test_d21$consumption, predicts_hour21)
mape_result_d[23] <- MAPE(test_d22$consumption, predicts_hour22)
mape_result_d[24] <- MAPE(test_d23$consumption, predicts_hour23)
#mape_result_d

plot(mape_result_d,type="l",ylab="MAPE Values (Quantity)",xlab="Hours",main="MAPE Values for Hourly Model with Lasso Penalty")

MAPE0d_reg_values=as.vector(abs(((hour0_test$consumption-predicts_hour0)/predicts_hour0)))
MAPE1d_reg_values=as.vector(abs(((hour1_test$consumption-predicts_hour1)/predicts_hour1)))
MAPE2d_reg_values=as.vector(abs(((hour2_test$consumption-predicts_hour2)/predicts_hour2)))
MAPE3d_reg_values=as.vector(abs(((hour3_test$consumption-predicts_hour3)/predicts_hour3)))
MAPE4d_reg_values=as.vector(abs(((hour4_test$consumption-predicts_hour4)/predicts_hour4)))
MAPE5d_reg_values=as.vector(abs(((hour5_test$consumption-predicts_hour5)/predicts_hour5)))
MAPE6d_reg_values=as.vector(abs(((hour6_test$consumption-predicts_hour6)/predicts_hour6)))
MAPE7d_reg_values=as.vector(abs(((hour7_test$consumption-predicts_hour7)/predicts_hour7)))
MAPE8d_reg_values=as.vector(abs(((hour8_test$consumption-predicts_hour8)/predicts_hour8)))
MAPE9d_reg_values=as.vector(abs(((hour9_test$consumption-predicts_hour9)/predicts_hour9)))
MAPE10d_reg_values=as.vector(abs(((hour10_test$consumption-predicts_hour10)/predicts_hour10)))
MAPE11d_reg_values=as.vector(abs(((hour11_test$consumption-predicts_hour11)/predicts_hour11)))
MAPE12d_reg_values=as.vector(abs(((hour12_test$consumption-predicts_hour12)/predicts_hour12)))
MAPE13d_reg_values=as.vector(abs(((hour13_test$consumption-predicts_hour13)/predicts_hour13)))
MAPE14d_reg_values=as.vector(abs(((hour14_test$consumption-predicts_hour14)/predicts_hour14)))
MAPE15d_reg_values=as.vector(abs(((hour15_test$consumption-predicts_hour15)/predicts_hour15)))
MAPE16d_reg_values=as.vector(abs(((hour16_test$consumption-predicts_hour16)/predicts_hour16)))
MAPE17d_reg_values=as.vector(abs(((hour17_test$consumption-predicts_hour17)/predicts_hour17)))
MAPE18d_reg_values=as.vector(abs(((hour18_test$consumption-predicts_hour18)/predicts_hour18)))
MAPE19d_reg_values=as.vector(abs(((hour19_test$consumption-predicts_hour19)/predicts_hour19)))
MAPE20d_reg_values=as.vector(abs(((hour20_test$consumption-predicts_hour20)/predicts_hour20)))
MAPE21d_reg_values=as.vector(abs(((hour21_test$consumption-predicts_hour21)/predicts_hour21)))
MAPE22d_reg_values=as.vector(abs(((hour22_test$consumption-predicts_hour22)/predicts_hour22)))
MAPE23d_reg_values=as.vector(abs(((hour23_test$consumption-predicts_hour23)/predicts_hour23)))


D_all_val=c(MAPE0d_reg_values,MAPE1d_reg_values,MAPE2d_reg_values,MAPE3d_reg_values,MAPE4d_reg_values,MAPE5d_reg_values,
           MAPE6d_reg_values,MAPE7d_reg_values,MAPE8d_reg_values,MAPE9d_reg_values,MAPE10d_reg_values,MAPE11d_reg_values,
           MAPE12d_reg_values,MAPE13d_reg_values,MAPE14d_reg_values,MAPE15d_reg_values,MAPE16d_reg_values,MAPE17d_reg_values,
           MAPE18d_reg_values,MAPE19d_reg_values,MAPE20d_reg_values,MAPE21d_reg_values,MAPE22d_reg_values,MAPE23d_reg_values)

summary(D_all_val)

MAPE_hourly=(MAPE_0+MAPE_1+MAPE_2+MAPE_3+MAPE_4+MAPE_5+MAPE_6+MAPE_7+MAPE_8+MAPE_9+
            MAPE_10+MAPE_11+MAPE_12+MAPE_13+MAPE_14+MAPE_15+MAPE_16+MAPE_17+MAPE_18+MAPE_19+
            MAPE_20+MAPE_21+MAPE_22+MAPE_23)/24


print(paste("MAPE values for Lag_48 results(PART A):",MAPE_48))
print(paste("MAPE values for Lag_168 results(PART A):",MAPE_168))
print(paste("MAPE values for Regression Model:",MAPE_reg))
print(paste("MAPE values(average of all hour) for hourly results(PART C):",MAPE_hourly))
print(paste("MAPE values for Part D results(PART D):",mean(mape_result_d)))

MAPE_dt=data.table("MAPE of Lag 48"=MAPE_48_values,"MAPE of Lag 168"=MAPE_168_values,"MAPE of Regr."=MAPE_reg_values,"MAPE of Seasonality"=C_all_val ,"MAPE of Lasso"=D_all_val)
melted_dt=melt(MAPE_dt)

feature_summary=melted_dt[,list("Average Mape"=mean(value),Minimum=min(value),"Lower Quartile"=quantile(value)[2],
                             "Upper Quartile"=quantile(value)[4],Maximum=max(value),"Standard Deviation"=sd(value))
                               ,by=list(variable)]
feature_summary                     

ggplot(melted_dt,aes(x=variable,y=value*100,fill=variable))+geom_boxplot(position=position_dodge())+
        theme(legend.position = "none")+ggtitle("Comparison of MAPE Values")+xlab("Models")+ylab("MAPE Values (Percentage)")


