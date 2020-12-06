getwd()
setwd("C:/Users/bahad/GitHub/IE582/Homework2")
library(data.table)
install.packages("plotly")
library(plotly)


x_train=fread("uWaveGestureLibrary_X_TRAIN")
x_test=fread("uWaveGestureLibrary_X_TEST")
y_train=fread("uWaveGestureLibrary_Y_TRAIN")
y_test=fread("uWaveGestureLibrary_Y_TEST")
z_train=fread("uWaveGestureLibrary_Z_TRAIN")
z_test=fread("uWaveGestureLibrary_Z_TEST")

class1_ind<-which(x_train$V1==1)[1]
class2_ind<-which(x_train$V1==2)[1]
class3_ind<-which(x_train$V1==3)[1]
class4_ind<-which(x_train$V1==4)[1]
class5_ind<-which(x_train$V1==5)[1]
class6_ind<-which(x_train$V1==6)[1]
class7_ind<-which(x_train$V1==7)[1]
class8_ind<-which(x_train$V1==8)[1]
#PART1
indexes<-c(class1_ind,class2_ind,class3_ind,class4_ind,class5_ind,class6_ind,class7_ind,class8_ind)
for(i in 1:8){
  gesture <- data.table(x=t(x_train[indexes[i],2:315]), 
                          y=t(y_train[indexes[i],2:315]),
                          z=t(z_train[indexes[i],2:315]))
  names(gesture)[names(gesture) == "x.V1"] <- "x"
  names(gesture)[names(gesture) == "y.V1"] <- "y"
  names(gesture)[names(gesture) == "z.V1"] <- "z"
  gesture$x_cm=cumsum(gesture$x)
  gesture$y_cm=cumsum(gesture$y)
  gesture$z_cm=cumsum(gesture$z)
  fig <- plot_ly(gesture, x = ~x_cm, y = ~y_cm, z = ~z_cm)
  fig  
}


#PART 2

