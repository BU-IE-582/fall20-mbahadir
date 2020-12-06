getwd()
library(data.table)
#install.packages("scatter3d")
#intall.packages("PerformanceAnalytics")

library(plotly)
library(scatterplot3d)

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
indexes<-c(class1_ind,class2_ind,class3_ind,class4_ind,class5_ind,class6_ind,class7_ind,class8_ind)
indexes

#PART1
gesture <- data.table(x=t(x_train[indexes[1],-1]), 
                      y=t(y_train[indexes[1],-1]),
                      z=t(z_train[indexes[1],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location)

gesture <- data.table(x=t(x_train[indexes[2],-1]), 
                      y=t(y_train[indexes[2],-1]),
                      z=t(z_train[indexes[2],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location)

gesture <- data.table(x=t(x_train[indexes[3],-1]), 
                      y=t(y_train[indexes[3],-1]),
                      z=t(z_train[indexes[3],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location)

gesture <- data.table(x=t(x_train[indexes[4],-1]), 
                      y=t(y_train[indexes[4],-1]),
                      z=t(z_train[indexes[4],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location)

gesture <- data.table(x=t(x_train[indexes[5],-1]), 
                      y=t(y_train[indexes[5],-1]),
                      z=t(z_train[indexes[5],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location)

gesture <- data.table(x=t(x_train[indexes[6],-1]), 
                      y=t(y_train[indexes[6],-1]),
                      z=t(z_train[indexes[6],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location)

gesture <- data.table(x=t(x_train[indexes[7],-1]), 
                      y=t(y_train[indexes[7],-1]),
                      z=t(z_train[indexes[7],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location)

gesture <- data.table(x=t(x_train[indexes[8],-1]), 
                      y=t(y_train[indexes[8],-1]),
                      z=t(z_train[indexes[8],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location)


