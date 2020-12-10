library(data.table)
library(plotly, quietly = TRUE)
library(scatterplot3d,  quietly = TRUE)
library(data.table,  quietly = TRUE)
library(dplyr,  quietly = TRUE)
library(ggfortify)

x_train=fread("uWaveGestureLibrary_X_TRAIN")
x_test=fread("uWaveGestureLibrary_X_TEST")
y_train=fread("uWaveGestureLibrary_Y_TRAIN")
y_test=fread("uWaveGestureLibrary_Y_TEST")
z_train=fread("uWaveGestureLibrary_Z_TRAIN")
z_test=fread("uWaveGestureLibrary_Z_TEST")

class1_ind<-which(x_train$V1==1)[1:2]
class2_ind<-which(x_train$V1==2)[1:2]
class3_ind<-which(x_train$V1==3)[1:2]
class4_ind<-which(x_train$V1==4)[1:2]
class5_ind<-which(x_train$V1==5)[1:2]
class6_ind<-which(x_train$V1==6)[1:2]
class7_ind<-which(x_train$V1==7)[1:2]
class8_ind<-which(x_train$V1==8)[1:2]
indexes<-c(class1_ind,class2_ind,class3_ind,class4_ind,class5_ind,class6_ind,class7_ind,class8_ind)


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
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location, xlab = "X Axis", ylab = "Y Axis",
              zlab = "Z Axis", main="Gesture 1")

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
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location, xlab = "X Axis", ylab = "Y Axis",
              zlab = "Z Axis", main="Gesture 2")

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
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location, xlab = "X Axis", ylab = "Y Axis",
              zlab = "Z Axis", main="Gesture 3")

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
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location, xlab = "X Axis", ylab = "Y Axis",
              zlab = "Z Axis", main="Gesture 4")

gesture <- data.table(x=t(x_train[indexes[9],-1]), 
                      y=t(y_train[indexes[9],-1]),
                      z=t(z_train[indexes[9],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location, xlab = "X Axis", ylab = "Y Axis",
              zlab = "Z Axis", main="Gesture 5")

gesture <- data.table(x=t(x_train[indexes[11],-1]), 
                      y=t(y_train[indexes[11],-1]),
                      z=t(z_train[indexes[11],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location, xlab = "X Axis", ylab = "Y Axis",
              zlab = "Z Axis", main="Gesture 6")

gesture <- data.table(x=t(x_train[indexes[13],-1]), 
                      y=t(y_train[indexes[13],-1]),
                      z=t(z_train[indexes[13],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location, xlab = "X Axis", ylab = "Y Axis",
              zlab = "Z Axis", main="Gesture 7")

gesture <- data.table(x=t(x_train[indexes[15],-1]), 
                      y=t(y_train[indexes[15],-1]),
                      z=t(z_train[indexes[15],-1]))
names(gesture)[names(gesture) == "x.V1"] <- "x"
names(gesture)[names(gesture) == "y.V1"] <- "y"
names(gesture)[names(gesture) == "z.V1"] <- "z"
gesture$x_v=cumsum(gesture$x)
gesture$y_v=cumsum(gesture$y)
gesture$z_v=cumsum(gesture$z)
gesture$x_location=cumsum(gesture$x_v)
gesture$y_location=cumsum(gesture$y_v)
gesture$z_location=cumsum(gesture$z_v)
scatterplot3d(gesture$x_location,gesture$y_location,gesture$z_location, xlab = "X Axis", ylab = "Y Axis",
              zlab = "Z Axis", main="Gesture 8")

train_xpos=transpose(cumsum(transpose(x_train[,-1])))
train_xpos$class=x_train[,1]
train_xpos<-train_xpos %>%
    mutate("id" = seq.int(nrow(train_xpos)))%>%
    melt(id.vars = c("id","class"),value.name="X")

train_ypos=transpose(cumsum(transpose(y_train[,-1])))
train_ypos$class=y_train[,1]
train_ypos<-train_ypos %>%
    mutate("id" = seq.int(nrow(train_ypos)))%>%
    melt(id.vars = c("id","class"),value.name="Y")

train_zpos=transpose(cumsum(transpose(z_train[,-1])))
train_zpos$class=z_train[,1]
train_zpos<-train_zpos %>%
    mutate("id" = seq.int(nrow(train_zpos)))%>%
    melt(id.vars = c("id","class"),value.name="Z")

long_form=copy(train_xpos)
long_form$Y=train_ypos$Y
long_form$Z=train_zpos$Z
long_form=long_form[,c(1,3,4,5,6,2)]
long_form$variable=as.integer(long_form$variable)
colnames(long_form)[1] <- "time_series_id"
colnames(long_form)[2] <- "time_index"
setorder(long_form, time_series_id)

long_form

long_form_pca <-prcomp(long_form[,c(3,4,5)], center = TRUE,scale. = TRUE)
long_form_pca
summary(long_form_pca)

long_form$class=as.factor(long_form$class)

autoplot(long_form_pca, data = long_form, colour = "class",
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

#long_form_pca1<- princomp(long_form[,c(3,4,5)], cor = TRUE)
#summary(long_form_pca1,loadings=TRUE)

class1=filter(long_form, time_series_id == indexes[1] | time_series_id == indexes[2]) 
class1=mutate(class1, red_dim= 0.2325778 * X + 0.7230122  * Y + 0.6505082   * Z )
class1$time_series_id=as.factor(class1$time_series_id)
class1$time_index=as.integer(class1$time_index)
str(class1)

ggplot(data=class1,
       aes(x=time_index, y=red_dim, colour=time_series_id))+
       geom_line()+labs(color="Time Series ID")+xlab("Time Index")+ylab("Reduced Dimension (1D)")+ggtitle("1D Result for class 1")

class2=filter(long_form, time_series_id == indexes[3] | time_series_id == indexes[4]) 
class2=mutate(class2, red_dim= 0.2325778 * X + 0.7230122  * Y + 0.6505082   * Z )
class2$time_series_id=as.factor(class2$time_series_id)
class2$time_index=as.integer(class2$time_index)
str(class2)

ggplot(data=class2,
       aes(x=time_index, y=red_dim, colour=time_series_id)) +
       geom_line()+labs(color="Time Series ID")+xlab("Time Index")+ylab("Reduced Dimension (1D)")+ggtitle("1D Result for class 2")

class3=filter(long_form, time_series_id == indexes[5] | time_series_id == indexes[6]) 
class3=mutate(class3, red_dim= 0.2325778 * X + 0.7230122  * Y + 0.6505082   * Z )
class3$time_series_id=as.factor(class3$time_series_id)
class3$time_index=as.integer(class3$time_index)
str(class3)

ggplot(data=class3,
       aes(x=time_index, y=red_dim, colour=time_series_id)) +
       geom_line()+labs(color="Time Series ID")+xlab("Time Index")+ylab("Reduced Dimension (1D)")+ggtitle("1D Result for class 3")

class4=filter(long_form, time_series_id == indexes[7] | time_series_id == indexes[8]) 
class4=mutate(class4, red_dim= 0.2325778 * X + 0.7230122  * Y + 0.6505082   * Z )
class4$time_series_id=as.factor(class4$time_series_id)
class4$time_index=as.integer(class4$time_index)
str(class4)

ggplot(data=class4,
       aes(x=time_index, y=red_dim, colour=time_series_id)) +
       geom_line()+labs(color="Time Series ID")+xlab("Time Index")+ylab("Reduced Dimension (1D)")+ggtitle("1D Result for class 4")

class5=filter(long_form, time_series_id == indexes[9] | time_series_id == indexes[10]) 
class5=mutate(class5, red_dim= 0.2325778 * X + 0.7230122  * Y + 0.6505082   * Z )
class5$time_series_id=as.factor(class5$time_series_id)
class5$time_index=as.integer(class5$time_index)
str(class5)

ggplot(data=class5,
       aes(x=time_index, y=red_dim, colour=time_series_id)) +
       geom_line()+labs(color="Time Series ID")+xlab("Time Index")+ylab("Reduced Dimension (1D)")+ggtitle("1D Result for class 5")

class6=filter(long_form, time_series_id == indexes[11] | time_series_id == indexes[12]) 
class6=mutate(class6, red_dim= 0.2325778 * X + 0.7230122  * Y + 0.6505082   * Z )
class6$time_series_id=as.factor(class6$time_series_id)
class6$time_index=as.integer(class6$time_index)
str(class6)

ggplot(data=class6,
       aes(x=time_index, y=red_dim, colour=time_series_id)) +
       geom_line()+labs(color="Time Series ID")+xlab("Time Index")+ylab("Reduced Dimension (1D)")+ggtitle("1D Result for class 6")

class7=filter(long_form, time_series_id == indexes[13] | time_series_id == indexes[14]) 
class7=mutate(class7, red_dim= 0.2325778 * X + 0.7230122  * Y + 0.6505082   * Z )
class7$time_series_id=as.factor(class7$time_series_id)
class7$time_index=as.integer(class7$time_index)
str(class7)

ggplot(data=class7,
       aes(x=time_index, y=red_dim, colour=time_series_id)) +
       geom_line()+labs(color="Time Series ID")+xlab("Time Index")+ylab("Reduced Dimension (1D)")+ggtitle("1D Result for class 7")

class8=filter(long_form, time_series_id == indexes[15] | time_series_id == indexes[16]) 
class8=mutate(class8, red_dim= 0.2325778 * X + 0.7230122  * Y + 0.6505082   * Z )
class8$time_series_id=as.factor(class8$time_series_id)
class8$time_index=as.integer(class8$time_index)
str(class8)

ggplot(data=class8,
       aes(x=time_index, y=red_dim, colour=time_series_id)) +
       geom_line()+labs(color="Time Series ID")+xlab("Time Index")+ylab("Reduced Dimension (1D)")+ggtitle("1D Result for class 8")

class1_filtered<-filter(long_form, class == 1) 
class2_filtered<-filter(long_form, class == 2) 
class3_filtered<-filter(long_form, class == 3) 
class4_filtered<-filter(long_form, class == 4) 
class5_filtered<-filter(long_form, class == 5) 
class6_filtered<-filter(long_form, class == 6) 
class7_filtered<-filter(long_form, class == 7) 
class8_filtered<-filter(long_form, class == 8) 

pca_class1 <-prcomp(class1_filtered[,c(3,4,5)], center = TRUE,scale. = TRUE)
pca_class1
summary(pca_class1)

#autoplot(pca_class1, data = class1_filtered, colour = "class",
#         loadings = TRUE, loadings.colour = 'blue',
#         loadings.label = TRUE, loadings.label.size = 3)
#library(devtools)
#library(devtools)
#install_github("vqv/ggbiplot")
#library(ggbiplot)
#ggbiplot(pca_class1, obs.scale = 1, var.scale = 1,
#  groups = pca_class1, ellipse = TRUE, circle = TRUE) +
#  scale_color_discrete(name = '') +
#  theme(legend.direction = 'horizontal', legend.position = 'top')

pca_class2 <-prcomp(class2_filtered[,c(3,4,5)], center = TRUE,scale. = TRUE)
pca_class2
summary(pca_class2)

pca_class3 <-prcomp(class3_filtered[,c(3,4,5)], center = TRUE,scale. = TRUE)
pca_class3
summary(pca_class3)

pca_class4 <-prcomp(class4_filtered[,c(3,4,5)], center = TRUE,scale. = TRUE)
pca_class4
summary(pca_class4)

pca_class5 <-prcomp(class5_filtered[,c(3,4,5)], center = TRUE,scale. = TRUE)
pca_class5
summary(pca_class5)

pca_class6 <-prcomp(class6_filtered[,c(3,4,5)], center = TRUE,scale. = TRUE)
pca_class6
summary(pca_class6)

pca_class7 <-prcomp(class7_filtered[,c(3,4,5)], center = TRUE,scale. = TRUE)
pca_class7
summary(pca_class7)

pca_class8 <-prcomp(class8_filtered[,c(3,4,5)], center = TRUE,scale. = TRUE)
pca_class8
summary(pca_class8)

x_dis=transpose(cumsum(cumsum(transpose(x_train[,-1]))))
y_dis=transpose(cumsum(cumsum(transpose(y_train[,-1]))))
z_dis=transpose(cumsum(cumsum(transpose(z_train[,-1]))))
df_pos<-cbind(x_train[,1],x_dis,y_dis,z_dis)
colnames(df_pos)[1] <- "class"
#df_pos

distances=dist(df_pos[,-1],method="euclidean")
summary(distances)
str(distances)

fit <- cmdscale(distances,eig=TRUE, k=2) # k is the number of dim
str(fit)

dt<-data.frame(fit$points,df_pos$class)
visualized_dist<-ggplot(dt, aes(x=X1 , y=X2,color=as.factor(df_pos.class)))+geom_point()+labs(color="Classes")+
xlab("X Coordinate")+ylab("Y Coordinate")+theme(legend.position = "bottom")
visualized_dist

#visualized_dist<-autoplot(fit, colour= factor(df_pos$class))

#visualized_dist <- visualized_dist + 
#  ggtitle("Observations") +
#  labs(x = "X Coordinate", y = "Y Coordinate") 
  #labs(colour = as.factor(df_pos$class))
  #scale_color_manual(labels = c("Male", "Female"), values = c(1, 2))
#print(visualized_dist)


