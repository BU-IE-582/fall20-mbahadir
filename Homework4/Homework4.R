## Student
library(data.table)
library(glmnet)
library(ggplot2)
library(lubridate, quietly=TRUE)
library(zoo, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(GGally, quietly=TRUE)
library(caTools)
library(rpart)
library(rattle)
library(caret)
library(e1071)
library(randomForest)
library(gbm)
library(fastDummies)

perf_dt=function(type,actual,forecast){
  name=type
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  FBias=sum(error)/sum(actual)
  MPE=sum(error/actual)/n
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2))/n
  MAD=sum(abs(error))/n
  WMAPE=MAD/mean
  l=data.frame(name,n,mean,sd,FBias,MAPE,RMSE,MAD,WMAPE)
  return(l)
}

std_eva=read.csv("turkiye-student-evaluation_R_Specific.csv")
std_eva=as.data.table(std_eva)

for (i in colnames(std_eva)){
  std_eva[[i]]=as.factor(std_eva[[i]])
}

std_eva[,y:=difficulty]
std_eva[,difficulty:=NULL]

str(std_eva)

set.seed(35)
spl=sample.split(std_eva$y, SplitRatio = 0.8)
train=subset(std_eva,spl==TRUE)
test=subset(std_eva,spl==FALSE)

train_mat=data.matrix(train[complete.cases(train),-c("y"),with=F])

result_vec=as.vector(t(train[complete.cases(train),"y"]))

cvfit=cv.glmnet(train_mat,result_vec,family="multinomial",nfolds = 10, type.measure="mse")

test_mat=data.matrix(test[complete.cases(test),-c("y")])

lasso_model_mse_min <- glmnet(train_mat,result_vec, alpha = 1, lambda = cvfit$lambda.min, standardize = FALSE,family="multinomial")
lasso_model_mse_1se <- glmnet(train_mat,result_vec, alpha = 1, lambda = cvfit$lambda.1se, standardize = FALSE,family="multinomial")
lasso_model_mse_10th <- glmnet(train_mat,result_vec, alpha = 1, lambda = cvfit$lambda[10], standardize = FALSE,family="multinomial")

# train_mat=data.matrix(train[complete.cases(train),-c("y"),with=F])

# result_vec=as.vector(t(train[complete.cases(train),"y"]))

# cvfit=cv.glmnet(train_mat,result_vec,family="multinomial",nfolds = 10)

# test_mat=data.matrix(test[complete.cases(test),-c("y")])

# lasso_model <- glmnet(train_mat,result_vec, alpha = 1, lambda = cvfit$lambda.min, standardize = FALSE,family="multinomial")

plot(cvfit)

cvfit$lambda.min

cvfit$lambda.1se

cvfit$lambda[10]

prediction_pra_mse_min <- predict(lasso_model_mse_min, s = cvfit$lambda.min, newx = test_mat, type="class")
prediction_pra_mse_1se <- predict(lasso_model_mse_1se, s = cvfit$lambda.1se, newx = test_mat, type="class")
prediction_pra_mse_10th <- predict(lasso_model_mse_10th, s = cvfit$lambda[10], newx = test_mat, type="class")

train_mat=data.matrix(train[complete.cases(train),-c("y"),with=F])

result_vec=as.vector(t(train[complete.cases(train),"y"]))

cvfit_mae=cv.glmnet(train_mat,result_vec,family="multinomial",nfolds = 10, type.measure="mae")

test_mat=data.matrix(test[complete.cases(test),-c("y")])

lasso_model_mae_min <- glmnet(train_mat,result_vec, alpha = 1, lambda = cvfit_mae$lambda.min, standardize = FALSE,family="multinomial")
lasso_model_mae_1se <- glmnet(train_mat,result_vec, alpha = 1, lambda = cvfit_mae$lambda.1se, standardize = FALSE,family="multinomial")
lasso_model_mae_10th <- glmnet(train_mat,result_vec, alpha = 1, lambda = cvfit_mae$lambda[10], standardize = FALSE,family="multinomial")

plot(cvfit_mae)

cvfit_mae$lambda.min

cvfit_mae$lambda.1se

cvfit_mae$lambda[10]

prediction_pra_mae_min <- predict(lasso_model_mae_min, s = cvfit_mae$lambda.min, newx = test_mat, type="class")
prediction_pra_mae_1se <- predict(lasso_model_mae_1se, s = cvfit_mae$lambda.1se, newx = test_mat, type="class")
prediction_pra_mae_10th <- predict(lasso_model_mae_10th, s = cvfit_mae$lambda[10], newx = test_mat, type="class")

perf_dt("Student Data Set for Lasso Function with min lambda and mse objective", as.numeric(test$y), as.numeric(prediction_pra_mse_min))
perf_dt("Student Data Set for Lasso Function with 1se lambda and mse objective", as.numeric(test$y), as.numeric(prediction_pra_mse_1se))
perf_dt("Student Data Set for Lasso Function with 10th lambda and mse objective", as.numeric(test$y), as.numeric(prediction_pra_mse_10th))

perf_dt("Student Data Set for Lasso Function with min lambda and mae objective", as.numeric(test$y), as.numeric(prediction_pra_mae_min))
perf_dt("Student Data Set for Lasso Function with 1se lambda and mae objective", as.numeric(test$y), as.numeric(prediction_pra_mae_1se))
perf_dt("Student Data Set for Lasso Function with 10th lambda and mae objective", as.numeric(test$y), as.numeric(prediction_pra_mae_10th))

confusionMatrix(data = as.factor(prediction_pra_mae_min), reference = as.factor(test$y), mode = "prec_recall")

fitControl=trainControl(method = "repeatedcv",
                        number = 10) 

lambda_grid <- expand.grid(alpha = 1, lambda = c(cvfit$lambda.min,
                                                 cvfit$lambda.1se,
                                                 cvfit$lambda[10]))

lm_model <- train(y ~ ., data = train, method = "glmnet", 
                  trControl = fitControl, tuneGrid = lambda_grid)

lm_model
plot(lm_model)

set.seed(35)

train_dt=cbind(train[,"y"],dummy_cols(train[,1:32])[,33:196])
test_dt=cbind(test[,"y"],dummy_cols(test[,1:32])[,33:196])

numFolds=trainControl(method="cv",number = 10)
cpGrid=expand.grid(.cp=(0:10)*0.02)
#minbucket_grid=expand.grid(.cp=(5:10))
for(i in 100:105){
  tr=train(y~.,
           data=train_dt, 
           method="rpart",
           trControl=numFolds,
           tuneGrid= cpGrid,
           # minbucket=minbucket_grid
           control= rpart.control(minbucket = i)
  )
  trellis.par.set(caretTheme())
  print(plot(tr))    
  print(tr)
}

numFolds=trainControl(method="cv",number = 10)
cpGrid=expand.grid(.cp=(0:10)*0.01)
tr_last=train(y~.,
              data=train_dt, 
              method="rpart",
              trControl=numFolds,
              tuneGrid= cpGrid,
              # minbucket=minbucket_grid
              control= rpart.control(minsplit = 8)
)
trellis.par.set(caretTheme())
print(plot(tr_last))    
print(tr_last)

reg_tree_std=tr$finalModel
fancyRpartPlot(reg_tree_std)
reg_tree_std$variable.importance

predicted_std=predict(reg_tree_std,newdata=test_dt,type="class")

table(predicted_std)

confusionMatrix(data = as.factor(predicted_std), reference = as.factor(test$y), mode = "prec_recall")

perf_dt("First Data Set for Decision Tree", as.numeric(predicted_std), as.numeric(test$y))

library(ranger)

fitControl=trainControl(method = "repeatedcv",
                        number = 10) 

rf_grid=expand.grid(mtry=c(4,8,10,15),
                    splitrule = c("extratrees"),
                    min.node.size= c(5))
rf_grid  

rf_fit=train(y ~ ., data = train, 
             method = "ranger", 
             trControl = fitControl, num.trees=500,
             tuneGrid = rf_grid) 

rf_fit
plot(rf_fit)

RandomForest_std=predict(rf_fit,newdata=test)

perf_dt("First Data Set for Random Forest", as.numeric(RandomForest_std), as.numeric(test$y))

confusionMatrix(data = as.factor(RandomForest_std), reference = as.factor(test$y), mode = "prec_recall")

perf_dt("First Data Set for Random Forest", as.numeric(RandomForest_std), as.numeric(test$y))

set.seed(35)

gbmGrid=expand.grid(interaction.depth = c(1, 3, 5), 
                    n.trees = (1:5)*50, 
                    shrinkage = c(0.1, 0.3, 0.5),
                    n.minobsinnode = 20)


gbm_fit=train(y ~ ., data = train, 
              method = "gbm", 
              trControl = fitControl,  
              tuneGrid = gbmGrid,
              verbose=F) #verbose is an argument from gbm, prints to screen

plot(gbm_fit)

predicted_sgb=predict(gbm_fit,test)

confusionMatrix(data = as.factor(predicted_sgb), reference = as.factor(test$y), mode = "prec_recall")

perf_dt("First Data Set for Random Forest", as.numeric(predicted_sgb), as.numeric(test$y))

lrp=confusionMatrix(data = as.factor(prediction_pra_mae_min), reference = as.factor(test$y), mode = "prec_recall")
lrp

dtr=confusionMatrix(data = as.factor(predicted_std), reference = as.factor(test$y), mode = "prec_recall")
dtr

rfr=confusionMatrix(data = as.factor(RandomForest_std), reference = as.factor(test$y), mode = "prec_recall")
rfr

sgbr=confusionMatrix(data = as.factor(predicted_sgb), reference = as.factor(test$y), mode = "prec_recall")
sgbr

lm_model

print(lrp$overall[1])

tr_last

print(dtr$overall[1])

rf_fit

print(rfr$overall[1])

gbm_fit

print(sgbr$overall[1])

train=list("Linear Regression with Lasso Results"=lm_model$results$Accuracy[which.min(lm_model$results$Accuracy)],
           "Decision Tree Results"=    tr_last$results$Accuracy[which.min(tr_last$results$Accuracy)],
           "Random Forest Results"=    rf_fit$results$Accuracy[which.min(rf_fit$results$Accuracy)],
           "Result Stochastic Gradient Boosting Results"=    gbm_fit$results$Accuracy[which.min(gbm_fit$results$Accuracy)])

t(train)

test=list("Linear Regression with Lasso Results"=lrp$overall[1],
          "Decision Tree Results"=    dtr$overall[1],
          "Random Forest Results"=    rfr$overall[1],
          "Result Stochastic Gradient Boosting Results"=    sgbr$overall[1])

t(test)



## Pisa

library(data.table)
library(glmnet)
library(ggplot2)
library(lubridate, quietly=TRUE)
library(zoo, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(GGally, quietly=TRUE)
library(caTools)
library(rpart)
library(rattle)
library(caret)
library(e1071)
library(randomForest)
library(gbm)
library(fastDummies)

perf_dt=function(type,actual,forecast){
  name=type
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  FBias=sum(error)/sum(actual)
  MPE=sum(error/actual)/n
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2))/n
  MAD=sum(abs(error))/n
  WMAPE=MAD/mean
  l=data.frame(name,n,mean,sd,FBias,MAPE,RMSE,MAD,WMAPE)
  return(l)
}

pisa_train=read.csv("pisa_train.csv")
pisa_test=read.csv("pisa_test.csv")
pisa_train=as.data.table(pisa_train,na.rm=TRUE)
pisa_test=as.data.table(pisa_test,na.rm=TRUE)
pisa_train=na.omit(pisa_train)
pisa_test=na.omit(pisa_test)

pisa_train$raceeth=as.factor(as.numeric(pisa_train$raceeth))
pisa_test$raceeth=as.factor(as.numeric(pisa_test$raceeth))

str(pisa_train)

str(pisa_test)

train_mat_pisa=data.matrix(pisa_train[complete.cases(pisa_train),-c("readingScore"),with=F])

result_vec_pisa=as.vector(t(pisa_train[complete.cases(pisa_train),"readingScore"]))

cvfit_pisa=cv.glmnet(train_mat_pisa,result_vec_pisa,family="gaussian",nfolds = 10,type.measure = "mse")

test_mat_pisa=data.matrix(pisa_test[complete.cases(pisa_test),-c("readingScore")])

lasso_model_pisa_mse_min <- glmnet(train_mat_pisa,result_vec_pisa, alpha = 1, lambda = cvfit_pisa$lambda.min, standardize = FALSE)
lasso_model_pisa_mse_1se <- glmnet(train_mat_pisa,result_vec_pisa, alpha = 1, lambda = cvfit_pisa$lambda.1se, standardize = FALSE)
lasso_model_pisa_mse_10th <- glmnet(train_mat_pisa,result_vec_pisa, alpha = 1, lambda = cvfit_pisa$lambda[10], standardize = FALSE)

plot(cvfit_pisa)

cvfit_pisa$lambda.min

cvfit_pisa$lambda.1se

cvfit_pisa$lambda[10]

prediction_pra_mse_pisa_min <- predict(lasso_model_pisa_mse_min, s = cvfit_pisa$lambda.min, newx = test_mat_pisa)
prediction_pra_mse_pisa_1se <- predict(lasso_model_pisa_mse_1se, s = cvfit_pisa$lambda.1se, newx = test_mat_pisa)
prediction_pra_mse_pisa_10th <- predict(lasso_model_pisa_mse_10th, s = cvfit_pisa$lambda[10], newx = test_mat_pisa)

train_mat_pisa=data.matrix(pisa_train[complete.cases(pisa_train),-c("readingScore"),with=F])

result_vec_pisa=as.vector(t(pisa_train[complete.cases(pisa_train),"readingScore"]))

cvfit_pisa_mae=cv.glmnet(train_mat_pisa,result_vec_pisa,family="gaussian",nfolds = 10,type.measure="mae")

test_mat_pisa=data.matrix(pisa_test[complete.cases(pisa_test),-c("readingScore")])

lasso_model_pisa_mae_min <- glmnet(train_mat_pisa,result_vec_pisa, alpha = 1, lambda = cvfit_pisa_mae$lambda.min, standardize = FALSE)
lasso_model_pisa_mae_1se <- glmnet(train_mat_pisa,result_vec_pisa, alpha = 1, lambda = cvfit_pisa_mae$lambda.1se, standardize = FALSE)
lasso_model_pisa_mae_10th <- glmnet(train_mat_pisa,result_vec_pisa, alpha = 1, lambda = cvfit_pisa_mae$lambda[10], standardize = FALSE)

plot(cvfit_pisa_mae)

cvfit_pisa_mae$lambda.min

cvfit_pisa_mae$lambda.1se

cvfit_pisa_mae$lambda[10]

prediction_pra_mae_pisa_min <- predict(lasso_model_pisa_mae_min, s = cvfit_pisa_mae$lambda.min, newx = test_mat_pisa)
prediction_pra_mae_pisa_1se <- predict(lasso_model_pisa_mae_1se, s = cvfit_pisa_mae$lambda.1se, newx = test_mat_pisa)
prediction_pra_mae_pisa_10th <- predict(lasso_model_pisa_mae_10th, s = cvfit_pisa_mae$lambda[10], newx = test_mat_pisa)

perf_dt("Pisa Data Set for Lasso Function with min lambda and mse objective", as.numeric(pisa_test$readingScore), prediction_pra_mse_pisa_min)
perf_dt("Pisa Data Set for Lasso Function with 1se lambda and mse objective", as.numeric(pisa_test$readingScore), prediction_pra_mse_pisa_1se)
perf_dt("Pisa Data Set for Lasso Function with 10th lambda and mse objective", as.numeric(pisa_test$readingScore), prediction_pra_mse_pisa_10th)

perf_dt("Pisa Data Set for Lasso Function with min lambda and mae objective", as.numeric(pisa_test$readingScore), prediction_pra_mae_pisa_min)
perf_dt("Pisa Data Set for Lasso Function with 1se lambda and mae objective", as.numeric(pisa_test$readingScore), prediction_pra_mae_pisa_1se)
perf_dt("Pisa Data Set for Lasso Function with 10th lambda and mae objective", as.numeric(pisa_test$readingScore), prediction_pra_mae_pisa_10th)

fitControl=trainControl(method = "repeatedcv",
                        number = 10) 

lambda_grid <- expand.grid(alpha = 1, lambda = c(cvfit_pisa_mae$lambda.min,
                                                 cvfit_pisa_mae$lambda.1se,
                                                 cvfit_pisa_mae$lambda[10]))

lm_model <- train(readingScore ~ ., data = pisa_train, method = "glmnet", 
                  trControl = fitControl, tuneGrid = lambda_grid)

lm_model
plot(lm_model)

set.seed(35)

pisa_train_dt=cbind(pisa_train,dummy_cols(pisa_train$raceeth)[2:8])
pisa_test_dt=cbind(pisa_test,dummy_cols(pisa_test$raceeth)[2:8])
pisa_train_dt[,raceeth:=NULL]
pisa_test_dt[,raceeth:=NULL]

numFolds=trainControl(method="cv",number = 10)
cpGrid=expand.grid(.cp=(0:10)*0.01)
#minbucket_grid=expand.grid(.cp=(5:10))
for(i in 5:10){
  tr=train(readingScore~.,
           data=pisa_train_dt, 
           method="rpart",
           trControl=numFolds,
           tuneGrid= cpGrid,
           # minbucket=minbucket_grid
           control= rpart.control(minbucket = i)
  )
  trellis.par.set(caretTheme())
  print(plot(tr))    
  print(tr)
}

numFolds=trainControl(method="cv",number = 10)
cpGrid=expand.grid(.cp=(0:10)*0.01)
tr_last=train(readingScore~.,
              data=pisa_train_dt, 
              method="rpart",
              trControl=numFolds,
              tuneGrid= cpGrid,
              # minbucket=minbucket_grid
              control= rpart.control(minbucket = 9)
)
trellis.par.set(caretTheme())
print(plot(tr_last))    
print(tr_last)

reg_tree_pisa=tr$finalModel
fancyRpartPlot(reg_tree_pisa)
reg_tree_pisa$variable.importance

predicted_pisa_dt=predict(reg_tree_pisa,newdata=pisa_test_dt)

perf_dt("Decision Tree with CV for Pisa Dataset",pisa_test_dt$readingScore,as.numeric(predicted_pisa_dt))

library(ranger)

fitControl=trainControl(method = "repeatedcv",
                        number = 10) 

rf_grid=expand.grid(mtry=c(4,8,10,15),
                    splitrule = c("variance"),
                    min.node.size= c(5))
rf_grid  

rf_fit=train(readingScore ~ ., data = pisa_train, 
             method = "ranger", 
             trControl = fitControl, num.trees=500,
             tuneGrid = rf_grid) 

rf_fit
plot(rf_fit)

RandomForest_pisa=predict(rf_fit,newdata=pisa_test)

perf_dt("First Data Set for Random Forest", as.numeric(RandomForest_pisa), as.numeric(pisa_test$readingScore))

set.seed(35)

gbmGrid=expand.grid(interaction.depth = c(1, 3, 5), 
                    n.trees = (1:5)*50, 
                    shrinkage = c(0.1, 0.3, 0.5),
                    n.minobsinnode = 20)


gbm_fit=train(readingScore ~ ., data = pisa_train, 
              method = "gbm", 
              trControl = fitControl,  
              tuneGrid = gbmGrid,
              verbose=F) #verbose is an argument from gbm, prints to screen

plot(gbm_fit)

predicted_pisa_sgb=predict(gbm_fit,pisa_test)

perf_dt("First Data Set for Stochastic Gradient Boosting", as.numeric(predicted_pisa_sgb), as.numeric(pisa_test$readingScore))

perf_dt("Pisa Data Set for Lasso Function with min lambda and mse objective", as.numeric(pisa_test$readingScore), prediction_pra_mse_pisa_min)
perf_dt("Decision Tree with CV for Pisa Dataset",pisa_test_dt$readingScore,as.numeric(predicted_pisa_dt))
perf_dt("First Data Set for Random Forest", as.numeric(RandomForest_pisa), as.numeric(pisa_test$readingScore))
perf_dt("First Data Set for Stochastic Gradient Boosting", as.numeric(predicted_pisa_sgb), as.numeric(pisa_test$readingScore))

lm_model

print(paste("The Smallest RMSE value in Penalized Regression Approach:",lm_model$results$RMSE[which.min(lm_model$results$RMSE)]))

tr_last

print(paste("The Smallest RMSE value in Decision Tree:",tr_last$results$RMSE[which.min(tr_last$results$RMSE)]))

rf_fit

print(paste("The Smallest RMSE value in Random Forest:",rf_fit$results$RMSE[which.min(rf_fit$results$RMSE)]))

gbm_fit

print(paste("The Smallest RMSE value in Stochastic Gradient Boosting:",gbm_fit$results$RMSE[which.min(gbm_fit$results$RMSE)]))

train=list("Linear Regression with Lasso Results"=lm_model$results$RMSE[which.min(lm_model$results$RMSE)],
           "Decision Tree Results"=    tr_last$results$RMSE[which.min(tr_last$results$RMSE)],
           "Random Forest Results"=    rf_fit$results$RMSE[which.min(rf_fit$results$RMSE)],
           "Result Stochastic Gradient Boosting Results"=    gbm_fit$results$RMSE[which.min(gbm_fit$results$RMSE)])

t(train)

perf_dt("Pisa Data Set for Lasso Function with min lambda and mse objective", as.numeric(pisa_test$readingScore), prediction_pra_mse_pisa_min)
perf_dt("Decision Tree with CV for Pisa Dataset",pisa_test_dt$readingScore,as.numeric(predicted_pisa_dt))
perf_dt("First Data Set for Random Forest", as.numeric(RandomForest_pisa), as.numeric(pisa_test$readingScore))
perf_dt("First Data Set for Stochastic Gradient Boosting", as.numeric(predicted_pisa_sgb), as.numeric(pisa_test$readingScore))



## Blog

library(data.table)
library(glmnet)
library(ggplot2)
library(lubridate, quietly=TRUE)
library(zoo, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(GGally, quietly=TRUE)
library(caTools)
library(rpart)
library(rattle)
library(caret)
library(e1071)
library(randomForest)
library(gbm)

perf_dt=function(type,actual,forecast){
  name=type
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  FBias=sum(error)/sum(actual)
  MPE=sum(error/actual)/n
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2))/n
  MAD=sum(abs(error))/n
  WMAPE=MAD/mean
  l=data.frame(name,n,mean,sd,FBias,MAPE,RMSE,MAD,WMAPE)
  return(l)
}

blog_data=read.csv("blogData_train.csv")
setnames(blog_data,"X1.0.2", "target")

blog_data=as.data.table(blog_data)
blog_data=blog_data[0:10000]
blog_data=na.omit(blog_data)
str(blog_data)

set.seed(35)
spl=sample.split(blog_data$target, SplitRatio = 0.8)
blog_train=subset(blog_data,spl==TRUE)
blog_test=subset(blog_data,spl==FALSE)

train_mat_blog=data.matrix(blog_train[complete.cases(blog_train),-c("target"),with=F])

result_vec_blog=as.vector(t(blog_train[complete.cases(blog_train),"target"]))

cvfit_blog=cv.glmnet(train_mat_blog,result_vec_blog,family="gaussian",nfolds = 10,type.measure = "mse")

test_mat_blog=data.matrix(blog_test[complete.cases(blog_test),-c("target")])

lasso_model_blog_mse_min <- glmnet(train_mat_blog,result_vec_blog, alpha = 1, lambda = cvfit_blog$lambda.min, standardize = FALSE)
lasso_model_blog_mse_1se <- glmnet(train_mat_blog,result_vec_blog, alpha = 1, lambda = cvfit_blog$lambda.1se, standardize = FALSE)
lasso_model_blog_mse_10th <- glmnet(train_mat_blog,result_vec_blog, alpha = 1, lambda = cvfit_blog$lambda[10], standardize = FALSE)

plot(cvfit_blog)

cvfit_blog$lambda.min

cvfit_blog$lambda.1se

cvfit_blog$lambda[10]

prediction_pra_mse_blog_min <- predict(lasso_model_blog_mse_min, s = cvfit_blog$lambda.min, newx = test_mat_blog)
prediction_pra_mse_blog_1se <- predict(lasso_model_blog_mse_1se, s = cvfit_blog$lambda.1se, newx = test_mat_blog)
prediction_pra_mse_blog_10th <- predict(lasso_model_blog_mse_10th, s = cvfit_blog$lambda[10], newx = test_mat_blog)

train_mat_blog=data.matrix(blog_train[complete.cases(blog_train),-c("target"),with=F])

result_vec_blog=as.vector(t(blog_train[complete.cases(blog_train),"target"]))

cvfit_blog_mae=cv.glmnet(train_mat_blog,result_vec_blog,family="gaussian",nfolds = 10,type.measure = "mae")

test_mat_blog=data.matrix(blog_test[complete.cases(blog_test),-c("target")])

lasso_model_blog_mae_min <- glmnet(train_mat_blog,result_vec_blog, alpha = 1, lambda = cvfit_blog_mae$lambda.min, standardize = FALSE)
lasso_model_blog_mae_1se <- glmnet(train_mat_blog,result_vec_blog, alpha = 1, lambda = cvfit_blog_mae$lambda.1se, standardize = FALSE)
lasso_model_blog_mae_10th <- glmnet(train_mat_blog,result_vec_blog, alpha = 1, lambda = cvfit_blog_mae$lambda[10], standardize = FALSE)

plot(cvfit_blog_mae)

cvfit_blog_mae$lambda.min

cvfit_blog_mae$lambda.1se

cvfit_blog_mae$lambda[10]

prediction_pra_mae_blog_min <- predict(lasso_model_blog_mae_min, s = cvfit_blog_mae$lambda.min, newx = test_mat_blog)
prediction_pra_mae_blog_1se <- predict(lasso_model_blog_mae_1se, s = cvfit_blog_mae$lambda.1se, newx = test_mat_blog)
prediction_pra_mae_blog_10th <- predict(lasso_model_blog_mae_10th, s = cvfit_blog_mae$lambda[10], newx = test_mat_blog)

perf_dt("Blog Data Set for Lasso Function with min lambda and mse objective", as.numeric(blog_test$target), prediction_pra_mse_blog_min)
perf_dt("Blog Data Set for Lasso Function with 1se lambda and mse objective", as.numeric(blog_test$target), prediction_pra_mse_blog_1se)
perf_dt("Blog Data Set for Lasso Function with 10th lambda and mse objective", as.numeric(blog_test$target), prediction_pra_mse_blog_10th)

perf_dt("Blog Data Set for Lasso Function with min lambda and mae objective", as.numeric(blog_test$target), prediction_pra_mae_blog_min)
perf_dt("Blog Data Set for Lasso Function with 1se lambda and mae objective", as.numeric(blog_test$target), prediction_pra_mae_blog_1se)
perf_dt("Blog Data Set for Lasso Function with 10th lambda and mae objective", as.numeric(blog_test$target), prediction_pra_mae_blog_10th)

fitControl=trainControl(method = "repeatedcv",
                        number = 10) 

lambda_grid <- expand.grid(alpha = 1, lambda = c(cvfit_blog_mae$lambda.min,
                                                 cvfit_blog_mae$lambda.1se,
                                                 cvfit_blog_mae$lambda[10]))

lm_model <- train(target ~ ., data = blog_train, method = "glmnet", 
                  trControl = fitControl, tuneGrid = lambda_grid)

lm_model
plot(lm_model)

set.seed(35)

cpGrid=expand.grid(.cp=(0:10)*0.01)
for(i in 5:10){
  tr=train(target~.,
           data=blog_train, 
           method="rpart",
           trControl=fitControl,
           tuneGrid= cpGrid,
           # minbucket=minbucket_grid
           control= rpart.control(minbucket = i)
  )
  trellis.par.set(caretTheme())
  print(plot(tr))    
  print(tr)
}

cpGrid=expand.grid(.cp=(0:10)*0.01)
tr_last=train(target~.,
              data=blog_train, 
              method="rpart",
              trControl=fitControl,
              tuneGrid= cpGrid,
              control= rpart.control(minsplit = 8)
)
trellis.par.set(caretTheme())
print(plot(tr_last))    
print(tr_last)

reg_tree_blog=tr_last$finalModel
fancyRpartPlot(reg_tree_blog)
reg_tree_blog$variable.importance

predicted_blog_dt=predict(reg_tree_blog,newdata=blog_test)

perf_dt("Decision Tree with CV for Blog Comment Dataset",blog_test$target,as.numeric(predicted_blog_dt))

library(ranger)

rf_grid=expand.grid(mtry=c(4,8,10,15),
                    splitrule = c("variance"),
                    min.node.size= c(5))
rf_grid  

rf_fit=train(target ~ ., data = blog_train, 
             method = "ranger", 
             trControl = fitControl, num.trees=500,
             tuneGrid = rf_grid) 

rf_fit
plot(rf_fit)

RandomForest_blog=predict(rf_fit,newdata=blog_test)

perf_dt("First Data Set for Random Forest", as.numeric(blog_test$target), as.numeric(RandomForest_blog))

set.seed(35)

blog_train[,X0.0.1:=NULL]
blog_train[,X0.0.2:=NULL]
blog_train[,X0.0.5:=NULL]
blog_train[,X0.0.7:=NULL]
blog_train[,X0.0.8:=NULL]
blog_train[,X0.0.9:=NULL]
blog_train[,X0.0.10:=NULL]
blog_train[,X0.0.11:=NULL]
blog_train[,X0.0.13:=NULL]
blog_train[,X0.0.234:=NULL]

blog_test[,X0.0.1:=NULL]
blog_test[,X0.0.2:=NULL]
blog_test[,X0.0.5:=NULL]
blog_test[,X0.0.7:=NULL]
blog_test[,X0.0.8:=NULL]
blog_test[,X0.0.9:=NULL]
blog_test[,X0.0.10:=NULL]
blog_test[,X0.0.11:=NULL]
blog_test[,X0.0.13:=NULL]
blog_test[,X0.0.234:=NULL]

gbmGrid=expand.grid(interaction.depth = c(1, 3, 5), 
                    n.trees = (1:5)*25, 
                    shrinkage = c(0.1, 0.3, 0.5),
                    n.minobsinnode = 20)


gbm_fit=train(target ~ ., data = blog_train, 
              method = "gbm", 
              trControl = fitControl,  
              tuneGrid = gbmGrid,
              verbose=F)

gbm_fit
plot(gbm_fit)

predicted_blog_sgb=predict(gbm_fit,blog_test)

perf_dt("First Data Set for Stochastic Gradient Boosting",as.numeric(blog_test$target), as.numeric(predicted_blog_sgb))

perf_dt("Blog Comment Set for Lasso Function with min lambda and mse objective", as.numeric(blog_test$target), prediction_pra_mse_blog_1se)
perf_dt("Decision Tree with CV for Blog Comment Dataset",blog_test$target,as.numeric(predicted_blog_dt))
perf_dt("First Data Set for Random Forest", as.numeric(blog_test$target), as.numeric(RandomForest_blog))
perf_dt("First Data Set for Stochastic Gradient Boosting", as.numeric(blog_test$target), as.numeric(predicted_blog_sgb) )

lm_model

print(paste("The Smallest RMSE value in Penalized Regression Approach:",lm_model$results$RMSE[which.min(lm_model$results$RMSE)]))

tr_last

print(paste("The Smallest RMSE value in Decision Tree:",tr_last$results$RMSE[which.min(tr_last$results$RMSE)]))

rf_fit

print(paste("The Smallest RMSE value in Random Forest:",rf_fit$results$RMSE[which.min(rf_fit$results$RMSE)]))

gbm_fit

print(paste("The Smallest RMSE value in Stochastic Gradient Boosting:",gbm_fit$results$RMSE[which.min(gbm_fit$results$RMSE)]))

train=list("Linear Regression with Lasso Results"=lm_model$results$RMSE[which.min(lm_model$results$RMSE)],
           "Decision Tree Results"=    tr_last$results$RMSE[which.min(tr_last$results$RMSE)],
           "Random Forest Results"=    rf_fit$results$RMSE[which.min(rf_fit$results$RMSE)],
           "Result Stochastic Gradient Boosting Results"=    gbm_fit$results$RMSE[which.min(gbm_fit$results$RMSE)])

t(train)

perf_dt("Blog Comment Data Set for Lasso Function with min lambda and mse objective", as.numeric(blog_test$target), prediction_pra_mse_blog_1se)
perf_dt("Decision Tree with CV for Blog Comment Dataset",blog_test$target,as.numeric(predicted_blog_dt))
perf_dt("First Data Set for Random Forest", as.numeric(blog_test$target), as.numeric(RandomForest_blog))
perf_dt("First Data Set for Stochastic Gradient Boosting", as.numeric(blog_test$target), as.numeric(predicted_blog_sgb) )



## Spam

library(data.table)
library(glmnet)
library(ggplot2)
library(lubridate, quietly=TRUE)
library(zoo, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(GGally, quietly=TRUE)
library(caTools)
library(rpart)
library(rattle)
library(caret)
library(e1071)
library(randomForest)
library(gbm)

perf_dt=function(type,actual,forecast){
  name=type
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  FBias=sum(error)/sum(actual)
  MPE=sum(error/actual)/n
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2))/n
  MAD=sum(abs(error))/n
  WMAPE=MAD/mean
  l=data.frame(name,n,mean,sd,FBias,MAPE,RMSE,MAD,WMAPE)
  return(l)
}

spam=read.csv("spambase.data")
spam=as.data.table(spam[1000:nrow(spam),])
spam=na.omit(spam)
spam$X1=as.factor(spam$X1)

str(spam)

table(spam$X1)

set.seed(35)
spl=sample.split(spam$X1, SplitRatio = 0.8)
train_spam=subset(spam,spl==TRUE)
test_spam=subset(spam,spl==FALSE)

train_mat_spam=data.matrix(train_spam[complete.cases(train_spam),-c("X1"),with=F])

result_vec_spam=as.vector(t(train_spam[complete.cases(train_spam),"X1"]))

cvfit_spam=cv.glmnet(train_mat_spam,result_vec_spam,family="binomial",nfolds = 10,type.measure = "mse")

test_mat_spam=data.matrix(test_spam[complete.cases(test_spam),-c("X1")])

lasso_model_spam_mse_min <- glmnet(train_mat_spam,result_vec_spam,family="binomial", alpha = 1, lambda = cvfit_spam$lambda.min, standardize = FALSE)
lasso_model_spam_mse_1se <- glmnet(train_mat_spam,result_vec_spam,family="binomial", alpha = 1, lambda = cvfit_spam$lambda.1se, standardize = FALSE)
lasso_model_spam_mse_10th <- glmnet(train_mat_spam,result_vec_spam,family="binomial", alpha = 1, lambda = cvfit_spam$lambda[10], standardize = FALSE)

plot(cvfit_spam)

cvfit_spam$lambda.min

cvfit_spam$lambda.1se

cvfit_spam$lambda[10]

prediction_pra_mse_spam_min <- predict(lasso_model_spam_mse_min, s = cvfit_spam$lambda.min, newx = test_mat_spam,type="class")
prediction_pra_mse_spam_1se <- predict(lasso_model_spam_mse_1se, s = cvfit_spam$lambda.1se, newx = test_mat_spam,type="class")
prediction_pra_mse_spam_10th <-predict(lasso_model_spam_mse_10th, s = cvfit_spam$lambda[10], newx = test_mat_spam,type="class")

train_mat_spam=data.matrix(train_spam[complete.cases(train_spam),-c("X1"),with=F])

result_vec_spam=as.vector(t(train_spam[complete.cases(train_spam),"X1"]))

cvfit_spam_mae=cv.glmnet(train_mat_spam,result_vec_spam,family="binomial",nfolds = 10,type.measure = "mae")

test_mat_spam=data.matrix(test_spam[complete.cases(test_spam),-c("X1")])

lasso_model_spam_mae_min <- glmnet(train_mat_spam,result_vec_spam,family="binomial", alpha = 1, lambda = cvfit_spam_mae$lambda.min, standardize = FALSE)
lasso_model_spam_mae_1se <- glmnet(train_mat_spam,result_vec_spam, family="binomial",alpha = 1, lambda = cvfit_spam_mae$lambda.1se, standardize = FALSE)
lasso_model_spam_mae_10th <- glmnet(train_mat_spam,result_vec_spam,family="binomial", alpha = 1, lambda = cvfit_spam_mae$lambda[10], standardize = FALSE)

plot(cvfit_spam_mae)
getwd()
cvfit_spam_mae$lambda.min

cvfit_spam_mae$lambda.1se

cvfit_spam_mae$lambda[10]

prediction_pra_mae_spam_min <- predict(lasso_model_spam_mae_min, s = cvfit_spam_mae$lambda.min, newx = test_mat_spam,type="class")
prediction_pra_mae_spam_1se <- predict(lasso_model_spam_mae_1se, s = cvfit_spam_mae$lambda.1se, newx = test_mat_spam,type="class")
prediction_pra_mae_spam_10th <- predict(lasso_model_spam_mae_10th, s = cvfit_spam_mae$lambda[10], newx = test_mat_spam,type="response")

perf_dt("Spam Data Set for Lasso Function with min lambda and mse objective", as.numeric(test_spam$X1), as.numeric(prediction_pra_mse_spam_min))
perf_dt("Spam Data Set for Lasso Function with 1se lambda and mse objective", as.numeric(test_spam$X1), as.numeric(prediction_pra_mse_spam_1se))
perf_dt("Spam Data Set for Lasso Function with 10th lambda and mse objective", as.numeric(test_spam$X1), as.numeric(prediction_pra_mse_spam_10th))

perf_dt("Spam Data Set for Lasso Function with min lambda and mae objective", as.numeric(test_spam$X1), as.numeric(prediction_pra_mae_spam_min))
perf_dt("Spam Data Set for Lasso Function with 1se lambda and mae objective", as.numeric(test_spam$X1), as.numeric(prediction_pra_mae_spam_1se))
perf_dt("Spam Data Set for Lasso Function with 10th lambda and mae objective", as.numeric(test_spam$X1), as.numeric(prediction_pra_mae_spam_10th))

confusionMatrix(data = as.factor(prediction_pra_mae_spam_min), reference = as.factor(test_spam$X1), mode = "prec_recall")

fitControl=trainControl(method = "repeatedcv",
                        number = 10) 

lambda_grid <- expand.grid(alpha = 1, lambda = c(cvfit_spam_mae$lambda.min,
                                                 cvfit_spam_mae$lambda.1se,
                                                 cvfit_spam_mae$lambda[10]))

lm_model <- train(X1 ~ ., data = train_spam, method = "glmnet", 
                  trControl = fitControl, tuneGrid = lambda_grid)

lm_model
plot(lm_model)

set.seed(35)

numFolds=trainControl(method="cv",number = 10)
cpGrid=expand.grid(.cp=(0:10)*0.02)
#minbucket_grid=expand.grid(.cp=(5:10))
for(i in 5:10){
  tr=train(X1~.,
           data=train_spam, 
           method="rpart",
           trControl=numFolds,
           tuneGrid= cpGrid,
           # minbucket=minbucket_grid
           control= rpart.control(minsplit = i)
  )
  trellis.par.set(caretTheme())
  print(plot(tr))    
  print(tr)
}

numFolds=trainControl(method="cv",number = 10)
cpGrid=expand.grid(.cp=(0:10)*0.01)
tr_last=train(X1~.,
              data=train_spam, 
              method="rpart",
              trControl=numFolds,
              tuneGrid= cpGrid,
              # minbucket=minbucket_grid
              control= rpart.control(minbucket = 10)
)
trellis.par.set(caretTheme())
print(plot(tr_last))    
print(tr_last)

reg_tree_spam=tr_last$finalModel
fancyRpartPlot(reg_tree_spam)
reg_tree_spam$variable.importance

predicted_spam=predict(reg_tree_spam,newdata=test_spam,type="class")

table(test_spam$X1,predicted_spam)

confusionMatrix(data = as.factor(predicted_spam), reference = as.factor(test_spam$X1), mode = "prec_recall")

perf_dt("Decision Tree-Daily",as.numeric(test_spam$X1),as.numeric(predicted_spam))

library(ranger)

fitControl=trainControl(method = "repeatedcv",
                        number = 10) 

rf_grid=expand.grid(mtry=c(4,8,10,15),
                    splitrule = c("extratrees"),
                    min.node.size= c(5))
rf_grid  

rf_fit=train(X1 ~ ., data = train_spam, 
             method = "ranger", 
             trControl = fitControl, num.trees=500,
             tuneGrid = rf_grid) 

rf_fit
plot(rf_fit)

PredictRandomForest_spam=predict(rf_fit,newdata=test_spam)

confusionMatrix(data = as.factor(PredictRandomForest_spam), reference = as.factor(test_spam$X1), mode = "prec_recall")

perf_dt("Random Forest-daily",as.numeric(test_spam$X1),as.numeric(PredictRandomForest_spam))

set.seed(35)

gbmGrid=expand.grid(interaction.depth = c(1, 3, 5), 
                    n.trees = (1:5)*50, 
                    shrinkage = c(0.1, 0.3, 0.5),
                    n.minobsinnode = 20)


gbm_fit=train(X1 ~ ., data = train_spam, 
              method = "gbm", 
              trControl = fitControl,  
              tuneGrid = gbmGrid,
              verbose=F) #verbose is an argument from gbm, prints to screen

plot(gbm_fit)

predicted_spam_sgb=predict(gbm_fit,test_spam)

confusionMatrix(data = as.factor(predicted_spam_sgb), reference = as.factor(test_spam$X1), mode = "prec_recall")

perf_dt("First Data Set for Stochastic Gradient Boosting", as.numeric(predicted_spam_sgb), as.numeric(test_spam$X1))

lrp=confusionMatrix(data = as.factor(prediction_pra_mae_spam_min), reference = as.factor(test_spam$X1), mode = "prec_recall")
lrp

dtr=confusionMatrix(data = as.factor(predicted_spam), reference = as.factor(test_spam$X1), mode = "prec_recall")
dtr

rfr=confusionMatrix(data = as.factor(PredictRandomForest_spam), reference = as.factor(test_spam$X1), mode = "prec_recall")
rfr

sgbr=confusionMatrix(data = as.factor(predicted_spam_sgb), reference = as.factor(test_spam$X1), mode = "prec_recall")
sgbr

lm_model

print(lrp$overall[1])

tr_last

print(dtr$overall[1])

rf_fit

print(rfr$overall[1])

gbm_fit

print(sgbr$overall[1])

train=list("Linear Regression with Lasso Results"=lm_model$results$Accuracy[which.min(lm_model$results$Accuracy)],
           "Decision Tree Results"=    tr_last$results$Accuracy[which.min(tr_last$results$Accuracy)],
           "Random Forest Results"=    rf_fit$results$Accuracy[which.min(rf_fit$results$Accuracy)],
           "Result Stochastic Gradient Boosting Results"=    gbm_fit$results$Accuracy[which.min(gbm_fit$results$Accuracy)])

t(train)

test=list("Linear Regression with Lasso Results"=lrp$overall[1],
          "Decision Tree Results"=    dtr$overall[1],
          "Random Forest Results"=    rfr$overall[1],
          "Result Stochastic Gradient Boosting Results"=    sgbr$overall[1])

t(test)




