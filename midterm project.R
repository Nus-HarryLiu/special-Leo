library(lubridate)  #for preprocessing datas and times

preprocessing = function(old_data){
  #drops two variables: 'casual','registered'
  cf = c("datetime","season","holiday","workingday",
         "weather","temp","atemp","humidity","windspeed") 
  new_data = old_data[, cf]
  
  #add more variances, from datetime
  new_data$hour= hour(old_data$datetime) #hour is an integer
  new_data$year = as.factor(year(old_data$datetime))
  new_data$month = as.factor(month(old_data$datetime))
  new_data$mday =  as.factor(mday(old_data$datetime))
  new_data$wday = ifelse(wday(old_data$datetime)==1, 7, wday(old_data$datetime)-1)
  new_data$wday = as.factor(new_data$wday)
  
  #season
  new_data$season = as.factor(new_data$season)
  
  #weather, make 4 to 3
  new_data$weather[new_data$weather == 4] = 3
  new_data$weather = as.factor(new_data$weather)
  
  #make holiday,workingday factor
  new_data$holiday = as.factor(new_data$holiday)
  new_data$workingday = as.factor(new_data$workingday)
  
  return(new_data)
}

batches = function(dataset){ #find the rows corresponds to the start and end of each month
  
  batch_index = matrix(nrow = 2, ncol = 24)
  cnt = 1
  tt_index = 1:dim(dataset)[1]
  
  for (y in unique(dataset$year)){
    for (m in unique(dataset$month)) { 
      tmp = tt_index[dataset$year == y & dataset$month == m]
      batch_index[1,cnt] = min(tmp)
      batch_index[2,cnt] = max(tmp)
      cnt = cnt + 1
    }
  }
  return(batch_index)
}


output = function(test_datetime, test_count, filename){#output
  
  result = data.frame(datetime = test_datetime, count = test_count)
  write.csv(result, file = filename, row.names=FALSE)
  
}

#preparing data for my future prediction
train = read.csv('/Users/harryliu/Desktop/stat 457/midterm project/train.csv')
test = read.csv('/Users/harryliu/Desktop/stat 457/midterm project/test.csv')
training = preprocessing(train)
training = training[,-c(1,13)]
bsd.train=cbind(training,train$count)
colnames(bsd.train)[13] <- 'count'
train.index=batches(training)
#     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24]
#[1,]    1  432  878 1324 1779 2235 2691 3147 3603  4056  4511  4967  5423  5876  6331  6786  7240  7696  8152  8608  9064  9520  9976 10431
#[2,]  431  877 1323 1778 2234 2690 3146 3602 4055  4510  4966  5422  5875  6330  6785  7239  7695  8151  8607  9063  9519  9975 10430 10886
testing=preprocessing(test)
testing.index=batches(testing)
#     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24]
#[1,]    1  258  461  745 1009 1297 1561 1849 2124  2388  2676  2939  3224  3512  3749  4037  4301  4589  4853  5141  5429  5693  5945  6208
#[2,]  257  460  744 1008 1296 1560 1848 2123 2387  2675  2938  3223  3511  3748  4036  4300  4588  4852  5140  5428  5692  5944  6207  6493
testing=testing[,-c(1,13)]

#Before I deal with with prediction, I need to firstly start with exploratory Data Analysis:
summary(bsd.train)
#season   holiday   workingday weather       temp           atemp          humidity        windspeed           hour         year          month      wday    
#1:2686   0:10575   0:3474     1:7192   Min.   : 0.82   Min.   : 0.76   Min.   :  0.00   Min.   : 0.000   Min.   : 0.00   2011:5422   5      : 912   1:1551  
#2:2733   1:  311   1:7412     2:2834   1st Qu.:13.94   1st Qu.:16.66   1st Qu.: 47.00   1st Qu.: 7.002   1st Qu.: 6.00   2012:5464   6      : 912   2:1539  
#3:2733                        3: 860   Median :20.50   Median :24.24   Median : 62.00   Median :12.998   Median :12.00               7      : 912   3:1551  
#4:2734                                 Mean   :20.23   Mean   :23.66   Mean   : 61.89   Mean   :12.799   Mean   :11.54               8      : 912   4:1553  
#                                       3rd Qu.:26.24   3rd Qu.:31.06   3rd Qu.: 77.00   3rd Qu.:16.998   3rd Qu.:18.00               12     : 912   5:1529  
#                                       Max.   :41.00   Max.   :45.45   Max.   :100.00   Max.   :56.997   Max.   :23.00               10     : 911   6:1584  
#                                                                                                                                     (Other):5415   7:1579  
#count      
#Min.   :  1.0  
#1st Qu.: 42.0  
#Median :145.0  
#Mean   :191.6  
#3rd Qu.:284.0  
#Max.   :977.0 
#By the summary function, it provides total number of observations for each categorical variable and some statistics for others, including other resonce varible:count
#For most of categorical variables, the number of observations for each category is roundly equal with three exceptions: holiday, workingday and weather
#due to whether the day is holiday, whether the day is neither holiday nor weekends and different weather situations.
library(ggplot2)
#scatter plots with one linear line and one fitting curves selected my Rstudio
ggplot(bsd.train,aes(x=temp,y=count))+geom_point(color='purple')+labs(x='temperature', y='count')+geom_smooth(method = 'lm',col='brown')+geom_smooth(method = 'auto')
ggplot(bsd.train,aes(x=atemp,y=count))+geom_point(color='pink')+labs(x='adjusted temperature', y='count')+geom_smooth(method = 'lm',col='brown')+geom_smooth(method = 'auto')
ggplot(bsd.train,aes(x=humidity,y=count))+geom_point(color='green')+labs(x='humidity', y='count')+geom_smooth(method = 'lm',col='brown')+geom_smooth(method = 'auto')
ggplot(bsd.train,aes(x=windspeed,y=count))+geom_point(color='orange')+labs(x='wind speed', y='count')+geom_smooth(method = 'lm',col='brown')+geom_smooth(method = 'auto')
ggplot(bsd.train,aes(x=hour,y=count))+geom_point(color='red')+labs(x='hour as integer', y='count')+geom_smooth(method = 'lm',col='brown')+geom_smooth(method = 'auto')
#box plots:
ggplot(data=bsd.train,aes(x=year,y=count,fill=year))+
  geom_boxplot(outlier.colour="brown",  outlier.size=1, notch=FALSE)+
  labs(x='Year', y= 'Count')
ggplot(data=bsd.train,aes(x=season,y=count,fill=season))+
  geom_boxplot(outlier.colour="brown",  outlier.size=1, notch=FALSE)+
  labs(x='Season', y= 'Count')
ggplot(data=bsd.train,aes(x=holiday,y=count,fill=holiday))+
  geom_boxplot(outlier.colour="brown",  outlier.size=1, notch=FALSE)+
  labs(x='holiday', y= 'Count')
ggplot(data=bsd.train,aes(x=workingday,y=count,fill=workingday))+
  geom_boxplot(outlier.colour="brown",  outlier.size=1, notch=FALSE)+
  labs(x='workingday', y= 'Count')
ggplot(data=bsd.train,aes(x=weather,y=count,fill=weather))+
  geom_boxplot(outlier.colour="brown",  outlier.size=1, notch=FALSE)+
  labs(x='weather', y= 'Count')
ggplot(data=bsd.train,aes(x=wday,y=count,fill=wday))+
  geom_boxplot(outlier.colour="brown",  outlier.size=1, notch=FALSE)+
  labs(x='which day in a week', y= 'Count')
#I reversed the order of analysis of those two plots in the report

#Tree option 1
tree.pred.opt=matrix(NA,nrow=nrow(testing),ncol=1)
library(rpart)
for (k in 1:24)
{
training=bsd.train[1:train.index[2,k],-12]
tree1 = rpart(count ~ ., data = training, control = list(cp=0))
#control = list(cp=0) is extremely importan for prediction using tree
# get index of CP with lowest xerror
opt = which.min(tree1$cptable[, "xerror"])  
cp.opt = tree1$cptable[opt,1]
tree.opt = prune(tree1, cp = cp.opt)
tree.pred.opt[testing.index[1,k]:testing.index[2,k],] = predict(tree.opt, testing[testing.index[1,k]:testing.index[2,k],])
}
output(test$datetime,tree.pred.opt,'/Users/harryliu/Desktop/stat 457/midterm project/tree4.csv')

#Tree option 2 better, but the difference is tiny.
tree.pred.opt=matrix(NA,nrow=nrow(testing),ncol=1)
library(rpart)
for (k in 1:24)
{
  training=bsd.train[1:train.index[2,k],-12]
  tree1 = rpart(count ~ ., data = training, control = list(cp=0))
  # get index of CP with lowest relative error
  opt = which.min(tree1$cptable[, "rel error"])  
  cp.opt = tree1$cptable[opt,1]
  tree.opt = prune(tree1, cp = cp.opt)
  tree.pred.opt[testing.index[1,k]:testing.index[2,k],] = predict(tree.opt, testing[testing.index[1,k]:testing.index[2,k],])
}
output(test$datetime,tree.pred.opt,'/Users/harryliu/Desktop/stat 457/midterm project/tree5.csv')


#random forest
#First consideration
pred.Random=matrix(NA,nrow=nrow(testing),ncol=1)
ms <- c(1:12)
nfold=10
#As I kept trying for future random forest,mtry in this question has the maximum of 12, I change my set of mtry from 1:30 to 1:12
ErrorMatrix = matrix(NA,length(ms),nfold)
for (k in 1:24)
  {
training=bsd.train[train.index[1,k]:train.index[2,k],]
infold <- sample(rep(1:nfold, length.out=ncol(training)))
library(randomForest)
for (j in 1:nfold)
{
  for (i in 1:length(ms))
  {
    rfModel = randomForest(count ~ ., data = training[infold!=j,],
                            mtry = ms[i], importance = TRUE, ntree=600)
    rf.pred = predict(rfModel, training[infold == j,])
    ErrorMatrix[i,j] = mean((rf.pred-training$count[infold==j])^2)
  }
}
optimal.mtry=ms[which.min(apply(ErrorMatrix,1,mean))]
rfModel2 = randomForest(count ~ ., data = training,mtry = optimal.mtry, importance = TRUE, ntree=600)
pred.rf = predict(rfModel2, testing[testing.index[1,k]:testing.index[2,k],])
pred.Random[testing.index[1,k]:testing.index[2,k],]<-pred.rf
}
output(test$datetime,pred.Random,'/Users/harryliu/Desktop/stat 457/midterm project/random forest model4.csv')

#best so far
pred.Random=matrix(NA,nrow=nrow(testing),ncol=1)
ms <- c(1:8)
#It became 1:8 since I kept deleting featured variables
nfold=8
EMatrix = matrix(NA,length(ms),nfold)
#defining error matrix for cross valiadation
library(randomForest)
for (k in 1:24){
  train<-bsd.train[1:train.index[2,k],-c(1,seq(10,12))]
  #read all historicl training data in each loop for each month
  infold = sample(rep(1:nfold, length.out=nrow(train)))
  for (j in 1:nfold){
    for (i in 1:length(ms)){
      rfModel = randomForest(count~., data = train[infold!=j,],mtry=ms[i],importance = TRUE,ntree=20) 
      rf.pred = predict(rfModel, train[infold==j,])
      EMatrix[i,j] = mean((rf.pred - train$count[infold==j])^2)
    }
  }
optimal.mtry=ms[which.min(apply(EMatrix,1,mean))]
rfModel2 = randomForest(count ~ ., data = train,mtry = optimal.mtry, importance = TRUE, ntree=20)
pred.Random[testing.index[1,k]:testing.index[2,k],]<-predict(rfModel2, testing[testing.index[1,k]:testing.index[2,k],])
}
output(test$datetime,pred.Random,'/Users/harryliu/Desktop/stat 457/midterm project/random forest model 31.csv')
  
#trying
pred.Random=matrix(NA,nrow=nrow(testing),ncol=1)
ms <- c(1:8)
nfold=8
EMatrix = matrix(NA,length(ms),nfold)
library(randomForest)
for (k in 1:24){
  train<-bsd.train[train.index[1,k]:train.index[2,k],-c(1,seq(10,12))]
  #read train data for each month, deleting season, year, month and wday
  infold = sample(rep(1:nfold, length.out=nrow(train)))
  for (j in 1:nfold){
    for (i in 1:length(ms)){
      rfModel = randomForest(count~., data = train[infold!=j,],mtry=ms[i],importance = TRUE,ntree=700) 
      rf.pred = predict(rfModel, train[infold==j,])
      EMatrix[i,j] = mean((rf.pred - train$count[infold==j])^2)
    }
  }
  optimal.mtry=ms[which.min(apply(EMatrix,1,mean))]
  rfModel2 = randomForest(count ~ ., data = train,mtry = optimal.mtry, importance = TRUE, ntree=700)
  pred.Random[testing.index[1,k]:testing.index[2,k],]<-predict(rfModel2, testing[testing.index[1,k]:testing.index[2,k],])
}
output(test$datetime,pred.Random,'/Users/harryliu/Desktop/stat 457/midterm project/random forest model 16.csv')

#aborted one
pred.Random=matrix(NA,nrow=nrow(testing),ncol=1)
ms <- c(1:11)
nfold=10
EMatrix = matrix(NA,length(ms),nfold)
library(randomForest)
for (k in 1:24){
  train<-bsd.train[1:train.index[2,k],-12]
  infold = sample(rep(1:nfold, length.out=nrow(train)))
  for (j in 1:nfold){
    for (i in 1:length(ms)){
      rfModel = randomForest(count~., data = train[infold!=j,],mtry=ms[i],importance = TRUE,ntree=100) 
      rf.pred = predict(rfModel, train[infold==j,])
      EMatrix[i,j] = mean((rf.pred - train$count[infold==j])^2)
    }
  }
  optimal.mtry=ms[which.min(apply(EMatrix,1,mean))]
  rfModel2 = randomForest(count ~ ., data = train,mtry = optimal.mtry, importance = TRUE, ntree=100)
  pred.Random[testing.index[1,k]:testing.index[2,k],]<-predict(rfModel2, testing[testing.index[1,k]:testing.index[2,k],])
}
output(test$datetime,pred.Random,'/Users/harryliu/Desktop/stat 457/midterm project/random forest model32.csv')

#boosting tree
boosting=matrix(NA,nrow=nrow(testing),ncol=1)
bsd.train$count=log(bsd.train$count+1)
#The initial result has negative value so that I used professor's code to solve this problem
library(gbm)
for (k in 1:24){
partial.train=bsd.train[1:train.index[2,k],]
#read historical data, no more deletion of featured varibales
boost.training=gbm(count~.,data=partial.train,
                   distribution="gaussian",
                   n.trees=2000,
                   interaction.depth=4,
                   cv.folds = 8)
cv.result=gbm.perf(boost.training)
boosting[testing.index[1,k]:testing.index[2,k],]<-
  predict(boost.training,newdata = testing[testing.index[1,k]:testing.index[2,k],],n.trees = cv.result)
}
boosting=exp(boosting)-1
output(test$datetime,boosting,"/Users/harryliu/Desktop/stat 457/midterm project/boosting tree14.csv")
bsd.train$count=exp(bsd.train$count)-1
#let variable count back to normal

?if


