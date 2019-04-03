#DATA EXPLORATION (loading the dataset)--------------------------
setwd("C:/Users/abcdef/Desktop/Air BNB Project")  
train.df <- read.csv("train_users_2.csv",header=TRUE)
age_gender.df<-read.csv("age_gender_bkts.csv", header=TRUE)


#Analyzing the variables for feature selection and data cleaning--------------


#ANALYZING THE VARIABLE AGE------------

summary(train.df)

train.df<-train.df[complete.cases(train.df[ , 6]),]

summary(train.df$age)  #there is a problem with the range of age, with max being 2014, min being 1, which seems problematic


#Further cleaning the age variable------------------

train.df$age[train.df$age<18|train.df$age>100] <- NA

train.df<-train.df[complete.cases(train.df[ , 6]),]
summary(train.df$age)
#cleaned according to age


#ANALYZING THE VARIABLE GENDER------------------
#We can see there is a problem in the gender, there is unknown, replace with na and remove, as well as remove other(as very less values), only keep male, female
train.df$gender[train.df$gender=='-unknown-']<-NA
train.df$gender[train.df$gender=='OTHER']<-NA
train.df<-train.df[complete.cases(train.df[ , 5]),]






# Analyzing DATES-------------------------------------
str(train.df$date_account_created)
#its a factor, so convert into ideal form

train.df$date_account_created<-as.Date(train.df$date_account_created)



#Data manipulation/Wrangling---------------------------

set.seed(42)

train.df<-train.df[-1]
train.df<-train.df[train.df$affiliate_provider!="naver"&train.df$affiliate_provider!="yandex"&train.df$affiliate_provider!="wayn"&train.df$affiliate_provider!="baidu",]
# train.df$agegroup<-cut(train.df$age, breaks = c(18, 35, 50, 70, 100), labels = c("A", "B", "C", "D"), include.lowest = TRUE)
# train.df<-train.df[-5]#Removing numeric age
train.df<-train.df[-c(4,6,8,9)]  #removing useless features
train.df<-train.df[-2]
train.df$affiliate_provider<-factor(train.df$affiliate_provider)
library(plyr)
train.df$first_affiliate_tracked<-factor(train.df$first_affiliate_tracked)
train.df<-train.df[train.df$first_device_type!="SmartPhone (Other)",]
train.df<-train.df[train.df$first_browser!="Android Browser"&train.df$first_browser!="AOL Explorer"&train.df$first_browser!="Apple Mail"&train.df$first_browser!="Avant Browser"&train.df$first_browser!="BlackBerry Browser"&train.df$first_browser!="Camino"&train.df$first_browser!="Chrome Mobile"&train.df$first_browser!="Chromium"&train.df$first_browser!="CometBird"&train.df$first_browser!="Comodo Dragon"&train.df$first_browser!="CoolNovo"&train.df$first_browser!="IceWeasel"&train.df$first_browser!="IE Mobile"&train.df$first_browser!="Iron"&train.df$first_browser!="Kindle Browser"&train.df$first_browser!="Maxthon"&train.df$first_browser!="Mobile Firefox"&train.df$first_browser!="Mozilla"&train.df$first_browser!="NetNewsWire"&train.df$first_browser!="Opera"&train.df$first_browser!="Opera Mobile"&train.df$first_browser!="Pale Moon"&train.df$first_browser!="PS Vita browser"&train.df$first_browser!="RockMelt"&train.df$first_browser!="SeaMonkey"&train.df$first_browser!="Silk"&train.df$first_browser!="SiteKiosk"&train.df$first_browser!="SlimBrowser"&train.df$first_browser!="Sogou Explorer"&train.df$first_browser!="Stainless"&train.df$first_browser!="TenFourFox"&train.df$first_browser!="TheWorld Browser"&train.df$first_browser!="wOSBrowser"&train.df$first_browser!="Yandex.Browser",]
train.df$first_browser<-factor(train.df$first_browser)

#Analyzing the effect of year on the number of users--------------------
datecomp.df<-train.df
datecomp.df$date_account_created<-as.character(datecomp.df$date_account_created)
datecomp.df$date_account_created<-substring(datecomp.df$date_account_created,1,4)
datecomp.df$date_account_created<-as.factor(datecomp.df$date_account_created)
library(ggplot2)
#plotdate<-ggplot(datecomp.df,aes(x=date_account_created))+geom_histogram(stat="count")

#Removing years 2010,2011,2012
train.df$year_account_created<-as.character(train.df$date_account_created)
train.df$year_account_created<-substring(train.df$year_account_created,1,4)
train.df$year_account_created<-as.factor(train.df$year_account_created)
train.df<-train.df[train.df$year_account_created!=2010&train.df$year_account_created!=2011&train.df$year_account_created!=2012,]
train.df<-train.df[-11] #removing year column
train.df$signup_flow<-as.numeric(train.df$signup_flow)



#BINARY CLASSIFICATION (BOOKED OR NOT)-------------------
#Preliminary analysis on a sample datasetfollowed by cros validation on whole dataset--------
trainrf<-train.df
trainrf$country_destination<-ifelse(trainrf$country_destination=="NDF",0,1)
library(ggplot2)
#plotvs<-ggplot(trainrf,aes(x=country_destination))+geom_histogram(stat="count")
trainrf<-trainrf[-c(1,2)]
sample_data<-sample(length(trainrf[,2]),size=0.80*(length(trainrf[,2])),replace=FALSE,set.seed(15))
grandbinary1<-trainrf[sample_data,]
samplebinary1<-trainrf[-sample_data,]
sample_data2<-sample(length(samplebinary1[,2]),size=0.85*(length(samplebinary1[,2])),replace=FALSE,set.seed(15))
trainbinary1<-samplebinary1[sample_data2,]
testbinary1<-samplebinary1[-sample_data2,]






#plotvs1<-ggplot(samplebinary1,aes(x=country_destination))+geom_histogram(stat="count") #good representation of whole data




#Logistic regression-----------------------
library(MASS)
modellogis<-glm(country_destination~.,family=binomial,data=trainbinary1)
summary(modellogis)

library(caret)
predlog<-predict(modellogis,newdata=testbinary1,type="response")
y_pred <- ifelse(predlog > 0.5, 1, 0)
cm<-confusionMatrix(y_pred,testbinary1$country_destination)

#k fold cross validation-----------------------------------------------
library(MASS)
library(caret)


trainrf<-train.df
trainrf$country_destination<-ifelse(trainrf$country_destination=="NDF",0,1)
library(ggplot2)
#plotvs<-ggplot(trainrf,aes(x=country_destination))+geom_histogram(stat="count")
trainrf<-trainrf[-c(1,2)]

#loop-----------------------
set.seed(1)
folds <- sample(rep(1:10, length.out = nrow(trainrf)), size = nrow(trainrf), replace = F)
CV_logistic <- lapply(1:10, function(x){ 
  model <- glm(country_destination~.,family=binomial, trainrf[folds != x, ])
  preds <- predict(model,  trainrf[folds == x,], type="response")
  y_pred <- ifelse(preds > 0.5, 1, 0)
  return(data.frame(y_pred, real = trainrf$country_destination[folds == x]))
})
#this produces a list of hold out predictions, to combine it to a data frame:
  
  CV_logistic<- do.call(rbind, CV_logistic)
  cm<-confusionMatrix(CV_logistic$y_pred, CV_logistic$real)
#roc
  library(pROC)
  auc.logis<-auc(CV_logistic$real,CV_logistic$y_pred)
  plot.logis<-plot(roc(CV_logistic$real,CV_logistic$y_pred))

  
  
  
  

#randomforest---------------------------------

trainbinary1$country_destination<-as.factor(trainbinary1$country_destination)
testbinary1$country_destination<-as.factor(testbinary1$country_destination)
library(randomForest)
modrf<-randomForest(country_destination~.,ntree=500,data=trainbinary1,importance=TRUE)
predrf<-predict(modrf,newdata=testbinary1)
cmrf<-confusionMatrix(predrf,testbinary1$country_destination)
varImpPlot(modrf,type=2)


#kfold---------------------------------------------
library(randomForest)
trainrf$country_destination<-as.factor(trainrf$country_destination)
set.seed(1)
folds <- sample(rep(1:10, length.out = nrow(trainrf)), size = nrow(trainrf), replace = F)
CV_rf <- lapply(1:10, function(x){ 
  modelrf <- randomForest(country_destination~.,ntree=500, trainrf[folds != x, ])
  predrf <- predict(modelrf,  trainrf[folds == x,])
  return(data.frame(predrf, real = trainrf$country_destination[folds == x]))
})

CV_rf<- do.call(rbind, CV_rf)
cmrf<-confusionMatrix(CV_rf$predrf, CV_rf$real)
#roc
library(pROC)
CV_rf$real<-as.numeric(CV_rf$real)
CV_rf$predrf<-as.numeric(CV_rf$predrf)
aucrf<-auc(CV_rf$real,CV_rf$predrf)
plot.rf<-plot(roc(CV_rf$real,CV_rf$predrf))







#rpart--------------------------------------
library(rpart)
modrpart<-rpart(country_destination~.,method='class',data=trainbinary1)
predrpart<-predict(modrpart,testbinary1)
dfpredrpart<-as.data.frame(predrpart)
dfpredrpart$pred<-ifelse(dfpredrpart$`0`>dfpredrpart$`1`,0,1)
cmrpart<-confusionMatrix(dfpredrpart$pred,testbinary1$country_destination)

#k fold-------------------------------
library(rpart)
trainrf$country_destination<-as.factor(trainrf$country_destination)
set.seed(1)
folds <- sample(rep(1:10, length.out = nrow(trainrf)), size = nrow(trainrf), replace = F)
CV_rpart <- lapply(1:10, function(x){ 
  modrpart<-rpart(country_destination~.,method='class', trainrf[folds != x, ])
  predrpart <- predict(modrpart,  trainrf[folds == x,])
  return(data.frame(predrpart, real = trainrf$country_destination[folds == x]))
})

CV_rpart<- do.call(rbind, CV_rpart)
dfpredrpart<-as.data.frame(CV_rpart)
dfpredrpart$pred<-ifelse(dfpredrpart$`X0`>dfpredrpart$`X1`,0,1)

cmrpart<-confusionMatrix(dfpredrpart$pred, CV_rpart$real)

#roc
library(pROC)
CV_rpart$real<-as.numeric(CV_rpart$real)
aucrpart<-auc(CV_rf$real,dfpredrpart$pred)
plot.rpart<-plot(roc(CV_rf$real,dfpredrpart$pred))






#KNN--------------------------------------------------
 #split the data to find optimal k,use dummy(DUMMY VARIABLE CODE IS AT THE BOTTOM)
kbackup<-traindum
kbackup<-kbackup[-c(1,2)]
kbackup$signup_flow<-as.numeric(kbackup$signup_flow)
kbackup$country_destination<-ifelse(kbackup$country_destination=="NDF",0,1)

sample_dataknn<-sample(length(kbackup[,2]),size=0.80*(length(kbackup[,2])),replace=FALSE,set.seed(15))
grand.knn<-kbackup[sample_dataknn,]
sample.knn<-kbackup[-sample_dataknn,] 
sample_knn2<-sample(length(sample.knn[,2]),size=0.85*(length(sample.knn[,2])),replace=FALSE,set.seed(15))
trainknn1<-sample.knn[sample_knn2,]
testknn1<-sample.knn[-sample_knn2,]

train.def <- sample.knn$country_destination[sample_knn2]
test.def <- sample.knn$country_destination[-sample_knn2]
 
 library(class)
 library(caret)
 knn.1 <-  knn(trainknn1, testknn1, train.def, k=1)
 knn.5 <-  knn(trainknn1, testknn1, train.def, k=5)
 knn.20 <- knn(train.knn, test.knn, train.def, k=20)  
  cmknn<-confusionMatrix(knn.5,testknn1$country_destination)
  
  
  
  
  
  #Visual------------------
  library(ElemStatLearn)
  set = testknn1
  X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
  X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
  grid_set = expand.grid(X1, X2)
  colnames(grid_set) = c('Age', 'EstimatedSalary')
  y_grid = knn(trainknn1, testknn1, train.def, k=5)
  plot(set[, -3],
       main = 'K-NN (Test set)',
       
       xlim = range(X1), ylim = range(X2))
  #contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
  points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
  points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
  
  
  
  
#k fold--------------------------------------------
  library(class)
  library(caret)
  kbackup<-traindum
  kbackup<-kbackup[-c(1,2)]
  kbackup$signup_flow<-as.numeric(kbackup$signup_flow)
  kbackup$country_destination<-ifelse(kbackup$country_destination=="NDF",0,1)
  
  set.seed(1)
  folds <- sample(rep(1:10, length.out = nrow(kbackup)), size = nrow(kbackup), replace = F)
  CV_knn <- lapply(1:10, function(x){ 
    
    knn.5 <-  knn(kbackup[folds != x, ],  kbackup[folds == x,], kbackup$country_destination[folds != x], k=5)
    return(data.frame(knn.5, real = kbackup$country_destination[folds == x]))
  })
  
  CV_knn<- do.call(rbind, CV_knn)
  cmknn<-confusionMatrix(CV_knn$knn.5, CV_knn$real)
  
  #roc
  library(pROC)
  CV_knn$real<-as.numeric(CV_knn$real)
  CV_knn$knn.5<-as.numeric(CV_knn$knn.5)
  aucknn<-auc(CV_knn$real,CV_knn$knn.5)
  plot.knn<-plot(roc(CV_knn$real,CV_knn$knn.5))
  
  
  
  
  
  
  
#SVM----------------------------------- 
library(e1071)
  kbackup<-traindum
  kbackup<-kbackup[-c(1,2)]
  kbackup$signup_flow<-as.numeric(kbackup$signup_flow)
  kbackup$country_destination<-ifelse(kbackup$country_destination=="NDF",0,1)
  kbackup$country_destination<-as.factor( kbackup$country_destination)
  
  sample_dataknn<-sample(length(kbackup[,2]),size=0.80*(length(kbackup[,2])),replace=FALSE,set.seed(15))
  grand.knn<-kbackup[sample_dataknn,]
  sample.knn<-kbackup[-sample_dataknn,] 
  sample_knn2<-sample(length(sample.knn[,2]),size=0.85*(length(sample.knn[,2])),replace=FALSE,set.seed(15))
  trainknn1<-sample.knn[sample_knn2,]
  testknn1<-sample.knn[-sample_knn2,]

modelsvm<-svm(country_destination~.,data=trainknn1)
predsvm<-predict(modelsvm,testknn1)
cmsvm<-confusionMatrix(predsvm,testknn1$country_destination)

#K fold----------------------------------- 
library(e1071)
kbackup<-traindum
kbackup<-kbackup[-c(1,2)]
kbackup$signup_flow<-as.numeric(kbackup$signup_flow)
kbackup$country_destination<-ifelse(kbackup$country_destination=="NDF",0,1)
kbackup$country_destination<-as.factor( kbackup$country_destination)

set.seed(1)
folds <- sample(rep(1:10, length.out = nrow(kbackup)), size = nrow(kbackup), replace = F)
CV_svm <- lapply(1:10, function(x){ 
  modsvm<-svm(country_destination~., trainrf[folds != x, ])
  predsvm <- predict(modsvm,  trainrf[folds == x,])
  return(data.frame(predsvm, real = trainrf$country_destination[folds == x]))
})

CV_svm<- do.call(rbind, CV_svm)
cmsvm<-confusionMatrix(CV_svm$predsvm, CV_svm$real)










#Neural Net----------------------------

library(nnet)
library(caret)
b <- nnet(country_destination~., data=trainbinary1, size=150,MaxNWts=84581,maxit=150,decay=.1)
prednnet<-predict(b, newdata = testbinary1)
y_nnet <- ifelse(prednnet > 0.5, 1, 0)
cmnnet<-confusionMatrix(y_nnet,testbinary1$country_destination)






#Naive Bayes--------------------------------
library(e1071)

modelnb<-naiveBayes(country_destination~.,data=trainbinary1)
prednb<-predict(modelnb,testbinary1)

cmnb<-confusionMatrix(prednb,testbinary1$country_destination)

#k fold-----------------------
library(e1071)
library(caret)


trainrf$country_destination<-as.factor(trainrf$country_destination)


set.seed(1)
folds <- sample(rep(1:10, length.out = nrow(trainrf)), size = nrow(trainrf), replace = F)
CV_nb <- lapply(1:10, function(x){ 
  modnb<-naiveBayes(country_destination~., trainrf[folds != x, ])
  prednb <- predict(modnb,  trainrf[folds == x,])
  return(data.frame(prednb, real = trainrf$country_destination[folds == x]))
})

CV_nb<- do.call(rbind, CV_nb)


cmnb<-confusionMatrix(CV_nb$prednb, CV_nb$real)

#roc
library(pROC)
CV_nb$real<-as.numeric(CV_nb$real)
CV_nb$prednb<-as.numeric(CV_nb$prednb)
aucnb<-auc(CV_nb$real,CV_nb$prednb)
plot.nb<-plot(roc(CV_nb$real,CV_nb$prednb))







#MULTI CLASS MODELING----------------------------------------
multi.df<-train.df

multi.df<-multi.df[multi.df$country_destination!='NDF',] #removing NDF
multi.df<-multi.df[multi.df$country_destination!='other',] #removing other, as its not inferencial
multi.df$country_destination<-factor(multi.df$country_destination)
multi.df$date_first_booking<-as.Date(multi.df$date_first_booking)
multi.df$Days_to_book<-multi.df$date_first_booking-multi.df$date_account_created
multi.df<-multi.df[multi.df$Days_to_book>=0,]
multi.df<-multi.df[-c(1,2)]
multi.df$Days_to_book<-as.numeric(multi.df$Days_to_book)
plotvs4<-ggplot(multi.df,aes(x=country_destination))+geom_histogram(stat="count")

multisp<-sample(length(multi.df[,2]),size=0.80*(length(multi.df[,2])),replace=FALSE,set.seed(15))
grand_multi<-multi.df[multisp,]
sample_multi<-multi.df[-multisp,]

multisp2<-sample(length(sample_multi[,2]),size=0.85*(length(sample_multi[,2])),replace=FALSE,set.seed(15))
train.multi<- sample_multi[multisp2,]
test.multi<-sample_multi[-multisp2,]


plotvs5<-ggplot(test_multi,aes(x=country_destination))+geom_histogram(stat="count")

 



#MAP-----------------
library(mapproj)
country_count<-as.data.frame(table(multi.df$country_destination))
country_count$Var1<- sub("AU", "Australia", country_count$Var1)
country_count$Var1<- sub("CA", "Canada", country_count$Var1)
country_count$Var1<- sub("DE", "Germany", country_count$Var1)
country_count$Var1<- sub("ES", "Spain", country_count$Var1)
country_count$Var1<- sub("FR", "France", country_count$Var1)
country_count$Var1<- sub("GB", "UK", country_count$Var1)
country_count$Var1<- sub("IT", "Italy", country_count$Var1)
country_count$Var1<- sub("NL", "New Zealand", country_count$Var1)
country_count$Var1<- sub("PT", "Portugal", country_count$Var1)
country_count$Var1<- sub("US", "USA", country_count$Var1)
country_count2<-country_count[-10,]
library(ggplot2)
library(dplyr)

WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

df <- data.frame(region=c('Hungary','Lithuania','Argentina'), 
                 value=c(4,10,11), 
                 stringsAsFactors=FALSE)
#With USA---------
p <- ggplot()
p <- p + geom_map(data=WorldData, map=WorldData,
                  aes(x=long, y=lat, group=group, map_id=region),
                  fill="white", colour="#7f7f7f", size=0.5)
p <- p + geom_map(data=country_count, map=WorldData,
                  aes(fill=Freq, map_id=Var1),
                  colour="#7f7f7f", size=0.5)
p <- p + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
p <- p + scale_fill_continuous(low="thistle2", high="darkred", 
                               guide="colorbar")
p <- p + scale_y_continuous(breaks=c())
p <- p + scale_x_continuous(breaks=c())
p <- p + labs(fill="legend", title="Title", x="", y="")
p <- p + theme_bw()
p <- p + theme(panel.border = element_blank())
p 

#without usa--------

p2 <- ggplot()
p2 <- p2 + geom_map(data=WorldData, map=WorldData,
                  aes(x=long, y=lat, group=group, map_id=region),
                  fill="white", colour="#7f7f7f", size=0.5)
p2 <- p2 + geom_map(data=country_count2, map=WorldData,
                  aes(fill=Freq, map_id=Var1),
                  colour="#7f7f7f", size=0.5)
p2 <- p2 + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
p2 <- p2 + scale_fill_continuous(low="thistle2", high="darkred", 
                               guide="colorbar")
p2 <- p2 + scale_y_continuous(breaks=c())
p2 <- p2 + scale_x_continuous(breaks=c())
p2 <- p2 + labs(fill="legend", title="Title", x="", y="")
p2 <- p2 + theme_bw()
p2 <- p2 + theme(panel.border = element_blank())
p2 


#KNN------------------------
kbackup<-traindum
kbackup<-kbackup[kbackup$country_destination!='NDF',] #removing NDF
kbackup<-kbackup[kbackup$country_destination!='other',]
kbackup$country_destination<-factor(kbackup$country_destination)
kbackup$country_destination<-as.numeric(kbackup$country_destination)
kbackup$date_first_booking<-as.Date(kbackup$date_first_booking)
kbackup$Days_to_book<-kbackup$date_first_booking-kbackup$date_account_created
kbackup<-kbackup[kbackup$Days_to_book>=0,]
kbackup<-kbackup[-c(1,2)]
kbackup$Days_to_book<-as.numeric(kbackup$Days_to_book)
kbackup$signup_flow<-as.numeric(kbackup$signup_flow)
sample_dataknn<-sample(length(kbackup[,2]),size=0.80*(length(kbackup[,2])),replace=FALSE,set.seed(15))
train.knn<-kbackup[sample_dataknn,]
test.knn<-kbackup[-sample_dataknn,] 




train.def <- kbackup$country_destination[sample_dataknn]
test.def <- kbackup$country_destination[-sample_dataknn]

library(class)
library(caret)
#knn.1 <-  knn(train.knn, test.knn, train.def, k=1)
knn.5 <-  knn(train.knn, test.knn, train.def, k=5)
#knn.20 <- knn(train.knn, test.knn, train.def, k=20)  
cmknn<-confusionMatrix(knn.5,test.knn$country_destination)


#k fold---------------------------
library(class)
library(caret)
set.seed(1)
folds <- sample(rep(1:10, length.out = nrow(kbackup)), size = nrow(kbackup), replace = F)
CV_knn <- lapply(1:10, function(x){ 
  
  knn.5 <-  knn(kbackup[folds != x, ],  kbackup[folds == x,], kbackup$country_destination[folds != x], k=5)
  return(data.frame(knn.5, real = kbackup$country_destination[folds == x]))
})

CV_knn<- do.call(rbind, CV_knn)
cmknn<-confusionMatrix(CV_knn$knn.5, CV_knn$real)

#roc
library(pROC)
CV_knn$knn.5<-as.numeric(CV_knn$knn.5)
CV_knn$real<-as.numeric(CV_knn$real)
knnroc<-multiclass.roc(CV_knn$real,CV_knn$knn.5)
knnauc<-auc(knnroc)



#Randomforest------------------------------------------

#test_multi$country_destination<-as.factor(test_multi$country_destination)
library(randomForest)
modrfvs<-randomForest(country_destination~.,ntree=500,data=train.multi,importance=TRUE)
predrfvs<-predict(modrfvs,newdata=test.multi)
cmrfvs<-confusionMatrix(predrfvs,test.multi$country_destination)
varImpPlot(modrf)
table(predrfvs)

#Randomforest with different sampling techniques-------------------------
library(caret)
downtrain<-downSample(train.multi[,-8],train.multi$country_destination)
uptrain<-upSample(train.multi[,-8],train.multi$country_destination)


multiup<-sample(length(uptrain[,2]),size=0.80*(length(uptrain[,2])),replace=FALSE,set.seed(15))
grand_multi2<-uptrain[multiup,]
sample_multi2<-uptrain[-multiup,]

multisp3<-sample(length(sample_multi2[,2]),size=0.85*(length(sample_multi2[,2])),replace=FALSE,set.seed(15))
train.multi2<- sample_multi2[multisp3,]
test.multi2<-sample_multi2[-multisp3,]

library(randomForest)
modrfvs2<-randomForest(Class~.,ntree=500,data=train.multi2,importance=TRUE)

predrfvs2<-predict(modrfvs2,newdata=test.multi2)
cmrfvs2<-confusionMatrix(predrfvs2,test.multi2$Class)


#removing US---------------------------
multi.dfnus<-multi.df
multi.dfnus<-multi.dfnus[multi.dfnus$country_destination!='US',]
multi.dfnus$country_destination<-factor(multi.dfnus$country_destination)
multisp2<-sample(length(multi.dfnus[,2]),size=0.80*(length(multi.dfnus[,2])),replace=FALSE,set.seed(15))
train_multinus<-multi.dfnus[multisp2,]
test_multius<-multi.dfnus[-multisp2,]

wts2<-100/table(train_multinus$country_destination)
modrfvs3<-randomForest(country_destination~.,ntree=500,data=train_multinus,importance=TRUE,classwt=wts2)

predrfvs3<-predict(modrfvs3,newdata=test_multius)
cmrfvs3<-confusionMatrix(predrfvs3,test_multius$country_destination)
library(ranger)
rangr<-ranger(country_destination~.,train_multinus,case.weights = wts2)




#SVM----------------------------------------
library(e1071)
backupsvm<-traindum
backupsvm<-backupsvm[-c(1,2,19,38)]
backupsvm$signup_flow<-as.numeric(backupsvm$signup_flow)
backupsvm<-backupsvm[backupsvm$country_destination!='other'&backupsvm$country_destination!='NDF',]
backupsvm$country_destination<-factor(backupsvm$country_destination)


sample_datasvm<-sample(length(backupsvm[,2]),size=0.80*(length(backupsvm[,2])),replace=FALSE,set.seed(15))
grand.svm<-backupsvm[sample_datasvm,]
sample.svm<-backupsvm[-sample_datasvm,] 
sample_svm2<-sample(length(sample.svm[,2]),size=0.85*(length(sample.svm[,2])),replace=FALSE,set.seed(15))
trainsvm1<-sample.svm[sample_svm2,]
testsvm1<-sample.svm[-sample_svm2,]
wts<-100/table(trainsvm1$country_destination)
tune.out <- tune(svm, country_destination~., data = trainsvm1, kernel="radial", 
                 ranges = list(gamma=c(0.1,0.5,1,2,4), 
                               cost = c(0.1,1,10,100,1000)
                 ), 
                 class.weights= wts,tunecontrol = tune.control(sampling="cross",cross=5))
modelsvm2<-svm(country_destination~.,data=trainsvm1, kernel="radial",gamma=2,cost=10,class.weights= wts)
predsvm2<-predict(modelsvm2,testsvm1)
cmsvm2<-confusionMatrix(predsvm2,testsvm1$country_destination)


#Naive Bayes---------------------------------------------------------
library(e1071)
multinb<-naiveBayes(country_destination~.,data=train.multi)
prednb2<-predict(multinb,test.multi)

cmnb2<-confusionMatrix(prednb2,test.multi$country_destination)


#Gradient Boosting---------------------------------
traingbm<-traindum
traingbm<-traingbm[traingbm$country_destination!='NDF'&traingbm$country_destination!='other',]
traingbm$country_destination<-factor(traingbm$country_destination)

traingbm$date_first_booking<-as.Date(traingbm$date_first_booking)
traingbm$Days_to_book<-traingbm$date_first_booking-traingbm$date_account_created
traingbm<-traingbm[traingbm$Days_to_book>=0,]
traingbm<-traingbm[-c(1,2,38)]
traingbm$Days_to_book<-as.numeric(traingbm$Days_to_book)
sample_gbm<-sample(length(traingbm[,2]),size=0.80*(length(traingbm[,2])),replace=FALSE,set.seed(15))

train_gbm1<-traingbm[sample_gbm,]
test_gbm1<-traingbm[-sample_gbm,]


small_gbm<-sample(length(test_gbm1[,2]),size = 0.80*(length(test_gbm1[,2])),replace=FALSE,set.seed(15))
train_small_gbm<-test_gbm1[small_gbm,]
test_small_gbm<-test_gbm1[-small_gbm,]
wtsgbm<-100/table(train_small_gbm$country_destination)

library(gbm)
library(MASS)
library(caret)
model_gbm<-gbm(country_destination~ . ,data = train_small_gbm,n.trees = 100,cv.folds=10,
               shrinkage = 0.01, interaction.depth = 4,keep.data = TRUE)
summary(model_gbm)
pred_gbm<-predict(model_gbm,newdata=test_small_gbm,n.trees = 100,type="response")
tablepredbm<-as.data.frame(pred_gbm)


#XG Boost------------------------------------
library(xgboost)
traingbm<-traindum
traingbm<-traingbm[traingbm$country_destination!='NDF'&traingbm$country_destination!='other',]
traingbm$country_destination<-factor(traingbm$country_destination)

traingbm$date_first_booking<-as.Date(traingbm$date_first_booking)
traingbm$Days_to_book<-traingbm$date_first_booking-traingbm$date_account_created
traingbm<-traingbm[traingbm$Days_to_book>=0,]
traingbm<-traingbm[-c(1,2,38)]
traingbm$Days_to_book<-as.numeric(traingbm$Days_to_book)
sample_gbm<-sample(length(traingbm[,2]),size=0.80*(length(traingbm[,2])),replace=FALSE,set.seed(15))

train_gbm1<-traingbm[sample_gbm,]
test_gbm1<-traingbm[-sample_gbm,]
labels<-train_gbm1['country_destination']
labels$country_destination<-as.factor(labels$country_destination)
y<-recode(labels$country_destination,'US'=0, 'FR'=1, 'CA'=2, 'GB'=3,'ES'=4, 'IT'=5, 'PT'=6, 'NL'=7, 'DE'=8, 'AU'=9)
lab_tab<-as.data.frame(y)



modelxg<- xgboost(data = data.matrix(train_gbm1[-3]), 
                         label = data.matrix(y), 
                         eta = 0.5,
                         max_depth = 10, 
                         nrounds = 25,
                         subsample = 0.7,
                         colsample_bytree = 0.8,
                         seed = 1,
                         eval_metric = "merror",
                         objective = "multi:softmax",
                         num_class = 10,
                         nthread = 3)


xgpred<-predict(modelxg,data.matrix(test_gbm1[-3]))
tabxg<-as.data.frame(xgpred)



#without US--------------------------
library(xgboost)
library(plyr)
traingbm<-traindum
traingbm<-traingbm[traingbm$country_destination!='NDF'&traingbm$country_destination!='other'&traingbm$country_destination!='US',]
traingbm$country_destination<-factor(traingbm$country_destination)

traingbm$date_first_booking<-as.Date(traingbm$date_first_booking)
traingbm$Days_to_book<-traingbm$date_first_booking-traingbm$date_account_created
traingbm<-traingbm[traingbm$Days_to_book>=0,]
traingbm<-traingbm[-c(1,2,38)]
traingbm$Days_to_book<-as.numeric(traingbm$Days_to_book)
sample_gbm<-sample(length(traingbm[,2]),size=0.80*(length(traingbm[,2])),replace=FALSE,set.seed(15))

train_gbm1<-traingbm[sample_gbm,]
test_gbm1<-traingbm[-sample_gbm,]
labels<-train_gbm1['country_destination']
y<-recode(labels$country_destination," 'FR'=0; 'CA'=1; 'GB'=2; 'ES'=3; 'IT'=4; 'PT'=5; 'NL'=6; 'DE'=7; 'AU'=8")



modelxg<- xgboost(data = data.matrix(train_gbm1[-3]), 
                  label = data.matrix(y), 
                  eta = 0.5,
                  max_depth = 10, 
                  nrounds = 25,
                  subsample = 0.7,
                  colsample_bytree = 0.8,
                  seed = 1,
                  eval_metric = "merror",
                  objective = "multi:softmax",
                  num_class = 9,
                  nthread = 3)


xgpred<-predict(modelxg,data.matrix(test_gbm1[-3]))
tabxg<-as.data.frame(xgpred)
tabxg<-recode(tabxg$xgpred,'0="FR";1="CA";2="GB";3="ES";4="IT";5="PT";6="NL";7="DE";8="AU"')
tabxg<-as.data.frame(tabxg)
cmxgb<-caret::confusionMatrix(tabxg$tabxg,test_gbm1$country_destination)


#DUMMY VARIABLE CODE-----------------------

#converting into dummy
traindum<-train.df

library(ade4)
library(data.table)
ohe_feats = c('affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(traindum[f])
  traindum[f] = NULL
  traindum = cbind(traindum, df_all_dummy)
}

save(list=ls(all=T),file='BNB Code.rdata')







































































































































































































































