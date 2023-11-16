# SVM
set.seed(1)
Heart <- read.csv('Heart.csv')
attach(Heart)
summary(Heart)
library(e1071)
library(caTools)
library(ROCR)
library(ISLR)
library(MASS)
library(magrittr)
library(ggplot2)
library(GGally)

#BIẾN ĐỔI 
chestpain <- as.factor(Heart$ChestPain)
Heart$ChestPain <- chestpain
thal <- as.factor(Heart$Thal)
Heart$Thal <- thal
ahd <- as.factor(Heart$AHD)
Heart$AHD <- ahd


heart1 <- Heart %>% dplyr::select(-c(X,Sex,ChestPain,RestECG,ExAng,Slope,Thal))
ggpairs(heart1, ggplot2::aes(colour=AHD))
sample <- sample(c(TRUE, FALSE), nrow(Heart), replace=TRUE, prob=c(0.7,0.3))
train.Heart <- Heart[sample,]  
test.Heart<- Heart[!sample,]  
head(train.Heart)

svmfit <- svm (AHD~., data = train.Heart , kernel ='linear',
               cost = 0.1, scale = FALSE )
plot(svmfit, train.Heart, RestBP~MaxHR)
summary(svmfit)

tune.out <- tune (svm ,AHD ~., data = train.Heart , kernel = 'linear', 
                  ranges = list ( cost = c (0.001 , 0.01 , 0.1 , 1, 5, 10, 100) ))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

AHDpred <- predict(bestmod, test.Heart)
table(predict = AHDpred, truth = test.Heart$AHD)

svmfit1 <- svm (AHD~., data = train.Heart, kernel = 'radial',
                gamma = 0.1, cost = 1)
plot(svmfit1, train.Heart, MaxHR~Chol)
summary(svmfit1)

tune.out1 <- tune (svm , AHD~., data = train.Heart,
                   kernel = 'radial', 
                   ranges = list (
                     cost = c(0.1 , 1, 10, 100 , 1000),
                     gamma = c(0.1 , 1, 2, 3 , 4)
                   ))
summary(tune.out1)
bestmod1<- tune.out1$best.model
summary(bestmod1)
AHDpred1 <- predict(bestmod1, test.Heart)
table(predict = AHDpred1, truth = test.Heart$AHD)

tune.out2 <- tune(svm, AHD~., data = train.Heart,
                  kernel ='polynomial',
                  ranges = list(
                    cost = c(0.1 , 1, 10, 100 , 1000),
                    degree = c(2,3,4,5,6,7,8,9),
                    gamma = c(0.1, 1, 2, 3, 4),
                    coef.0 = c(0.1, 1, 2, 3, 4)
                  ))
summary(tune.out2)
bestmod2 <- tune.out2$best.model
summary(bestmod2)
AHDpred2 <- predict(bestmod2, test.Heart)
table(predict= AHDpred2, truth = test.Heart$AHD)
plot(bestmod2, train.Heart, RestBP~MaxHR)
#ROCcurve for training
svmfit.opt <- svm(AHD~., data = train.Heart , kernel ='linear',
                  cost = 0.1, scale = FALSE, decision.value = T )
fitted <- attributes (
  predict ( svmfit.opt , train.Heart, decision.values = TRUE )
)$ decision.values
pred<- prediction(fitted, train.Heart$AHD)
pred
perf<- performance(pred,'tpr','fpr')
plot(perf,col='red', main= 'Training Data')

ldafit.opt <- lda(AHD~., data = train.Heart)
lda.pred <- predict(ldafit.opt, train.Heart)
pred1<- prediction(lda.pred$posterior[,2],train.Heart$AHD)
perf1<- performance(pred1,'tpr','fpr')
plot(perf1,add=T, col='blue')

svmfit.flex <- svm(AHD~., data = train.Heart , kernel ='radial',gamma = 0.001,
                   cost = 10, decision.value =T)
fitted2 <- attributes(
  predict( svmfit.flex, train.Heart, decision.values = TRUE)
)$decision.values
pred2 <- prediction(fitted2, train.Heart$AHD)
perf2 <- performance(pred2, 'tpr','fpr')
plot(perf2, add =T)

svmfit.flex2 <- svm(AHD~., data = train.Heart, kernel ='radial',gamma=0.01, 
                    cost =1, decision.value =T)
fitted3 <- attributes(
  predict(svmfit.flex2, train.Heart, decision.values=TRUE)
)$decision.values
pred3 <- prediction(fitted3, train.Heart$AHD)
perf3 <- performance(pred3,'tpr','fpr')
plot(perf3, add =T, col ='green')

svmfit.flex3 <- svm(AHD~., data = train.Heart, kernel ='radial', gamma= 0.1,
                    cost=1, decision.value =T)
fitted4 <- attributes(
  predict(svmfit.flex3, train.Heart, decision.values=TRUE)
)$decision.values
pred4 <- prediction(fitted4, train.Heart$AHD)
perf4<- performance(pred4,'tpr','fpr')
plot(perf4, add=T,col='blue')

svmfit.flex4 <- svm(AHD~., data = train.Heart, kernel='polynomial', degree =5,
                    cost=1,gamma =0.1, coef.0 = 0.1, decision.value =T)
fitted5 <- attributes(
  predict(svmfit.flex4, train.Heart, decision.values = TRUE)
)$decision.values
pred5 <- prediction(fitted5,train.Heart$AHD)
perf5 <- performance(pred5,'tpr','fpr')
plot(perf5, add=T, col='pink')
# ROC curve for test
fitted.test <- attributes (
  predict ( svmfit.opt , test.Heart, decision.values = TRUE )
)$ decision.values
pred.test <- prediction(fitted.test, test.Heart$AHD)
perf.test <- performance(pred.test,'tpr','fpr')
plot(perf.test,col='red', main= 'Testing Data')

lda.predtest <- predict(ldafit.opt, test.Heart)
predlda.test <- prediction(lda.predtest$posterior[,2],test.Heart$AHD)
perflda.test<- performance(predlda.test,'tpr','fpr')
plot(perflda.test,add=T, col='blue')

fitted2.test <- attributes(
  predict( svmfit.flex, test.Heart, decision.values = TRUE)
)$decision.values
pred.test2 <- prediction(fitted2.test, test.Heart$AHD)
perf.test2 <- performance(pred.test2, 'tpr','fpr')
plot(perf.test2, add =T)

fitted3.test <- attributes(
  predict(svmfit.flex2, test.Heart, decision.values=TRUE)
)$decision.values
pred.test3 <- prediction(fitted3.test, test.Heart$AHD)
perf.test3 <- performance(pred.test3,'tpr','fpr')
plot(perf.test3, add =T, col ='green')

fitted4.test <- attributes(
  predict(svmfit.flex3, test.Heart, decision.values=TRUE)
)$decision.values
pred.test4 <- prediction(fitted4.test, test.Heart$AHD)
perf.test4<- performance(pred.test4,'tpr','fpr')
plot(perf.test4, add=T,col='blue')

fitted5.test <- attributes(
  predict(svmfit.flex4, test.Heart, decision.values=TRUE)
)$decision.values
pred.test5 <- prediction(fitted5.test, test.Heart$AHD)
perf.test5 <- performance(pred.test5,'tpr','fpr')
plot(perf.test5, add=T, col='pink')