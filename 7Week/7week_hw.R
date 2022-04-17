# tidyverse 패키지 설치 (tibble 및 여러가지 함수 사용)
#install.packages("tidyverse")
library(dplyr)
# ggplot2 패키지 설치 (plot 그리기)
#install.packages("ggplot2")
library(ggplot2)
# patchwork 패키지 설치 (plot 모아서 보기)
#install.packages(("patchwork"))
library(patchwork)
# psych 패키지 설치 (describe() 함수 사용(기술 통계))
#install.packages("psych")
library(psych)
# caret 패키지 설치 (createDataPartition() 함수 사용(층화 추출), 최적화)
#install.packages("caret")
library(caret)
# rpart 패키지 설치 (decision tree)
#install.packages("rpart")
#install.packages("raprt.plot")
library(rpart)
library(rpart.plot)
# rattle 패키지 설치 (fancyRpartPlot() 함수 사용)
#install.packages("rattle")
library(rattle)

getwd()
setwd("C:/Users/cksgo/Desktop/모든것/수업자료/3학년 1학기/2022_Basic_DataScience/7주차/실습")

data <- read.csv (
  file = "bank.csv",
  header = TRUE
)

# 간단한 데이터 EDA
str(data)
describe(data)

# 데이터 변환
data <- data.frame(age = data$age,
                   job = as.factor(data$job),
                   marital = as.factor(data$marital),
                   education = as.factor(data$education),
                   default = as.factor(data$default),
                   balance = data$balance,
                   housing = as.factor(data$housing),
                   loan = as.factor(data$loan),
                   contact = as.factor(data$contact),
                   day = as.factor(data$day),
                   month = as.factor(data$month),
                   duration = data$duration,
                   campaign = data$campaign,
                   pdays = data$pdays,
                   previous = data$previous,
                   poutcome = as.factor(data$poutcome),
                   deposit = as.factor(data$deposit))


# 기술통계 확인
describe(data)

# 데이터 전처리 (결측치 삭제)
# (확인결과 poutcome 변수는 결측치가 대부분을 차지하여 처리하지 않고 대신 예측변수로 사용 안하는 것으로 판단)
data$job <- ifelse(data$job == 'unknown', NA, data$job)
table(is.na(data$job))
data <- data %>% filter(!is.na(job))

data$education <- ifelse(data$education == 'unknown', NA, data$education)
table(is.na(data$education))
data <- data %>% filter(!is.na(education))

data$contact <- ifelse(data$contact == 'unknown', NA, data$contact)
table(is.na(data$contact))
data <- data %>% filter(!is.na(contact))

# 수치형 변수 극단값 확인 및 제거
# pdays 변수 제외 (수치형 값이면서 명목형 변수의 의미를 가지는 특정값 존재(-1))

# previous
boxplot(data$previous)$stats
data$previous <- ifelse(data$previous < 0 | data$previous > 2, NA, data$previous)
table(is.na(data$previous))
data <- data %>% filter(!is.na(previous))

# campaign
boxplot(data$campaign)$stats
data$campaign <- ifelse(data$campaign < 1 | data$campaign > 6, NA, data$campaign)
table(is.na(data$campaign))
data <- data %>% filter(!is.na(campaign))

# balance
boxplot(data$balance)$stats
data$balance <- ifelse(data$balance < -2282 | data$balance > 4256, NA, data$balance)
table(is.na(data$balance))
data <- data %>% filter(!is.na(balance))

# age
boxplot(data$age)$stats
data$age <- ifelse(data$age < 18 | data$age > 74, NA, data$age)
table(is.na(data$age))
data <- data %>% filter(!is.na(age))

# duration
boxplot(data$duration)$stats
data$duration <- ifelse(data$duration < 2 | data$duration > 1058, NA, data$duration)
table(is.na(data$duration))
data <- data %>% filter(!is.na(duration))

# 데이터 확인 (남은 record는 5,755개 (11,162 -> 5,755 : 5,407개 record 삭제됨))
str(data)

# 수치형 변수 스케일링 및 명목형 변수 수치형으로 변환 (pdays 변수와 poutcome 변수는 사용안함)
data_numeric <- data.frame(age = data$age,
                           balance = data$balance,
                           duration = data$duration,
                           campaign = data$campaign,
                           previous = data$previous)

data_factor <- data.frame(job = as.factor(data$job),
                          marital = as.factor(data$marital),
                          education = as.factor(data$education),
                          default = as.factor(data$default),
                          housing = as.factor(data$housing),
                          loan = as.factor(data$loan),
                          contact = as.factor(data$contact),
                          day = as.factor(data$day),
                          month = as.factor(data$month))

data_target <- data.frame(deposit = as.factor(data$deposit))

preProcValues = preProcess(data_numeric) 
data_numeric_scaled <- predict(preProcValues, data_numeric)

# 명목형 데이터 수치형으로 변환
dummy <- dummyVars(" ~ .", data=data_factor)
data_factor_numeric <- data.frame(predict(dummy, newdata = data_factor))

# 전처리된 데이터 통합 (pdays, poutcome 제거)
data <- cbind(data_numeric_scaled, data_factor_numeric)
data <- cbind(data, data_target)

str(data)
head(data)

# tr set, ts set 추출
set.seed(1)
idx = createDataPartition(data$deposit, p = .7, list = F)
tr_set <- data[idx, ]
ts_set <- data[-idx, ]
#-----------------------------------------------------------------

data <- tibble(age = data$age,
               job = as.factor(data$job),
               marital = as.factor(data$marital),
               education = as.factor(data$education),
               default = as.factor(data$default),
               balance = data$balance,
               housing = as.factor(data$housing),
               loan = as.factor(data$loan),
               contact = as.factor(data$contact),
               day = as.factor(data$day),
               month = as.factor(data$month),
               duration = data$duration,
               campaign = data$campaign,
               pdays = data$pdays,
               previous = data$previous,
               poutcome = as.factor(data$poutcome),
               deposit = as.factor(data$deposit))

data$job <- ifelse(data$job == 'unknown', NA, data$job)
table(is.na(data$job))
data <- data %>% filter(!is.na(job))

data$education <- ifelse(data$education == 'unknown', NA, data$education)
table(is.na(data$education))
data <- data %>% filter(!is.na(education))

data$contact <- ifelse(data$contact == 'unknown', NA, data$contact)
data$contact
table(is.na(data$contact))
data <- data %>% filter(!is.na(contact))
count(data)

# previous
boxplot(data$previous)$stats
data$previous <- ifelse(data$previous < 0 | data$previous > 2, NA, data$previous)
table(is.na(data$previous))
data <- data %>% filter(!is.na(previous))

# campaign
boxplot(data$campaign)$stats
data$campaign <- ifelse(data$campaign < 1 | data$campaign > 6, NA, data$campaign)
table(is.na(data$campaign))
data <- data %>% filter(!is.na(campaign))

# balance
boxplot(data$balance)$stats
data$balance <- ifelse(data$balance < -2282 | data$balance > 4256, NA, data$balance)
table(is.na(data$balance))
data <- data %>% filter(!is.na(balance))

# age
boxplot(data$age)$stats
data$age <- ifelse(data$age < 18 | data$age > 74, NA, data$age)
table(is.na(data$age))
data <- data %>% filter(!is.na(age))

# duration
boxplot(data$duration)$stats
data$duration <- ifelse(data$duration < 2 | data$duration > 1058, NA, data$duration)
table(is.na(data$duration))
data <- data %>% filter(!is.na(duration))

set.seed(1)
idx = createDataPartition(data$deposit, p = .7, list = F)
tr_set <- data[idx, ]
ts_set <- data[-idx, ]

# maxdepth 최적화 -> 8
# minsplite 최적화 -> 49
# minbucket 최적화 -> 16
# gini
fit_6 <- rpart(deposit~housing+month+duration+previous,
               data = tr_set,
               cp = 0.002,
               maxdepth = 8,
               minsplit = 49,
               minbucket = 16
)

#7995       
pred = predict(fit_6, ts_set, type = "class",)
print(data.frame(ts_set, pred))
confusionMatrix(ts_set$deposit, pred, positive = 'yes')
#결정나무 트리 끝

#nnet시작
#install.packages("neuralnet")
library(neuralnet)
library(nnet)
library(tidyverse)
library(rpart)
library(e1071)
library(caret)

#명목형 변수 수치형으로 변경경
#housing
data$housing <- ifelse(data$housing == 'no', 0, data$housing)
data$housing <- ifelse(data$housing == 'yes', 1, data$housing)
#month
data$month <- ifelse(data$month == 'jan', 1, data$month)
data$month <- ifelse(data$month == 'feb', 2, data$month)
data$month <- ifelse(data$month == 'mar', 3, data$month)
data$month <- ifelse(data$month == 'apr', 4, data$month)
data$month <- ifelse(data$month == 'may', 5, data$month)
data$month <- ifelse(data$month == 'jun', 6, data$month)
data$month <- ifelse(data$month == 'jul', 7, data$month)
data$month <- ifelse(data$month == 'aug', 8, data$month)
data$month <- ifelse(data$month == 'sep', 9, data$month)
data$month <- ifelse(data$month == 'oct', 10, data$month)
data$month <- ifelse(data$month == 'nov', 11, data$month)
data$month <- ifelse(data$month == 'dec', 12, data$month)
#duration,month,housing 3개 사용
head(data)
table(data$month)

deposit<- factor(data$deposit, levels = c("yes", "no"))
housing <- as.numeric(data$housing) 
month<- as.numeric(data$month)
duration<- as.numeric(data$duration)
ds<- data.frame(deposit, housing, month, duration)
str(ds)

# 데이터 스케일링
preProcValues = preProcess(ds) 
#The function preProcess estimates the required parameters for each operation and predict.preProcess is used to apply them to specific data sets. This function can also be interfaces when calling the train function.
ds <- predict(preProcValues, ds)
summary(ds)

set.seed(1)
indexes = createDataPartition(ds$deposit, p = .7, list = F)
tr_set = ds[indexes, ]
ts_set = ds[-indexes, ]

#결정나무모델
fit = rpart(deposit~., 
            data = tr_set
)

printcp(fit)
library(rattle)
fancyRpartPlot(fit)

pred = predict(fit, ts_set, type = "class" )
print(data.frame(ts_set, pred))
confusionMatrix(pred, ts_set$deposit)

#7532

#인공신경망망
modelLookup("nnet")

tuneGrid = expand.grid(size = 16:18, decay = 10 ** (-5:-3))
trControl=trainControl(method='repeatedcv', number = 10, repeats = 2)
model = train(deposit ~.,
              data = tr_set,
              method = 'nnet',
              maxit = 100, #반복횟수 epoch 수
              metric = 'Accuracy',
              trControl = trControl,
              tuneLength = 3, #파라미터 조합 갯수
              tuneGrid = tuneGrid
)

#파라미터 뜻
#1. size : hidden node 개수
#2. maxit : 최대반복횟수
#3. decay : overfitting을 피하기 위해 사용하는 weight decay parameter
#4. rang : 초기 랜덤 가중치. weights on [-rang, rang]. 기본값 = 0.5

model
model$finalModel
#weight : 1e-05, 레이어노드 17개
#final 모델의 수렴 출력 converged ->수렴
model$finalModel$convergence 
# 1 if maxit reached, 0 otherwise

#모델평가
pred <- predict(model$finalModel, newdata = ts_set, type = "class") 
pred <- factor(pred, levels = c("yes","no"))
print(data.frame(ts_set$deposit, pred))
confusionMatrix(pred, ts_set$deposit)
#Accuracy : 0.7671
#Sensitivity : 0.7720          
#Specificity : 0.7624

#install.packages("devtools")
library(devtools)
source('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(model)

tr_set

#epoch수를 다르게 하며 학습 곡선 그리기 decay(weight)값은 0.0001~0.001사이, 히든노드는 16~18개
testAccuracy = array(1:10)
trainAccuracy = array(1:10)
#이거 돌리지 마셈 오래걸림
tuneGrid = expand.grid(size = 16:18, decay = 10 ** (-5:-3))
trControl=trainControl(method='repeatedcv', number = 2, repeats = 1)
for(i in seq(50,500,50)){
  model = train(deposit ~.,
                data = tr_set,
                method = 'nnet',
                maxit = i, #반복횟수 epoch 수
                metric = 'Accuracy',
                trControl = trControl,
                tuneLength = 3, #파라미터 조합 갯수
                tuneGrid = tuneGrid
  )
  pred <- predict(model$finalModel, newdata = ts_set, type = "class") 
  pred <- factor(pred, levels = c("yes","no"))
  print(data.frame(ts_set$deposit, pred))
  test <- caret::confusionMatrix(pred, ts_set$deposit)
  testAccuracy[i/50] = test$overall[1]
  
  pred <- predict(model$finalModel, newdata = tr_set, type = "class") 
  pred <- factor(pred, levels = c("yes","no"))
  print(data.frame(tr_set$deposit, pred))
  test <- caret::confusionMatrix(pred, tr_set$deposit)
  trainAccuracy[i/50] = test$overall[1]
}

ggplot() +
  geom_line(aes(x =c(50,100,150,200,250,300,350,400,450,500), y = testAccuracy),col="red")+
  geom_line(aes(x =c(50,100,150,200,250,300,350,400,450,500), y = trainAccuracy),col="blue")


#concrete_model <-
#  neuralnet(deposit ~ month+housing+duration, data = tr_set, hidden = 3)
#)
#plot(concrete_model) # plot the neural networks




#못하겠음 svm을 활용하여 모델만들기
# C for cost 

# optimize model - rough

data <- read.csv (
  file = "bank.csv",
  header = TRUE
)


#housing
data$housing <- ifelse(data$housing == 'no', 0, data$housing)
data$housing <- ifelse(data$housing == 'yes', 1, data$housing)
#month
data$month <- ifelse(data$month == 'jan', 1, data$month)
data$month <- ifelse(data$month == 'feb', 2, data$month)
data$month <- ifelse(data$month == 'mar', 3, data$month)
data$month <- ifelse(data$month == 'apr', 4, data$month)
data$month <- ifelse(data$month == 'may', 5, data$month)
data$month <- ifelse(data$month == 'jun', 6, data$month)
data$month <- ifelse(data$month == 'jul', 7, data$month)
data$month <- ifelse(data$month == 'aug', 8, data$month)
data$month <- ifelse(data$month == 'sep', 9, data$month)
data$month <- ifelse(data$month == 'oct', 10, data$month)
data$month <- ifelse(data$month == 'nov', 11, data$month)
data$month <- ifelse(data$month == 'dec', 12, data$month)

mode<- factor(data$deposit, levels = c("yes", "no"))
housing <- as.numeric(data$housing) 
month<- as.numeric(data$month)
duration<- as.numeric(data$duration)
ds<- data.frame(mode, housing, month, duration)
str(ds)

table(data$month)
# data preprocessing using caret::preProcess
# center and scale 
preProcValues = preProcess(ds) 

#The function preProcess estimates the required parameters for each operation and predict.preProcess is used to apply them to specific data sets. This function can also be interfaces when calling the train function.
ds <- predict(preProcValues, ds)

summary(ds)

# split the data into tr and ts sets 
set.seed(4) # random seed
indexes = createDataPartition(ds$mode, p = .6, list = F)
train = ds[indexes, ]
test = ds[-indexes, ]
train
test

names(getModelInfo("svm"))
modelLookup("svmLinear2")

trControl <- trainControl(method='repeatedcv', number = 5, repeats = 1)
model1 <- train(mode ~.,
                data = train,
                method = 'svmLinear2',
                metric = 'Accuracy',
                tuneLength = 3, #파라미터 조합 갯수
                trControl = trControl
)




# show model 
model1

plot(model1)
# show final model
model1$finalModel
model1$bestTune

# Model Evaluation
# Predict testing set
pred <- predict(model1, test) 
# pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)
acc1 <- confusionMatrix(pred, test$mode)$overall['Accuracy']
acc1

# for more optimization
# grid values
tuneGrid = expand.grid(cost = 10**(-4:0))
tuneGrid
trControl <- trainControl(method = 'repeatedcv', 
                          number = 5, 
                          repeats = 1, 
                          returnResamp = 'final')
model2 <- train(mode ~.,
                data = train,
                method = 'svmLinear2',
                metric = 'Accuracy',
                trControl = trControl,
                tuneGrid = tuneGrid
)
model2
model2$finalModel
model2$bestTune

plot(model2)

# Model Evaluation

#Model Evaluation
#Predict test set
pred <- predict(model2, test) 
#pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)
acc2 <- confusionMatrix(pred, test$mode)$overall['Accuracy']
acc2
# Non-linear kernel - Radial

modelLookup("svmRadial")

# optimization
trControl <- trainControl(method='repeatedcv', number = 5, repeats = 1)
model3 <- train(mode ~.,
                data = train,
                method = 'svmRadial',
                metric = 'Accuracy',
                trControl = trControl,
                #tuneGrid=grid,
                tuneLength = 3
)

# show model 
model3

plot(model3)
# show final model
model3$finalModel
model3$bestTune

# Model Evaluation
# Predict testing set
pred <- predict(model3, newdata = test) 
pred <- factor(pred, levels = c("yes","no" ))
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)
acc3 <- confusionMatrix(pred, test$mode)$overall['Accuracy']
acc3
# Non-linear kernel - Polynomial

modelLookup("svmPoly")
# optimization
trControl <- trainControl(method='repeatedcv', number = 5, repeats = 1)
model4 <- train(mode ~.,
                data = train,
                method = 'svmPoly',
                metric = 'Accuracy',
                trControl = trControl,
                #tuneGrid=grid,
                tuneLength = 3
)
#Accuracy : 0.7856  
#Sensitivity : 0.7905          
#Specificity : 0.7812

# show model 
model4

plot(model4)
#show final model
model4$finalModel
model4$bestTune

# Model Evaluation
# Predict testing set
pred <- predict(model4, newdata = test) 
pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)
acc4 <- confusionMatrix(pred, test$mode)$overall['Accuracy']

result <- tibble(Model = c('SVM Linear', 
                           'SVM Linear w/choice of cost',
                           'SVM Radial'),
                 Accuracy = c(acc1, 
                              acc2, 
                              acc3)
)

result %>% arrange(desc(Accuracy))


#SVM Radial이 가장높음
#Accuracy : 0.7856
#Sensitivity : 0.7905          
#Specificity : 0.7812

