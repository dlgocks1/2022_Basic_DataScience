library(rpart)
library(e1071)
library(caret)
library(tidyverse)
library(rattle)
library(rpart.plot)

dataset <- read.csv(file = "C:/Users/cksgo/Documents/weatherAUS.csv", 
                    header = TRUE)
dataset
class(dataset)
head(dataset)

ds <- data.frame(LOCATION = as.factor(dataset$Location),
                 RAINFALL = dataset$Rainfall, #The amount of rainfall recorded for the day in mm
                 WINDGUSTSPEED = dataset$WindGustSpeed, #The speed (km/h) of the strongest wind gust in the 24 hours to midnight
                 HUMIDITY9AM = dataset$Humidity9am, #Humidity (percent) at 9am
                 HUMIDITY3PM = dataset$Humidity3pm,
                 RAINTODAY = dataset$RainToday,
                 RAINTOMORROW = as.factor(dataset$RainTomorrow)
)

#데이터 요약 통계랑 출력
summary(ds)

#상관관계 확인
#cor(ds$RAINTOMORROW, ds$RAINTODAY, method = "pearson")

#극단치 확인
b1 <- boxplot(ds$RAINFALL)$stats
b2 <- boxplot(ds$WINDGUSTSPEED)$stats
b3 <- boxplot(ds$HUMIDITY3PM)$stats #극단치 X
b4 <- boxplot(ds$HUMIDITY9AM)$stats

#극단치 범위에 포함되는 데이터 개수 파악
table(ds$RAINFALL < b1[1,])
table(ds$RAINFALL > b1[5,])
table(ds$HUMIDITY9AM < b4[1,])
table(ds$HUMIDITY9AM > b4[5,])
table(ds$WINDGUSTSPEED < b2[1,])
table(ds$WINDGUSTSPEED > b2[5,])

ds$RAINFALL <- ifelse(ds$RAINFALL < b1[1,] | ds$RAINFALL > b1[5,], NA, ds$RAINFALL)
ds$WINDGUSTSPEED <- ifelse(ds$WINDGUSTSPEED < b2[1,] | ds$WINDGUSTSPEED > b2[5,], NA, ds$WINDGUSTSPEED)
ds$HUMIDITY9AM <- ifelse(ds$HUMIDITY9AM < b4[1,] | ds$HUMIDITY9AM > b4[5,], NA, ds$HUMIDITY9AM)

count(ds)#145460 
is.na(ds)
table(is.na(ds)) #NA값 많음
ds <- na.omit(ds)
count(ds)#103438개
levels(ds$RAINTOMORROW) #level = no, yes


#테스트셋 분할
indexes = createDataPartition(ds$RAINTOMORROW, p = .6, list = F) 
indexes
trainset = ds[indexes, ]
testset = ds[-indexes, ]


# minsplit: 분기를 시도하기 위해서 각 노드에 포함되어야 하는 관측치의 최소 갯수
# minbucket: 가 작을수록 트리가 잘게 쪼개지므로 모델이 복잡해집니다. => train 데이터에 대한 높은 예측율 =>
# but. 새로운 데이터 적용 시 예측율 낮아짐(오버핏 발생)
# nsplist : 가지의 분기 수 (nsplit+1 의 리프 노드가 생성)
# cp: 복잡성 0~1사이 값?

fit <- rpart(RAINTOMORROW~., 
             data = trainset,
             cp = -1, 
             minsplit = 2,
             minbucket = 3) 
fit
printcp(fit)
pred = predict(fit, trainset, type = "class", )
print(data.frame(trainset, pred))
confusionMatrix(trainset$RAINTOMORROW, pred,positive = 'Yes')
#정확도 93% 높긴한데, 가지가 95개 이건 못씀

pred = predict(fit, testset, type = "class", )
confusionMatrix(testset$RAINTOMORROW, pred,positive = 'Yes')
#정확도 93% 높긴한데, 가지가 95개 이건 못씀

fit <- rpart(RAINTOMORROW~., 
             data = trainset,
             method="class",
             cp = 0,
             minsplit = 4,
             minbucket = 2,
             maxdepth = 5
             )

fit
printcp(fit)
#가지치기 11개 나쁘지 않은 듯

# plot the tree 
windows()
plot(fit)
text(fit, cex = 0.9, xpd = TRUE)
readline('Enter to resume ')
dev.off() 
# close window

windows()
fancyRpartPlot(fit)
readline('Enter to resume ')
dev.off() # close window

windows()
rpart.plot(fit)
readline('Enter to resume ')
dev.off() # close window


# Evaluate the performance of the prediction model
pred = predict(fit, trainset, type = "class", )
print(data.frame(trainset, pred))
confusionMatrix(trainset$RAINTOMORROW, pred,positive = 'Yes')
#정확도 87
pred = predict(fit, testset, type = "class", )
confusionMatrix(testset$RAINTOMORROW, pred,positive = 'Yes')


ds2 <- ds[,-c(1)]#Location뺌
fit <- rpart(RAINTOMORROW~., 
             data = ds2,
             method="class",
             cp = -1,
             minsplit = 4,
             minbucket = 2
             #maxdepth = 5
)

fit <- rpart(RAINTOMORROW~., 
             data = ds2,
             method="class",
             cp = 0,
             parms = list(split = 'information'),
             minsplit = 4,
             minbucket = 2,
             maxdepth = 5
) 

# prediction model 
printcp(fit)
fit
#가지 8개

# plot the tree 
windows()
plot(fit)
text(fit, cex = 0.9, xpd = TRUE)
readline('Enter to resume ')
dev.off() # close window

pred = predict(fit, trainset, type = "class", )
confusionMatrix(trainset$RAINTOMORROW, pred, positive = 'Yes')

pred = predict(fit, testset, type = "class", )
confusionMatrix(testset$RAINTOMORROW, pred, positive = 'Yes')
#정확도 87으로 차이 X LOCATION은 적절한 변수 X

windows()
fancyRpartPlot(fit)
readline('Enter to resume ')
dev.off() # close window

windows()
rpart.plot(fit)
readline('Enter to resume ')
dev.off() # close window

pred = predict(fit, testset, type = "class", )
print(data.frame(testset, pred))
confusionMatrix(testset$RAINTOMORROW, pred, positive = 'Yes')
#testset에 대한 정확도 86%


#모든변수포함하는 dataset
dataset
dsall <- data.frame(MINTEMP = dataset$MinTemp,
                    MAXTEMP = dataset$MaxTemp,
                    EVAPORATION = dataset$Evaporation,
                    WINDGUSTDIR = dataset$WindGustDir,
                    WINDDIR9AM = dataset$WindDir9am,
                    WINDDIR3PM = dataset$WindDir9am,
                    LOCATION = as.factor(dataset$Location),
                 RAINFALL = dataset$Rainfall, #The amount of rainfall recorded for the day in mm
                 WINDGUSTSPEED = dataset$WindGustSpeed, #The speed (km/h) of the strongest wind gust in the 24 hours to midnight
                 HUMIDITY9AM = dataset$Humidity9am, #Humidity (percent) at 9am
                 HUMIDITY3PM = dataset$Humidity3pm,
                 RAINTODAY = dataset$RainToday,
                 RAINTOMORROW = as.factor(dataset$RainTomorrow)
)

count(dsall)#145460 
is.na(dsall)
table(is.na(dsall)) #NA값 많음
dsall <- na.omit(dsall)
count(dsall)#72110개

indexes = createDataPartition(dsall$RAINTOMORROW, p = .6, list = F) 
indexes
trainset = dsall[indexes, ]
testset = dsall[-indexes, ]

fit <- rpart(RAINTOMORROW~MINTEMP+MAXTEMP+EVAPORATION+WINDGUSTDIR+WINDDIR9AM+
               WINDDIR3PM+LOCATION+RAINFALL+WINDGUSTSPEED+HUMIDITY9AM+HUMIDITY9AM+
             HUMIDITY3PM+RAINTODAY, 
             data = trainset,
             method="class",
             parms = list(split = 'information'), #별차이없는데
             cp = -1,
             #minsplit = 4,
             #minbucket = 10,
             #maxdepth = 5
)

fit
printcp(fit)

windows()
plot(fit)
text(fit, cex = 0.9, xpd = TRUE)
readline('Enter to resume ')
dev.off() # close window

windows()
fancyRpartPlot(fit)
readline('Enter to resume ')
dev.off() # close window

windows()
rpart.plot(fit)
readline('Enter to resume ')
dev.off() # close window

pred = predict(fit, trainset, type = "class", )
confusionMatrix(trainset$RAINTOMORROW, pred, positive = 'Yes')
#trainset정확도 information : 0.8493 gini : 85%
#   p=0.6일떄   information : 0.8481 gini : 0.8445

pred = predict(fit, testset, type = "class", )
confusionMatrix(testset$RAINTOMORROW, pred, positive = 'Yes')
#testset 정확도 information : 0.8421 gini : 0.8425
#   p=0.6일떄   information : 0.8445 gini : 0.842

#Split criterion (분리기준) 의 종류,
# 별차이 없음
#Pruning(가지치기) 여부를 선택함에 따라
# maxdepth설정을 안하면 미친듯이 가지침
# tree의 형태 및 정확도는 어떻게 변화하는가? 
#정확도를 최대로 하는 최적 파라미터는 무엇인가? 
# HUMIDITY3PM,WINDGUSTSPEED, HUMIDITY9AM등이 가장 최적파라미터

ds2 <- ds[,-c(1)]#Location뺌

indexes = createDataPartition(ds2$RAINTOMORROW, p = .1, list = F) 
indexes
trainset = ds2[indexes, ]
testset = ds2[-indexes, ]
count(trainset) 

fit <- rpart(RAINTOMORROW~HUMIDITY3PM+WINDGUSTSPEED+HUMIDITY9AM+RAINFALL, 
             data = trainset,
             cp = -1,
             minsplit = 4,
             minbucket = 2,
             #maxdepth = 4
) 


pred = predict(fit, trainset, type = "class", )
confusionMatrix(trainset$RAINTOMORROW, pred, positive = 'Yes')
#1% : 0.8879 5% : 0.8747 10% : 0.8706 20% : 0.8711 30% : 0.8691 40% : 0.8681 50% : 0.8678
pred = predict(fit, testset, type = "class", )
confusionMatrix(testset$RAINTOMORROW, pred, positive = 'Yes')
#1% : 0.8555 5% : 0.861 10% : 0.8646 20% : 0.8666 30% : 0.8661 40% : 0.867 50% : 0.8672

Trainingscore <- c(0.8879,0.8747,0.8706,0.8711,0.8691,0.8681,0.8678)
Cross_validation <- c(0.8555,0.8610,0.8652,0.8666,0.8661,0.8670,0.8672)
TrainsetCount <- c(0.01,0.05,0.1,0.2,0.3,0.4,0.5)

ggplot() + 
  geom_line(aes(x = TrainsetCount, y = Trainingscore),col="red",size=2)+
  geom_line(aes(x = TrainsetCount, y = Trainingscore),col="red",size=20,alpha=.1)+
  geom_line(aes(x = TrainsetCount, y = Cross_validation),col="blue",size=2) +
  geom_line(aes(x = TrainsetCount, y = Cross_validation),col="blue",size=20,alpha=0.1)

