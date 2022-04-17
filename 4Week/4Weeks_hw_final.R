# 패키지 로드
library(rpart)
library(e1071)
library(caret)
library(tidyverse)
library(rattle)
library(rpart.plot)

# 데이터 불러오기
setwd('C:/R_data')
data <- read.csv('weatherAUS.csv')

## B. 데이터 설명
dim(data)
unlist(lapply(sapply(data, FUN = "class"), FUN = "[", 1)) # 각 열의 속성 확인

## F. 목표 변수 : RainTomorrow

## G. 데이터 전처리
# 데이터 형변환 > factor형 변수로
ds <- data.frame(
  DATE = as.factor(data$Date),
  LOCATION = as.factor(data$Location),
  MINTEMP = data$MinTemp,
  MAXTEMP = data$MaxTemp,
  RAINFALL = data$Rainfall,
  EVAPORATION = data$Evaporation,
  SUNSHINE = data$Sunshine,
  WINDGUSTDIR = as.factor(data$WindGustDir),
  WINDGUSTSPEED = data$WindGustSpeed,
  WINDDIR9AM = as.factor(data$WindDir9am),
  WINDDIR3PM = as.factor(data$WindDir3pm),
  WINDSPEED9AM = data$WindSpeed9am,
  WINDSPEED3PM = data$WindSpeed3pm,
  HUMIDITY9AM = data$Humidity9am,
  HUMIDITY3PM = data$Humidity3pm,
  PRESSURE9AM = data$Pressure9am,
  PRESSURE3PM = data$Pressure3pm,
  CLOUD9AM = data$Cloud9am,
  CLOUD3PM = data$Cloud3pm,
  TEMP9AM = data$Temp9am,
  TEMP3PM = data$Temp3pm,
  RAINTODAY = as.factor(data$RainToday),
  RAINTOMORROW = as.factor(data$RainTomorrow)
)

# 변경된 각 열의 속성 확인
unlist(lapply(sapply(ds, FUN = "class"), FUN = "[", 1))

# 극단치 확인 (사용할 예측 변수만)
b1 <- boxplot(ds$RAINFALL)$stats
b2 <- boxplot(ds$WINDGUSTSPEED)$stats
b3 <- boxplot(ds$HUMIDITY3PM)$stats #극단치 X
b4 <- boxplot(ds$HUMIDITY9AM)$stats

# 극단치 범위에 포함되는 데이터 개수 파악
table(ds$RAINFALL < b1[1,])
table(ds$RAINFALL > b1[5,])
table(ds$HUMIDITY9AM < b4[1,])
table(ds$HUMIDITY9AM > b4[5,])
table(ds$WINDGUSTSPEED < b2[1,])
table(ds$WINDGUSTSPEED > b2[5,])

# 극단치 NA 값으로 대체
ds$RAINFALL <- ifelse(ds$RAINFALL < b1[1,] | ds$RAINFALL > b1[5,], NA, ds$RAINFALL)
ds$WINDGUSTSPEED <- ifelse(ds$WINDGUSTSPEED < b2[1,] | ds$WINDGUSTSPEED > b2[5,], NA, ds$WINDGUSTSPEED)
ds$HUMIDITY9AM <- ifelse(ds$HUMIDITY9AM < b4[1,] | ds$HUMIDITY9AM > b4[5,], NA, ds$HUMIDITY9AM)

# 결측치 제거
nrow(ds) # 145460 
ds <- na.omit(ds)
nrow(ds) # 44811

## H. 예측 변수 선정하기 - 히스토그램
his <- function(c) {
  col = data[[c]]
  name = names(data[c])
  
  res = ggplot(data, aes(x = col, fill = RainTomorrow, color = RainTomorrow)
  ) + geom_histogram(alpha=0.5, position="identity") + xlab(name)
  
  return (res)
}

bar <- function(c) {
  col = data[[c]]
  name = names(data[c])
  
  res = ggplot(data, aes(x = col, fill = RainTomorrow, color = RainTomorrow)
  ) + geom_bar(alpha=0.5, position="identity") + xlab(name)
  
  return (res)
}

his(4) # 수치형 속성 시각화
bar(22) # 명목형 속성 시각화

# 위 결과 기반 예측 변수 선정 (목표 변수 포함)
df <- subset(ds, select=c( WINDGUSTSPEED, HUMIDITY9AM, HUMIDITY3PM, RAINTODAY, RAINTOMORROW))
str(df)
nrow(df)

## I. 목표 변수 기반 층화 추출
df_index <- createDataPartition(df$RAINTOMORROW, p = 0.6, list = F)
df_train <- df[df_index, ]
df_test <- df[-df_index, ]
nrow(df_train)
nrow(df_test) 

## J,K. 최적 파라미터, 적절한 예측 변수 선정 후 모델 실행 및 정확도 측정
# 모델 생성
fit <- rpart(RAINTOMORROW~., 
             data = df_train,
             method="class",
             cp = 0,
             minsplit = 4,
             minbucket = 2,
             maxdepth = 5
)


pred = predict(fit, df_train, type = "class", )
confusionMatrix(df_train$RAINTOMORROW, pred, positive = 'Yes') # Testset에 대한 정확도평가

pred = predict(fit, df_test, type = "class", )
confusionMatrix(df_test$RAINTOMORROW, pred, positive = 'Yes') # Testset에 대한 정확도평가

# plot the tree 
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

## L. 모든 변수 포함 모델 생성 및 정확도 평가
dsall <- data.frame(MINTEMP = data$MinTemp,
                    MAXTEMP = data$MaxTemp,
                    EVAPORATION = data$Evaporation,
                    WINDGUSTDIR = data$WindGustDir,
                    WINDDIR9AM = data$WindDir9am,
                    WINDDIR3PM = data$WindDir9am,
                    LOCATION = as.factor(data$Location),
                    RAINFALL = data$Rainfall, #The amount of rainfall recorded for the day in mm
                    WINDGUSTSPEED = data$WindGustSpeed, #The speed (km/h) of the strongest wind gust in the 24 hours to midnight
                    HUMIDITY9AM = data$Humidity9am, #Humidity (percent) at 9am
                    HUMIDITY3PM = data$Humidity3pm,
                    RAINTODAY = data$RainToday,
                    RAINTOMORROW = as.factor(data$RainTomorrow)
)

nrow(dsall) # 145460 
dsall <- na.omit(dsall)
nrwo(dsall) # 72110

indexes = createDataPartition(dsall$RAINTOMORROW, p = .6, list = F) 
trainset = dsall[indexes, ]
testset = dsall[-indexes, ]

fit <- rpart(RAINTOMORROW~MINTEMP+MAXTEMP+EVAPORATION+WINDGUSTDIR+WINDDIR9AM+
               WINDDIR3PM+LOCATION+RAINFALL+WINDGUSTSPEED+HUMIDITY9AM+HUMIDITY9AM+
               HUMIDITY3PM+RAINTODAY, 
             data = trainset,
             method="class",
             cp = -1,
             minsplit = 4,
             minbucket = 2,
             #maxdepth = 5
)
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
# trainset 정확도 93%

pred = predict(fit, testset, type = "class", )
confusionMatrix(testset$RAINTOMORROW, pred, positive = 'Yes')
# testset 정확도 84%

## M. Split Criterion, Pruning 여부에 따른 tree의 형태 및 정확도 비교 -> 큰 차이 없음 
fit <- rpart(RAINTOMORROW~MINTEMP+MAXTEMP+EVAPORATION+WINDGUSTDIR+WINDDIR9AM+
               WINDDIR3PM+LOCATION+RAINFALL+WINDGUSTSPEED+HUMIDITY9AM+HUMIDITY9AM+
               HUMIDITY3PM+RAINTODAY, 
             data = trainset,
             method="class",
             parms = list(split = 'information'), #
             cp = -1,
             minsplit = 4,
             minbucket = 2,
             #maxdepth = 5
)

pred = predict(fit, testset, type = "class", )
confusionMatrix(testset$RAINTOMORROW, pred, positive = 'Yes')

fit2 <- rpart(RAINTOMORROW~MINTEMP+MAXTEMP+EVAPORATION+WINDGUSTDIR+WINDDIR9AM+
                WINDDIR3PM+LOCATION+RAINFALL+WINDGUSTSPEED+HUMIDITY9AM+HUMIDITY9AM+
                HUMIDITY3PM+RAINTODAY, 
              data = trainset,
              method="class",
              parms = list(split = 'gini'),
              cp = -1,
              minsplit = 4,
              minbucket = 2,
              #maxdepth = 5
)

pred = predict(fit2, testset, type = "class", )
confusionMatrix(testset$RAINTOMORROW, pred, positive = 'Yes')

## N. 학습 곡선
pred = predict(fit, trainset, type = "class", )
confusionMatrix(trainset$RAINTOMORROW, pred, positive = 'Yes')
# 1% : 0.8879 5% : 0.8747 10% : 0.8706 20% : 0.8711 30% : 0.8691 40% : 0.8681 50% : 0.8678
pred = predict(fit, testset, type = "class", )
confusionMatrix(testset$RAINTOMORROW, pred, positive = 'Yes')
# 1% : 0.8555 5% : 0.861 10% : 0.8646 20% : 0.8666 30% : 0.8661 40% : 0.867 50% : 0.8672

Trainingscore <- c(0.8879,0.8747,0.8706,0.8711,0.8691,0.8681,0.8678)
Cross_validation <- c(0.8555,0.8610,0.8652,0.8666,0.8661,0.8670,0.8672)
TrainsetCount <- c(0.01,0.05,0.1,0.2,0.3,0.4,0.5)

ggplot() + 
  geom_line(aes(x = TrainsetCount, y = Trainingscore),col="red",size=2)+
  geom_line(aes(x = TrainsetCount, y = Trainingscore),col="red",size=20,alpha=.1)+
  geom_line(aes(x = TrainsetCount, y = Cross_validation),col="blue",size=2) +
  geom_line(aes(x = TrainsetCount, y = Cross_validation),col="blue",size=20,alpha=0.1)

