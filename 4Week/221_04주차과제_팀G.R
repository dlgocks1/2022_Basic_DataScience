# 패키지 로드
library(rpart)
library(e1071)
library(caret)
library(tidyverse)
library(rattle)
library(rpart.plot)

# 데이터 불러오기
data <- read.csv("C:/Users/pha-r/Desktop/Subject/데이터사이언스기초/4주차/과제/weatherAUS.csv")

## B. 데이터 설명
dim(data)
unlist(lapply(sapply(data, FUN = "class"), FUN = "[", 1)) # 각 열의 속성 확인

## F. 목표 변수 : RainTomorrow

################################################################################
## G. 데이터 전처리, 탐색
# 데이터 형변환 : 명목형을 factor형으로
data <- data.frame(
  Date = as.factor(data$Date),
  Location = as.factor(data$Location),
  MinTemp = data$MinTemp,
  MaxTemp = data$MaxTemp,
  Rainfall = data$Rainfall,
  Evaporation = data$Evaporation,
  Sunshine = data$Sunshine,
  WindGustDir = as.factor(data$WindGustDir),
  WindGustSpeed = data$WindGustSpeed,
  WindDir9am = as.factor(data$WindDir9am),
  WindDir3pm = as.factor(data$WindDir3pm),
  WindSpeed9am = data$WindSpeed9am,
  WindSpeed3pm = data$WindSpeed3pm,
  Humidity9am = data$Humidity9am,
  Humidity3pm = data$Humidity3pm,
  Pressure9am = data$Pressure9am,
  Pressure3pm = data$Pressure3pm,
  Cloud9am = as.factor(data$Cloud9am),
  Cloud3pm = as.factor(data$Cloud3pm),
  Temp9am = data$Temp9am,
  Temp3pm = data$Temp3pm,
  RainToday = as.factor(data$RainToday),
  RainTomorrow = as.factor(data$RainTomorrow)
)

# 변경된 각 열의 속성 확인
unlist(lapply(sapply(data, FUN = "class"), FUN = "[", 1))

################################################################################
# 극단치 확인 (수치형)
box_MinTemp <- boxplot(data$MinTemp)$stats
box_MaxTemp <- boxplot(data$MaxTemp)$stats
box_Rainfall <- boxplot(data$Rainfall)$stats
box_Evaporation <- boxplot(data$Evaporation)$stats
box_Sunshine <- boxplot(data$Sunshine)$stats
box_WindGustSpeed <- boxplot(data$WindGustSpeed)$stats
box_WindSpeed9am <- boxplot(data$WindSpeed9am)$stats
box_WindSpeed3pm<- boxplot(data$WindSpeed3pm)$stats
box_Humidity9am <- boxplot(data$Humidity9am)$stats
box_Humidity3pm <- boxplot(data$Humidity3pm)$stats
box_Pressure9am <- boxplot(data$Pressure9am)$stats
box_Pressure3pm <- boxplot(data$Pressure3pm)$stats
box_Temp9am <- boxplot(data$Temp9am)$stats
box_Temp3pm <- boxplot(data$Temp3pm)$stats

# 극단치를 NA로 대체
data$MinTemp <- ifelse(data$MinTemp < box_MinTemp[1,] | data$MinTemp > box_MinTemp[5,], NA, data$MinTemp)
data$MaxTemp <- ifelse(data$MaxTemp < box_MaxTemp[1,] | data$MaxTemp > box_MaxTemp[5,], NA, data$MaxTemp)
data$WindGustSpeed <- ifelse(data$WindGustSpeed < box_WindGustSpeed[1,] | data$WindGustSpeed > box_WindGustSpeed[5,], NA, data$WindGustSpeed)
data$WindSpeed9am <- ifelse(data$WindSpeed9am < box_WindSpeed9am[1,] | data$WindSpeed9am > box_WindSpeed9am[5,], NA, data$WindSpeed9am)
data$WindSpeed3pm <- ifelse(data$WindSpeed3pm < box_WindSpeed3pm[1,] | data$WindSpeed3pm > box_WindSpeed3pm[5,], NA, data$WindSpeed3pm)
data$Humidity9am <- ifelse(data$Humidity9am < box_Humidity9am[1,] | data$Humidity9am > box_Humidity9am[5,], NA, data$Humidity9am)
data$Pressure9am <- ifelse(data$Pressure9am < box_Pressure9am[1,] | data$Pressure9am > box_Pressure9am[5,], NA, data$Pressure9am)
data$Pressure3pm <- ifelse(data$Pressure3pm < box_Pressure3pm[1,] | data$Pressure3pm > box_Pressure3pm[5,], NA, data$Pressure3pm)
data$Temp9am <- ifelse(data$Temp9am < box_Temp9am[1,] | data$Temp9am > box_Temp9am[5,], NA, data$Temp9am)
data$Temp3pm <- ifelse(data$Temp3pm < box_Temp3pm[1,] | data$Temp3pm > box_Temp3pm[5,], NA, data$Temp3pm)

################################################################################
# 결측치 열별 확인
colSums(is.na(data))

# 결측치 제거
nrow(data)
data <- na.omit(data)
nrow(data)
colSums(is.na(data))

################################################################################
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

################################################################################
# 초기 예측 변수 선정 (목표 변수 : RainTomorrow)
data_select <- subset(data, select=c(Location, Rainfall, WindGustSpeed,
                                     Humidity9am, Humidity3pm, RainToday, RainTomorrow))

# 최적화 이후 최종 예측 변수 선정 (목표 변수 : RainTomorrow)
data_select <- subset(data, select=c(Rainfall, WindGustSpeed, Humidity9am,
                                     Humidity3pm, RainTomorrow))

str(data_select)

## I. 목표 변수 기반 층화 추출
data_select_index <- createDataPartition(data_select$RainTomorrow, p = 0.7, list = F)
data_select_train <- data_select[data_select_index,]
data_select_test <- data_select[-data_select_index,]

## J,K. 최적 파라미터, 적절한 예측 변수 선정 후 모델 실행 및 정확도 측정
# 모델 생성
fit_select <- rpart(
  RainTomorrow~.,
  data = data_select_train,
  minsplit = 475,
  minbucket = 215,
  cp = 0.001,
  maxdepth = 6,
  method="class",
  parms = list(split = 'gini') # or 'information'
)

# data_select_train에 대한 정확도 측정
pred_select_train = predict(fit_select, data_select_train, type = "class",)
confusionMatrix(data_select_train$RainTomorrow, pred_select_train, positive = 'Yes')

# pred_select_test에 대한 정확도 측정
pred_select_test = predict(fit_select, data_select_test, type = "class",)
confusionMatrix(data_select_test$RainTomorrow, pred_select_test, positive = 'Yes')

# plot the tree 
windows()
plot(fit_select)
text(fit_select, cex = 0.9, xpd = TRUE)

windows()
fancyRpartPlot(fit_select)

windows()
rpart.plot(fit_select)
################################################################################
## L. 모든 변수 포함 모델 생성 및 정확도 평가
data_all_index <- createDataPartition(data$RainTomorrow, p = 0.7, list = F)
data_all_train <- data[data_all_index,]
data_all_test <- data[-data_all_index,]

fit_all <- rpart(
  RainTomorrow~Date+Location+MinTemp+MaxTemp+Rainfall+Evaporation+Sunshine+
    WindGustDir+WindGustSpeed+WindDir9am+WindDir3pm+WindSpeed9am+WindSpeed3pm+
    Humidity9am+Humidity3pm+Pressure9am+Pressure3pm+Cloud9am+Cloud3pm+Temp9am+
    Temp3pm+RainToday,
  data = data_all_train,
  minsplit = 2,
  minbucket = 1,
  cp = -1,
  method="class",
  parms = list(split = 'gini') # or 'information'
)

# pred_all_train에 대한 정확도 측정
pred_all_train = predict(fit_all, data_all_train, type = "class",)
confusionMatrix(data_all_train$RainTomorrow, pred_all_train, positive = 'Yes')

# pred_all_test에 대한 정확도 측정
pred_all_test = predict(fit_all, data_all_test, type = "class",)
confusionMatrix(data_all_test$RainTomorrow, pred_all_test, positive = 'Yes')

# 모든 변수를 포함시킬경우 매우 복잡한 트리의 형태를 가지며 과적합으로 인해
# 트레이닝셋의 정확도는 매우 높지만 테스트셋의 정확도는 낮아진다.

# plot the tree
windows()
plot(fit_all)
text(fit_all, cex = 0.9, xpd = TRUE)

windows()
fancyRpartPlot(fit_all)

windows()
rpart.plot(fit_all)

################################################################################
## N. 학습 곡선
data_lc <- createDataPartition(data_select$RainTomorrow, p = 0.2, list = F)
data_lc <- data_select[data_lc,]
data_lc_index <- createDataPartition(data_lc$RainTomorrow, p = 0.7, list = F)
data_lc_train <- data_select[data_lc_index,]
data_lc_test <- data_select[-data_lc_index,]

fit_lc <- rpart(
  RainTomorrow~.,
  data = data_lc_train,
  minsplit = 475,
  minbucket = 215,
  cp = 0.001,
  maxdepth = 6,
  method="class",
  parms = list(split = 'gini')
)

# data_select_train에 대한 정확도 측정
pred_lc_train = predict(fit_lc, data_lc_train, type = "class",)
confusionMatrix(data_lc_train$RainTomorrow, pred_lc_train, positive = 'Yes')

# pred_select_test에 대한 정확도 측정
pred_lc_test = predict(fit_lc, data_lc_test, type = "class",)
confusionMatrix(data_lc_test$RainTomorrow, pred_lc_test, positive = 'Yes')

Trainingscore <- c(0.8629, 0.8624, 0.8407, 0.8374, 0.8373, 0.8372, 0.8323, 0.8337, 0.8361, 0.8431)
Cross_validation <- c(0.7879, 0.8360, 0.8372, 0.8410, 0.8424, 0.8427, 0.8488, 0.8532, 0.8563, 0.8516)
TrainsetCount <- c(370, 1850, 3700, 7400, 11100, 14800, 18500, 22200, 25900, 29600)

ggplot() + 
  geom_line(aes(x = TrainsetCount, y = Trainingscore),col="red",size=2)+
  geom_line(aes(x = TrainsetCount, y = Trainingscore),col="red",size=20,alpha=.1)+
  geom_line(aes(x = TrainsetCount, y = Cross_validation),col="blue",size=2) +
  geom_line(aes(x = TrainsetCount, y = Cross_validation),col="blue",size=20,alpha=0.1)