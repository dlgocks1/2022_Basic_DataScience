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


# 데이터 로드
data <- read.csv (
  file = "bank.csv",
  header = TRUE
)

# 데이터가 잘 로드되었는지 확인
head(data)
str(data)

# 데이터 탐색 - 우선 상관계수가 높은 변수들을 눈여겨봄 (결과: duration이 약 0.53으로 가장 높음)
#상관계수 계산 위한 목표변수 수치형 변환
corData <- data
corData$deposit <- ifelse(corData$deposit == "yes", 1, corData$deposit)
corData$deposit <- ifelse(corData$deposit == "no", 0, corData$deposit)
corData$deposit <- as.numeric(corData$deposit)

# 수치형 변수들의 spearman 상관계수 cor로 알아보기 (범주형(deposit)은 numeric으로 변환(0, 1))
cor (corData$age, corData$deposit, method = "spearman", use="complete.obs")
cor (corData$day, corData$deposit, method = "spearman", use="complete.obs")
cor (corData$balance, corData$deposit, method = "spearman", use="complete.obs")
cor (corData$duration, corData$deposit, method = "spearman", use="complete.obs")
cor (corData$campaign, corData$deposit, method = "spearman", use="complete.obs")
cor (corData$pdays, corData$deposit, method = "spearman", use="complete.obs")
cor (corData$previous, corData$deposit, method = "spearman", use="complete.obs")

# 기술통계 확인
describe(data)

# 목표변수는 deposit 변수로 선정
# deposit 변수는 binary 범주형 변수로, term deposit을 subscribe 했는지, 안했는지의
# 'yes' or 'no' 값을 가지며
# 이 dataset은 다른 예측변수들의 값을 가진 고객이 term deposit을 subscribe 할까, 안할까에 대한
# 예측 분류로 사용할 수 있기 때문.

# 데이터 전처리 단계
# 우선 데이터 형 변환 (범주형 --> factor)
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

# 데이터 형 변환이 잘 되었는지 확인
str(data)


# 결측치와 극단값 등 이상치를 갖는 record 제거

# 범주형 변수 결측치 확인 (job은 levels 가 많아서 other로 통틀어 나오기 때문에 unknown을 따로 비교해서 확인)
summary(data)
sum(data$job == "unknown")
sum(data$poutcome == "unknown")

# 결측치가 있는 변수에 결측치 갖는 행 삭제 (확인 결과 job, education, contact, poutcome 에 unknown 값 존재)
# (확인결과 poutcome 변수는 결측치가 대부분을 차지하여 처리하지 않고 대신 예측변수로 사용 안하는 것으로 판단)
count(data)
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
data$age <- ifelse(is.na(data$age), 40.17, data$age)

#data <- data %>% filter(!is.na(duration))

# 데이터 확인 (남은 record는 5,755개 (11,162 -> 5,755 : 5,407개 record 삭제됨))
count(data)
str(data)

# 결측치를 삭제하지 않고 대체하는 방법도 써보기 위해 임의로 결측치 생성
#set.seed(2)
#mis_idx = createDataPartition(data$deposit, p = .01, list = F)
#data$age[mis_idx] = NA
#sum(is.na(data$age))

# age 결측치 대체 (평균값으로 대체함 (평균값: 40.17, 중앙값: 37))
describe(data$age)
data$age <- ifelse(is.na(data$age), 40.17, data$age)

# 전처리된 데이터에서 기술 통계 다시 확인
describe(data)

data_yes <- filter(data, deposit == "yes")
data_no <- filter(data, deposit == "no")
describe(data_yes)
describe(data_no)


# 사전가지치기 모델 생성

# 적절한 예측변수 집합 선정
# pdays 변수 제외 (수치형 값이면서 명목형 변수의 의미를 가지는 특정값 존재(-1))
# poutcome 변수 제외 (결측치가 대부분이여서 처리하지 않고 대신 예측변수로 사용 X)
# 예측변수 선정 방법은 히스토그램을 그려보고
# 목표변수가 겹치는 구간이 상대적으로 적은 데이터가 분포하는 변수로 선정


HT1 <- ggplot(data, aes(x = age, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_histogram(bins =71, alpha=0.8, position="identity")

HT2 <- ggplot(data, aes(x = job, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_bar(alpha=0.8, position="identity")

HT3 <- ggplot(data, aes(x = marital, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_bar(alpha=0.8, position="identity")

HT4 <- ggplot(data, aes(x = education, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_bar(alpha=0.8, position="identity")

HT5 <- ggplot(data, aes(x = default, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_bar(alpha=0.8, position="identity")

HT6 <- ggplot(data, aes(x = balance, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_histogram(bins =71, alpha=0.8, position="identity")

HT7 <- ggplot(data, aes(x = housing, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_bar(alpha=0.8, position="identity")

HT8 <- ggplot(data, aes(x = loan, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_bar(alpha=0.8, position="identity")

HT9 <- ggplot(data, aes(x = contact, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_bar(alpha=0.8, position="identity")

HT10 <- ggplot(data, aes(x = day, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_bar(alpha=0.8, position="identity")

HT11 <- ggplot(data, aes(x = month, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_bar(alpha=0.8, position="identity")

HT12 <- ggplot(data, aes(x = duration, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_histogram(bins=71, alpha=0.8, position="identity")

HT13 <- ggplot(data, aes(x = campaign, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_histogram(bins =71, alpha=0.8, position="identity")

HT14 <- ggplot(data, aes(x = previous, fill = deposit, color = deposit)) +
  theme(legend.position="top") + geom_histogram(bins =71, alpha=0.8, position="identity")

# 히스토그램 비교 (patchwork 라이브러리를 로드하면 밑에 연산으로 plot을 같이 띄울 수 있음 (4개씩 보기))
(HT1 + HT2) / (HT3 + HT4)
(HT5 + HT6) / (HT7 + HT8)
(HT9 + HT10)/ (HT11 + HT12)
HT13 + HT14

(HT7 +HT14) / (HT11 + HT12) 


# 예측변수 선정 - housing, marital, month, duration, previous
# housing은 차이가 500이상씩 존재 (상대적으로 뚜렷함)
# marital 도 차이가 500이상씩 존재함 // 하나는 비슷
# month는 값별 deposit 차이가 매우 크게 분포
# duration은 약 200을 cutoff로 하여 큰 차이를 보임
# previous는 0.0일 때만 deposit 이 'no' 값을 갖는 경우가 존재함

# tr, ts set 층화추출 (creatDataPartition() 함수 사용 (층화 추출을 해주는 함수))
# tr set : ts set = 70 : 30 으로 설정
# seed 값을 줘서 매번 다른 set을 추출하는 것 방지
set.seed(1)
idx = createDataPartition(data$deposit, p = .7, list = F)
tr_set <- data[idx, ]
ts_set <- data[-idx, ]


# tr set 으로 decision tree 모델 학습 (위에서 선정한 예측변수 대입)
fit_1 <- rpart(deposit~housing+marital+month+duration+previous,
             data = tr_set
             )

# ts set 으로 성능 평가
# 0.7711의 정확도 보임
pred = predict(fit_1, ts_set, type = "class",)
print(data.frame(ts_set, pred))
confusionMatrix(ts_set$deposit, pred, positive = 'yes')


# 예측변수를 더 최적화하기 위해서 하나씩 빼보고 더해가며 최적 예측변수 집합 구함 (정확도를 기준으로)
# 결과: marital 변수를 빼도 정확도가 동일하여 뺌.
fit_2 <- rpart(deposit~housing+month+duration+previous,
               data = tr_set
               )

# ts set 으로 성능 평가
# 0.7711 의 정확도 보임
pred = predict(fit_2, ts_set, type = "class",)
print(data.frame(ts_set, pred))
confusionMatrix(ts_set$deposit, pred, positive = 'yes')

# 예측변수 집합 최적화 끝


# cp 최적화
# caret 패키지 사용 (rpart를 이용하면 cp 파라미터 최적화 가능)
modelLookup("rpart")

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE)

system.time(rpartFit1 <- train(deposit~housing+month+duration+previous,
                               data = tr_set,
                               method = "rpart",
                               tuneLength = 10,
                               trControl = train.control,
                               metric = "ROC"))


rpartFit1
plot(rpartFit1)


# rpartFit1 출력에서 cp = 0.004069176 을 최적값으로 출력해줌.
# 또한 plot에서도 확인할 수 있음.
# 이를 적용하여 cp = 0.004 로 나무모델 생성

fit_3 <- rpart(deposit~housing+month+duration+previous,
               data = tr_set
               )
#cp = 0.004

# ts set 으로 성능 평가
# 0.8094 의 정확도 보임
pred = predict(fit_3, ts_set, type = "class",)
print(data.frame(ts_set, pred))
confusionMatrix(ts_set$deposit, pred, positive = 'yes')

# cp 최적화 끝


# maxdepth 최적화
# caret 패키지 사용 (rpart2 를 이용하면 maxdepth 파라미터 최적화 가능)
modelLookup("rpart2")

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE)

system.time(rpartFit2 <- train(deposit~housing+month+duration+previous,
                               data = tr_set,
                               method = "rpart2",
                               tuneLength = 10,
                               trControl = train.control,
                               metric = "ROC"))

rpartFit2
plot(rpartFit2)

# rpartFit2 출력에서 maxdepth = 8 혹은 9를 최적값으로 출력해줌 (반복 시마다 8 or 9 출력).
# 또한 plot에서도 확인할 수 있음.
# 이를 적용하여 maxdepth = 8 로 나무모델 생성

fit_4 <- rpart(deposit~housing+month+duration+previous,
               data = tr_set,
               cp = 0.004,
               maxdepth = 8
               )

# ts set 으로 성능 평가
# 0.8094 의 정확도 보임
pred = predict(fit_4, ts_set, type = "class",)
print(data.frame(ts_set, pred))
confusionMatrix(ts_set$deposit, pred, positive = 'yes')

# maxdepth 최적화 끝

# minsplit 최적화
# for loop 이용 1부터 100까지 minsplit 을 조절하면서 최대 정확도를 보이는 최대 minsplit 값 선택

# i 와 dataframe을 인자로 받아 인자로 받은i와 minsplit을 i로 하여 나온 정확도를 벡터로 생성하여
# 인자로받은 dataframe에 결합하여 반환해주는 함수 정의 (optMinSplit())
optMinSplit <- function(i, df) {
  fit_o <- rpart(deposit~housing+month+duration+previous,
                 data = tr_set,
                 cp = 0.004,
                 maxdepth = 8,
                 minsplit = i
  )
  pred = predict(fit_o, ts_set, type = "class",)
  
  print(i)
  print(confusionMatrix(pred, ts_set$deposit)[[3]][1])
  
  df <- rbind(df, c(i, confusionMatrix(pred, ts_set$deposit)[[3]][1]))
  
  return(df)
}

# 빈 dataframe 생성
df_optMinSplit <- data.frame()

# for loop을 이용하여 minsplit 값을 1부터 100까지 넣어가며 결과 확인
for(i in 1:100) {
  df_optMinSplit <- optMinSplit(i, df_optMinSplit)
}

# 그래프로 시각화 (x -> 최적화 파라미터  //  y -> accuracy)
colnames(df_optMinSplit) = c('minsplit', 'Accuracy')

H_optMinSplit <- ggplot(df_optMinSplit, aes(x = minsplit , y = Accuracy)) +
  theme(legend.position="top") + geom_line()

H_optMinSplit

# 최적 파라미터는 minsplit = 52 인 것을 확인
# 이를 적용하여 나무모델 생성 64

fit_5 <- rpart(deposit~housing+month+duration+previous,
               data = tr_set,
               cp = 0.004,
               maxdepth = 8,
               minsplit = 64
               )

# ts set 으로 성능 평가
# 0.8094 의 정확도 보임
pred = predict(fit_5, ts_set, type = "class",)
print(data.frame(ts_set, pred))
confusionMatrix(ts_set$deposit, pred, positive = 'yes')

# minsplit 최적화 끝

# minbucket 최적화
# for loop 이용 1부터 100까지 minbucket 을 조절하면서 최대 정확도를 보이는 최대 minbucket 값 선택

# i 와 dataframe을 인자로 받아 인자로 받은i와 minbucket을 i로 하여 나온 정확도를 벡터로 생성하여
# 인자로받은 dataframe에 결합하여 반환해주는 함수 정의 (optMinBucket())
optMinBucket <- function(i, df) {
  fit_o <- rpart(deposit~housing+month+duration+previous,
                 data = tr_set,
                 cp = 0.004,
                 maxdepth = 8,
                 minsplit = 64,
                 minbucket = i
  )
  pred = predict(fit_o, ts_set, type = "class",)
  
  print(i)
  print(confusionMatrix(pred, ts_set$deposit)[[3]][1])
  
  df <- rbind(df, c(i, confusionMatrix(pred, ts_set$deposit)[[3]][1]))
  
  return(df)
}

# 빈 dataframe 생성
df_optMinBucket <- data.frame()

# for loop을 이용하여 minbucket 값을 1부터 100까지 넣어가며 결과 확인
for(i in 1:100) {
  df_optMinBucket <- optMinBucket(i, df_optMinBucket)
}

# 그래프로 시각화 (x -> 최적화 파라미터  //  y -> accuracy)
colnames(df_optMinBucket) = c('minbucket', 'Accuracy')

H_optMinBucket <- ggplot(df_optMinBucket, aes(x = minbucket , y = Accuracy)) +
  theme(legend.position="top") + geom_line()

H_optMinBucket

# 최적 파라미터는 minbucket = 17 인 것을 확인
# 이를 적용하여 나무모델 생성

fit_6 <- rpart(deposit~housing+month+duration+previous,
               data = tr_set,
               cp = 0.004,
               maxdepth = 8,
               minsplit = 64,
               minbucket = 17
)

# ts set 으로 성능 평가 
# 0.8094 의 정확도 보임 0.7786          
pred = predict(fit_6, ts_set, type = "class",)
print(data.frame(ts_set, pred))
confusionMatrix(ts_set$deposit, pred, positive = 'yes')


# split criterion 최적화 ('gini' or 'information')
# split criterion 의 default 값은 'gini' 이므로 'information'만 확인
fit_info <- rpart(deposit~housing+month+duration+previous,
               data = tr_set,
               parms = list(split = 'information'),
               cp = 0.004,
               maxdepth = 8,
               minsplit = 64,
               minbucket = 17
)

# ts set 으로 성능 평가 0.7786          
# 0.8001 의 정확도 보임 ==> split criterion 이 'gini' 인 모델보다 정확도 0.0083 하락
# 따라서 split criterion 의 최적값은 'gini' (default 값)
pred = predict(fit_info, ts_set, type = "class",)
print(data.frame(ts_set, pred))
confusionMatrix(ts_set$deposit, pred, positive = 'yes')


# 사전 가지치기 모델에 적용할 training set 과 test set 비율 최적화 (그래프)

opt_tr_set <- function(model, df) {
  for(i in 6:9) {
    percent = i*0.1
    mean_acc = 0
    
    for(k in 1:5) {
      set.seed(k+1)
      idx_o = createDataPartition(data$deposit, p = percent, list = F)
      tr_set_o <- data[idx_o, ]
      ts_set_o <- data[-idx_o, ]
      
      pred = predict(model, ts_set_o, type = "class",)
      confusionMatrix(ts_set_o$deposit, pred, positive = 'yes')
      
      mean_acc = mean_acc + confusionMatrix(pred, ts_set_o$deposit)[[3]][1]
      
    }
    
    mean_acc = mean_acc / 5
    
    df <- rbind(df, c(percent, mean_acc))
    
  }
  
  return(df)
  
}

# model 을 사전가지치기 모델로 하여 trainingset 비율 최적화
df_optSet_pre <- data.frame()
df_optSet_pre <- opt_tr_set(fit_6, df_optSet_pre)

df_optSet_pre

# 그래프로 시각화 (x -> 최적화 파라미터  //  y -> accuracy)
colnames(df_optSet_pre) = c('tr_set_percent', 'Accuracy')

H_optSet_pre <- ggplot(df_optSet_pre, aes(x = tr_set_percent , y = Accuracy)) +
  theme(legend.position="top") + geom_line()

H_optSet_pre

# training set : test set 의 비율이 7:3 인 경우가 평균적으로 정확도 0.8113557을 보이며 최적 비율인 것을 확인
# 기존에 사용한 training set과 같은 비율임.

# 최적화 완료된 사전 가지치기 모델 생성

fit_pre <- rpart(deposit~housing+month+duration+previous,
                 data = tr_set,
                 parms = list(split = 'gini'),
                 cp = 0.004,
                 maxdepth = 8,
                 minsplit = 64,
                 minbucket = 17
) 

# ts set 으로 성능 평가
# Accuracy = 0.8094  //  Sensitivity = 0.7979  //  Specificity = 0.8208
pred = predict(fit_pre, ts_set, type = "class",)
print(data.frame(ts_set, pred))
confusionMatrix(ts_set$deposit, pred, positive = 'yes')

pred = predict(fit_pre, tr_set, type = "class",)
print(data.frame(tr_set, pred))
confusionMatrix(tr_set$deposit, pred, positive = 'yes')

# 사전 가지치기 모델 시각화
windows()
fancyRpartPlot(fit_pre)
readline('Enter to resume ')
  dev.off()


# 사전 가지치기 모델 생성 및 최적화 끝


# 사후 가지치기 모델 생성
# training set 및 test set 은 사전 가지치기 모델에서 사용한 dataset 과 동일함.

# 우선 Full tree 모델 생성
fit_full <- rpart(deposit~housing+month+duration+previous,
                  data = tr_set,
                  method = 'class',
                  cp = 0,
                  minsplit = 1,
                  minbucket = 1,
                  maxdepth = 30
)

# Full tree 시각화
windows()
fancyRpartPlot(fit_full)
readline('Enter to resume ')
dev.off()

#Full tree tr_set 적용 (Accuracy = 1  //  Sensitivity = 1  //  Specificity = 1))
pred_full = predict(fit_full, tr_set, type = "class",)
print(data.frame(tr_set, pred_full))
confusionMatrix(tr_set$deposit, pred_full, positive = 'yes')

#Full tree ts_set 적용 (Accuracy = 0.7514  //  Sensitivity = 0.7578  //  Specificity = 0.7459)
pred_full_t = predict(fit_full, ts_set, type = "class",)
print(data.frame(ts_set, pred_full_t))
confusionMatrix(ts_set$deposit, pred_full_t, positive = 'yes')


#사후 가지치기 cp 최적화 0.0042387250     값으로 설정
#SE범위 내에서 간단한 모델로 선택
plotcp(fit_full)
printcp(fit_full)
fit_full

# 사후 가지치기 (cp를 최적화) 모델 생성  ( cp 는 cptable 을 이용하여 최적화, cp = 0.001525941)
# = fit_full$cptable[which.min(fit_full$cptable[, "xerror"]), "CP"]
#print(cp_opt)

fit_post <- prune(fit_full, cp=2.7288e-03)


# 사후 가지치기 나무모델 시각화
windows()
fancyRpartPlot(fit_post)
readline('Enter to resume ')
dev.off()

# 사후 가지치기 모델 tr_set 적용 (Accuracy = 0.8595  //  Sensitivity = 0.8441  //  Specificity = 0.8752)
#해찬 사후 가지치기 모델 tr_set 적용 (Accuracy = 0.83  //  Sensitivity = 0.8188  //  Specificity = 0.8411)
pred_post = predict(fit_post, tr_set, type = "class",)
print(data.frame(tr_set, pred_post))
confusionMatrix(tr_set$deposit, pred_post, positive = 'yes')

# 사후 가지치기 모델 ts_set 적용 (Accuracy = 0.8181  //  Sensitivity = 0.8063  //  Specificity = 0.8299)
#해찬 사후 가지치기 모델 ts_set 적용 (Accuracy = 0.8007  //  Sensitivity = 0.7849  //  Specificity = 0.8169)
pred_post_t = predict(fit_post, ts_set, type = "class",)
print(data.frame(ts_set, pred_post_t))
confusionMatrix(ts_set$deposit, pred_post_t, positive = 'yes')


# 사후 가지치기 모델 최적화 검증

# cp 값이 최적화되었는지 확인 (cptable 을 이용해 구한 최적 cp 값에서 - 0.001 ~ +0.009 구간의 accuracy를 측정)

optCpPost <- function(cp_opt_o, df) {
  fit_post_o <- prune(fit_full, cp=cp_opt_o)
  pred_post_o = predict(fit_post_o, ts_set, type = "class",)
  confusionMatrix(ts_set$deposit, pred_post_o, positive = 'yes')
  
  #print(cp)
  print(confusionMatrix(pred_post_o, ts_set$deposit)[[3]][1])
  
  df <- rbind(df, c(cp_opt_o, confusionMatrix(pred_post_o, ts_set$deposit)[[3]][1]))
  
}

df_optCp_post <- data.frame()
#cp_opt_o = cp_opt - 0.002
cp_opt_o = 0.0042387250 - 0.002

for(i in 1:10) {
  cp_opt_o = cp_opt_o + 0.001
  
  df_optCp_post <- optCpPost(cp_opt_o, df_optCp_post)
  
}

df_optCp_post

# 그래프로 시각화 (x -> 최적화 파라미터  //  y -> accuracy)
colnames(df_optCp_post) = c('CP', 'Accuracy')

H_optCp_post <- ggplot(df_optCp_post, aes(x = CP , y = Accuracy)) +
  theme(legend.position="top") + geom_line()

H_optCp_post

# cp = 0.001525941 일 때 정확도 0.8180765을 보이며 최적 cp인 것을 확인
# 이게 최적의 CP인지는 정확도로 판별할 수 있지만, 트리의 복잡도를
# 같이 생각하는게 맞는 듯

# model 을 사전가지치기 모델로 하여 trainingset 비율 최적화
df_optSet_post <- data.frame()
df_optSet_post <- opt_tr_set(fit_post, df_optSet_post)

df_optSet_post

# 그래프로 시각화 (x -> 최적화 파라미터  //  y -> accuracy)
colnames(df_optSet_post) = c('tr_set_percent', 'Accuracy')

H_optSet_post <- ggplot(df_optSet_post, aes(x = tr_set_percent , y = Accuracy)) +
  theme(legend.position="top") + geom_line()

H_optSet_post

# training set : test set 의 비율이 7:3 인 경우가 평균적으로 정확도 0.8454229 를 보이며 최적 비율인 것을 확인
# 기존에 사용한 training set과 같은 비율임.


# 사후 가지치기 모델 생성 및 최적화 끝

# 결과:
# 사전 가지치기 모델: Accuracy = 0.8094  //  Sensitivity = 0.7979  //  Specificity = 0.8208
# 사후 가지치기 모델: Accuracy = 0.8181  //  Sensitivity = 0.8063  //  Specificity = 0.8299

# 사후 가지치기 모델이 정확도, 민감도, 특이도 모두 근소하게 우세하므로
# 사후 가지치기 모델을 선택하여 rule set 을 추출.


# rule set 추출
rattle::asRules(fit_post, F)
fit_post

# console 에 출력되는 rule에서 cover는 해당 노드에 물리는 데이터 레코드 수를 의미하고
# prob 은 목표변수가 positive class 인 비율을 의미한다. (prob 이 0 or 1 에 가까울수록 불순도가 낮음)


# 가장 중요한 rule 은 많은 데이터에 적용되는 rule 로
# Rule number: 64 가 cover = 1144 (전체 데이터의 28%)로 가장 압도적으로 높다.
# 또한 prob = 0.06 으로 준수한 신뢰도를 보여준다.
# 가장 중요한 rule:
# if (duration< 286.5) && (month=aug,jan,jul,may,nov) && (previous< 0.5) && (age< 60.5) && (age>=26.5)
#     && (day=2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
#   then (deposit = "no") else (deposit = "yes")


# 가장 신뢰도가 높은 rule 은 불순도가 가장 낮은 rule 로
# Rule number: 65 가 prob = 1.00 으로 pure 한 결과를 보여주며
# cover = 7 로 똑같이 prob = 1.00 인 몇 개의 rule 보다 조금 근소하게 높다.
# 가장 신뢰도가 높은 rule:
# if (duration< 286.5) && (month=aug,jan,jul,may,nov) && (previous< 0.5) && (age< 60.5)
#     && (age>=26.5) && (day=1,3)
#   then (deposit = "yes") else (deposit = "no")


# 규칙 추출 끝