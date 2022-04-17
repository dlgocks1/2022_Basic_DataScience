library(dplyr)
library(ggplot2)
library(patchwork)
library(psych)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(tidyverse)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(randomForest)
library(klaR) # for NaiveBayes (=nb) function
library(readxl)

data = read_excel(path="./titanic3.xls", sheet ="titanic3")

#corData <- data
#corData$deposit <- ifelse(corData$survived == 1, TRUE, corData$survived)
#corData$deposit <- ifelse(corData$survived == 0, FALSE, corData$survived)
#corData$survived
data$survived <- as.numeric(data$survived)
data$ticket <- as.numeric(data$ticket)
data$survived <- factor(data$survived, levels = c(0,1), labels = c("False", "True"))

data <- tibble(survived = as.factor(data$survived),
               name = data$name,
               pclass = as.factor(data$pclass),
               sex  = as.factor(data$sex),
               age  =  data$age,
               sibsp  = data$sibsp,
               parch  = data$parch,
               ticket  = data$ticket,
               fare  =  data$fare,
               cabin  = data$cabin,
               embarked  = as.factor(data$embarked),
               boat  = data$boat)

# 데이터 형 변환이 잘 되었는지 확인
str(data)
missmap(data)
#boat cabin은 결측치가 너무 많음 변수 제외
table(is.na(data$boat))
table(is.na(data$cabin))

data <- data[-12]
data <- data[-10]

missmap(data)
#NA값 mice패키지로 채워넣기
mice_mod <- mice(data[, c("age","embarked","ticket")], method='rf')
mice_complete <- complete(mice_mod)
missmap(mice_complete)

#데이터 옮기기
data$age <- mice_complete$age
data$embarked <- mice_complete$embarked
data$ticket <- mice_complete$ticket

#데이터 다시 체크
missmap(data)
count(data)

set.seed(1)
#테스트셋 트레인셋 분리
idx = createDataPartition(data$survived, p = .7, list = F)
tr_set <- data[idx, ]
ts_set <- data[-idx, ]

#Data Visualization
#age는 상관관계
ggplot(data, aes(age, colour = survived)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Deposit")

#visual 2
ggplot(data, aes(parch, colour = survived)) +
  geom_freqpoly(binwidth = 1)

ggplot(data, aes(sibsp, colour = survived)) +
  geom_freqpoly(binwidth = 1)

ggplot(data, aes(ticket, colour = survived)) +
  geom_freqpoly(binwidth = 100000)

c <- ggplot(data, aes(x=ticket, fill=survived, color=survived)) +
  geom_histogram(binwidth = 100000)#티켓번호호
c + theme_bw()

#visual 3
c <- ggplot(data, aes(x=fare, fill=survived, color=survived)) +
  geom_histogram(binwidth = 100) + labs(title="비용과 살아남음 상관관계계")
c + theme_bw()


#visual 5
ggpairs(data)

#install.packages("prettyR")
library(prettyR)

#범주형 데이터들의 독립성 테스트
#p-value > 0.05 이면 서로 독립이다. 귀무가설 기각 
#카이제곱
head(data)
crosstab <- xtabs(formula = ~pclass+sex,data)
chisq.test(crosstab,simulate.p.value = TRUE)

crosstab <- xtabs(formula = ~pclass+name,data)
chisq.test(crosstab,csimulate.p.value = TRUE)

crosstab <- xtabs(formula = ~sex+name,data)
chisq.test(crosstab,simulate.p.value = TRUE)

crosstab <- xtabs(formula = ~sex+embarked,data)
chisq.test(crosstab,simulate.p.value = TRUE)



#연속형 데이터들의 독립성 테스트
#산점도와 공분산 이용, 두값이 서로 상관없이 변한다면 0
cov(data$age,data$fare)
ggplot(data, aes(x=age, y=fare)) + geom_point()

ggplot(data, aes(x=age, y=parch)) + geom_point()
cov(data$age,data$parch)

ggplot(data, aes(x=age, y=sibsp)) + geom_point()
cov(data$age,data$sibsp)

ggplot(data, aes(x=age, y=ticket)) + geom_point()
cov(data$age,data$ticket)

ggplot(data, aes(x=age, y=fare)) + geom_point()
cov(data$age,data$fare)

data

#i.	두 변수 간의 독립성 검증
#- 일반적인 독립과 클래스내 독립성의 구별, 
#- 여러가지 방법을 사용하고 결과를 비교 고찰 

# check the class ratioes
prop.table(table(tr_set$survived)) * 100
prop.table(table(ts_set$survived)) * 100

#create objects x which holds the predictor variables and y which holds the response variables
x = tr_set[,-1]
head(x)
head(tr_set)
y = tr_set$survived

# optimize model 
modelLookup("nb")

# set up tune grid values
fL <- seq(0, 1, 0.2)

usekernel <- c(TRUE, FALSE)
#usekernel <- FALSE

adjust <- seq(1, 5, 0.5)
grid <- expand.grid(fL=fL, usekernel=usekernel, adjust=adjust)
head(grid)

# optimization
model = train(x,
              y,
              'nb',
              trControl=trainControl(method='cv',number=10),
              tuneGrid=grid
)
model

# Step 7: Model Evaluation

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = ts_set) 
Predict

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(Predict, ts_set$survived)
#Plot Variable performance
X <- varImp(model) # Variable importance - RF
X

plot(X)




