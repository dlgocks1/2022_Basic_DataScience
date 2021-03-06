library(e1071)
library(klaR)
library("dplyr")
library("ggpubr")
library("caret")
library("rpart")
library(ggplot2)
library(patchwork)
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

data<-read.csv("./bank.csv",header=T)

head(data)
#pdays, poutcome 제거
data <- data[-14]
data <- data[-15]

# 데이터가 잘 로드되었는지 확인
head(data)
str(data)

corData <- data
corData$deposit <- ifelse(corData$deposit == "yes", 1, corData$deposit)
corData$deposit <- ifelse(corData$deposit == "no", 0, corData$deposit)
#corData$deposit <- as.numeric(corData$deposit)
data$deposit <- as.factor(corData$deposit)
#data$deposit <- as.factor(data$deposit)
data$deposit
# 기술통계 확인
describe(data)

# 목표변수는 deposit 변수로 선정
# deposit 변수는 binary 범주형 변수로, term deposit을 subscribe 했는지, 안했는지의
# 'yes' or 'no' 값을 가지며
# 이 dataset은 다른 예측변수들의 값을 가진 고객이 term deposit을 subscribe 할까, 안할까에 대한
# 예측 분류로 사용할 수 있기 때문.
# 우선 데이터 형 변환 (범주형 --> factor)
# 데이터 형 변환이 잘 되었는지 확인
str(data)

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

# campaign
boxplot(data$campaign)$stats
data$campaign <- ifelse(data$campaign < 1 | data$campaign > 6, NA, data$campaign)
table(is.na(data$campaign))

# balance
boxplot(data$balance)$stats
data$balance <- ifelse(data$balance < -2282 | data$balance > 4256, NA, data$balance)
table(is.na(data$balance))

# age
boxplot(data$age)$stats
data$age <- ifelse(data$age < 18 | data$age > 74, NA, data$age)
table(is.na(data$age))

# duration
boxplot(data$duration)$stats
data$duration <- ifelse(data$duration < 2 | data$duration > 1058, NA, data$duration)
table(is.na(data$duration))

missmap(data)
#NA값 mice패키지로 채워넣기
mice_mod <- mice(data[, c("previous","campaign","balance","age","duration")], method='rf')
mice_complete <- complete(mice_mod)
missmap(mice_complete)

data$previous <- mice_complete$previous
data$campaign <- mice_complete$campaign
data$balance <- mice_complete$balance
data$age <- mice_complete$age
data$duration <- mice_complete$duration

#데이터 다시 체크
missmap(data)
head(data)

#####EDA####

## NA check


data[is.na(data)==TRUE,]

## numeric/ character check
for (i in 1:17){
  
  if(is.numeric(data[0,i])){
    
    cat("수치형",colnames(data)[i],"\n")
  }
  else if(is.character(data[0,i])){
    cat("명목형",colnames(data)[i],"\n")
  }
  else{
    cat("다른거",colnames(data)[i],"\n")
    
  }
}


head(data)
data_num<-data.frame(data$age,data$balance,data$day,data$duration,data$campaign,data$previous)
data_chr<-data.frame(data$job,data$marital,data$education,data$default,data$housing,data$loan,data$contact,data$month)


#normal_check<-function(x){
#  s<-sample(x,5000)
#  k<-shapiro.test(s) 
#  print(k$p.value>=0.05)
#  print(k$p.value)
#}

#sapply(data_num,function(x)normal_check(x)) ##  p-value>0.05이상일 경우 norm 


#hist(k$data.age)

#s<-sample(k$data.balance,5000)
#ggqqplot(k$data.balance)
#shapiro.test(s)


#ggqqplot(k$data.age)
#s<-sample(k$data.age,2000)
#shapiro.test(s)
data

#-----------------------------------------------------------------------------------------
#####checking independence
## in categoricla var , when checking independence, set p-value>0.01 
## in numerical var, not following normal distribution -> output may have low reliability when using cor or t-test..etc 

data_y<-data[data$deposit==1,]
data_y_chr<-data.frame(data_y$job,data_y$marital,data_y$education,data_y$default,data_y$housing,data_y$loan,data_y$contact,data_y$month,data_y$day)
data_y_num<-data.frame(data_y$age,data_y$balance,data_y$duration,data_y$campaign,data_y$previous)

data_n<-data[data$deposit==0,]
data_n_chr<-data.frame(data_n$job,data_n$marital,data_n$education,data_n$default,data_n$housing,data_n$loan,data_n$contact,data_n$month,data_n$day)
data_n_num<-data.frame(data_n$age,data_n$balance,data_n$duration,data_n$campaign,data_n$previous)

chisq_anls<-function(x){
  max = 0
  min = 1000
  for(i in 1:(ncol(x)-1)){
    j<-i
    for( k in (i+1):(ncol(x))){
      indep_check<-chisq.test(x[,j],x[,k],simulate.p.value = TRUE)
      
      ##if(indep_check$p.value > 0.01){
      max = indep_check$p.value
      maxindex <- c(colnames(x)[j],colnames(x)[k])
      print(max)
      print(maxindex)
      ## }
      # if(indep_check$p.value < min){
      #  min = indep_check$p.value
      #    minindex <- c(colnames(x)[j],colnames(x)[k])
      # }
      
    }
  }
  
  #print("min")
  #print(minindex)
}

cor_anls<-function(x){
  
  max = 0
  min = 1000
  
  
  for(i in 1:(ncol(x)-1)){
    j<-i
    for(k in (i+1):ncol(x)){
      
      a<-abs(cor(x[,j],x[,k]))
      
      
      if(a > -0.3 & 0.3>a){
        max = a
        maxindex <- c(colnames(x)[j],colnames(x)[k])
        print("max")
        print(maxindex)
      }
      # if(a < min){
      #  min = a
      # minindex <- c(colnames(x)[j],colnames(x)[k])
      #}
      
      
    }
  }
  
  # print("min")
  #print(minindex)
  
}

#case 5 total class independece checking
#명목&수치형 Oneway Anova, T test 로해보기
#plan: 명목형/연속형변수 별로 독립성 강한 변수들 추출후 통합해서 독립성검정

#step1 명목형 연속형 변수 각각 독립성 강한것들 먼저 추출

data_chr<-data.frame(data$job,data$marital,data$education,data$default,data$housing,data$loan,data$contact,data$month,data$day)
chisq_anls(data_chr)

chisq.test(data$job,data$housing)

chisq.test(data$marital,data$housing)

##명목형변수에서 추출한 것들 (p-value>0.0004 정도면 독립에 가깝다고 판단) *marital & housing 경우 최저
data$marital
data$default
data$housing


data_num<-data.frame(data$age,data$balance,data$duration,data$campaign,data$previous)

cor_anls(data_num)

##연속형변수에서 추출한것들 (0.3>cor>-0.3 경우 )
data$age
data$balance
data$duration
data$campaign
data$previous


#step2 추출한것들 가지고 명목형 연속형 변수 혼합 비교 
# p-value>0.001 -> 그룹간 평균 유사하다는 의미 (판단기준: p-value>0.001)-> 상관도 낮다고 판단->독립 가능성 큼
data$marital
data$default
data$housing

chr_indep<-data.frame(data$marital,
                      data$default,
                      data$housing)

data$age
data$balance
data$duration
data$campaign
data$previous

num_indep<-data.frame(data$age,
                      data$balance,
                      data$duration,
                      data$campaign,
                      data$previous)

for(i in 1:length(num_indep)){
  
  for(k in 1:length(chr_indep)){
    
    result<-aov(num_indep[,i]~chr_indep[,k])
    print(colnames(num_indep)[i])
    print(colnames(chr_indep)[k])
    a<-summary(result)
    print(a)
  }
  
}

## 최종답 

data$duration

data$default
data$marital
data$housing

#campaing경우 marital를 제외하고는 default과 housing 과는 독립
summary(aov(data$campaign~data$marital))

data_indep<-data.frame(data$duration,
                       data$default,
                       data$marital,
                       data$housing)

colnames(data_indep)<-c("duration","default","marital","housing")

colnames(data_indep)


#### 독립성검증에 대한 평가: 수치형변수형의 normaml한 분포를 따르지 않으므로 검증방법과 결과값에 한계가 있다.


#-----------------------------------------------------------------------------------------
###naivebayesian & bayesian
#case1 class no
chisq_anls(data_n_chr)

#min : data_n.job"     "data_n.marital" -> dependant한 경향
#max : data_n.default" "data_n.day" -> independant한 경향

#min
data_n$marital
p_x_y<-nrow(data_n[data_n$job=="technician"&data_n$marital=="married",])/nrow(data_n)
p_y<-nrow(data_n)/nrow(data)
p_x1_y<-nrow(data_n[data_n$job=="technician",])/nrow(data_n)
p_x2_y<-nrow(data_n[data_n$marital=="married",])/nrow(data_n)

#output:: bayesian 
p_y*p_x_y 
#output:: naivebayesian
p_x1_y*p_x2_y*p_y

#max
p_x_y<-nrow(data_n[data_n$default=="no"&data_n$day==5,])/nrow(data_n)
p_y<-nrow(data_n)/nrow(data)
p_x1_y<-nrow(data_n[data_n$default=="no",])/nrow(data_n)
p_x2_y<-nrow(data_n[data_n$day==5,])/nrow(data_n)

#output:: bayesian 
p_y*p_x_y 
#output:: naivebayesian
p_x1_y*p_x2_y*p_y

##numeric case
cor_anls(data_n_num)

#[1] "max" -> dependent
#[1] "data_n.campaign" "data_n.previous"

#[1] "min"->independent
#[1] "data_n.age"      "data_n.duration"

data_n$default

#max -> dependent case
p_x_y<-nrow(data_n[data_n$campaign<2&data_n$previous==1,])/nrow(data_n)
p_x1_y<-nrow(data_n[data_n$campaign<2,])/nrow(data_n)
p_x2_y<-nrow(data_n[data_n$previous==1,])/nrow(data_n)


cor_anls(data_n_num)

#output:: bayesian 
p_y*p_x_y 
#output:: naivebayesian
p_x1_y*p_x2_y*p_y



#min-> independent case
data_n$duration

p_x_y<-nrow(data_n[data_n$age<41&data_n$duration<100,])/nrow(data_n)
p_y<-nrow(data_n)/nrow(data)

p_x1_y<-nrow(data_n[data_n$age<41,])/nrow(data_n)
p_x2_y<-nrow(data_n[data_n$duration<100,])/nrow(data_n)


#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y







#-----------------------------------------------------------------------------------------
#case 2 class yes
#in char
chisq_anls(data_y_chr)
#1] "max"
#[1] "data_y.marital" "data_y.housing"
#[1] "min"
#[1] "data_y.job"     "data_y.marital"

#max
data_y$housing

p_x_y<-nrow(data_y[data_y$marital=="married"&data_y$housing=="yes",])/nrow(data_y)
p_y<-nrow(data_y)/nrow(data)

p_x1_y<-nrow(data_y[data_y$marital=="married",])/nrow(data_y)
p_x2_y<-nrow(data_y[data_y$housing=="yes",])/nrow(data_y)

#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y

#min
data_y$marital

p_x_y<-nrow(data_y[data_y$job=="management"& data_y$marital=="married",])/nrow(data_y)
p_y<-nrow(data_y)/nrow(data)

p_x1_y<-nrow(data_y[data_y$job=="management",])/nrow(data_y)
p_x2_y<-nrow(data_y[data_y$marital=="married",])/nrow(data_y)

#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y


#in numeric var 
cor_anls(data_y_num)

#[1] "max"->dependent case
#[1] "data_y.age"     "data_y.balance"

#[1] "min"->independent case
#[1] "data_y.age"      "data_y.campaign"

data_y$balance

#max->dependent case
p_x_y<-nrow(data_y[data_y$age>32&data_y$balance<1097,])/nrow(data_y)
p_y<-nrow(data_y)/nrow(data)

p_x1_y<-nrow(data_y[data_y$age>32,])/nrow(data_y)
p_x2_y<-nrow(data_y[data_y$balance<1097,])/nrow(data_y)

#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y


#min->independent case

p_x_y<-nrow(data_y[data_y$age>32&data_y$campaign>2,])/nrow(data_y)
p_y<-nrow(data_y)/nrow(data)

p_x1_y<-nrow(data_y[data_y$age>32,])/nrow(data_y)
p_x2_y<-nrow(data_y[data_y$campaign>2,])/nrow(data_y)

#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y
#-----------------------------------------------------------------------------------------

#case3 all class case

##p-value값 기준으로 독립성 max min 찾기
chisq.test(data$default,data$housing) ##max p-value=0.8615 **p-value>0.0004 경우 독립에가깝다고 앞서 판단함
summary(aov(data$age~data$marital)) ##min  p-value =2e-16 

as.factor(data$default)
as.factor(data$housing)
##most high independent case

# yes class
p_x_y<-nrow(data_y[data_y$default=="yes"&data_y$housing=="no",])/nrow(data_y)
p_y<-nrow(data_y)/nrow(data)

p_x1_y<-nrow(data_y[data_y$default=="yes",])/nrow(data_y)
p_x2_y<-nrow(data_y[data_y$housing=="no",])/nrow(data_y)

#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y

# no class
p_x_y<-nrow(data_n[data_n$default=="yes"&data_n$housing=="no",])/nrow(data_n)
p_y<-nrow(data_n)/nrow(data)

p_x1_y<-nrow(data_n[data_n$default=="yes",])/nrow(data_n)
p_x2_y<-nrow(data_n[data_n$housing=="no",])/nrow(data_n)

#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y


##least indepence case

as.factor(data$marital)
# yes class
p_x_y<-nrow(data_y[data_y$age>32&data_y$marital=="married",])/nrow(data_y)
p_y<-nrow(data_y)/nrow(data)

p_x1_y<-nrow(data_y[data_y$age>32,])/nrow(data_y)
p_x2_y<-nrow(data_y[data_y$marital=="married",])/nrow(data_y)

#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y

#no class

p_x_y<-nrow(data_n[data_n$age>32&data_n$marital=="married",])/nrow(data_n)
p_y<-nrow(data_n)/nrow(data)

p_x1_y<-nrow(data_n[data_n$age>32,])/nrow(data_n)
p_x2_y<-nrow(data_n[data_n$marital=="married",])/nrow(data_n)

#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y




##네이브 베이지안/ 베이지안 계산 결과 -> 독립에 가까운 쌍일수록 두 값의 차이가 적다 

#-----------------------------------------------------------------------------------------
##naviebayesian model & decision Tree model

index<-createDataPartition(data$deposit,p=0.7,list=FALSE)
tr_data<-data[index,]
test_data<-data[-index,]

tr_data$deposit<-factor(tr_data$deposit)

#nb1<-naiveBayes(deposit~.,data=tr_data,)
nb<-NaiveBayes(deposit~.,data=tr_data,FL=1,usekernal=T)

#data(iris)

dt<-rpart(deposit~duration+default+marital+housing,data=tr_data,cp=0.11)

plotcp(dt)
printcp(dt)
fancyRpartPlot(dt)

predict_nb <- predict(nb1, newdata = test_data, )
predict_dt<-predict(dt,newdata=test_data,type="class")

confusionMatrix(predict_nb, as.factor(test_data$deposit),positive = "yes")
confusionMatrix(predict_dt, as.factor(test_data$deposit))

help("NaiveBayes")

##optimization


#-----------------------------------------------------------------------------------------
#수치형 변수의 모델링, 
#PDF, 정규 분포로 각각 모델링하여 클래스 조건부 확률은 (위에 있는듯)
#분류 예측 확률을 계산하고 결과를 비교 고찰

set.seed(1)
index<-createDataPartition(data$deposit,p=0.7,list=FALSE)
tr_set<-data[index,]
ts_set<-data[-index,]

# check the class ratioes
prop.table(table(tr_set$deposit)) * 100
prop.table(table(ts_set$deposit)) * 100

cor_anls(data_num)

#create objects x which holds the predictor variables and y which holds the response variables
x = tr_set[,-15]
#모든 클래스 내에서 독립성이 가장 높은 값 
#data$duration,
#data$default,
#data$marital,
#data$housing
#를 써서 nb모델 만들기
xtest <- cbind(x[12],x[5],x[3],x[7])
head(x)
head(xtest)
y = tr_set$deposit
# optimize model 
modelLookup("nb")

# set up tune grid values
fL <- seq(0, 1, 0.2) #NB식에서 class조건부확률이 0이되는것을 방지하기 위해 더해주는 수
usekernel <- c(TRUE, FALSE)#False면 PDF, True면 정규분포
adjust <- seq(1, 2, 0.5) #kernal의 bandwidth
grid <- expand.grid(fL=fL, usekernel=usekernel, adjust=adjust)
head(grid)

# optimization
model = train(xtest,
              y,
              'nb',
              trControl=trainControl(method='cv',number=10),
              tuneGrid=grid
)
model

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = ts_set) 
Predict


#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(Predict, ts_set$deposit)

#독립성이 높은 변수 4개 0.7456
#모든 변수 다 썼을 때 0.7759



#-------------------------------------------------------------------------------------
#독립적이지 않은 친구들 job marital housing age
head(x)
xtest <- cbind(x[2],x[3],x[7],x[1])
head(xtest)
y = tr_set$deposit
# optimize model 
modelLookup("nb")

# optimization
model = train(xtest,
              y,
              'nb',
              trControl=trainControl(method='cv',number=10),
              tuneGrid=grid
)
model

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = ts_set) 
Predict

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(Predict, ts_set$deposit)

# --------------------------------------------

#전체다 
head(x)
y = tr_set$deposit
# optimize model 
modelLookup("nb")

# optimization
model = train(x,
              y,
              'nb',
              trControl=trainControl(method='cv',number=10),
              tuneGrid=grid
)
model

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = ts_set) 
Predict

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(Predict, ts_set$deposit)





#-------------------------------------------------------------------------------------
#변수들 중요도
model = train(x,
              y,
              'nb',
              trControl=trainControl(method='cv',number=10),
              tuneGrid=grid
)
model
#Plot Variable performance
X <- varImp(model) # Variable importance - RF
X
plot(X)
dev.off()

#randomForest로 변수 중요도보기 수치형 변수 X
library(randomForest)
model_rf <- randomForest(deposit~.,tr_set)
randomForest::importance(model_rf)
randomForest::varImpPlot(model_rf)
