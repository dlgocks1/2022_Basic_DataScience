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
data2<-read.csv("./bank.csv",header=T)

# 데이터가 잘 로드되었는지 확인
head(data)
str(data)

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
# 우선 데이터 형 변환 (범주형 --> factor)
# 데이터 형 변환이 잘 되었는지 확인
str(data)

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
#previous","campaign","balance","age","duration
describe(data)
data$balance

for (i in 0:length(data$age)){
  data$age[i] = (data$age[i]-41.23) / (11.91)
}

for (i in 0:length(data$balance)){
  data$balance[i] = (data$balance[i]-1528.54) / (3225.41)
}

for (i in 0:length(data$campaign)){
  data$campaign[i] = (data$campaign[i]-2.51 ) / (2.72)
}

cov(data$age,data$balance)
cov(data$age,data$campaign)
cov(data$balance,data$campaign)

cor(data$age,data$balance)
cor(data$age,data$campaign)
cor(data$balance,data$campaign)

cov(data2$age,data2$balance) # 공분산 : 두변수가 각각 평균으로부터 변화하는 방향에 관한 기대값 
cov(data2$age,data2$campaign)
cov(data2$balance,data2$campaign)

cor(data2$age,data2$balance) # 상관계수 : 표준화된 공분산 0이면 상관관계 X
cor(data2$age,data2$campaign)
cor(data2$balance,data2$campaign)




ggplot(data, aes(x=age, y=balance)) + geom_point()
t.test()

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


##numeric var-> normal distribution check 

data_num<-data.frame(data$age,data$balance,data$day,data$duration,data$campaign,data$pdays,data$previous)
data_chr<-data.frame(data$job,data$marital,data$education,data$default,data$housing,data$loan,data$contact,data$month,data$poutcome)

normal_check<-function(x){
  s<-sample(x,5000)
  k<-shapiro.test(s) 
  print(k$p.value>=0.05)
  print(k$p.value)
}

sapply(data_num,function(x)normal_check(x)) ##  p-value>0.05이상일 경우 norm 

#normalization in numeric var
#k<-data_num
#for(i in 1:11162){
#  if(k$data.age[i]>65){
#    k$data.age[i]=65
#  }
#  else if(k$data.age[i]<20){
#    k$data.age[i]=20
#  }
#}

hist(k$data.age)

s<-sample(k$data.balance,5000)
ggqqplot(k$data.balance)
shapiro.test(s)

ggqqplot(k$data.age)
s<-sample(k$data.age,2000)
shapiro.test(s)



#####checking independence
## in categoricla var , when checking independence, set p-value>0.01 
## in numerical var, not following normal distribution -> output may have low reliability when using cor or t-test..etc 

data_y<-data[data$deposit=="yes",]
data_y_chr<-data.frame(data_y$job,data_y$marital,data_y$education,data_y$default,data_y$housing,data_y$loan,data_y$contact,data_y$month,data_y$day)
data_y_num<-data.frame(data_y$age,data_y$balance,data_y$duration,data_y$campaign,data_y$previous)

data_n<-data[data$deposit=="no",]
data_n_chr<-data.frame(data_n$job,data_n$marital,data_n$education,data_n$default,data_n$housing,data_n$loan,data_n$contact,data_n$month,data_n$day)
data_n_num<-data.frame(data_n$age,data_n$balance,data_n$duration,data_n$campaign,data_n$previous)


# case1: no class categorical var

chisq_anls<-function(x){
  
  for(i in 1:(ncol(x)-1)){
    
    j<-i
    
    for( k in (i+1):(ncol(x))){
      indep_check<-chisq.test(x[,j],x[,k],simulate.p.value = TRUE)
      if(indep_check$p.value>0.05){
        print(j)
        print(k)
        print(colnames(x)[j])
        print(colnames(x)[k])
        print(indep_check$p.value)
      }
    }
  }
}

chisq_anls(data_n_chr)

colnames(data_n_chr)[7]## min indep
colnames(data_n_chr)[9]

colnames(data_n_chr)[4]## max indep
colnames(data_n_chr)[10]

colnames(data_n_chr)[4]
colnames(data_n_chr)[6]
colnames(data_n_chr)[9]
colnames(data_n_chr)[10]

nclass<-data.frame(data_n_chr[,4],data_n_chr[,6],data_n_chr[,9],data_n_chr[,10])
colnames(nclass)<-c(colnames(data_n_chr)[4],
                    colnames(data_n_chr)[6],
                    colnames(data_n_chr)[9],
                    colnames(data_n_chr)[10])

##case2 no class numeric var
colnames(data_n_num)

cor_anls<-function(x){
  
  for(i in 1:(ncol(x)-1)){
    j<-i
    for(k in (i+1):ncol(x)){
      
      a<-cor(x[,j],x[,k])
      
      if(0.3>a& a>-0.3){
        print(j)
        print(k)
        print(colnames(x)[j])
        print(colnames(x)[k])
        print(a)
      }
      
      
    }
  }
}

cor_anls(data_n_num)

colnames(data_n_num)[5]##min
colnames(data_n_num)[6]

colnames(data_n_num)[2]##max
colnames(data_n_num)[5]


colnames(data_n_num)[1]
colnames(data_n_num)[2]
colnames(data_n_num)[3]
colnames(data_n_num)[4]
colnames(data_n_num)[5]

nclass<-data.frame(data_n_chr[,4],data_n_chr[,6],data_n_chr[,9],data_n_chr[,10],data_n_num[,1],data_n_num[,2],data_n_num[,3],data_n_num[,4],data_n_num[,5])
colnames(nclass)<-c(colnames(data_n_chr)[4],
                    colnames(data_n_chr)[6],
                    colnames(data_n_chr)[9],
                    colnames(data_n_chr)[10],
                    colnames(data_n_num)[1],
                    colnames(data_n_num)[2],
                    colnames(data_n_num)[3],
                    colnames(data_n_num)[4],
                    colnames(data_n_num)[5]
)

str(nclass)
## case3: yes class categorical var


chisq_anls(data_y_chr)  

colnames(data_y_chr)[8]##min
colnames(data_y_chr)[9]

colnames(data_y_chr)[4]##max
colnames(data_y_chr)[10]

colnames(data_y_chr)[2]
colnames(data_y_chr)[3]
colnames(data_y_chr)[4]
colnames(data_y_chr)[10]



#case4 yes class numeric var

colnames(data_y_num)


cor_anls(data_y_num)

colnames(data_y_num)[5]##min
colnames(data_y_num)[6]

colnames(data_y_num)[2]##max
colnames(data_y_num)[4]

colnames(data_y_num)[1]
colnames(data_y_num)[2]
colnames(data_y_num)[3]
colnames(data_y_num)[4]
colnames(data_y_num)[5]



yclass<-data.frame(data_y_chr[,2],data_y_chr[,3],data_y_chr[,4],data_y_chr[,10],data_y_num[,1],data_y_num[,2],data_y_num[,3],data_y_num[,4],data_y_num[,5])
colnames(yclass)<-c(colnames(data_y_chr)[2],
                    colnames(data_y_chr)[3],
                    colnames(data_y_chr)[4],
                    colnames(data_y_chr)[10],
                    colnames(data_y_num)[1],
                    colnames(data_y_num)[2],
                    colnames(data_y_num)[3],
                    colnames(data_y_num)[4],
                    colnames(data_y_num)[5]
)

str(yclass)

#case 5 total class independece checking


t.test(data)





##naviebayesian model & decision Tree model

index<-createDataPartition(data$deposit,p=0.75,list=FALSE)
tr_data<-data[index,]
test_data<-data[-index,]

tr_data$deposit<-factor(tr_data$deposit)

nb1<-naiveBayes(deposit~.,data=tr_data,)
nb<-NaiveBayes(deposit~.,data=tr_data,FL=1,usekernal=T)

data(iris)


dt<-rpart(deposit~.,data=tr_data,cp=0.11)

printcp(dt)
plotcp(dt)


predict_nb <- predict(nb1, newdata = test_data, )
predict_dt<-predict(dt,newdata=test_data,type="class")

confusionMatrix(predict_nb, as.factor(test_data$deposit),positive = "yes")
confusionMatrix(predict_dt, as.factor(test_data$deposit),positive = "yes")

help("NaiveBayes")

##optimization



###naivebayesian & bayesian

#in numeric var , not following normal distribution -> output may be low reliablity

#case1 class no
colnames(data_n_chr)[7]## min 
colnames(data_n_chr)[9]

p_x_y<-nrow(data_n[data_n$contact=="cellular"&data_n$poutcome=="failure",])/nrow(data_n)
p_y<-nrow(data_n)/nrow(data)


p_x1_y<-nrow(data_n[data_n$contact=="cellular",])/nrow(data_n)
p_x2_y<-nrow(data_n[data_n$poutcome=="failure",])/nrow(data_n)


colnames(data_n_chr)[4] ##max
as.factor(data_n_chr[,4])
colnames(data_n_chr)[10]
as.factor(data_n_chr[,10])

p_x_y<-nrow(data_n[data_n$default=="no"&data_n$day==5,])/nrow(data_n)
p_y<-nrow(data_n)/nrow(data)

p_x1_y<-nrow(data_n[data_n$default=="no",])/nrow(data_n)
p_x2_y<-nrow(data_n[data_n$day==5,])/nrow(data_n)




#in numeric var 
colnames(data_n_num)[5]##min 
colnames(data_n_num)[6]
hist(data_n$previous)



p_x_y<-nrow(data_n[data_n$pdays>-100&data_n$previous>4,])/nrow(data_n)
p_y<-nrow(data_n)/nrow(data)

p_x1_y<-nrow(data_n[data_n$pdays>-100,])/nrow(data_n)
p_x2_y<-nrow(data_n[data_n$previous>4,])/nrow(data_n)


colnames(data_n_num)[2]##max
hist(data_n$balance)
colnames(data_n_num)[5]
hist(data_n$pdays)

p_x_y<-nrow(data_n[data_n$balance>2000&data_n$pdays>-100,])/nrow(data_n)
p_y<-nrow(data_n)/nrow(data)

p_x1_y<-nrow(data_n[data_n$balance>2000,])/nrow(data_n)
p_x2_y<-nrow(data_n[data_n$pdays<-100,])/nrow(data_n)


#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y


#case 2 class yes

colnames(data_y_chr)[8]##min
as.factor(data_y_chr[,8])
colnames(data_y_chr)[9]
as.factor(data_y_chr[,9])

p_x_y<-nrow(data_y[data_y$month=="apr"&data_y$poutcome=="success",])/nrow(data_y)
p_y<-nrow(data_y)/nrow(data)


p_x1_y<-nrow(data_y[data_y$month=="apr",])/nrow(data_y)
p_x2_y<-nrow(data_y[data_y$poutcome=="success",])/nrow(data_y)

colnames(data_y_chr)[4]##max
as.factor(data_y_chr[,4])
colnames(data_y_chr)[10]
as.factor(data_y_chr[,10])


p_x_y<-nrow(data_y[data_y$default=="yes"& data_y$day==9,])/nrow(data_y)
p_y<-nrow(data_y)/nrow(data)


p_x1_y<-nrow(data_y[data_y$default=="yes",])/nrow(data_y)
p_x2_y<-nrow(data_y[data_y$day==10,])/nrow(data_y)

#in numeric var 

colnames(data_y_num)[5]##min
colnames(data_y_num)[6]

p_x_y<-nrow(data_y[data_y$pdays>-100&data_y$previous>4,])/nrow(data_y)
p_y<-nrow(data_y)/nrow(data)

p_x1_y<-nrow(data_y[data_y$pdays>-100,])/nrow(data_y)
p_x2_y<-nrow(data_y[data_y$previous>4,])/nrow(data_y)


colnames(data_y_num)[2]##max
colnames(data_y_num)[4]
hist(data_y$campaign)

p_x_y<-nrow(data_y[data_y$balance>2000&data_y$campaign>3,])/nrow(data_y)
p_y<-nrow(data_y)/nrow(data)

p_x1_y<-nrow(data_y[data_y$balance>2000,])/nrow(data_y)
p_x2_y<-nrow(data_y[data_y$campaign>3,])/nrow(data_y)

#output:: bayesian 
p_y*p_x_y 

#output:: naivebayesian
p_x1_y*p_x2_y*p_y
