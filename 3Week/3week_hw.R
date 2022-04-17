library(dplyr)
library(tidyverse)

dataset <- read.csv("C:/Users/cksgo/Documents/weight_height.csv")
tc <- as_tibble(dataset)
tc1 <- data.frame(Gender = as.factor(tc$Gender),
                  Height = tc$Height,
                  weight = tc$Weight
                  )

#Data에 대한 DEA
#남, 녀 각각, 전체 평균치, 표준편차

#전체 키의 평균 및 표준편차
apply(matrix(tc1$Height),2,mean)
apply(matrix(tc1$Height),2,sd)

tm <- tc1[tc1$Gender=="Male",]
tf <- tc1[tc1$Gender=="Female",]

#남자 키, 몸무게의 평균 및 표준편차
apply(matrix(tm$Height),2,mean)
apply(matrix(tm$Height),2,sd)
apply(matrix(tm$weight),2,mean)
apply(matrix(tm$weight),2,sd)

#여자 키, 몸무게의 평균 및 표준편차
apply(matrix(tf$Height),2,mean)
apply(matrix(tf$Height),2,sd)
apply(matrix(tf$weight),2,mean)
apply(matrix(tf$weight),2,sd)

#남녀 각각 데이터 개수 동일
count(tf)
count(tm)

#남, 녀 각각 전체 상관계수
p1 <- ggplot(tm, aes(x = Height)) + 
  geom_histogram(binwidth = 2,color="black", fill="blue")
p2 <- ggplot(tf, aes(x = Height)) + 
  geom_histogram(binwidth = 2,color="black", fill="red")
p1
p2

#키에 관한 그래프프
ggplot () +
  geom_histogram(data = tm,aes(x=Height),fill="blue",alpha=.3,bins = 50) +  
  geom_histogram(data = tf,aes(x=Height),fill="red",alpha=.3,bins = 50) +
  geom_vline(aes(xintercept = mean(tm$Height)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mean(tf$Height)),
             color="red", linetype="dashed", size=1)

#몸무게에 관한 그래프
ggplot () +
  geom_histogram(data = tm,aes(x=weight),fill="blue",alpha=.3,bins = 50) +  
  geom_histogram(data = tf,aes(x=weight),fill="red",alpha=.3,bins = 50) +
  geom_vline(aes(xintercept = mean(tm$weight)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mean(tf$weight)),
             color="red", linetype="dashed", size=1)

#키의 cut off 구하기
meanresult = 10000;meanheight = 0
for(i in 1:(max(tc1$Height))){
  #mean보다 여자 키가 클때 error1
  error1cnt = count(tm[tf$Height>i,])
  #mean보다 남자 키가 작을때 error2
  error2cnt = count(tm[tm$Height<i,])
  if (error1cnt+error2cnt < meanresult){
    meanresult = error1cnt+error2cnt
    meanheight = i
  }
}
meanheight 
meanresult #최소 에러 갯수

#몸무게의 cut off 구하기
meanweightresult = 10000;meanweight = 0
for(i in 1:(max(tc1$weight))){
  #mean보다 여자 키가 클때 error1
  error1cnt = count(tm[tf$weight>i,])
  #mean보다 남자 키가 작을때 error2
  error2cnt = count(tm[tm$weight<i,])
  if (error1cnt+error2cnt < meanweightresult){
    meanweightresult = error1cnt+error2cnt
    meanweight = i
  }
}
meanweight 
meanweightresult
#최소 에러 갯수 가 height가 더 적음으로, Height를 사용하는것이 최적


#cut off를 구한 키 그래프
ggplot () +
  geom_histogram(data = tm,aes(x=Height),fill="blue",alpha=.3,bins = 50) +  
  geom_histogram(data = tf,aes(x=Height),fill="red",alpha=.3,bins = 50) +
  geom_vline(aes(xintercept = meanheight),
             color="black", linetype="dashed", size=1) 

#cut off를 구한 몸무게 그래프
ggplot () +
  geom_histogram(data = tm,aes(x=weight),fill="blue",alpha=.3,bins = 50) +  
  geom_histogram(data = tf,aes(x=weight),fill="red",alpha=.3,bins = 50) +
  geom_vline(aes(xintercept = meanweight),
             color="black", linetype="dashed", size=1) 



ggplot() + 
  geom_point(data = tm,aes(x=Height,y=weight),color="blue",alpha=.2)+ 
  geom_smooth(method = "lm",formula = y~splines::ns(x, 2)) +
  geom_point(data = tf,aes(x=Height,y=weight),color="red",alpha=.2)+
  #geom_abline(slope = slope,intercept = int) +
  geom_smooth(data = tf,aes(x=Height,y=weight),color="red",method = "lm",formula = y~x)+
  geom_vline(aes(xintercept = meanheight),
           color="black", linetype="dashed", size=1)+ 
  geom_hline(yintercept = meanweight)

#Height 선을 직선 경계 결정선으로 사용?


  
  

  