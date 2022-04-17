#setwd("C:\\woodystudio\\R\\DSF2022\\week3\\") #디렉토리 설정
weight_height <- read.csv("C:/Users/cksgo/Documents/weight_height.csv") #csv파일 읽어오기
male_wh_us <- weight_height[weight_height$Gender=="Male",] #Male만 추출
female_wh_us <- weight_height[weight_height$Gender=="Female",] #Female만 추출

#inch를 cm로, lb를 kg로 변경
male_wh <- data.frame(Gender=male_wh_us[,1], Height_cm=male_wh_us[,2]*2.54, Weight_kg=male_wh_us[,3]*0.453592)
female_wh <- data.frame(Gender=female_wh_us[,1], Height_cm=female_wh_us[,2]*2.54, Weight_kg=female_wh_us[,3]*0.453592)

#head()를 이용하여 잘 변경되었는지 확인
head(male_wh)
head(female_wh)

#결측값이 있는지 확인
table(is.na(male_wh))
table(is.na(female_wh))

#데이터 요약 통계랑 출력
summary(male_wh)
summary(female_wh)

#데이터 표준편자 확인
sd(male_wh$Height_cm)
sd(male_wh$Weight_kg)
sd(female_wh$Height_cm)
sd(female_wh$Weight_kg)

#극단치 확인
#극단치를 제거할 것인가?
b1 <- boxplot(male_wh$Height_cm)$stats
b2 <- boxplot(male_wh$Weight_kg)$stats
b3 <- boxplot(female_wh$Height_cm)$stats
b4 <- boxplot(female_wh$Weight_kg)$stats

#극단치 범위에 포함되는 데이터 개수 파악
table(male_wh$Height_cm < b1[1,])
table(male_wh$Height_cm > b1[5,])
table(male_wh$Weight_kg < b2[1,])
table(male_wh$Weight_kg > b2[5,])
table(female_wh$Height_cm < b3[1,])
table(female_wh$Height_cm > b3[5,])
table(female_wh$Weight_kg < b4[1,])
table(female_wh$Weight_kg > b4[5,])
#상하위 0.3% 내외의 극단치들이 존재
#극단치를 제거하기로 결정

#극단치 제거
male_wh$Height_cm <- ifelse(male_wh$Height_cm < b1[1,] | male_wh$Height_cm > b1[5,], NA, male_wh$Height_cm)
male_wh$Weight_kg <- ifelse(male_wh$Weight_kg < b2[1,] | male_wh$Weight_kg > b2[5,], NA, male_wh$Weight_kg)
female_wh$Height_cm <- ifelse(female_wh$Height_cm < b3[1,] | female_wh$Height_cm > b3[5,], NA, female_wh$Height_cm)
female_wh$Weight_kg <- ifelse(female_wh$Weight_kg < b4[1,] | female_wh$Weight_kg > b4[5,], NA, female_wh$Weight_kg)
male_wh_ev <- na.omit(male_wh)
female_wh_ev <- na.omit(female_wh)

#극단치를 제거한 데이터의 요약 통계량 출력
summary(male_wh_ev)
summary(female_wh_ev)
#평균과 중앙값에는 유의미한 변화가 없음

#극단치를 제거한 데이터 표준편자 확인
sd(male_wh_ev$Height_cm)
sd(male_wh_ev$Weight_kg)
sd(female_wh_ev$Height_cm)
sd(female_wh_ev$Weight_kg)
#전체적으로 표준편차가 0.3정도 줄었음을 확인

#남성의 키/몸무게 상관관계 확인
cor(male_wh_ev$Height_cm, male_wh_ev$Weight_kg, method = "pearson")
#0.8508444

#여성의 키/몸무게 상관관계 확인
cor(female_wh_ev$Height_cm, female_wh_ev$Weight_kg, method = "pearson")
#0.8410599

#남성의 키-몸무게 상관관계가 여성의 키/몸무게 상관관계보다 약간 더 강한 양의 상관관계임을 확인

#남성의 키-몸무게 상관관계 그래프 및 추세선
plot(Weight_kg ~ Height_cm, data = male_wh_ev, xlab = "키(cm)", ylab = "몸무게(kg)")
abline(lm(Weight_kg ~ Height_cm, data = male_wh_ev), col = "blue", lwd = 3)

#여성의 키-몸무게 상관관계 그래프 및 추세선
plot(Weight_kg ~ Height_cm, data = female_wh_ev, xlab = "키(cm)", ylab = "몸무게(kg)")
abline(lm(Weight_kg ~ Height_cm, data = female_wh_ev), col = "red", lwd = 3)

##########3번 항목###

###키 기반 예측###

#남/여 키 데이터만 추출하여 각각 저장
male_height <- male_wh_ev[,2]
female_height <- female_wh_ev[,2]

#히스토그램 x축 범위 설정
#0.1cm단위로 자르니 가시성이 떨어져 1cm단위로 설정
x_range = seq(140,200,by=1)

#남/여 키를 따로 히스토그램에 저장
male_height_hist = hist(male_height, breaks=x_range, plot = FALSE)
female_height_hist = hist(female_height, breaks=x_range, plot = FALSE)

#y축 범위 설정을 위한 최대 빈도수 파악
y_max = max(max(male_height_hist$counts), max(female_height_hist$counts))

#plot함수를 이용하여 히스토그램 그리기
plot(male_height_hist, col = adjustcolor("blue",alpha = 0.3), ann = FALSE, axes = FALSE, ylim = c(0,y_max))
plot(female_height_hist, col = adjustcolor("red", alpha = 0.3), add = TRUE)

title(xlab="Height(cm)", ylab="Frequncy")

#x축 눈금 추가
axis(side=1,at=seq(140,200,1))

#y축 눈금 추가
axis(side=2,at=seq(0,500,10))

#범례 설정
legend("topright",c("male","female"),fill=c("blue","red"))


##남/여 키가 교차되는 169cm를 cut-off값으로 정하면 되겠다고 판단 (169cm 이상이 남자)

#키 기반 예측모델 생성 function name : height_guess
height_guess <- function(x){
  if (x>=169){
    return("Male")
  }
  else{
    return("Female")
  }
}


### 몸무게 기반 예측###
#키 데이터와 구성방식 동일

male_weight <- male_wh_ev[,3]
female_weight <- female_wh_ev[,3]

x_range = seq(35,110,by=1)

male_weight_hist = hist(male_weight, breaks=x_range, plot = FALSE)
female_weight_hist = hist(female_weight, breaks=x_range, plot = FALSE)

y_max = max(max(male_weight_hist$counts), max(female_weight_hist$counts))

plot(male_weight_hist, col = adjustcolor("blue",alpha = 0.3), ann = FALSE, axes = FALSE, ylim = c(0,y_max))
plot(female_weight_hist, col = adjustcolor("red", alpha = 0.3), add = TRUE)

title(xlab="Weight(kg)", ylab="Frequncy")

axis(side=1,at=seq(35,110,1))
axis(side=2,at=seq(0,1000,10))

legend("topright",c("male","female"),fill=c("blue","red"))

##남/여 몸무게가 교차되는 74kg를 cut-off값으로 정하면 되겠다고 판단 (74kg 이상이 남자)

#몸무게 기반 예측모델 생성 function name : weight_guess
weight_guess <- function(x){
  if (x>=74){
    return("Male")
  }
  else{
    return("Female")
  }
}

###키 기반 예측모델 / 몸무게 기반 예측모델 정확도 비교###

#남/여 성별-키-몸무게 데이터 병합
both_wh <- rbind(male_wh_ev, female_wh_ev)

##키 기반 예측모델 오류율
#키를 기반으로 성별을 예측하고, 원래 성별과 예측한 성별을 한개의 데이터 프레임에 저장
forecast_height <- apply(both_wh[c('Height_cm')], 1, height_guess)
result_height <- cbind(Original = both_wh[,1], Forecast = forecast_height)

#table()을 이용하여 원래 성별과 예측 성별이 얼마나 일치하는지 확인
table(result_height[,1]==result_height[,2])
#1620개의 오류값을 가짐
#1620/9885*100 = 16.39%의 오류율

#몸무게 기반 예측모델 오류율 (키 기반과 동일 구성)
forecast_weight <- apply(both_wh[c('Weight_kg')], 1, weight_guess)
result_weight <- cbind(Original = both_wh[,1], Forecast = forecast_weight)
table(result_weight[,1]==result_weight[,2])
#871개의 오류
#871/9885*100=8.11%의 오류율

##키 기반 예측모델보다 몸무게를 기반으로 예측하는 모델이 오류율이 더 낮다


##########4번과제###
#산점도 그리기
library(ggplot2)
ggplot() + geom_point(mapping = aes(x=Height_cm, y=Weight_kg), data = male_wh_ev, col="blue", alpha = 0.5) + geom_point(mapping = aes(x=Height_cm, y=Weight_kg), data = female_wh_ev, col="red", alpha = 0.5) + geom_abline(intercept=90, slope=-0.1)
#y=-x+240 그래프를 기준으로 남/여 구분
#산점도의 형태가 길게 늘어진 형태라 기울기가 0에 가까울 수록 정확한 결과를 가져올 것이라 생각
#기울기를 -0.1이라 잡고 가장 적은 오류를 가져올 것이라 판단되는 y절편을 선택

###예측함수 생성###
forecast_both<-c()
both_guess <- function(matrix){
  for (i in 1:nrow(matrix)){
    x <- matrix[i,1]
    y <- matrix[i,2]
    if (y>=-0.1*x+90){
      forecast_both <- rbind(forecast_both,"Male")
    }
    else{
      forecast_both <- rbind(forecast_both,"Female")
    }
  }
  forecast_both <- data.frame(forecast_both)
}

#본래 성별과 예측 성별 병합
result_both <- data.frame(Original = both_wh[,1], Forecast = both_guess(both_wh[,-1]))

#table함수를 이용하여 오류 값 개수 파악
table(result_both[,1]==result_both[,2])

#904개의 오류
#9.15% 오류율을 가지는 모델

################### ###
###항공대에 적용하려면 어떻게 해야할까?###

#항공대의 성비가 남:여=5:1정도라고 가정했을 때, 두가지 상황이 존재한다.

#1, 남 여 합쳐 1만명의 데이터가 주어졌을 때 (즉, 수집된 데이터의 성비가 5:1일 때)
#2, 남 여 각각 5천명의 데이터가 주어졌을때 (즉, 수집된 데이터의 성비가 1:1일 때)

#1번의 상황에서 전세계 남/여의 구분 예측모델을 작성하기 위해서는 여성의 분포를 5배 해주는 것이 맞다고 생각. 하지만 항공대생들의 키/몸무게 분포를 확인하기 위해서는 그대로 반영.

#2번의 상황에서 전세계 남/여의 구분 예측모델을 작성하기 위해서는 스케일링 없이 작성해도 된다고 생각. 하지만 항공대생들의 키/몸무게 분포를 확인하기 위해서는 남자의 분포를 5배 늘려줘야한다고 생각.