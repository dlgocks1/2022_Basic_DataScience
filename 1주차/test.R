datatest <- read.csv("과제1.csv",header=TRUE)
datatest

model <- lm(뽑은여부~성별+나이+별점+선호도,data = datatest)
model

#model = -0.616210 +  0.156892성별Male + 0.002522나이 + 0.106892별점 + 0.013109선호도

#use the fitted model to predict the value for the new observation
print("A정당을 뽑을까")
print(predict(model, newdata = data.frame(성별="Female",나이=40,별점=4,선호도=80)))
print(predict(model, newdata = data.frame(성별="Male",나이=23,별점=1,선호도=20)))
print(predict(model, newdata = data.frame(성별="Female",나이=53,별점=0,선호도=0)))
print(predict(model, newdata = data.frame(성별="Female",나이=63,별점=5,선호도=99)))


