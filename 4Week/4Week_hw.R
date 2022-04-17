library(rpart)
library(e1071)
library(caret)
library(tidyverse)
#library(rattle)
#library(rpart.plot)

dataset <- read.csv(file = "C:/Users/cksgo/Documents/GermanCredit-data.csv", 
                    header = TRUE)
dataset
class(dataset)
head(dataset)

ds <- data.frame(DURATION = dataset$DURATION,
                 AMOUNT = dataset$AMOUNT,
                 EMPLOYMENT = dataset$EMPLOYMENT,
                 AGE = dataset$AGE, 
                 JOB = dataset$JOB,
                 RESPONSE = as.factor(dataset$RESPONSE)
                 )
ds
class(ds$RESPONSE)
class(ds)

indexes = createDataPartition(ds$RESPONSE, p = .6, list = F) 
indexes
trainset = ds[indexes, ]
testset = ds[-indexes, ]

# Fit a prediction model
# rpart parameters 
# minsplit: 
# minbucket:
# cp:
fit <- rpart(RESPONSE~., 
             data = ds,
             cp = -1, 
             minsplit = 2,
             minbucket = 1 ) 
fit
# prediction model 
printcp(fit)

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

# Evaluate the performance of the prediction model
pred = predict(fit, testset, type = "class", )
print(data.frame(testset, pred))
confusionMatrix(testset$RESPONSE, pred, positive = '1')
readline('Enter to resume ')

fit <- rpart(RESPONSE~., 
             data = ds,
             # cp = -1, 
             minsplit = 4,
             minbucket = 2 ) 

# prediction model 
printcp(fit)
fit


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

pred = predict(fit, testset, type = "class", )
print(data.frame(testset, pred))
confusionMatrix(testset$RESPONSE, pred, positive = '1')


# Evaluate the performance of the prediction model
fit <- rpart(RESPONSE~., 
             data = ds,
             minsplit = 4,
             minbucket = 2 ) 
fit
pred = predict(fit, trainset, type = "class", )
print(data.frame(trainset, pred))
confusionMatrix(trainset$RESPONSE, pred, positive = '1')
