options(repos = c(CRAN = "http://cran.rstudio.com"))
install.packages("readxl")

library(readxl)

search()
getwd()

titanic_data = read_excel(path = "./titanic3.xls", sheet="titanic3")
t1 = titanic_data
str(t1)

head(t1)
tail(t1)

sapply(t1, typeof)
t2 <- data.frame(name =t1$name,
                 pclass = factor(t1$pclass, levels = c(1, 2, 3)),
                 age = t1$age, 
                 parch = as.integer(t1$parch), 
                 sibsp = as.integer(t1$sibsp),
                 sex = factor(t1$sex, levels = c("female", "male")),
                 fare = t1$fare,
                 survived = factor(t1$survived, levels = c(0, 1)))
str(t2)

row_na = apply(t2, 1, anyNA)
row_na
t3 <- t2[!row_na,]
anyNA(t3)
head(t3)

tm <- t3[t3$sex == "male",]
tm
tf <- t3[t3$sex == "female",]
tf
tm = tm[order(tm$age,-(tm$fare),tm$name),]
tm
tf = tf[order(tf$age,-(tf$fare),tf$name),]
tf
tm;tf

t4 = rbind(tm[1:100,],tf[1:100,])
t4

t5 <- data.frame(ID = 1:nrow(t4), t4, sum=t4$parch + t4$sibsp)
t5
t5 <- cbind(ID = 1:nrow(t4), t4, sum = t4$parch + t4$sibsp)
str(t5)

sample(x =1:10, size=5)
t6 = t5[sample(x =1:nrow(t5)),]
head(t6)

numeric_columns <- sapply(t6, is.numeric) # numeric: double or integer, 
# note: correction
t7 <- t6[,numeric_columns]
apply(t7, 2, sum, na.rm = TRUE)
