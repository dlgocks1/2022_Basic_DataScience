b <- c(1, 2, 4, 8)
b

typeof(b)

1:5

c <- 1:5
c
typeof(c)

x <- seq(1,2,0.1)
x
typeof(x)

d <- c("영수","호철","순이")
e <- c(TRUE,T,FALSE,T,F)
d
e

vector()
a <= vector("integer", length = 3)
a

d
length(d)
typeof(d)
mode(d)
class(d)
str(d)
attributes(d)

d <- c(d,"James")
d
c(F,0)
c(1,"character")
c(1,"character",T)
as.character(c(1,2,3))
is.numeric("o")

x <- c(0.5,NA,0.7); y<- c("mary","james","jane")
x; y;
is.na(x); is.na(y)
anyNA(x); anyNA(y);

1/0
0/0
pi

a <- c(11,22,33,44,55)
a
a[1]
a[0]
a[10]
a[c(3,1,2)]
a[4:1]
a[7] <- 77
a
a[-1]
a[c(-1,-3,-2)]
a[a<30]

length(1:10)
length("character")
nchar("character")

a = c(1,3,5,7); b=c(1,2,4,8)
5*a; 5+a;
a <4
a +b; a-b; a/b; a*b;
sum(a);min(a);mean(a);max(a);

x = c(1,2,NA,0/0)
x
sum(x)
sum(x,na.rm = T)
a %*% b; a %o%b;

a = c(1:5); b =c(1,-1)
a;b
b + a
sum(c(T,F,T))

y <- 1:10
y
attr(y, "my_attribute") <- "This ia a vector"
attr(y, "my_attribute")
attr(y, "my_attribute2") <- "second attribute"
attributes(y)
str(attributes(y))
structure(y)

z = y[1:5]
z
attr(z,"my_attribute")

x <- c(a=1,b=2,c=3)
x
names(x)
x <- 1:3; 
names(x)[[1]] <-c("a")
x
names(x)

x <- factor(c("a","b","b","a"))
x
class(x)
mode(x)
levels(x)
structure(x)
x <- factor(x,levels = c("a","b","c"))
x
table(x)

m <- matrix(1:12, 3,4,F)
m
m <- matrix(1:12, 3,4,T)
m
mat_3d <- array(1:12,c(2,3,2))
mat_3d

rbind(1:5,6:10,11:15)
cbind(1:5,6:10,11:15)

m
typeof(m)
class(m)
mode(m)
str(m)
attributes(m)
dim(m)
length(m)
nrow(m)
mat_3d
dimnames(mat_3d) <- list(c("one","two"),c("a","b","c"),c("A","B"))
mat_3d

m
m[m<=7]
m[2,]
m[,1]
m[2,3] <-0
m
m[c(1,3),2:4]
rownames(m) <- c("A","B","C")
colnames(m) <- c("a","b","c","d")
m
rownames(m)
m[c("A","B"),c("d","a")]

m = matrix(1:6, nrow = 3, ncol = 2)
m
n = matrix(data = 1:6,nrow = 2,ncol = 3)
n

m %*% n
n * n
rowSums(m)
colSums(m)

m
apply(m, 1, min)
apply(m, 2, max)
m[1,][2]

matrix(c(1,"a",1+2i,T),2,2)
matrix(list(1,"a",1+2i,T),2,2)

m <- 1:6
dim(m) <- c(2,3)
m

x <- list(1,"a",T,"hello")
x
x_vector = 1:3
x_vector
x_list = as.list(x_vector)
x_list

x <- vector("list",length = 5)
x

l1 = list(fruits = c("apple","banana","oragne"),
          count = c(2,3),
          sales = c(F,T,T,F))
l1
l1[1]
l1[[1]]
l1[[1]][1]
l1$fruits
l1$fruits[1]

xlist <- list(a = "Karthing Ram",b=1:10, data= head(iris))
xlist
names(l1)
attributes(l1)
names(l1) <- c("과일","숫자","세일")
l1


df <- data.frame(x =1:3, y=c("a","b","c"))
df
str(df)
is.list(df)
class(df)
mode(df)
structure(df)
df
df[2,"x"]
df["2","c"]
df[1]
df$x
df[[2]]


subset(df,x<3,select = c(y))
df[df[,1]<3,"y"]

for(i in 1:10){
  print(i*i)
}

i =0
while(i<=10){
  print(i)
  i = i+1
}

add <- function(a,b=0)
  {
  result = a+b
  return(result)
}
add(1,5)

x <- data.frame(Name = c("James","길동","Marry"),
                Score = c(100,50,70))
row.names(x) <- c(1,2,3)
write.csv(x = x,file = "Hello.csv",row.names = T)

y <- read.csv(file = "Hello.csv",
              header = T,
              na.strings = "NA",
              stringsAsFactors = default.stringsAsFactors())
y
