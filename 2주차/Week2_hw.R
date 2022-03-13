#1번 과제 
sum_posneg <- function(matrix, dir = 1) {
  x <- dim(matrix)[1]
  y <- dim(matrix)[2] #x,y 크기를 저장
  if(dir==1){
    nmatrix2 = matrix(NA,1,2) #결과 매트릭스를 생성
    for (i in 1:x){
      matrix2 <- matrix[i,] #dir에 따라 여기위치바꾸기 1, or ,1
      matrixp <- matrix2[matrix2>0] #matrix2에서 0이상인 값 모두 도출
      matrixm <- matrix2[matrix2<0] #0이하인 값 도출
      sump = 0  #양수인 수의 합
      summ = 0  #음수인 수의 합
      for (i in matrixp){ #다 더해주기
        sump = sump+i
      }
      for (i in matrixm){
        summ = summ + i
      }
      nmatrix = cbind(sump, summ)#sump,summ 매트릭스로 합치기
      nmatrix2 = rbind(nmatrix2,nmatrix) #결과 매트릭스에 합치기
    }
    return(nmatrix2[-1,])
  }
  else if(dir==2){#이하동문
    nmatrix2 = matrix(NA,2,1) 
    nmatrix2
    for (i in 1:y){
      matrix2 <- matrix[,i]
      matrixp <- matrix2[matrix2>0]
      matrixm <- matrix2[matrix2<0]
      sump = 0
      summ = 0
      for (i in matrixp){
        sump = sump+i
      }
      for (i in matrixm){
        summ = summ + i
      }
      nmatrix = rbind(sump, summ)
      nmatrix2 = cbind(nmatrix2,nmatrix)
    }
    return(nmatrix2[,-1])
  }
  return (-1)
}

#1번 apply
sum_posneg_apply <- function(matrix, dir = 1) {
  x <- dim(matrix)[1]
  y <- dim(matrix)[2]
  if(dir==1){
    nmatrix2 = matrix(NA,1,2) #결과매트릭스 생성
    for (i in 1:x){
      matrix2 <- matrix[i,] #dir에 따라 행, 열매트릭스 가져오기
      matrixp <- matrix(matrix2[matrix2>0])
      matrixm <- matrix(matrix2[matrix2<0])
      if(length(matrixm) == 0){ #매트릭스 길이가 0이면 0인 매트릭스로 초기화
        matrixm <- matrix(0)
      }
      if(length(matrixp) == 0){
        matrixp <- matrix(0)
      }
      sump <- matrix(apply(matrixp,2,sum)) #sum 값을 간단하게 도출
      summ <- matrix(apply(matrixm,2,sum))
      nmatrix = cbind(sump, summ)
      nmatrix2 = rbind(nmatrix2,nmatrix)
    }
    return(nmatrix2[-1,])
  }
  else if(dir==2){#이하동문
    nmatrix2 = matrix(NA,2,1) 
    for (i in 1:y){
      matrix2 <- matrix[,i]
      matrixp <- matrix(matrix2[matrix2>0])
      matrixm <- matrix(matrix2[matrix2<0])
      if(length(matrixm) == 0){
        matrixm <- matrix(0)
      }
      if(length(matrixp) == 0){
        matrixp <- matrix(0)
      }
      sump <- matrix(apply(matrixp,2,sum))
      summ <- matrix(apply(matrixm,2,sum))
      nmatrix = rbind(sump, summ)
      nmatrix2 = cbind(nmatrix2,nmatrix)
    }
    return(nmatrix2[,-1])
  }
}

#2번
findstr_1 <-function(strmatrix,findstr,dir=1){
  x <- dim(strmatrix)[1]#사이즈 가져오기
  y <- dim(strmatrix)[2]
  if(dir==1){
    nmatrix2 = matrix(NA,1,1) #결과 매트릭스 생성
    for (i in 1:x){
      matrix2 <- strmatrix[i,] 
      matrixfind <- str_detect(matrix2,findstr) #findstr이 있는지를 찾는다.
      nmatrix = rbind(sum(matrixfind))
      nmatrix2 = rbind(nmatrix2,nmatrix)
    }
    nmatrix2<-nmatrix2[-1,]
    return(matrix(nmatrix2,x,1))
  }
  else if(dir==2){#이하동문
    nmatrix2 = matrix(NA,1,1) 
    for (i in 1:y){
      matrix2 <- strmatrix[,i] 
      matrixfind <- str_detect(matrix2,findstr)
      nmatrix = rbind(sum(matrixfind))
      nmatrix2 = rbind(nmatrix2,nmatrix)
    }
    nmatrix2<-nmatrix2[-1,]
    nmatrix2 <- matrix(nmatrix2,1,y)
    return(nmatrix2)
  }
}

#2번 apply
findstr_apply <-function(strmatrix,findstr,dir=1){
  x <- dim(strmatrix)[1]
  y <- dim(strmatrix)[2]
  if(dir==1){
    nmatrix2 = matrix(NA,1,1)
    for (i in 1:x){
      matrix2 <- strmatrix[i,]
      matrixfind <- str_detect(matrix2,findstr)
      sumstr <- matrix(apply(matrix(matrixfind),2,sum))
      nmatrix = rbind(sumstr)
      nmatrix2 = rbind(nmatrix2,nmatrix)
    }
    nmatrix2<-nmatrix2[-1,]
    return(matrix(nmatrix2,x,1))
  }
  else if(dir==2){
    nmatrix2 = matrix(NA,1,1)
    for (i in 1:y){
      matrix2 <- strmatrix[,i] 
      matrixfind <- str_detect(matrix2,findstr)
      sumstr <- matrix(apply(matrix(matrixfind),2,sum))
      nmatrix = rbind(sumstr)
      nmatrix2 = rbind(nmatrix2,nmatrix)
    }
    nmatrix2<-nmatrix2[-1,]
    nmatrix2 <- matrix(nmatrix2,1,y)
    return(nmatrix2)
  }
}


matrix <- matrix(c(1,-1,-2,-1,1,1,1,1,1,1,1,1),4,3,T)
matrix
#1번
sum_posneg(matrix,1)
sum_posneg(matrix,2)
#1번 apply
sum_posneg_apply(matrix,1)
sum_posneg_apply(matrix,2)

#2번
install.packages("stringr")
library(stringr)

strmatrix <- matrix(c('aa', 'ba', 'ccb', 'Ab', 'Bbc','ab'),2,3)
strmatrix

findstr_1(strmatrix,'a',1)
findstr_1(strmatrix,'a',2)

findstr_apply(strmatrix,'aa',1)
findstr_apply(strmatrix,'aa',2)

