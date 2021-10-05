setwd("C:/Users/Xiangtian Duan/Desktop/statistical programming")
a <- scan("1581-0.txt",what="character",skip=156,encoding="UTF-8")
n <- length(a)
a <- a[-((n-2909):n)] ## strip license




split_punct<-function(x){
  
  i <- grep(x,a,fixed = TRUE)
  b <- gsub(x,"",a,fixed = TRUE)
  xs <- rep('',length(a)+length(i))
  ii <- i+1:length(i)
  xs[ii] <- paste(x)
  xs[-ii] <- b
  

  return (xs)

}


a<-split_punct(",")
a<-split_punct(".")
a<-split_punct(";")
a<-split_punct("!")
a<-split_punct("?")

# separate according to":"
a<-gsub(":"," : ",a,fixed = TRUE)
b <- unlist(strsplit(a," "))
b
