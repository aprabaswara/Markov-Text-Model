#Group member:
#1. Aditya Prabaswara Mardjikoen (s2264710)
#2. Huiying Chen (s2264943)
#3. Xiangtian Duan (s2248742)
setwd("C:/Users/Aditya Prabaswara/Markov-Text-Model")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license
split_punct <- function(x){
  ia <- grep(",",x,fixed=TRUE)#searching for , in vector x
  ib <- grep(":",x,fixed=TRUE)#searching for : in vector x
  ic <- grep(".",x,fixed=TRUE)#searching for . in vector x
  id <- grep("?",x,fixed=TRUE)#searching for ? in vector x
  ie <- grep("!",x,fixed=TRUE)#searching for ! in vector x
  ij <- grep(";",x,fixed=TRUE)#searching for ; in vector x
  s <- min(ia,ib,ic,id,ie,ij) #searching for the smallest index
  xs <- rep("",length(ia)+length(ib)+length(ic)+length(id)+length(ie)
            +length(ij)+length(x)) 
  punc <- c(",",":",".","?","!",";") #store punctuation in a vector
  xs[1:s] <- x[1:s]#Replace the first until the s element of vector xs 
  #by using the first until the s element of vector x
  xs[s] <- unlist(strsplit(x[s], punc)) #Remove punctuation from a 
  #vector
  xs[ia+1:length(ia)] <- paste(",") #fill their punctuation
  xs
}#function for splitting punctuation
a <- split_punct(a)
a <- tolower(a)#Change every capital letter in the vector into lowercase
k <- unique(a) #Vector containing every unique words in vector a 
j <- match(a,k)#Find index of unique words in vector a
freq <- tabulate(nchar(k))#Tabulate unique words in vector a