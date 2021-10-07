#########################################################################################################################################################################################
##Group member:
##1. Aditya Prabaswara Mardjikoen(s2264710)
##2. Xiangtian Duan(s2248742)
##3. Huiying Chen(s2264943)
#########################################################################################################################################################################################
##Input the bible the text data into R Studio.
setwd("C:/Users/Aditya Prabaswara/Markov-Text-Model")##Set working directory.
a <- scan("1581-0.txt",what="character",skip=156)##Read text data into a vector from the '1581-0.txt' file.
n <- length(a) ##Find the length of vector a.
a <- a[-((n-2909):n)] ##Strip license.
#########################################################################################################################################################################################
##Define the function to split the punctuation (',','.','?','!',':',';').
split_punct<-function(x,vec){
  
  vec<-gsub(x, paste0(" ",x," "),vec,fixed=TRUE)
  vec<- unlist(strsplit(vec," "))
  
  return(vec)
  
}

#############################################################################################################################################################################################
##Splitting punctuation and generate word vector of the most common word.
a<-split_punct(",",a)##Split all ',' from vector a.
a<-split_punct(".",a)##Split all '.' from vector a.
a<-split_punct(";",a)##Split all ';' from vector a.
a<-split_punct("!",a)##Split all '!' from vector a.
a<-split_punct("?",a)##Split all '?' from vector a.
a<-split_punct(":",a)##Split all ':' from vector a.
a_original<-a##Store original text after splitting all punctuation.
a <- tolower(a)##Change every capital letter in the vector a into lowercase.
k <- unique(a) ##Vector containing every unique words in vector a. 
j <- match(a,k)##Find index of unique words in vector a.
freq <- tabulate(j)##Count up how many time each unique word occurs in the text.
freq_sort <- sort(freq,decreasing=TRUE)##Sort unique word frequency from the unique words.
threshold <- freq_sort[1:1000]##Set the threshold number of word occurs in vector a.
b<-k[which(threshold>=92)]##Create vector of the most common word.
###############################################################################################################################################################################
##Create matrix A
r <- match(a,b)##Find index of the most common words in vector a.
C <- cbind(r[1:length(r)-1],r[2:length(r)])##Two column matrix in which the first column is the index of common words, and the next column is the index for the following word.
C <- C[!rowSums(is.na(C)), ]##Drop rows that contains NA.
A <- matrix(data = 0, nrow = length(b), ncol = length(b))
for (row in 1:nrow(C)){
  i=C[row,1]
  j=C[row,2]
  A[i,j]=A[i,j]+1
}
A <- A/rowSums(A)
###############################################################################################################################################################################
##Simulation of 50-word sections by taking 50 random sample from vector b.
index <- 1:length(b)##Initialize the index of vector b.
text_sample <- rep(0,50)##Store 0 in a vector that consists 50 elements.
text_sample[1] <- sample(index,size=1)##Initialize the first index to take a text from b.
##Generate the rest of the sample.
for (i in c(2:50)){
  text_sample[i] <- sample(index,1,A[text_sample[i-1]])##Initialize the rest of the index to take a text from b.
}
capital_text<-a_original[match(b[text_sample],tolower(a_original))]##Find index of b that we got from sampling in the original vector a which still contains capital letter but have been separated based on the punctuation.
b_capital<-unlist(regmatches(capital_text, gregexpr("\\b[A-Z]\\w+", capital_text)))##Find all text that started with capital letters.
prob <- rep(0,length(b_capital))##store 0 in a probability vector.
for (i in 1:length(b_capital)){
  prob[i]=length(a_original[a_original==b_capital[i]])/(length(a_original[a_original==b_capital[i]])+length(a_original[a_original==tolower(b_capital[i])]))##Calculate the probability of capital letter that appear in the original vector a which still contains capital letter but have been separated based on the punctuation.
}
capital_match<-match(tolower(b_capital[prob>0.5]),b[text_sample])##Find all lowercase text in vector that have a capital letter in the original bible text.
b[text_sample][capital_match]<-b_capital[prob>0.5]##Replace all text that has probability having capital letter greater than 0.5.
cat(b[text_sample])##Printing out 50 text