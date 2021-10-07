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
split_punct<-function(vec){
  
  #Splitting ',' from text
  ia <- grep(",",a,fixed=TRUE)
  #ib <- grep(":",a,fixed=TRUE)
  '%ni%' <- Negate('%in%') 
  s <- 1:length(a)
  na <- s[(s %ni% ia) == TRUE]
  xs <- rep("",length(ia)+length(a))
  xs[ia+1:length(ia)] <-paste(',')
  xs[ia+1:length(ia)-1] <-gsub(',','',a[ia],fixed=TRUE)
  sa <- grep(",",xs,fixed=TRUE)
  j <- 1:length(xs)
  nsa <- j[(j %ni% sa)& (j %ni% (ia+1:length(ia)-1))== TRUE]
  xs[nsa]<-gsub(',','',a[na],fixed=TRUE)
  #Splitting ';' from text
  id <- grep(";",xs,fixed=TRUE)
  d <- 1:length(xs)
  nd <- d[(d %ni% id) == TRUE]
  xd <- rep("",length(id)+length(xs))
  xd[id+1:length(id)] <-paste(';')
  xd[id+1:length(id)-1] <-gsub(';','',xs[id],fixed=TRUE)
  sd <- grep(";",xd,fixed=TRUE)
  l <- 1:length(xd)
  nsd <- l[(l %ni% sd)& (l %ni% (id+1:length(id)-1))== TRUE]
  xd[nsd]<-gsub(';','',xs[nd],fixed=TRUE)
  #Splitting '?' from text
  ie <- grep("?",xd,fixed=TRUE)
  e <- 1:length(xd)
  ne <- e[(e %ni% ie) == TRUE]
  xe <- rep("",length(ie)+length(xd))
  xe[ie+1:length(ie)] <-paste('?')
  xe[ie+1:length(ie)-1] <-gsub('?','',xd[ie],fixed=TRUE)
  se <- grep("?",xe,fixed=TRUE)
  p <- 1:length(xe)
  nse <- p[(p %ni% se)& (p %ni% (ie+1:length(ie)-1))== TRUE]
  xe[nse]<-gsub('?','',xd[ne],fixed=TRUE)
  #Splitting '!' from text
  ik <- grep("!",xe,fixed=TRUE)
  k <- 1:length(xe)
  nk <- k[(k %ni% ik) == TRUE]
  xk <- rep("",length(ik)+length(xe))
  xk[ik+1:length(ik)] <-paste('!')
  xk[ik+1:length(ik)-1] <-gsub('!','',xe[ik],fixed=TRUE)
  sk <- grep("!",xk,fixed=TRUE)
  r <- 1:length(xk)
  nsk <- r[(r %ni% sk)& (r %ni% (ik+1:length(ik)-1))== TRUE]
  xk[nsk]<-gsub('!','',xe[nk],fixed=TRUE)
  #Splitting '.' from text
  iz <- grep(".",xk,fixed=TRUE)
  z <- 1:length(xk)
  nz <- z[(z %ni% iz) == TRUE]
  xz <- rep("",length(iz)+length(xk))
  xz[iz+1:length(iz)] <-paste('.')
  xz[iz+1:length(iz)-1] <-gsub('.','',xk[iz],fixed=TRUE)
  sz <- grep(".",xz,fixed=TRUE)
  q <- 1:length(xz)
  nsz <- q[(q %ni% sz)& (q %ni% (iz+1:length(iz)-1))== TRUE]
  xz[nsz]<-gsub('.','',xk[nz],fixed=TRUE)
  #Splitting ':' from text
  iv <- grep(":",xz,fixed=TRUE)
  v <- 1:length(xz)
  v1 <- sub(":.*", "", xz[iv])
  v2 <- sub(".*:", "", xz[iv])
  nv <- v[(v %ni% iv) == TRUE]
  xv <- rep("",length(iv)+length(xz))
  xv[iv+1:length(iv)] <-paste(':')
  xv[iv+1:length(iv)-1] <-gsub(':','',v1,fixed=TRUE)
  sv <- grep(":",xv,fixed=TRUE)
  t <- 1:length(xv)
  nsv<-t[(t %ni% sv)&(t %ni%(iv+1:length(iv)-1))== TRUE]
  xv[nsv]<-gsub(':','',xz[nv],fixed=TRUE)
  xfinal <- rep("",length(v2)+length(xv))
  ig <- grep(":",xv,fixed=TRUE)
  g <- 1:length(xv)
  ng <- g[(g %ni% ig) == TRUE]
  xfinal[ig+1:length(ig)] <-paste(':')
  xfinal[ig+1:length(ig)+1] <-gsub(':','',v2,fixed=TRUE)
  sg <- grep(":",xfinal,fixed=TRUE)
  h <- 1:length(xfinal)
  nsg<-h[(h %ni% sg)&(h %ni%(ig+1:length(ig)+1))==TRUE]
  xfinal[nsg]<- gsub(':','',xv[ng],fixed=TRUE)
  xfinal<- xfinal[which(xfinal != "")]
  return(xfinal)
  
}

#############################################################################################################################################################################################
##Splitting punctuation and generate word vector of the most common word.
a<-split_punct(a)##Split all punctuation from vector a.
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
A <- matrix(data = 0, nrow = length(b), ncol = length(b))##Initialize matrix A.
for (row in 1:nrow(C)){
  i=C[row,1]
  j=C[row,2]
  A[i,j]=A[i,j]+1
}
A <- A/rowSums(A)##Convert every element in matrix A  so its value will lies between 0 and 1 by dividing each element in each rows with its row sums to make the sum of each rows in matrix A equals to 1.
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