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
split_punct <- function(x){
  #######################################################################################################################################################################################
  ##Splitting text based on punctuation ','.
  ia <- grep(",",x,fixed=TRUE)##Finds the indices of the words containing an ',' in vector a.
  '%ni%' <- Negate('%in%')##Creates a function which returns the logical negation of 'in' function.
  s <- 1:length(x)##Generate integer sequences from 1 to the length of vector x.
  na <- s[(s %ni% ia) == TRUE]##Finds the indices of the words that doesn't containing an ',' in vector a.
  xs <- rep("",length(ia)+length(x))##Vector to store words with/without ','.
  xs[ia+1:length(ia)] <-paste(',')##Put ',' into an index after its original index location.
  xs[ia+1:length(ia)-1] <-gsub(',','',x[ia],fixed=TRUE)##Put words that contains ',' in vector x into vector xs without ',' before the location of ',' in vector xs.
  sa <- grep(",",xs,fixed=TRUE)##Finds the indices of the words containing an ',' in vector xs.
  j <- 1:length(xs)##Generate integer sequences from 1 to the length of vector xs.
  nsa <- j[(j %ni% sa)& (j %ni% (ia+1:length(ia)-1))== TRUE]##Finds the indices of the words that doesn't containing an ',' and sentence that originally has ',' in vector a.
  xs[nsa]<-gsub(',','',x[na],fixed=TRUE)##Replace all element '' in vector xs by element in vector a that doesn't contain ','.
  #######################################################################################################################################################################################
  ##Splitting text based on punctuation ';'
  id <- grep(";",xs,fixed=TRUE)##Finds the indices of the words containing an ';' in vector xs.
  d <- 1:length(xs)##Generate integer sequences from 1 to the length of vector xs.
  nd <- d[(d %ni% id) == TRUE]##Finds the indices of the words that doesn't containing an ';' in vector xs.
  xd <- rep("",length(id)+length(xs))##Vector to store words with/without ';'.
  xd[id+1:length(id)] <-paste(';')##Put ';' into an index after its original index location.
  xd[id+1:length(id)-1] <-gsub(';','',xs[id],fixed=TRUE)##Put words that contains ';' in vector xs into vector xd without ';' before the location of ';' in vector xd.
  sd <- grep(";",xd,fixed=TRUE)##Finds the indices of the words containing an ';' in vector xd.
  l <- 1:length(xd)##Generate integer sequences from 1 to the length of vector xd.
  nsd <- l[(l %ni% sd)& (l %ni% (id+1:length(id)-1))== TRUE]##Finds the indices of the words that doesn't containing an ';' and sentence that originally has ';' in vector xs.
  xd[nsd]<-gsub(';','',xs[nd],fixed=TRUE)##Replace all element '' in vector xd by element in vector xs that doesn't contain ';'.
  #######################################################################################################################################################################################
  ##Splitting text based on punctuation '?'
  ie <- grep("?",xd,fixed=TRUE)##Finds the indices of the words containing an '?' in vector xd.
  e <- 1:length(xd)##Generate integer sequences from 1 to the length of vector xd.
  ne <- e[(e %ni% ie) == TRUE]##Finds the indices of the words that doesn't containing an '?' in vector xd.
  xe <- rep("",length(ie)+length(xd))##Vector to store words with/without '?'.
  xe[ie+1:length(ie)] <-paste('?')##Put '?' into an index after its original index location.
  xe[ie+1:length(ie)-1] <-gsub('?','',xd[ie],fixed=TRUE)##Put words that contains '?' in vector xd into vector xe without '?' before the location of '?' in vector xd.
  se <- grep("?",xe,fixed=TRUE)##Finds the indices of the words containing an '?' in vector xe.
  p <- 1:length(xe)##Generate integer sequences from 1 to the length of vector xe.
  nse <- p[(p %ni% se)& (p %ni% (ie+1:length(ie)-1))== TRUE]##Finds the indices of the words that doesn't containing an '?' and sentence that originally has '?' in vector xd.
  xe[nse]<-gsub('?','',xd[ne],fixed=TRUE)##Replace all element '' in vector xe by element in vector xd that doesn't contain '?'.
  #######################################################################################################################################################################################
  ##Splitting text based on punctuation '!'
  ik <- grep("!",xe,fixed=TRUE)##Finds the indices of the words containing an '!' in vector xe.
  k <- 1:length(xe)##Generate integer sequences from 1 to the length of vector xe.
  nk <- k[(k %ni% ik) == TRUE]##Finds the indices of the words that doesn't containing an '!' in vector xe.
  xk <- rep("",length(ik)+length(xe))##Vector to store words with/without '!'.
  xk[ik+1:length(ik)] <-paste('!')##Put '!' into an index after its original index location.
  xk[ik+1:length(ik)-1] <-gsub('!','',xe[ik],fixed=TRUE)##Put words that contains '!' in vector xe into vector xk without '!' before the location of '!' in vector xk.
  sk <- grep("!",xk,fixed=TRUE)##Finds the indices of the words containing an '!' in vector xk.
  r <- 1:length(xk)##Generate integer sequences from 1 to the length of vector xk.
  nsk <- r[(r %ni% sk)& (r %ni% (ik+1:length(ik)-1))== TRUE]##Finds the indices of the words that doesn't containing an '!' and sentence that originally has '!' in vector xe.
  xk[nsk]<-gsub('!','',xe[nk],fixed=TRUE)##Replace all element '' in vector xk by element in vector xe that doesn't contain '!'.
  #######################################################################################################################################################################################
  ##Splitting text based on punctuation '.'
  iz <- grep(".",xk,fixed=TRUE)##Finds the indices of the words containing an '.' in vector xk.
  z <- 1:length(xk)##Generate integer sequences from 1 to the length of vector xk.
  nz <- z[(z %ni% iz) == TRUE]##Finds the indices of the words that doesn't containing an '.' in vector xk.
  xz <- rep("",length(iz)+length(xk))##Vector to store words with/without '.'.
  xz[iz+1:length(iz)]<-paste('.')##Put '.' into an index after its original index location.
  xz[iz+1:length(iz)-1]<-gsub('.','',xk[iz],fixed=TRUE)##Replace all element '' in vector xz by element in vector xk that doesn't contain '.'.
  sz <- grep(".",xz,fixed=TRUE)##Finds the indices of the words containing an '.' in vector xz.
  q <- 1:length(xz)##Generate integer sequences from 1 to the length of vector xz.
  nsz <- q[(q %ni% sz)& (q %ni% (iz+1:length(iz)-1))== TRUE]##Finds the indices of the words that doesn't containing an '.' and sentence that originally has '.' in vector xk.
  xz[nsz]<-gsub('.','',xk[nz],fixed=TRUE)##Replace all element '' in vector xz by element in vector xk that doesn't contain '.'.
  #######################################################################################################################################################################################
  ##Splitting text based on punctuation ':'
  iv <- grep(":",xz,fixed=TRUE)##Finds the indices of the words containing an ':' in vector xz.
  v <- 1:length(xz)##Generate integer sequences from 1 to the length of vector xz.
  v1 <- sub(":.*", "", xz[iv])##Find text before ':' for a letter that contain this punctuation in vector xz.
  v2 <- sub(".*:", "", xz[iv])##Find text after ':' for a letter that contain this punctuation in vector xz.
  nv <- v[(v %ni% iv) == TRUE]##Finds the indices of the words that doesn't containing an ':' in vector xz.
  xv <- rep("",length(iv)+length(xz))##Vector to store words with/without ':'.
  xv[iv+1:length(iv)] <-paste(':')##Put ':' into an index after its original index location.
  xv[iv+1:length(iv)-1] <-gsub(':','',v1,fixed=TRUE)##Replace all element '' in vector xv by element in vector v1 that doesn't contain ':'.
  sv <- grep(":",xv,fixed=TRUE)##Finds the indices of the words containing an ':' in vector xv.
  t <- 1:length(xv)##Generate integer sequences from 1 to the length of vector xv.
  nsv<-t[(t %ni% sv)&(t %ni%(iv+1:length(iv)-1))== TRUE]##Finds the indices of the words that doesn't containing an ':' and sentence that originally has ':' in vector xv.
  xv[nsv]<-gsub(':','',xz[nv],fixed=TRUE)##Replace all element '' in vector xv by element in vector xz that doesn't contain ':'.
  xfinal<- rep("",length(v2)+length(xv))##Vector to store all text in the bible.
  ig <- grep(":",xv,fixed=TRUE)##Finds the indices of the words containing an ':' in vector xv.
  g <- 1:length(xv)##Generate integer sequences from 1 to the length of vector xv.
  ng <- g[(g %ni% ig) == TRUE]##Finds the indices of the words that doesn't containing an ':' in vector xv.
  xfinal[ig+1:length(ig)] <-paste(':')##Put ':' into an index after its original index location.
  xfinal[ig+1:length(ig)+1] <-gsub(':','',v2,fixed=TRUE)##put vector v2 after the location of ':' om vector xfinal.
  sg <- grep(":",xfinal,fixed=TRUE)##Finds the indices of the words containing an ':' in vector xfinal.
  h <- 1:length(xfinal)##Generate integer sequences from 1 to the length of vector xfinal.
  nsg<-h[(h %ni% sg)&(h %ni%(ig+1:length(ig)+1))==TRUE]##Finds the indices of the words that doesn't containing an ':' and also doesn't contain the next word after ':' in vector xfinal.
  xfinal[nsg]<- gsub(':','',xv[ng],fixed=TRUE)##Replace all element '' in vector xfinal by element in vector xv that doesn't contain ':'.
  xfinal<- xfinal[which(xfinal != "")]##Remove all element "" in vector xfinal.
  return(xfinal)##Return vector that has been separated by all of its punctuation.
}
#############################################################################################################################################################################################
##Splitting punctuation and generate word vector of the most common word.
a <- split_punct(a)##Splitting all punctuation in vector a.
a <- tolower(a)##Change every capital letter in the vector a into lowercase.
k <- unique(a) ##Vector containing every unique words in vector a. 
j <- match(a,k)##Find index of unique words in vector a.
freq <- tabulate(j)##Count up how many time each unique word occurs in the text.
freq_sort <- sort(freq,decreasing=TRUE)##Sort unique word frequency from the unique words.
threshold <- freq_sort[1000]##Set the threshold number of word occurs in vector a.
b_list<-c()##Create empty vector.
p<-0##Set initial value for looping.
##Generate index vector.
for (i in freq_sort){
  p<-p+1
  ##Generate index from 1 until m, where m near 1000.
  if (i >= threshold){
    b_list<-c(b_list,p)
  }
}
b1<-c()##Create empty vector.
for (i in b_list) {
  b1<-c(b1,j[i])##Generate index of the most common word.
}
b<-k[b1]##Create vector of the most common word.
###############################################################################################################################################################################
##Create matrix A
r <- match(a,b)##Find index of the most common words in vector a.
C <- cbind(r[1:length(r)-1],r[2:length(r)])##Two column matrix in which the first column is the index of common words, and the next column is the index for the following word.
if (any(is.na(rowSums(C)))==TRUE){C<-na.omit(C)}##Drop rows that contains NA.
###############################################################################################################################################################################
##Simulation of 50-word sections by taking 50 random sample from vector b.
index <- 1:length(b)##Initialize the index of vector b.
text_sample <- rep(0,50)##Store 0 in a vector that consists 50 elements.
text_sample[1] <- sample(index,size=1)##Initialize the first index to take a text from b.
##Generate the rest of the sample.
for (i in c(2:50)){
  text_sample[i] <- sample(index,size=1,prob=A[text_sample[i-1]])##Initialize the rest of the index to take a text from b.
}
cat(b[text_sample])##Printing out 50 text