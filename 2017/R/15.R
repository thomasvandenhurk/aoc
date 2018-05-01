library(BMS)
library(tictoc)
prev <- c(679,771)
count <- 0
for (i in 1:5000000){
  prev <- (prev*c(16807, 48271))%%2147483647
  str1<-str2<-NULL
  
  while(is.null(str1)){
    if(prev[1]%%4!=0){
      prev[1] <- (prev[1]*16807)%%2147483647
    }
    else{
      str1 <- intToBits(prev[1])
    }
  }
  
  while(is.null(str2)){
    if(prev[2]%%8!=0){
      prev[2] <- (prev[2]*48271)%%2147483647
    }
    else{
      str2 <- intToBits(prev[2])
    }
  }
  
  if (sum(str1[1:16]==str2[1:16])==16){
    count <- count + 1
    print(count)
    print(paste("i is", i))
  }
}
