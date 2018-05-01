library(digest)

start <- "reyedfim"
num <- 0
password <- rep("_",8)
print(password)

while (sum(password=="_")>0){
  temp <- digest(paste(start,as.character(num),sep="",collapse = ""),algo="md5",serialize = F)
  if(substring(temp,1,5)=="00000" && !is.na(as.numeric(substring(temp,6,6)))){
    if(as.numeric(substring(temp,6,6))<8 && password[as.numeric(substring(temp,6,6))+1]=="_")
    password[as.numeric(substring(temp,6,6))+1]<-substring(temp,7,7)
    print(password)
  }
  num <- num + 1
}
