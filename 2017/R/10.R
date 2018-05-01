code <- 0:255

l<-c(56, 51, 44, 48, 44, 49, 57, 51, 44, 49, 44, 50, 53, 52, 44, 50, 51, 55, 44, 49, 56, 55, 44, 52, 48, 44, 56, 56, 44, 50, 55, 44, 50, 44, 50, 53, 53, 44, 49, 52, 57, 44, 50, 57, 44, 52, 50, 44, 49, 48, 48, 17, 31, 73, 47, 23)


skipsize <- 0
currentpos <- 1

for (k in 1:64){
for (i in 1:length(l)){
  while(currentpos>length(code)){
    currentpos <- currentpos - length(code)
  }
  if(l[i]>0){
  if((l[i]+currentpos-1)<=length(code)){
    code[currentpos:(currentpos+l[i]-1)]<-code[(currentpos+l[i]-1):currentpos]
  }
  else {
    toplace <- NULL
    toplace <- c(code[currentpos:length(code)],code[1:(l[i]-length(code)+currentpos-1)])
    code[currentpos:length(code)]<-toplace[length(toplace):(length(toplace)-length(code)+currentpos)]
    code[1:((l[i]-length(code)+currentpos-1))] <- toplace[(length(toplace)-length(code)+currentpos-1):1]
  }
  }
  currentpos <- currentpos + l[i] + skipsize
  skipsize <- skipsize + 1
}
}

densehash <- NULL
cur<-NULL
for (i in 1:16){
  cur <- code[((i-1)*16+1)]
  for (j in 2:16){
    cur <- bitwXor(cur,code[((i-1)*16+j)])
  }
  densehash[i]<-cur
}

d9a7de4a809c56bf3a9465cb84392c8e