library(BMS)

get_hash <- function(number){

  code <- 0:255
  l1 <- c(118,98,113,117,103,107,104,108,45)

  l2 <- get_bytes(as.character(number))

  l3 <- c(17, 31, 73, 47, 23)

  l <- c(l1, l2, l3)


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

  knothash <- NULL
  for (j in 1:length(densehash)){
    if (length(strsplit(sprintf("%x",densehash[j]),"")[[1]])==2)
    knothash <- c(knothash, sprintf("%x",densehash[j]))
    else
    knothash <- c(knothash, paste("0",sprintf("%x",densehash[j]),collapse="",sep=""))
  }
  knothash <- paste(knothash,collapse="")
  knothash
}

get_bytes <- function(number){

  tek <- strsplit(number,"")
  numb <- NULL
  for (i in 1:length(tek[[1]])){
    if (tek[[1]][i]=="0")
      numb <- c(numb,48)
    else if (tek[[1]][i]=="1")
      numb <- c(numb,49)
    else if (tek[[1]][i]=="2")
      numb <- c(numb,50)
    else if (tek[[1]][i]=="3")
      numb <- c(numb,51)
    else if (tek[[1]][i]=="4")
      numb <- c(numb,52)
    else if (tek[[1]][i]=="5")
      numb <- c(numb,53)
    else if (tek[[1]][i]=="6")
      numb <- c(numb,54)
    else if (tek[[1]][i]=="7")
      numb <- c(numb,55)
    else if (tek[[1]][i]=="8")
      numb <- c(numb,56)
    else
      numb <- c(numb,57)
  }
  numb
}



hash <- NULL

for (i in 1:128){
hash[i] <- get_hash(i-1)
}

freeused <- list(NULL)
for (i in 1:length(hash)){
  temp <- strsplit(hash[i],"")
  temp2 <- NULL
  for (j in 1:length(temp[[1]])){
    temp2<-c(temp2,hex2bin(temp[[1]][j]))
  }
  freeused[[i]] <- temp2
}

total <- 0
for (i in 1:length(freeused)){
  for (j in 1:length(freeused[[i]])){
    total <- total + freeused[[i]][j]
  }
}

freeused_copy <- freeused
freeused <- freeused_copy

groups <- 0
for (i in 1:length(freeused)){
  if(freeused[[i]][1]!=0){
    groups <- groups + 1
    freeused[[i]][1] <- groups
    if(freeused[[i]][2]==0){
      groups <- groups + 1
    }
  }

  for (j in 2:length(freeused[[i]])){
    if(freeused[[i]][j]!=0){
      if(freeused[[i]][j-1]!=0){
        freeused[[i]][j] <- freeused[[i]][j-1]
      }
      else
        groups <- groups + 1
      freeused[[i]][j] <- groups
    }
  }
}


maxlevel<-NULL
for(i in 1:length(freeused)){
  maxlevel[i] <- max(freeused[[i]])
}
maxlevel <- c(1, maxlevel)

listlevels <- NULL
lev <- 1
for(i in 1:groups){
  if(i>maxlevel[lev+1])
    lev <- lev + 1
  listlevels[i] <- list(lev)
}



for (j in 1:groups){ #changeto
  changed <- 1
  while(changed==1){
    changed<-0
    for (i in 1:length(listlevels[[j]])){ #working level
      ind <- which(freeused[[listlevels[[j]][i]]]==j)

      if(length(ind)>0){
      #check rij boven
        if(listlevels[[j]][i]>1){
          ind2 <- unique(freeused[[listlevels[[j]][i]-1]][ind[which(freeused[[listlevels[[j]][i]-1]][ind]!=0)]])
          ind2 <- ind2[ind2!=j]
          if (length(ind2)>0){
            ind3 <- NULL
            for(k in 1:length(ind2)){
              ind3 <- c(ind3,which(freeused[[listlevels[[j]][i]-1]]==ind2[k]))
            }
            #print("changefrom")
            #print(freeused[[listlevels[[j]][i]-1]])
            freeused[[listlevels[[j]][i]-1]][ind3] <- j
            #print("changeto")
            #print(freeused[[listlevels[[j]][i]-1]])

            listlevels[[j]] <- unique(c(listlevels[[j]],listlevels[[j]][i]-1))
            changed<-1
          }
        }

        #check rij onder
        if(listlevels[[j]][i]<128){
          ind2 <- unique(freeused[[listlevels[[j]][i]+1]][ind[which(freeused[[listlevels[[j]][i]+1]][ind]!=0)]])
          ind2 <- ind2[ind2!=j]
          if (length(ind2)>0){
            ind3 <- NULL
            for(k in 1:length(ind2)){
              ind3 <- c(ind3,which(freeused[[listlevels[[j]][i]+1]]==ind2[k]))
            }

            #print("changefrom")
            #print(freeused[[listlevels[[j]][i]+1]])
            freeused[[listlevels[[j]][i]+1]][ind3] <- j
            #print("changeto")
            #print(freeused[[listlevels[[j]][i]+1]])
            listlevels[[j]] <- unique(c(listlevels[[j]],listlevels[[j]][i]+1))
            changed<-1
          }
        }
      }
    }
  }
}


all_numb <- NULL
for(i in 1:128){
  all_numb <- unique(c(all_numb,freeused[[i]]))
}
all_numb <- length(all_numb[all_numb!=0])










