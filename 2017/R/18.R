instruc=c("	set i 31	",
"	set a 1	",
"	mul p 17	",
"	jgz p p	",
"	mul a 2	",
"	add i -1	",
"	jgz i -2	",
"	add a -1	",
"	set i 127	",
"	set p 618	",
"	mul p 8505	",
"	mod p a	",
"	mul p 129749	",
"	add p 12345	",
"	mod p a	",
"	set b p	",
"	mod b 10000	",
"	snd b	",
"	add i -1	",
"	jgz i -9	",
"	jgz a 3	",
"	rcv b	",
"	jgz b -1	",
"	set f 0	",
"	set i 126	",
"	rcv a	",
"	rcv b	",
"	set p a	",
"	mul p -1	",
"	add p b	",
"	jgz p 4	",
"	snd a	",
"	set a b	",
"	jgz 1 3	",
"	snd b	",
"	set f 1	",
"	add i -1	",
"	jgz i -11	",
"	snd a	",
"	jgz f -16	",
"	jgz a -19	")

for (i in 1:length(instruc)){
  instruc[i]<-gsub("\t","",instruc[i])
}

instruc2 <- NULL
for(i in 1:length(instruc)){
  instruc2[i] <- strsplit(instruc[i]," ")
}



register <- NULL
for (i in 1:length(instruc2)){
  if (instruc2[[i]][1]!="jgz"){
  register <- c(register, instruc2[[i]][2])
  }
}
register <- register[!duplicated(register)]
values <- rbind(rep(0,length(register)),rep(0,length(register)))
values[2,3]<-1
i<-c(1,1)
player<-1
bool <- c(FALSE,FALSE)
count <- 0

listsendreceive <- NULL
listsendreceive2 <- NULL


while(!bool[1] || !bool[2]){

  jumped <- 0
  if(instruc2[[i[player]]][1] == "snd"){
    #print(listsendreceive[[1]])
    #print(listsendreceive[[2]])
    if(player==1)
      listsendreceive2 <- c(listsendreceive2,values[player,which(register == instruc2[[i[player]]][2])])
    if(player==2){
      count <- count + 1
      listsendreceive <- c(listsendreceive,values[player,which(register == instruc2[[i[player]]][2])])
    }
  }

  else if (instruc2[[i[player]]][1] == "set"){
    if (!is.na(as.numeric(instruc2[[i[player]]][3])))
      values[player,which(register == instruc2[[i[player]]][2])] <- as.numeric(instruc2[[i[player]]][3])
    else
      values[player,which(register == instruc2[[i[player]]][2])] <- values[player,which(register == instruc2[[i[player]]][3])]
  }

  else if (instruc2[[i[player]]][1] == "add"){
    if (!is.na(as.numeric(instruc2[[i[player]]][3])))
      values[player,which(register == instruc2[[i[player]]][2])] <- values[player,which(register == instruc2[[i[player]]][2])] + as.numeric(instruc2[[i[player]]][3])
    else
      values[player,which(register == instruc2[[i[player]]][2])] <- values[player,which(register == instruc2[[i[player]]][2])] + values[player,which(register == instruc2[[i[player]]][3])]
  }

  else if (instruc2[[i[player]]][1] == "mul"){
    if (!is.na(as.numeric(instruc2[[i[player]]][3])))
      values[player,which(register == instruc2[[i[player]]][2])] <- values[player,which(register == instruc2[[i[player]]][2])] * as.numeric(instruc2[[i[player]]][3])
    else
      values[player,which(register == instruc2[[i[player]]][2])] <- values[player,which(register == instruc2[[i[player]]][2])] * values[player,which(register == instruc2[[i[player]]][3])]
  }

  else if (instruc2[[i[player]]][1] == "mod"){
    if (!is.na(as.numeric(instruc2[[i[player]]][3])))
      values[player,which(register == instruc2[[i[player]]][2])] <- values[player,which(register == instruc2[[i[player]]][2])] %% as.numeric(instruc2[[i[player]]][3])
    else
      values[player,which(register == instruc2[[i[player]]][2])] <- values[player,which(register == instruc2[[i[player]]][2])] %% values[player,which(register == instruc2[[i[player]]][3])]
  }

  else if (instruc2[[i[player]]][1] == "rcv"){

    if(player==1){
    if(length(listsendreceive)>0){
      values[player,which(register == instruc2[[i[player]]][2])] <- listsendreceive[1]
      listsendreceive <- listsendreceive[-1]
      bool[player] <- FALSE
    }
    else{
      bool[player] <- TRUE
      jumped <- 1
    }
    }

    if(player==2){
    if(length(listsendreceive2)>0){
      values[player,which(register == instruc2[[i[player]]][2])] <- listsendreceive2[1]
      listsendreceive2 <- listsendreceive2[-1]
      bool[player] <- FALSE
    }
    else{
      bool[player] <- TRUE
      jumped <- 1
    }
    }

  }

  else {
    if(is.na(as.numeric(instruc2[[i[player]]][2]))){
      if(values[player,which(register == instruc2[[i[player]]][2])]>0){
        jumped<-1
        if (!is.na(as.numeric(instruc2[[i[player]]][3])))
          i[player] <- i[player] + as.numeric(instruc2[[i[player]]][3])
        else
          i[player] <- i[player] + values[player,which(register == instruc2[[i[player]]][3])]
      }
    }
    else{
      jumped <- 1
      i[player] <- i[player] + as.numeric(instruc2[[i[player]]][3])
    }
  }

  if (jumped != 1)
  i[player] <- i[player] + 1

  player <- otherplayer(player)

  #print(instruc2[[i[player]]])
  #print(register)
  #print(values)
  #Sys.sleep(1)

}

otherplayer <- function(player){
  if(player==1){
    return(2)
  }
  else
    return(1)
}
