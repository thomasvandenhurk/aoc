pipes=c("42/37",
"28/28",
"29/25",
"45/8",
"35/23",
"49/20",
"44/4",
"15/33",
"14/19",
"31/44",
"39/14",
"25/17",
"34/34",
"38/42",
"8/42",
"15/28",
"0/7",
"49/12",
"18/36",
"45/45",
"28/7",
"30/43",
"23/41",
"0/35",
"18/9",
"3/31",
"20/31",
"10/40",
"0/22",
"1/23",
"20/47",
"38/36",
"15/8",
"34/32",
"30/30",
"30/44",
"19/28",
"46/15",
"34/50",
"40/20",
"27/39",
"3/14",
"43/45",
"50/42",
"1/33",
"6/39",
"46/44",
"22/35",
"15/20",
"43/31",
"23/23",
"19/27",
"47/15",
"43/43",
"25/36",
"26/38",
"1/10")

pipesstr <- matrix((rep(0,length(pipes)*2)), ncol = 2)
for (i in 1:length(pipes)){
  pipesstr[i,1:2] <- as.numeric(strsplit(pipes[i],"/")[[1]]) 
}

ind <- 1
lpipes <- length(pipes)
temp <- which(pipesstr==0)
temp[temp>lpipes] <- temp[temp>lpipes]-57
indlist <- NULL
for (i in 1:length(temp)){
 indlist[i] <- list(temp[i])
}
lastvalues <- pipesstr[temp,2]

while(ind<=length(indlist)){
  temp <- which(pipesstr==lastvalues[ind])
  temp[temp>lpipes] <- temp[temp>lpipes]-lpipes
  temp <- temp[!duplicated(temp)]
  temp <- setdiff(temp,indlist[[ind]])
  
  if(length(temp) == 0){
    ind <- ind + 1
  } else if(length(temp) == 1){
    indlist[[ind]] <- c(indlist[[ind]],temp)
    if(pipesstr[temp,1]!=pipesstr[temp,2]){
      lastvalues[ind] <- pipesstr[temp,which(pipesstr[temp,]!=lastvalues[ind])]
    }
  } else{
    lindlist <- length(indlist)
    for (i in 2:length(temp)){
      indlist[[(lindlist+i-1)]] <- c(indlist[[ind]],temp[i])
      if(pipesstr[temp[i],1]!=pipesstr[temp[i],2]){
        lastvalues[(lindlist+i-1)] <- pipesstr[temp[i],which(pipesstr[temp[i],]!=lastvalues[ind])]
      } else{
        lastvalues[(lindlist+i-1)] <- lastvalues[ind]
      }
    }
    indlist[[ind]] <- c(indlist[[ind]],temp[1])
    if(pipesstr[temp[1],1]!=pipesstr[temp[1],2]){
      lastvalues[ind] <- pipesstr[temp[1],which(pipesstr[temp[1],]!=lastvalues[ind])]
    }
    
  }
}

curvalue <- 0
indlistmax <- NULL
temp <- 0
for(i in 1:length(indlist)){
  if(length(indlist[[i]]) >= curvalue){
    curvalue <- length(indlist[[i]])
    if(sum(pipesstr[indlist[[i]],])>temp){
    temp <- sum(pipesstr[indlist[[i]],])
    indlistmax <- indlist[[i]]
    }
  }
}
#34

