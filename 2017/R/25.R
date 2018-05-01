curpos <- 0 
vecwithones <- NULL
curstate <- "A"

for (i in 1:12523873){
  temp <- which(vecwithones==curpos)
  if(length(temp)==0){
    vecwithones <- c(vecwithones, curpos)
    if(curstate == "A"){
      curpos <- curpos + 1
      curstate <- "B"
    } else if(curstate == "B"){
      curpos <- curpos + 1
      curstate <- "C"
    } else if(curstate == "C"){
      curpos <- curpos - 1
      curstate <- "D"
    } else if(curstate == "D"){
      curpos <- curpos + 1
      curstate <- "E"
    } else if(curstate == "E"){
      curpos <- curpos - 1
      curstate <- "A"
    } else {
      curpos <- curpos + 1
      curstate <- "A"
    }
  } else {
    if(curstate == "A"){
      curpos <- curpos - 1
      curstate <- "E"
    } else if(curstate == "B"){
      curpos <- curpos + 1
      curstate <- "F"
    } else if(curstate == "C"){
      curpos <- curpos + 1
      curstate <- "B"
      vecwithones <- vecwithones[-temp]
    } else if(curstate == "D"){
      curpos <- curpos - 1
      curstate <- "C"
      vecwithones <- vecwithones[-temp]
    } else if(curstate == "E"){
      curpos <- curpos + 1
      curstate <- "D"
      vecwithones <- vecwithones[-temp]
    } else {
      curpos <- curpos + 1
      curstate <- "C"
    }
  }
  
  if ((i/100000)==round(i/100000)){
    print(i)
  }
}

