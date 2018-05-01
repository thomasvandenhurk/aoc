instruc=c("	set b 67	",
"	set c b	",
"	jnz a 2	",
"	jnz 1 5	",
"	mul b 100	",
"	sub b -100000	",
"	set c b	",
"	sub c -17000	",
"	set f 1	",
"	set d 2	",
"	set e 2	",
"	set g d	",
"	mul g e	",
"	sub g b	",
"	jnz g 2	",
"	set f 0	",
"	sub e -1	",
"	set g e	",
"	sub g b	",
"	jnz g -8	",
"	sub d -1	",
"	set g d	",
"	sub g b	",
"	jnz g -13	",
"	jnz f 2	",
"	sub h -1	",
"	set g b	",
"	sub g c	",
"	jnz g 2	",
"	jnz 1 3	",
"	sub b -17	",
"	jnz 1 -23	")


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
register <- c("a","b","c","d","e","f","g","h")
values <- c(1,rep(0,length(register)-1))

i<-1

while(i<=length(instruc2)){
  if(i==12){
    print(values)
  }
  
  jumped <- 0
  if(instruc2[[i]][1] == "sub"){
    if (!is.na(as.numeric(instruc2[[i]][3])))
      values[which(register == instruc2[[i]][2])] <- values[which(register == instruc2[[i]][2])] - as.numeric(instruc2[[i]][3])
    else
      values[which(register == instruc2[[i]][2])] <- values[which(register == instruc2[[i]][2])] - values[which(register == instruc2[[i]][3])]
    
  }
  
  else if (instruc2[[i]][1] == "set"){
    if (!is.na(as.numeric(instruc2[[i]][3])))
      values[which(register == instruc2[[i]][2])] <- as.numeric(instruc2[[i]][3])
    else
      values[which(register == instruc2[[i]][2])] <- values[which(register == instruc2[[i]][3])]
  }

  else if (instruc2[[i]][1] == "mul"){
    if (!is.na(as.numeric(instruc2[[i]][3])))
      values[which(register == instruc2[[i]][2])] <- values[which(register == instruc2[[i]][2])] * as.numeric(instruc2[[i]][3])
    else
      values[which(register == instruc2[[i]][2])] <- values[which(register == instruc2[[i]][2])] * values[which(register == instruc2[[i]][3])]
  }
  
  else {
    if(is.na(as.numeric(instruc2[[i]][2]))){
      if(values[which(register == instruc2[[i]][2])]!=0){
        jumped<-1
        if (!is.na(as.numeric(instruc2[[i]][3])))
          i <- i + as.numeric(instruc2[[i]][3])
        else
          i <- i + values[which(register == instruc2[[i]][3])]
      }
    }
    else{
      jumped <- 1
      if (!is.na(as.numeric(instruc2[[i]][3])))
        i <- i + as.numeric(instruc2[[i]][3])
      else
        i <- i + values[which(register == instruc2[[i]][3])]
    }
  }
  
  if (jumped != 1)
    i <- i + 1
}


is.prime <- function(num) {
  if (num == 2) {
    TRUE
  } else if (any(num %% 2:(num-1) == 0)) {
    FALSE
  } else { 
    TRUE
  }
}

vec <- seq(106700,123700,17)
tot <- 0
for (i in 1:length(vec)){
  tot <- tot + sum(is.prime(vec[i]))
}

