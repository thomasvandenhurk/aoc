#2,500,000 - 2,600,000
#2,512,235 - 2,512,237
map=c("	.#.....##..##..###.###..#	",
"	..##..######.#.###.##.#.#	",
"	###..#..#####.##.##.#...#	",
"	###......##..###.#...#.#.	",
"	.#.###.##..#.####.#..#...	",
"	..#.#.#####...##.####.###	",
"	..#..#.#..###.#..###.###.	",
"	#########...#....##..#.#.	",
"	.###..#######..####...###	",
"	#####...#..##...###..##..	",
"	..#......##.#....#...####	",
"	.##.#..#####.#####.##.##.	",
"	####.##.###.#..#.#.#.....	",
"	#....##.####.#.#..#.#.##.	",
"	###...##...#.###.#.#.####	",
"	.#.#...#.#.##.##....##.#.	",
"	#..##.#.#..#....###..####	",
"	#####...#..#.###...##.###	",
"	##.#..####.###...#....###	",
"	###.#####.....#....#.##..	",
"	####.##.....######.#..#.#	",
"	.#.....####.##...###..##.	",
"	....########.#..###.#..##	",
"	##.##..#...#...##.#....##	",
"	.#.######.##....####.#.##	")


mapcoer <- NULL
for (i in 1:length(map)){
  map[i] <- gsub("\t","",map[i])
  map[i] <- gsub("#","1",map[i])
  map[i] <- gsub("\\.","0",map[i])
  mapcoer <- c(mapcoer,as.numeric(strsplit(map[i],"")[[1]]))
}

ind <- which(mapcoer==1)
x <- ind%%25
x[which(x==0)] <- 25
x <- x - 13
y <- -((floor((ind-1)/25)+1)-13)
z <- rep(3,length(y))
infected <- as.matrix(cbind(x,y,z),ncol=3)


count <- 0
currentpos <- NULL
currentpos$x <- 0
currentpos$y <- 0
currentpos$lm <- "N"

for (i in 1:10000){
  temp <- which(infected[,1]==currentpos$x)
  temp2 <- which(infected[temp,2]==currentpos$y)
  if(length(temp2)==0){
    infected <- rbind(infected,c(currentpos$x,currentpos$y,1))
    temp <- nrow(infected)
  } else{
    temp <- temp[temp2]
  }
  
  temp_2 <- infected[temp,3]
  if(temp_2==1){
    infected[temp,3] <- 2
    currentpos <- move_clean(currentpos)
  } else if(temp_2==2){
    count <- count + 1
    infected[temp,3] <- 3
    currentpos <- move_weakened(currentpos)
  } else if(temp_2==3){
    infected[temp,3] <- 4
    currentpos <- move_infected(currentpos)
  } else {
    infected[temp,3] <- 1
    currentpos <- move_flagged(currentpos)
  }
  
  if ((i/100000)==round(i/100000)){
    print(i)
  }
}

move_infected <- function(currentpos){
  if(currentpos$lm == "S"){
    currentpos$x <- currentpos$x - 1
    currentpos$lm <- "W"
  } else if(currentpos$lm == "N"){
    currentpos$x <- currentpos$x + 1
    currentpos$lm <- "E"
  } else if(currentpos$lm == "E"){
    currentpos$y <- currentpos$y - 1
    currentpos$lm <- "S"
  } else{
    currentpos$y <- currentpos$y + 1
    currentpos$lm <- "N"
  }
  currentpos
}

move_clean <- function(currentpos){
  if(currentpos$lm == "N"){
    currentpos$x <- currentpos$x - 1
    currentpos$lm <- "W"
  } else if(currentpos$lm == "S"){
    currentpos$x <- currentpos$x + 1
    currentpos$lm <- "E"
  } else if(currentpos$lm == "W"){
    currentpos$y <- currentpos$y - 1
    currentpos$lm <- "S"
  } else{
    currentpos$y <- currentpos$y + 1
    currentpos$lm <- "N"
  }
  currentpos
}

move_flagged <- function(currentpos){
  if(currentpos$lm == "N"){
    currentpos$y <- currentpos$y - 1
    currentpos$lm <- "S"
  } else if(currentpos$lm == "S"){
    currentpos$y <- currentpos$y + 1
    currentpos$lm <- "N"
  } else if(currentpos$lm == "W"){
    currentpos$x <- currentpos$x + 1
    currentpos$lm <- "E"
  } else{
    currentpos$x <- currentpos$x - 1
    currentpos$lm <- "W"
  }
  currentpos
}

move_weakened <- function(currentpos){
  if(currentpos$lm == "N"){
    currentpos$y <- currentpos$y + 1
  } else if(currentpos$lm == "S"){
    currentpos$y <- currentpos$y - 1
  } else if(currentpos$lm == "W"){
    currentpos$x <- currentpos$x - 1
  } else{
    currentpos$x <- currentpos$x + 1
  }
  currentpos
}


