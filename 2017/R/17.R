library(tictoc)
curloop <- 0
curpos <- 1
lencl <- 1
input <- 354

#a
for (i in 1:2017){
  num_add_steps <- input%%lencl
  curpos <- curpos+num_add_steps
  while(curpos>lencl){
    curpos <- curpos - lencl
  }
  curloop <- c(curloop[1:curpos],i, curloop[-(1:curpos)])
  lencl <- lencl + 1
  curpos <- curpos+1
}


#b
curloop <- 1
curpos <- 2
lencl <- 2
input <- 354

tic()
for (i in 2:50000000){
  num_add_steps <- input%%lencl
  curpos <- curpos+num_add_steps
  while(curpos>lencl){
    curpos <- curpos - lencl
  }
  lencl <- lencl + 1
  if(curpos == 1){
    curloop <- i
  }
  curpos <- curpos+1
}
toc()
