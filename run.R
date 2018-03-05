rr <- 0
run <- function(){
  result <- 0
  steps <- 0
  O <- 1000
  reward <- c(1, 0, 1, 0, 0, 0)
  for(i in 1:O){
      tmp <- cftp(xu, xl, L)
      if(all(tmp[[1]]==reward)){
        result <- result + 1
      }
      steps <- c(steps, tmp[[2]])
    }
  steps[-1]
  return(list(result/(O), min(steps), max(steps), (sum(steps)/(O))))
  # min(steps)
  # max(steps)
  # sum(steps)/(O+1)
}


ave_run <- function(){
for(j in 1:5){
result <- 0
steps <- 0
O <- 1000
reward <- c(1, 0, 1, 0, 0, 0)
for(i in 1:O){
    tmp <- cftp(xu, xl, L)
    if(all(tmp[[1]]==reward)){
      result <- result + 1
    }
    steps <- c(steps, tmp[[2]])
  }
result/(O)
min(steps)
max(steps)
sum(steps)/(O+1)
rr[j] <- result/(O)
}
return(sum(rr)/5)
}
