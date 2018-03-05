#系内客数2
P <- matrix(0, 6, 6)
P[1,1] <- c(3/10)
P[1,2] <- c(5/10)
P[1,3] <- c(2/10)
P[2,2] <- c(3/10)
P[2,4] <- c(3/10)
P[2,5] <- c(4/10)
P[3,3] <- c(1/10)
P[3,5] <- c(4/10)
P[3,6] <- c(5/10)
P[4,1] <- c(9/10)
P[4,4] <- c(1/10)
P[5,1] <- c(5/10)
P[5,5] <- c(5/10)
P[6,6] <- c(6/10)
P[6,1] <- c(4/10)
I <- diag(1, 6, 6)


T <- -1
L <- 6 #queueing networkのノード数
K <- 2 #系内客数の合計、閉ジャクソンの場合は固定？CFTPでは動きうる
xu <- c(K,0,0,0,0,0) #単調CFTPのための上限と下限。これらを更新関数でupdate
xl <- c(0,0,0,0,0,K)
myu <- rep(c(1.0), 6)
library(RMarkov)
theta <- ctmc.st(P - I)$x
a <- 0
for(i in 1:L){
  a[i] <- c(theta[i]/myu[i])
}


# rr <- 0
# for(j in 1:5){
# result <- 0
# steps <- 0
# O <- 1000
# reward <- c(2, 0, 0, 0, 0, 0)
# for(i in 1:O){
#     tmp <- cftp(xu, xl, L)
#     if(all(tmp[[1]]==reward)){
#       result <- result + 1
#     }
#     steps <- c(steps, tmp[[2]])
#   }
# result/(O)
# min(steps)
# max(steps)
# sum(steps)/(O+1)
# rr[j] <- result
# }
#
# sum(rr)/5000
