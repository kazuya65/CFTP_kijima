lfunction <- function(i, l, k, a){ #lを求める式gの式
  gu <- 0　#分子
  gl <- 0　#分母
  for(s in 0:l){
    gu <- gu + (a[i]^s)*(a[i+1]^(k-s))
  }
  for(s in 0:k){
    gl <- gl + (a[i]^s)*(a[i+1]^(k-s))
  }
  g <- gu/gl
  return(g)
}


ufunction <- function(x, lambda){ #更新関数,　引数は時刻t1,t2と状態x,乱数列lambda
  i <- trunc(lambda)
  k <- x[i] + x[i+1]

  for(j in 0:k){
    l <- j #lに0~kまで代入
    if(l != 0){ #lが０以上のときの動き
      g1 <- lfunction(i, l-1, k, a)
      g2 <- lfunction(i, l, k, a)
      if(g1 <= lambda-i && lambda-i < g2){
        break; #式を満たしていればlを保存してループを抜ける
      }
    }else{ #lが０のときはg1を０としなければならない？
      g1 <- 0
      g2 <- lfunction(i, l, k, a)
      if(g1 <= lambda-i && lambda-i < g2){
        break;
      }
    }
  }
    for(j in 1:L){ #更新部分（iのサーバーへl人，i+1のサーバーへk-l人）
      if(j == i){
        x[j] <- l
      }
      else if(j == i+1){
        x[j] <- k-l
      }
      else{
        x[j] <- x[j]
      }
    }
  return(x)
}



# rufunction <- function(t1, x, lambda){ #更新関数の再帰Φ
#   for(i in 1:-t1){ #-Tから0までのステップ数だけ更新を繰り返す
#     x <- ufunction(x, lambda[i])
#   }
#   return(x)
# }
#
# rlfunction <- function(t1, x, lambda){ #更新関数の再帰Φ
#   for(i in 1:-t1){ #-Tから0までのステップ数だけ更新を繰り返す
#     x <- ufunction(x, lambda[i])
#   }
#   return(x)
# }

cftp <- function(xu0, xl0, L){
  break_flag <- 0
  T <- -1
  lambda <- runif(-T, min=1, max=L) #乱数列の最初の乱数を生成
  while(1){
    xu <- xu0
    xl <- xl0
    t <- T
    for(i in 1:-T){
      cat("xu=", xu, "xl=", xl, "\n")
      xu <- ufunction(xu, lambda[i]) #maximumに更新関数
      xl <- ufunction(xl, lambda[i]) #minimumに更新関数
      if(all(xu == xl)){ #更新した結果同じ状態が出力されていれば離脱
        break_flag <- 1
      }
      # if(all(xu >= xl)==FALSE){
      #   cat("xu=", xu, "xl=", xl, "\n")
      #   stop("error")
      # }
    }
    cat("xu=", xu, "xl=", xl, "\n")
    cat("\n")
    if(break_flag==1){
      break
    }
    T <- T*2
    lambda <- c(runif(-(T-t), min=1, max=L), lambda) #乱数列を伸ばす（サイズTまで）
  }
  return(list(xu, T)) #結果の状態とかかったステップ数をリストとして出力
}

cftp(xu, xl, L)
