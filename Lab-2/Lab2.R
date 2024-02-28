name <- "UMAMAHESWARABABU MADDELA"
liuid <- "umama339"

sheldon_game<-function(player1,player2){
  choices <- c("rock","lizard","spock","scissors", "paper")
  stopifnot((player1 %in% choices) & (player2 %in% choices))
  play1<-which(choices == player1)
  play2<-which(choices == player2)
  if(any(play1+c(1,3)==play2)){
    return("Player 1 wins!")
  } else if(any(play1+c(2,4)==play2)){
    return("Player 2 wins!")
  } else {
    return("Draw!")
  }
}


my_moving_median <- function(x,n, ...){
  stopifnot(is.vector(x) & is.numeric(x) & is.numeric(n) & length(n)==1)
  temp<- list(...)
  med<-c()
  if(!is.null(temp$na.rm)){
    for (i in 1:(length(x)-n)){
      med[i]<-median(x[i:(i+n)], na.rm=TRUE)
    }
  } else{
    for (i in 1:(length(x)-n)){
      med[i]<-median(x[i:(i+n)])
    }
  }
  return(med)
}

for_mult_table=function(from,to){
  stopifnot(is.numeric(from) & is.numeric(to) & (length(from)==1) & (length(to)==1) )
  len<-from:to
  vec<-c()
  for(i in len){
    temp<-len*i
    vec<-c(vec,temp)
  }
  mul_tab<-matrix(vec,nrow = length(len), byrow = TRUE)
  colnames(mul_tab)<-len
  rownames(mul_tab)<-len
  return(mul_tab)
}

find_cumsum=function(x,find_sum){
  stopifnot((is.numeric(x))&(is.numeric(find_sum))&(length(find_sum)==1))
  cumsm<-0
  i<-1
  while(cumsm<find_sum){
    cumsm<-cumsm+x[i]
    i=i+1
    if(i>length(x)){
      break
    } 
  }
  return(cumsm)
}

while_mult_table=function(from,to){
  stopifnot(is.numeric(from) & is.numeric(to) & (length(from)==1) & (length(to)==1) )
  len<-from:to
  vec<-c()
  i=from
  while(i<=to){
    temp<-len*i
    vec<-c(vec,temp)
    i=i+1
  }
  mul_tab<-matrix(vec,nrow = length(len), byrow = TRUE)
  colnames(mul_tab)<-len
  rownames(mul_tab)<-len
  return(mul_tab)
}


repeat_find_cumsum=function(x,find_sum){
  stopifnot((is.numeric(x))&(is.numeric(find_sum))&(length(find_sum)==1))
  cumsm<-0
  i<-1
  repeat{
    if(cumsm<find_sum){
      cumsm<-cumsm+x[i]
    }
    i=i+1
    if(i>length(x)){
      break
    }
  }
  return(cumsm)
}


repeat_my_moving_median <- function(x,n, ...){
  stopifnot(is.vector(x) & is.numeric(x) & is.numeric(n) & length(n)==1)
  temp<- list(...)
  med<-c()
  i<-1
  repeat{
    if(!is.null(temp$na.rm)){
      med[i]<-median(x[i:(i+n)], na.rm=TRUE)
    }else {
      med[i]<-median(x[i:(i+n)])
    }
    i=i+1
    if(i>(length(x)-n)){
      break
    }
  }
  return(med)
}


in_environment=function(env){
  return((ls(env)))
}

cov<-function(X){
  stopifnot(is.data.frame(X))
  coef_var<-lapply(X, function(x) sd(x)/mean(x))
  coef_var<-unlist(coef_var)
  return((coef_var))
}


moment=function(i){
  stopifnot(is.numeric(i)==TRUE)
  function(x){
    mom<-0
    for (j in 1:length(x)){
      sum<-((x[j]-mean(x))^i)
      mom<-mom+sum
    }
    return(mom/length(x))
  }
}

