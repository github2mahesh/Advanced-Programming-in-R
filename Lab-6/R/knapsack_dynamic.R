#' Knapsack problem using greedy algortihm
#'
#' @param x a data.frame with two variables v and w (values and weights)
#' @param W size (Max weight capacity)
#'
#' @return the maximum values and elements to be carried by knapsack
#' 
#' @export
#'

knapsack_dynamic <-function(x,W){
  stopifnot(is.data.frame(x),
            is.numeric(W),
            W>0,
            is.numeric(x$w),
            is.numeric(x$v),
            x$v>=0,
            x$w>=0)
  
  mat<- matrix(nrow = 1+nrow(x), ncol = 1+W)
  mat[,1]<-0
  mat[1,]<-0
  for (i in 2:nrow(mat)){
    for (j in 2:ncol(mat)){
      if (x$w[i-1]>(j-1)){
        mat[i,j]<-mat[i-1,j]
      } else if (x$w[i-1] <= j-1){
        mat[i,j]=max(mat[i-1,j],x$v[i-1]+mat[i-1,j-x$w[i-1]])
      }
    }
  }
  
  val=mat[nrow(mat),ncol(mat)]
  elements=c()
  k=nrow(x)+1
  while (k>1) {
    if(val %in% mat[k-1,]){
      k<-k-1
    }else{
      elements<-c(elements,k-1)
      val=val-x$v[k-1]
      k<-k-1
    }
  }
  output <- list(
    value = round(mat[nrow(mat),ncol(mat)]),
    elements = sort(elements, decreasing = FALSE)
  )
  return(output)
}