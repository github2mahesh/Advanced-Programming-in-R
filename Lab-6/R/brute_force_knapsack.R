#' Knapsack problem using brute force knapsack algortihm
#'
#' @param x a data.frame with two variables v and w (values and weights)
#' @param W size (Max weight capacity)
#' @param parallel bool
#' @return the maximum values and elements to be carried by knapsack
#' @importFrom utils combn
#' @export

brute_force_knapsack <- function(x,W, parallel=FALSE){
  stopifnot(is.data.frame(x),
            is.numeric(W),
            W>0,
            is.numeric(x$w),
            is.numeric(x$v),
            x$v>=0,
            x$w>=0)
  
  
  id=c(1:nrow(x))
  elements<-c()
  value<-0
  
  if(parallel==TRUE){
    cores <- parallel::detectCores()-2
    
    
    cl <- parallel::makeCluster(cores, type = "PSOCK")
    parallel::clusterExport(cl, varlist=c("x","W","elements","value"), envir=environment())
    
    values<-parallel::parLapply(cl, 1:nrow(x), function(i, x,W) {
      
      for (a in 1:nrow(x)){
        y=as.matrix(combn(id,a))
        for (i in 1:ncol(y)){
          tv<-0
          tw<-0
          for (j in 1:nrow(y)){
            tw <- tw + x$w[y[j,i]]
            tv <- tv + x$v[y[j,i]]
          }
          if(tw<=W && tv>=value){
            value=max(value,tv)
            elements= y[,i]
          }
        }
      }
      output<-list(
        value = round(value),
        elements = elements)
      return(output)
      
    }, x, W)
    i=1
    a=values[[i]]
    val=a[[1]]
    elem=a[[2]]
    return(list("value"=val,"elements"=elem))
    parallel::stopCluster(cl)
    
  }
  
  else
  {
    brute_force<-function(x,W)
    {
      for (a in 1:nrow(x)){
        y=as.matrix(combn(id,a))
        for (i in 1:ncol(y)){
          tv<-0
          tw<-0
          for (j in 1:nrow(y)){
            tw <- tw + x$w[y[j,i]]
            tv <- tv + x$v[y[j,i]]
          }
          if(tw<=W && tv>=value){
            value=max(value,tv)
            elements= y[,i]
          }
        }
      }
      output<-list(
        value = round(value),
        elements = elements)
      return(output)
      
    }
    a = brute_force(x,W)
    val=a[[1]]
    elem=a[[2]]
    return(list("value"=val,"elements"=elem))
  }
  
}
