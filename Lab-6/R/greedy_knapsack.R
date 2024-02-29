#' Knapsack problem using greedy algortihm
#'
#' @param x a data.frame with two variables v and w (values and weights)
#' @param W size (Max weight capacity)
#'
#' @return the maximum values and elements to be carried by knapsack
#' 
#' @export
#'

greedy_knapsack <- function(x,W){
  stopifnot(is.data.frame(x),
            is.numeric(W),
            W>0,
            is.numeric(x$w),
            is.numeric(x$v),
            x$v>=0,
            x$w>=0)
  x$ratio<- x$v / x$w
  new_x <- x[order(x$ratio,decreasing = TRUE),]
  elements<- c()
  value<-0
  tw <- 0
  i = 1
  while(tw<=W){
    tv<-0
    tw <- tw + new_x$w[i]
    tv <- tv + new_x$v[i]
    if (tw <= W){
      value = value + tv
      elements <- c(elements,as.numeric(row.names(new_x)[i])) 
    }
    i=i+1
  }
  output <- list(
    value = round(value),
    elements = sort(elements, decreasing = FALSE)
  )
  return(output)
}