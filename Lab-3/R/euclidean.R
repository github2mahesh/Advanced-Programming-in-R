#' Greatest Common Divisor
#' @param x number
#' @param y number
#' @description Finds the greatest common divisor of two numbers
#' @return the Greatest common divisor between two numbers 
#' @references 
#' https://en.wikipedia.org/wiki/Euclidean algorithm
#' @export
#' @examples 
#' euclidean(123612,13892347912)
#' euclidean(100, 1000)



euclidean <-
function(x, y) {
  stopifnot(is.numeric(x),is.numeric(y))
  while(y!=0) 
  {
    r=x%%y    # x= y * Q + r
    x<-y
    y<-r
  }
  return(x)
}
