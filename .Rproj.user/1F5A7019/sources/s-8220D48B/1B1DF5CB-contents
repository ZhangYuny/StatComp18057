#' @title Cramer-von Mises statistic
#' @description calculate the two-sample Cramer-von Mises statistic value
#' @param x a numeric vector of data values
#' @param y either a numeric vector of data values
#' @return the numeric value of the statistic
#' @examples
#' \dontrun{
#' x <- rnorm(50)
#' y <- runif(30)
#' # Do x and y come from the same distribution?
#' cvm.stat(x, y)
#'
#' # test if x is stochastically larger than x2
#' x2 <- rnorm(50, -1)
#' plot(ecdf(x), xlim = range(c(x, x2)))
#' plot(ecdf(x2), add = TRUE, lty = "dashed")
#' cvm.stat(x, x2)
#' }
#' @export
cvm.stat<-function(x,y){
  n <- length(x)
  m <- length(y)
  Fn<-ecdf(x)
  Gm<-ecdf(y)
  t<-(m*n/(m+n)^2)*(sum((Fn(x)-Gm(x))^2)+sum((Fn(y)-Gm(y))^2))
  return(t)
}
