#' @title Correlation coefficient
#' @description Simultaneous calculation of correlation coefficients of multiple pairs of vectors
#' @param xs a (non-empty) numeric matrix of data values
#' @param ys a (non-empty) numeric matrix of data values
#' @return the correlation coefficient of multiple pairs of vectors
#' @examples
#' \dontrun{
#' x <- replicate(5, runif(10), simplify = FALSE)
#' y <- replicate(5, rnorm(10), simplify = FALSE)
#' corr(x,y)
#' }
#' @export
corr<-function(xs,ys){
  c1<-Map(cov,xs,ys)
  v1<-vapply(xs,var,double(1))
  v2<-vapply(ys,var,double(1))
  unlist(Map(function(c,v1,v2) c/((sqrt(v1))*(sqrt(v2))),c1,v1,v2))
}
