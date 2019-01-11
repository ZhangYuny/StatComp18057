#' @title  K nearest neighbor statistics
#' @description The Kth nearest neighbor statistic measures the proportion of first through Kth nearest
#' neighbor coincidences
#' @param z An M x d data.frame or matrix, where each of the M rows is a point or a (column) vector (where d=1).
#' @param ix a Permutation of the row Index set of z
#' @param sizes vector of samples sizes
#' @param k The number of nearest neighbours to compute.
#' @return the proportion of first through Kth nearest neighbor coincidences
#' @examples
#' \dontrun{
#' library(RANN)
#' x <- matrix(rnorm(100*2,0,1),ncol=2);
#' y <- cbind(rnorm(50),rnorm(50,0,2))
#' z <- rbind(x,y)
#' ix<-1:150
#' k=5
#' sizes<-c(100,50)
#' t3<-Tn(z,ix,sizes,k)
#' print(t3)
#' }
#' @export
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  o <- rep(0, NROW(z))
  z <- as.data.frame(cbind(z, o))
  NN <- nn2(z,k=k)
  block1 <- NN$nn.idx[1:n1, ]
  block2 <- NN$nn.idx[(n1+1):n, ]
  i1 <- sum(block1 < n1 + .5)
  i2 <- sum(block2 > n1 + .5)
  return((i1 + i2) / (k * n))
}
