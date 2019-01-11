## ------------------------------------------------------------------------
cvm.stat<-function(x,y){
  n <- length(x)
  m <- length(y)
  Fn<-ecdf(x)
  Gm<-ecdf(y)
  t<-(m*n/(m+n)^2)*(sum((Fn(x)-Gm(x))^2)+sum((Fn(y)-Gm(y))^2))
  return(t)
}

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
corr<-function(xs,ys){
  c1<-Map(cov,xs,ys)
  v1<-vapply(xs,var,double(1))
  v2<-vapply(ys,var,double(1))
  unlist(Map(function(c,v1,v2) c/((sqrt(v1))*(sqrt(v2))),c1,v1,v2))
}

