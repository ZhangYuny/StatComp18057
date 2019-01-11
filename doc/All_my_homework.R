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

## ------------------------------------------------------------------------
x <- rnorm(10)
y <- rnorm(10)
plot(x, y, xlab="Ten random values", ylab="Ten other values", xlim=c(-1, 1), ylim=c(-1, 1), pch=21, col="black", bg="black", bty="l", tcl=0.4, main="How to customize a plot with R", las=1, cex=1.5)

## ------------------------------------------------------------------------
data("InsectSprays")
aov.spray <- aov(sqrt(count) ~ spray, data = InsectSprays)
aov.spray
summary(aov.spray)

## ------------------------------------------------------------------------
data("women")
fit <- lm(weight ~ height, data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height, women$weight, xlab = "Height (in inches)", ylab = "Weight (in pounds)")
abline(fit)

## ------------------------------------------------------------------------
x <- c(0,1,2,3,4); p <- c(0.1,0.2,0.2,0.2,0.3)
    cp <- cumsum(p); m <- 1000; r <- numeric(m)
    r1 <- x[findInterval(runif(m),cp)+1]
    ct1 <- as.vector(table(r1));
    f1<-rbind(x,ct1/sum(ct1));rownames(f1)<-c("x","frequency")
    f1
    ct1/sum(ct1)/p


## ------------------------------------------------------------------------
set.seed(123); n<-1000
r2<-sample(c(0,1,2,3,4),n,replace=T,c(0.1,0.2,0.2,0.2,0.3))
ct2<-as.vector(table(r2))
f2<-rbind(x,ct2/sum(ct2));rownames(f2)<-c("x","frequency")
f2
ct2/sum(ct2)/p

## ------------------------------------------------------------------------
set.seed(123)
n <- 1000;j<-k<-0;y <- numeric(n)
while (k < n) {
u <- runif(1)
j <- j + 1
x <- runif(1) #random variate from g
if (4*x^2*(1-x) > u) {
#we accept x
k <- k + 1
y[k] <- x
}
}
j    # #(experiments) for n random numbers 

