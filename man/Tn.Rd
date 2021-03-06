% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Tn.stat.R
\name{Tn}
\alias{Tn}
\title{K nearest neighbor statistics}
\usage{
Tn(z, ix, sizes, k)
}
\arguments{
\item{z}{An M x d data.frame or matrix, where each of the M rows is a point or a (column) vector (where d=1).}

\item{ix}{a Permutation of the row Index set of z}

\item{sizes}{vector of samples sizes}

\item{k}{The number of nearest neighbours to compute.}
}
\value{
the proportion of first through Kth nearest neighbor coincidences
}
\description{
The Kth nearest neighbor statistic measures the proportion of first through Kth nearest
neighbor coincidences
}
\examples{
\dontrun{
library(RANN)
x <- matrix(rnorm(100*2,0,1),ncol=2);
y <- cbind(rnorm(50),rnorm(50,0,2))
z <- rbind(x,y)
ix<-1:150
k=5
sizes<-c(100,50)
t3<-Tn(z,ix,sizes,k)
print(t3)
}
}
