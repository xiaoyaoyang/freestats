\name{cv.work}
\alias{cv.work}
\title{K-fold cross validation}
\usage{
cv.work(fun, k = 5, data, cost, response = "y", ...)
}
\arguments{
  \item{fun}{The model function to call on the data}

  \item{k}{The number of folds}

  \item{data}{The data}

  \item{cost}{Cost function for the
  error:'cost.mse','cost.01'}

  \item{response}{Character vector indicating which column
  is the response}

  \item{\dots}{Extra arguments for model function}
}
\value{
A single cross-validated error
}
\description{
Calculate cross-validation error
}
\examples{
set.seed(188)
X <- rnorm(n=100,mean=3,sd=2)
y <- rnorm(100) + X
dat <- data.frame(y=y,X=X)
cv.work(fun=lm,k=5,data=dat,cost=cost.mse,response='y',formula=y~X)
}
\author{
Xiaoyao Yang
}

