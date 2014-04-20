\name{classify}
\alias{classify}
\title{Generic classify function}
\usage{
classify(pars, dat)
}
\arguments{
  \item{pars}{Result returned by training algorithm}

  \item{dat}{Data set to be classified}
}
\value{
An label obtained by classification method determined by
pars
}
\description{
Returning classification result
}
\details{
Returning label for classification problem

This is the S3 generic method for returning classification
result.

For more information \code{\link{classify.default}}
}
\examples{
set.seed(1024)
z <- runif(n=5)
mydata <- fakedata(w=z,n=100)
X<- mydata$S[,1:4]
y <- mydata$y
w <- rep(1/100,100)
pars <- decisionStump(X=X,w=w,y=y)
classify(pars,X)
}
\author{
Xiaoyao Yang
}

