\name{classify.ab}
\alias{classify.ab}
\title{Classify for adaboost result}
\usage{
\method{classify}{ab}(pars, dat)
}
\arguments{
  \item{pars}{Result returned by adaboost algorithm}

  \item{dat}{Data set to be classified}
}
\value{
An label (vector) obtained by classification method
determined by pars
}
\description{
Returning classification result returned by function
adaBoost
}
\details{
Specific designed for result returned by adaBoost function

For more information \code{\link{adaBoost}}
}
\examples{
set.seed(1024)
z <- runif(n=5)
mydata <- fakedata(w=z,n=100)
X<- mydata$S[,1:4]
y <- mydata$y
res <- adaBoost(dat.train=X,y.train=y,B=3)
classify.ab(res,X)
}
\author{
Xiaoyao Yang
}

