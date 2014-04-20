\name{classify.ds}
\alias{classify.ds}
\title{classify.ds}
\usage{
\method{classify}{ds}(...)
}
\arguments{
  \item{\dots}{See \code{\link{classify.default}} for
  argument details}
}
\value{
See \code{\link{classify.default}} for more information.
}
\description{
Returning classification for DecisionStump
}
\details{
Returning label for classification problem

\code{\link{classify}} is the S3 generic method for
returning classification result.

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
classify.ds(pars,X)
}
\author{
Xiaoyao Yang
}

