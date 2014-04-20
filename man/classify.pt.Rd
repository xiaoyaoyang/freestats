\name{classify.pt}
\alias{classify.pt}
\title{Classify for perceptrain result}
\usage{
\method{classify}{pt}(pars, dat)
}
\arguments{
  \item{pars}{Result returned by training algorithm}

  \item{dat}{Data set to be classified (Here
  cbind(dat,1)=S)}
}
\value{
An label obtained by classification method determined by
pars
}
\description{
Returning classification result
}
\details{
For consistency, using X(data set) instead of S
([dataset,1]) for classify function.

For more information \code{\link{perceptrain}}
}
\examples{
set.seed(1024)
z <- runif(n=3)
mydata <- fakedata(w=z,n=100)
r <- perceptrain(S=mydata$S,y=mydata$y,alpha_k=1,endcost=0)
classify.pt(r,mydata$S[,1:(NCOL(mydata$S)-1)])
}
\author{
Xiaoyao Yang
}

