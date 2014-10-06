\name{perceptrain}
\alias{perceptrain}
\title{An original perceptron algorithm}
\usage{
perceptrain(S, y, alpha_k = 1, endcost = 0)
}
\arguments{
  \item{S}{Each row represents a data points with last
  column equal to 1; S=[X,1]}

  \item{y}{Class label for data points in S}

  \item{alpha_k}{The speed of converge}

  \item{endcost}{The termination condition of cost
  function}
}
\value{
\item{z}{Normal vector of a hyperplane: z=c(-c,Vh) }
\item{Z_history}{Trajactory of normal vector of a
hyperplane} \item{NumofIteration}{Number of iterations for
algorithm}
}
\description{
Train data with perceptron algorithm
}
\details{
S is especially designed for perceptron.

For more information \code{\link{fakedata}}
}
\examples{
set.seed(1024)
z <- runif(n=3)
mydata <- fakedata(w=z,n=100)
r <- perceptrain(S=mydata$S,y=mydata$y,alpha_k=1,endcost=0)
r
}
\author{
Xiaoyao Yang. Also, thanks Ran Fu for improving function by
introducing matrix computation method.
}

