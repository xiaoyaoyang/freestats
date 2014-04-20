\name{fakedata}
\alias{fakedata}
\title{Generating seperatable data}
\usage{
fakedata(w, n)
}
\arguments{
  \item{w}{The w[1:d] is the normal vector of a hyperplane,
  w[d+1] = -c is the negative offset parameter}

  \item{n}{Sample size}
}
\value{
\item{S}{n by (d+1) sample matrix with last col equal to 1}
\item{y}{Normal vector of the associated class labels}
}
\description{
fakedata for algorithm like perceptron
}
\examples{
fakedata(c(1,1,1),10)
}
\author{
Xiaoyao Yang
}

