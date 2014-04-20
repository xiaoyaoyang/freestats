\name{decisionStump}
\alias{decisionStump}
\title{Decision Stump Algorithm}
\usage{
decisionStump(X, w, y)
}
\arguments{
  \item{X}{Data matrix / Data frame}

  \item{w}{Weight that given to each observation. Used in
  calculate cost function.}

  \item{y}{Class label for data points in X, must be -1 or
  1}
}
\value{
\item{j}{The best dimention to cut the tree}
\item{theta}{Value that seperate tree in the best
dimention} \item{m}{the routine label value (for now only
1)}
}
\description{
Do classification with tree method in one step
}
\examples{
set.seed(1024)
z <- runif(n=5)
mydata <- fakedata(w=z,n=100)
X<- mydata$S[,1:4]
y <- mydata$y
w <- rep(1/100,100)
pars <- decisionStump(X=X,w=w,y=y)
}
\author{
Xiaoyao Yang
}

