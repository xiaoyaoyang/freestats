\name{adaBoost}
\alias{adaBoost}
\title{Adaboost algorithm}
\usage{
adaBoost(train = decisionStump, dat.train, y.train, B = 10, ...)
}
\arguments{
  \item{train}{Function of weak learner that would be used
  in adaboost, must have form train(dat.train,w,y.train)}

  \item{dat.train}{Training data set}

  \item{y.train}{Label for training data set}

  \item{B}{Number of weak learners that will used}

  \item{\dots}{Other parameters that need to passed in
  train function}
}
\value{
\item{alpha}{The weight for different weak learners}
\item{allPars}{A list of parameters for different weak
learners}
}
\description{
Do classification using adaboost algorithm with
decisionStump as weak learner
}
\details{
Train function can be any weak learner algorithm. For now,
train function must has form train(X,w,y,...). see more in
\code{\link{decisionStump}}

If you have any good weak learner but can't use it in this
function, feel free to let me know.
}
\examples{
set.seed(1024)
z <- runif(n=5)
mydata <- fakedata(w=z,n=100)
X<- mydata$S[,1:4]
y <- mydata$y
res <- adaBoost(dat.train=X,y.train=y,B=3)
}
\author{
Xiaoyao Yang
}

