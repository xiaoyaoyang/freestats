
#' @title An original perceptron algorithm
#' @description Train data with perceptron algorithm
#' @export perceptrain
#' @return \item{z}{Normal vector of a hyperplane: z=c(-c,Vh) }
#' \item{Z_history}{Trajactory of normal vector of a hyperplane}
#' \item{NumofIteration}{Number of iterations for algorithm}
#' @author Xiaoyao Yang. Also, thanks Ran Fu for improving function by introducing matrix computation method.
#' @details 
#' S is especially designed for perceptron.
#' 
#' For more information \code{\link{fakedata}}
#' @param S Each row represents a data points with last column equal to 1; S=[X,1]
#' @param y Class label for data points in S
#' @param alpha_k The speed of converge
#' @param endcost The termination condition of cost function
#' @examples
#' 
#' set.seed(1024)
#' z <- runif(n=3)
#' mydata <- fakedata(w=z,n=100)
#' r <- perceptrain(S=mydata$S,y=mydata$y,alpha_k=1,endcost=0)
#' r
#' 

perceptrain<-function(S,y,alpha_k=1,endcost=0){
    d <- dim(S)[2] - 1
    n <- dim(S)[1]
    x.matrix <- cbind(1, S[,1:d])
    z <- x.matrix[2,]
    Z_history <- matrix(0, 0, ncol = d + 1)
    NumofIteration = 0
    Cost.gradient = 10000
    while (sum(Cost.gradient^2) > endcost) {
        y_cost <- classify.pti(S,z) #designed for calculate cost function
        y_cost[ y_cost==y ] <- 0
        (Cost.gradient <- colSums(y_cost*x.matrix))  #using matrix computation
        Z_history <- rbind(Z_history, z)
        z <- z - alpha_k * Cost.gradient
        NumofIteration <- NumofIteration + 1
    }
    res <- list(z = z, Z_history = Z_history, NumofIteration = NumofIteration)
    class(res) <- "pt"
    return(res)
}

classify.pti<-function(S,z){
    #z<-c(-c,Vh) x<-c(x,1)
    d<-length(z)-1;
    yy<-as.vector(sign(z%*%rbind(1,t(S)[1:d,])))
    return(yy)
}

