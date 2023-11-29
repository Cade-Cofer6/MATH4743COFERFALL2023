#' Title
#'
#' @param iter The number of iterations the function goes through
#' @param n the size of the given group
#' @param p the probability
#'
#' @return a colored graph
#' @export
#'
#' @examples
mybin <- function(iter, n, p ){

   sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)

    succ=c()
       for( i in 1:iter){

        sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

        succ[i]=sum(sam.mat[,i])
      }
    #Make a table of successes
    succ.tab=table(factor(succ,levels=0:n))

    #Make a barplot of the proportions
   iter.lab = paste0("iter = ", iter)
    n.lab = paste0("n = ", n)
    p.lab = paste0("p = ", p)
    lab = paste(iter.lab, n.lab, p.lab, sep = ", ")
    barplot(succ.tab/(iter), col = rainbow(n+1), main = "Binomial Simulation", sub = lab, xlab = "Number of Successes")
    succ.tab/iter
}


