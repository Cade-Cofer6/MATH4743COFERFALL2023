

#' Title
#'
#' @param mu
#' @param sigma
#'
#' @return a dnorm curve
#' @export
#'
#' @examples
myncurve <- function(mu, sigma){

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(mu, sigma,length=1000)

  ycurve=dnorm(xcurve,mu, sigma)

  polygon(c(mu ,xcurve, sigma),c(0,ycurve,0),col="Red")

}

