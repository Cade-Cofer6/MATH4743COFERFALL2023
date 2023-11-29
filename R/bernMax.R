#' Title
#'
#' @param x left bound
#' @param p right bound
#'
#' @return returns a solution to the bernouli equation
#' @export
#'
#' @examples
bernMax <- function(x, p){
  n = length(x)
  holder = p^sum(x) * (1-p)*(n-sum(x))
}
