#' Project-1 Overbooking problem
#'
#' @param N number of seats in the flight
#' @param gamma probabilty the plane will be truly overbooked
#' @param p probabilty of a "show"
#'
#' @return two graphs one discrete and one continous and a list of vals
#' @export
#'
#' @examples ntickets(400, 0.2, 0.95)
ntickets <- function(N, gamma, p){


  # Uses qbinom to calculate the value of nd
  calc_Binom_Dist <- function(n){
    return(N - qbinom(gamma, n, p))
  }

  nd <- round(optimise(calc_Binom_Dist, interval = c(1, N), maximum = FALSE)$minimum)


  # uses pnorm to calculate the value of nc
  calc_Norm_Dist <- function(n) {
    return(N - qnorm(1 - gamma, n, p))
  }

  nc <- round(optimise(calc_Norm_Dist, interval = c(1, N), maximum = FALSE)$minimum)

  n_seq <- seq(1, N, by = 10)
  sd_Seq = sd(c(n_seq))
  objective_Function <- 1 - gamma - pnorm(1, mean = n_seq, sd = sd_Seq)
  plot(n_seq, objective_Function, type = "o", xlab = "n", ylab = "Objective", col = "blue")
  plot(n_seq, objective_Function, type = "l", xlab = "n", ylab = "Objective")

  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))

}
