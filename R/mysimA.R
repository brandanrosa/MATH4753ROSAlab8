#' mysimA
#'
#' This function simulations a Uniform(0,1) Distribution
#'
#' @param iter Number of iterations for the simulation
#'
#' @return a histogram of the simulation and a named list containing: quantiles, mean, variance (of the sim), and the theoretical variance.
#' @export
#'
#' @importFrom stats dgamma dunif optimize quantile rgamma runif var
#'
#' @examples \dontrun{mysimA(iter=10000)}
mysimA <- function(iter) {

  x <- iter

  randos <- runif(n = x, min = 0, max = 1)

  histo <- graphics::hist(randos, xlab = "ASim",
                freq = FALSE,
                col = "grey",
                main = "Histogram of ASim")

  graphics::curve(dunif(x, 0, 1), add = TRUE, col = "hotpink", lwd = 3, to = 1)

  q <- quantile(randos, probs = c(.25, .5, .975))
  meanA <- mean(randos)
  varA <- var(randos)
  un <- dunif(x, 0, 1)
  varAtheory <- (max(randos) - min(randos))^2/12

  l <- list(q = q, meanA = meanA, varA = varA, varAtheory = varAtheory)
  print(l)
}
