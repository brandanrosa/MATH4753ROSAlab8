#' mysimB
#'
#' This function simulations a Gamma(alpha=2, beta=1) Distribution
#'
#' @param iter Number of iterations for the simulation
#'
#' @return a histogram of the simulation with a theoretical curve and a named list containing: quantiles, mean, variance (of the sim), and the theoretical variance.
#' @export
#'
#' @importFrom stats dgamma dunif optimize quantile rgamma runif var
#'
#' @examples \dontrun{mysimB(iter=10000)}
mysimB <- function(iter) {
  x <- iter
  rands <- rgamma(n = iter, 2, 1)
  histo <- graphics::hist(rands, xlab = "BSim",
                freq = FALSE,
                col = "grey",
                main = "Histogram of BSim",
                ylim = c(0, 0.4),
                xlim = c(0, 14))

  graphics::curve(dgamma(x, shape = 2, scale = 1), add = TRUE, col = "green", lwd = 3)

  q <- quantile(rands, probs = c(.25, .5, .975))
  meanB <- mean(rands)
  varB <- var(rands)
  gam <- dgamma(x, shape = 2, scale = 1)
  varBtheory <- 2*1^2

  l <- list(q=q, meanB=meanB, varB=varB, varBtheory=varBtheory)
  l
}
