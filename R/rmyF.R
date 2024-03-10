#' rmyF
#'
#' a w-F theorem simulation of a Gamma(alpha=2, beta=1) using an objective function approach.
#'
#' @param iter iter Number of iterations for the simulation
#'
#' @return a histogram of the simulation with a theoretical curve
#' @export
#'
#' @importFrom stats dgamma dunif optimize quantile rgamma runif var
#'
#' @examples \dontrun{rmyF(iter=10000)}
rmyF <- function(iter) {

  x <- NULL
  w <- runif(1)
  rands <- rgamma(n = iter, 2, 1)
  f <- function(x) {
    abs(1-w-x*exp(-x)-exp(-x))
    }

  histo <- graphics::hist(rands, xlab = "B",
                freq = FALSE,
                col = "grey",
                main = "Histogram of y",
                ylim = c(0, 0.4),
                xlim = c(0, 14))

  graphics::curve(dgamma(x, shape = 2, scale = 1), add = TRUE, col = "hotpink", lwd = 3)

  op <- optimize(f, lower = 0, upper = 20)
}
