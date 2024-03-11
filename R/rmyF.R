#' rmyF
#'
#' a w-F theorem simulation of a Gamma(alpha=2, beta=1) using an objective function approach.
#'
#' @param iter iter Number of iterations for the simulation
#'
#' @return a histogram of the simulation with a theoretical curve and a named list containing: optimaization min, quantiles, mean, variance (of the sim), and the theoretical variance.
#' @export
#'
#' @importFrom stats dgamma dunif optimize quantile rgamma runif var
#'
#' @examples \dontrun{rmyF(iter=10000)}
rmyF <- function(iter) {
  x <- NULL
  y <- seq(0, 20, length = 10000)
  ans <- vector(mode = "numeric", length = iter)

  f <- function(x) {
    abs(1-w-x*exp(-x)-exp(-x))
  }

  for (i in 1:iter) {
    w <- runif(1)
    v <- f(y)
    index <- which.min(v)
    ans[i] <- y[index]
  }

  xx <- ans

  h <- graphics::hist(xx,
            freq = FALSE,
            ylim = c(0,0.4),
            xlim = c(0,12),
            col = "darkgray",
            main = "Histogram of y",
            xlab = "B")

  graphics::curve(dgamma(x, shape = 2, scale = 1), add = TRUE, col = "hotpink", lwd = 3)

  op <- optimize(f, lower = 0, upper = 20)
  q <- quantile(xx, probs = c(.25, .5, .975))
  meanF <- mean(xx)
  varF <- var(xx)
  varFtheory <- 2*1^2

  l <- list(opimization=op, q=q, meanF=meanF, varF=varF, varFtheory=varFtheory)
  l
}
