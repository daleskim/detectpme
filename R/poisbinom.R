#' Poisson Binomial PDF
#'
#' @param p A numeric vector of binomial probabilities.
#'
#' @return A numeric matrix of outcomes and probabilities.
#' @export
#'
#' @examples

poisson.binomial.pdf <- function(p) { # Input is a vector of probabilities
  # Input Checks:
  # 1. Numeric Probabilities
  if (is.numeric(p) != TRUE) {
    stop("Invalid input. Probabilities must be numeric.")
  }
  # 2. Proper Range
  if (sum(0 <= p & p <= 1) != length(p)) {
    stop("Invalid input. Probabilities must be between 0 and 1.")
  }

  # Function to calculate a single outcome (to be applied to all outcomes)
  get.single.probability <- function(x, p) {
  # x is number of successes
  # p is a vector of probabilities
  n <- 1:length(p) # Vector index to be permuted
  q <- 1 - p # Probability of failures
  y <- combn(n, x) # All combinations of successes, given x

  # Settle 0 number of successes first (all failures)
  if (x == 0) {
    return(prod(q))
  }

  # If number of successes is other than 0, calculate it
  output <- sum(apply(y, 2, function(z) prod(c(p[z], q[-z]))))
    return(output)
  }

  x <- 0:length(p) # Vector of number of successes

  # Calculate probabilities
  prob <- sapply(x, get.single.probability, p = p)

  # Bind output into a matrix
  output <- cbind(x, prob)
  colnames(output) <- c("Successes", "Probability")

  return(output)

}
