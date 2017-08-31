#' Poisson Binomial PDF
#'
#' @param p A numeric vector of binomial probabilities.
#'
#' @return A numeric matrix of outcomes and probabilities.
#' @export
#'
#' @examples
#' # 1. I have three fair coins. What is the probability of the number of heads,
#' #    when they are all flipped simultaneously?
#'
#' poisson.binomial.pdf(c(.5, .5, .5))
#'
#' #      Successes Probability
#' # [1,]         0       0.125
#' # [2,]         1       0.375
#' # [3,]         2       0.375
#' # [4,]         3       0.125
#'
#' # 2. I have two biased coins. The probability of heads is 0.2 and 0.4. What
#' #    is the probability of the number of heads when they are all flipped
#' #    simultaneously?
#'
#' poisson.binomial.pdf(c(0.2, 0.4))
#'
#' #      Successes Probability
#' # [1,]         0        0.48
#' # [2,]         1        0.44
#' # [3,]         2        0.08

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
