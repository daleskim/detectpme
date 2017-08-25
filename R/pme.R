#' Calculate Point of Minimum Error
#'
#' @param x A matrix or data frame of binary responses to infrequency items.
#' @param p A vector of probabilities of random responses to infrequency items.
#'
#' @return A vector of length 1, indicating the point of minimum error.
#' @export
#'
#' @examples

pme <- function(x, p) {

  if(is.data.frame(x) == TRUE) x <- as.matrix(x)
  pme.input.check(x, p)

  # Sum over rows, then create proportion table
  prop.data <- table(factor(apply(x, 1, sum), levels = 0:ncol(x)))/nrow(x)
  cumulative.prop <- cumsum(rev(prop.data)) # Proportion of data removed at >= x

  # Poisson Binomial Probabilities
  pois.prob <- poisson.binomial.pdf(p)
  cumulative.prob <- cumsum(rev(pois.prob[, 2])) # Probability of eliminating RR
  names(cumulative.prob) <- rev(pois.prob[, 1])

  # Cutoff
  output <- as.numeric(names(which.min(cumulative.prop - cumulative.prob)))
  names(output) <- "Point of Minimum Error"

  return(output)

}
