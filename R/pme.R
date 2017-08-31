#' Calculate Point of Minimum Error
#'
#' @param x A matrix or data frame of binary responses to infrequency items.
#' @param p A vector of probabilities of random responses being flagged by
#' infrequency items.
#'
#' @return A list of class "pme" containing diagnostics and the cutoff point.
#' @export

pme <- function(x, p) {

  if(is.data.frame(x) == TRUE) x <- as.matrix(x)
  pme.input.check(x, p)

  # Sum over rows, then create proportion table
  prop.data <- table(factor(apply(x, 1, sum), levels = 0:ncol(x)))/nrow(x)

  # Proportion of data removed at >= x
  cumulative.prop <- rev(cumsum(rev(prop.data)))

  # Poisson Binomial Probabilities
  pois.prob <- poisson.binomial.pdf(p)

  # Probability of eliminating RR
  cumulative.prob <- rev(cumsum(rev(pois.prob[, 2])))
  names(cumulative.prob) <- pois.prob[, 1]

  # ROC Table
  roc.table <- cbind(cumulative.prop, cumulative.prob)
  colnames(roc.table) <- c(
    "Proportion of Data Eliminated",
    "Probability of Random Responder Eliminated"
  )

  # Cutoff - Euclidian Distance Method
  distances <- sqrt(roc.table[, 1]^2 + (1 - roc.table[, 2])^2)
  cutoff <- as.numeric(names(which.min(distances)))

  # Output
  output <- list(roc.table, cutoff)
  names(output) <- c("Diagnostic Table", "Cutoff")
  class(output) <- "pme"

  return(output)

}
