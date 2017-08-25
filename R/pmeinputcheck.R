pme.input.check <- function(x, p) {

  if(is.null(dim(x))) stop("Data is not a matrix or data frame.")
  if(length(dim(x)) != 2) stop("Data is not a matrix or data frame.")
  if(ncol(x)!=length(p)) stop("Numer of items and probabilities do not match.")
  if(is.numeric(x) == FALSE) stop("Data must be numeric.")
  if(sum(is.na(x)) > 1) stop("Missing values must be handled.")
  if(sum(x > 1) > 1) stop("Missing values must be handled.")
  if((sum(x==0)+sum(x==1))!=nrow(x)*ncol(x)) stop("Data must be 0 or 1")

}
