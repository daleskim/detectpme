print.pme <- function(pme.obj) {

  cat("Point of Minimum Error:", pme.obj[["Cutoff"]])

}

plot.pme <- function(pme.obj, main = "ROC Curve",
  xlab = "Proportion of Data Elminated",
  ylab = "Probability of Random Responder Eliminated",
  size = 3) {

  x <- pme.obj[["Diagnostic Table"]][, 1]
  y <- pme.obj[["Diagnostic Table"]][, 2]
  z <- pme.obj[["Cutoff"]] + 1

  # Initialize Plot
  plot(NA, type = "n", xlim = 0:1, ylim = 0:1,
    main = main, xlab = xlab, ylab = ylab)

  # Set Points
  points(x, y)
  points(x[z], y[z], cex = size)
  lines(x, y)

}
