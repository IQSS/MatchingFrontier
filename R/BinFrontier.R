BinFrontier <- function(treatment, data, formula, metric, QOI, match.on, ratio, verbose) {

  treat <- data[[treatment]]

  if (is.null(attr(metric, "breaks"))) breaks <- "sturges"
  else breaks <- attr(metric, "breaks")

  if (verbose) cat("Computing bins...\n")

  if (is.character(breaks) && identical(tolower(breaks), "median")) {
    bins.list <- getBinsAtMedian(data, match.on, treat, metric, verbose)
  }
  else {
    bins.list <- getBins(data, match.on, breaks)
  }
  strata <- assignToBins(data, match.on, bins.list)

  if (verbose) cat("Calculating frontier...\n")

  if (QOI == "FSATE") {
    frontier <- binsToFrontierFSATE(strata, treat, metric, verbose, ratio)
  }
  else if (QOI == "SATT") {
    frontier <- binsToFrontierSATT(strata, treat, metric, verbose, ratio)
  }

  if (verbose) cat("Done!\n")

  return(frontier)
}
