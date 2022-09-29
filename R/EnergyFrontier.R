EnergyFrontier <- function(treatment, data, formula, metric, QOI, ratio, verbose){

  treat <- data[[treatment]]

  if (is.null(attr(metric, "distance.mat"))) distance.mat <- "scaled_euclidean"
  else distance.mat <- attr(metric, "distance.mat")

  if (is.character(distance.mat) && length(distance.mat) == 1L) {
    if (verbose) cat("Computing distance matrix...\n")

    new.formula <- update(formula, NULL ~ .)

    distance.mat <- match_arg(distance.mat, c("mahalanobis", "robust_mahalanobis", "scaled_euclidean", "euclidean"))

    distance.mat <- {
      if (distance.mat == "mahalanobis") {
        MatchIt::mahalanobis_dist(new.formula, data)
      }
      else if (distance.mat == "scaled_euclidean") {
        MatchIt::scaled_euclidean_dist(new.formula, data)
      }
      else if (distance.mat == "euclidean") {
        MatchIt::euclidean_dist(new.formula, data)
      }
      else if (distance.mat == "robust_mahalanobis") {
        MatchIt::robust_mahalanobis_dist(new.formula, data)
      }
    }

  }
  else {
    if (inherits(distance.mat, "dist")) distance.mat <- as.matrix(distance.mat)

    if (!is.matrix(distance.mat) || !all(dim(distance.mat) == length(treat)) ||
        !all(abs(diag(distance.mat)) < 1e-8) || any(distance.mat < 0) ||
        !check_symmetric(unname(distance.mat))) {
      customStop("'distance.mat' must be one of \"mahalanobis\", \"scaled_euclidean\", or \"euclidean\" or a square, symmetric, N x N distance matrix when metric = \"energy\".",
                 "makeFrontier()")
    }
  }

  if (verbose) cat("Calculating frontier...\n")

  if (QOI == "SATE") {
    frontier <- energyToFrontierSATE(distance.mat, treat, verbose, ratio)
  }
  else if (QOI == "FSATE") {
    frontier <- energyToFrontierFSATE(distance.mat, treat, verbose, ratio)
  }
  else if (QOI == "SATT") {
    frontier <- energyToFrontierSATT(distance.mat, treat, verbose, ratio)
  }

  if (verbose) cat("Done!\n")

  return(frontier)
}
