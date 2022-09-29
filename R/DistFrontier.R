DistFrontier <- function(treatment, data, formula, metric, QOI, ratio, verbose){

  treat <- data[[treatment]]

  if (is.null(attr(metric, "distance.mat"))) distance.mat <- "mahalanobis"
  else distance.mat <- attr(metric, "distance.mat")

  if (is.character(distance.mat) && length(distance.mat) == 1L && distance.mat != "custom") {
    if (verbose) cat("Computing distance matrix...\n")

    distance.mat <- match_arg(distance.mat, c("mahalanobis", "robust_mahalanobis", "scaled_euclidean", "euclidean"))

    distance.mat <- {
      if (distance.mat == "mahalanobis") {
        MatchIt::mahalanobis_dist(formula, data)
      }
      else if (distance.mat == "scaled_euclidean") {
        MatchIt::scaled_euclidean_dist(formula, data)
      }
      else if (distance.mat == "euclidean") {
        MatchIt::euclidean_dist(formula, data)
      }
      else if (distance.mat == "robust_mahalanobis") {
        MatchIt::robust_mahalanobis_dist(formula, data)
      }
    }
  }
  else {
    if (verbose) cat("Checking distance matrix...\n")

    if (inherits(distance.mat, "dist")) distance.mat <- as.matrix(distance.mat)

    okay <- is.matrix(distance.mat) && !any(distance.mat < 0)
    if (okay) {
      if (all(dim(distance.mat) == length(treat))) {
        if (!check_symmetric(unname(distance.mat))) okay <- FALSE
        else distance.mat <- distance.mat[treat == 1, treat == 0]
      }
      else if (nrow(distance.mat) != sum(treat == 1) || ncol(distance.mat) != sum(treat == 0)) {
        okay <- FALSE
      }
    }
    if (!okay) {
      customStop("'distance.mat' must be one of \"mahalanobis\", \"scaled\", or \"euclidean\" or a square, symmetric, N x N or N1 x N0 distance matrix when metric = \"dist\".",
                 "makeFrontier()")
    }
  }

  if (verbose) cat("Calculating frontier...\n")

  if (QOI == "FSATE") {
    frontier <- distToFrontierFSATE(distance.mat, treat, verbose, ratio)
  }
  else if (QOI == "FSATT") {
    frontier <- distToFrontierFSATT(distance.mat, treat, verbose, ratio)
  }

  if (verbose) cat("Done!\n")

  return(frontier)
}