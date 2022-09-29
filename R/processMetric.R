#Returns metric with type and breaks or distance.mat as attributes
processMetric <- function(metric, breaks = NULL, distance.mat = NULL) {
  if (length(metric) != 1 || !is.character(metric)) {
    customStop("'metric' must be a string.", 'makeFrontier()')
  }

  metric <- try(match_arg(tolower(metric), c("l1", "l1median", "l2", "l2median",
                                             "dist",  "mahal", "euclid", "custom",
                                             "energy")), silent = TRUE)
  if (inherits(metric, "try-error")){
    customStop("'metric' must be either 'L1', 'L2', 'dist', or 'energy'.", 'makeFrontier()')
  }

  if (metric == "mahal") {
    metric <- "dist"
    attr(metric, "distance.mat") <- "mahalanobis"
  }
  else if (metric == "euclid") {
    metric <- "dist"
    attr(metric, "distance.mat") <- "euclidean"
  }
  else if (metric == "custom") {
    metric <- "dist"
    if (is.null(distance.mat)) {
      customStop("'distance.mat' must be specified when metric = \"custom\".", "makeFrontier()")
    }
    attr(metric, "distance.mat") <- distance.mat
  }
  else if (metric %in% c("dist", "energy")) {
    attr(metric, "distance.mat") <- distance.mat
  }
  else if (metric %in% c("l1median", "l2median")) {
    metric <- substring(metric, 1, 2)
    attr(metric, "breaks") <- "median"
  }
  else  if (metric %in% c("l1", "l2")) {
    attr(metric, "breaks") <- breaks
  }

  attr(metric, "type") <- metricType(metric)

  if (attr(metric, "type") == "bin" && !is.null(distance.mat)) {
    customWarning("'distance.mat' is ignored with bin-based frontiers", "makeFrontier()")
  }
  else if (attr(metric, "type") != "bin" && !is.null(breaks)) {
    customWarning("'breaks' is ignored with non-bin-based frontiers", "makeFrontier()")
  }

  metric
}

metric2info <- function(metric) {
  metric <- tolower(metric)
  if (startsWith(metric, "l1")) "L1 statistic"
  else if (startsWith(metric, "l2")) "L2 statistic"
  # else if (metric == "mahal") "average pairwise Mahalanobis distance"
  # else if (metric == "euclid") "average pairwise Euclidean distance"
  # else if (metric == "custom") "average pairwise distance"
  else if (metric == "dist") "average pairwise distance"
  else if (metric == "energy") "energy distance"
}

metricType <- function(metric) {
  switch(metric,
         "dist" = "dist",
         "l1" = "bin",
         "l2" = "bin",
         "energy" = "energy",
         stop("Unrecognized metric."))
}