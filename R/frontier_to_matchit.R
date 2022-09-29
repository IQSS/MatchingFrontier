#Edit for new metrics!
frontier_to_matchit <- function(frontier.object, N, Ndrop) {

  if (!inherits(frontier.object, "matchFrontier")) {
    customStop("'frontier.object' must be a matchFrontier object, the output of a call to makeFrontier().")
  }
  if (missing(N) && missing(Ndrop)) {
    customStop("one of 'N' or 'Ndrop' must be specified.")
  }
  else if (!missing(N) && !missing(Ndrop)) {
    customStop("only one of 'N' or 'Ndrop' may be specified.")
  }
  else if (!missing(Ndrop)) {
    if (!is.numeric(Ndrop) || length(Ndrop) != 1 ||
        Ndrop < 0 || Ndrop > frontier.object$n - 1) {
      customStop(paste0("'Ndrop' must be a single number between 0 and ", frontier.object$n - 1, "."))
    }
    N <- frontier.object$n - Ndrop
  }
  else if (!missing(N)) {
    if (!is.numeric(N) || length(N) != 1 ||
        N < 1 || N > frontier.object$n) {
      customStop(paste0("'N' must be a single number between 1 and ", frontier.object$n, "."))
    }
  }

  Ns <- frontier.object$n - frontier.object$frontier$Xs
  ind <- which.min(Ns[Ns >= N])
  drop.inds <- unlist(frontier.object$frontier$drop.order[seq_len(ind)])

  estimand <- switch(frontier.object$QOI,
                     "SATE" = "ATE",
                     "FSATE" = "ATE",
                     "SATT" = "ATT",
                     "FSATT" = "ATT")

  lab <- rownames(frontier.object$data)
  treat <- setNames(frontier.object$data[[frontier.object$treatment]], lab)

  if (!is.null(frontier.object$matched.to)) {
    is.na(frontier.object$matched.to)[drop.inds,] <- TRUE

    mm <- frontier.object$matched.to
    rownames(mm) <- lab

    weights <- as.numeric(!is.na(frontier.object$matched.to[,1])) + tabulate(frontier.object$matched.to, nbins = nrow(frontier.object$matched.to))/ncol(frontier.object$matched.to)
    names(weights) <- lab

    if (estimand == "ATT") {
      mm <- mm[treat == 1,, drop = FALSE]
    }

    match.matrix <- array(NA_character_, dim = dim(mm), dimnames = dimnames(mm))
    match.matrix[] <- lab[mm]
    rm(mm)
  }
  else {
    match.matrix <- NULL
    weights <- setNames(rep(1, nrow(frontier.object$data)), lab)
    weights[drop.inds] <- 0
  }

  method <- "matchingFrontier"
  attr(method, "method") <- paste0("matching frontier (minimizing the ", metric2info(frontier.object$metric), ")")
  info <- list(method = method,
               distance = NULL,
               replace = if (inherits(frontier.object, "distFrontier")) TRUE else NULL,
               ratio = if (inherits(frontier.object, "distFrontier")) ncol(match.matrix) else NULL,
               mahalanobis = FALSE,
               distance_is_matrix = frontier.object$metric == "custom")

  # if (inherits(frontier.object, "distFrontier")) {
  #   info$distance <- "user"
  #   attr(info$distance, "custom") <- switch(frontier.object$metric,
  #                                           "mahal" = "Mahalanobis",
  #                                           "euclid" = "Euclidean",
  #                                           NULL)
  # }

  out <- list(
    match.matrix = match.matrix,
    weights = weights,
    X = frontier.object$data[frontier.object$match.on],
    call = frontier.object$call,
    info = info,
    estimand = estimand,
    formula = reformulate(frontier.object$match.on, frontier.object$treatment),
    treat = treat,
    discarded = rep(FALSE, length(treat)))

  class(out) <- "matchit"
  return(out)

}
