makeFrontier.matchit <- function(x, metric = 'dist',
                                 breaks = NULL, distance.mat = NULL,
                                 data = NULL,
                                 verbose = TRUE, ...){

  call <- match.call()
  call[[1]] <- quote(makeFrontier)

  QOI <- switch(x[["estimand"]],
                "ATE" = "FSATE",
                "ATT" = "FSATT",
                customStop("makeFrontier() can only be used on matchit objects for which the specified estimand is the ATT or ATE.", "makeFrontier()"))

  # Check the frontier arguments
  processed_metric <- processMetric(metric, breaks, distance.mat)

  checkArgs.matchit(QOI = QOI, metric = processed_metric, matchit = x, ...)

  if ("QOI" %in% ...names()) {
    customStop("the 'QOI' argument cannot be used with matchit objects. See help(\"makeFrontier.matchit\") for details.",
               "makeFrontier()")
  }

  match.on <- all.vars(delete.response(terms(x[["formula"]])))

  data <- MatchIt::match.data(x, drop.unmatched = FALSE, data = data)

  treatment <- all.vars(update(x[["formula"]], . ~ 0))

  checkDat(data, treatment, match.on)

  # formula <- x[["formula"]]
  formula <- reformulate(match.on, treatment)

  matchit.mahalanobis <- is.null(x[["distance"]]) && isTRUE(x[["info"]][["mahalanobis"]])

  frontier <- MatchItFrontier(treatment = treatment,
                              data = data,
                              formula = formula,
                              metric = processed_metric, QOI = QOI,
                              match.on = match.on,
                              match.matrix = x[["match.matrix"]],
                              ps = if (!matchit.mahalanobis) x[["distance"]]/sd(x[["distance"]]),
                              info = x[["info"]],
                              verbose = verbose)

  matched.to <- frontier[["matched.to"]]
  distance <- frontier[["distance"]]
  frontier[c("matched.to", "distance")] <- NULL

  out <- list(
    frontier = frontier,
    treatment = treatment,
    QOI = QOI,
    metric = as.character(processed_metric),
    data = data,
    match.on = match.on,
    matched.to = matched.to,
    distance = distance,
    call = call,
    n = switch(QOI,
               # "SATE" =,
               "FSATE" = sum(x[["weights"]] > 0),
               # "SATT" = sum(x[["treat"]] == 0 & x[["weights"]] > 0),
               "FSATT" = sum(x[["treat"]] == 1 & x[["weights"]] > 0))
  )

  class(out) <- c("MatchItFrontier", "matchFrontier")

  return(out)
}

MatchItFrontier <- function(treatment, data, formula, metric, QOI,
                            match.on = NULL, match.matrix, ps = NULL, info, verbose) {

  treat <- data[[treatment]]
  covs.mat <- NULL

  #Convert match.matrix to matched.to
  matched.to <- matrix(NA_integer_, nrow = length(treat), ncol = 1,
                       dimnames = list(rownames(data), NULL))
  matched.to[rownames(match.matrix), 1] <- setNames(seq_along(treat), rownames(data))[match.matrix[,1]]

  matched <- which(!is.na(matched.to[,1]))

  bins.list <- distance.type <- NULL

  metric.type <- attr(metric, "type")
  if (metric.type == "dist") {
    if (is.null(attr(metric, "distance.mat"))) distance.mat <- "mahalanobis"
    else distance.mat <- attr(metric, "distance.mat")

    if (is.character(distance.mat) && length(distance.mat) == 1L) {
      if (verbose) cat("Computing distance matrix...\n")

      distance.type <- match_arg(distance.mat, c("mahalanobis", "robust_mahalanobis", "scaled_euclidean", "euclidean"))

      distance.mat <- switch(
        distance.type,
        "mahalanobis" = MatchIt::mahalanobis_dist(formula, data),
        "scaled_euclidean" = MatchIt::scaled_euclidean_dist(formula, data),
        "euclidean" = MatchIt::euclidean_dist(formula, data),
        "robust_mahalanobis" = MatchIt::robust_mahalanobis_dist(formula, data)
      )
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
    strata <- NULL
  }
  else if (metric.type == "bin") {
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
    distance.mat <- NULL
  }
  else if (metric.type == "energy") {
    if (is.null(attr(metric, "distance.mat"))) distance.mat <- "scaled"
    else distance.mat <- attr(metric, "distance.mat")

    if (is.character(distance.mat) && length(distance.mat) == 1L) {
      if (verbose) cat("Computing distance matrix...\n")

      new.formula <- update(formula, NULL ~ .)

      distance.type <- match_arg(distance.mat, c("mahalanobis", "robust_mahalanobis", "scaled_euclidean", "euclidean"))

      distance.mat <- switch(
        distance.type,
        "mahalanobis" = MatchIt::mahalanobis_dist(new.formula, data),
        "scaled_euclidean" = MatchIt::scaled_euclidean_dist(new.formula, data),
        "euclidean" = MatchIt::euclidean_dist(new.formula, data),
        "robust_mahalanobis" = MatchIt::robust_mahalanobis_dist(new.formula, data)
      )
    }
    else {
      if (inherits(distance.mat, "dist")) distance.mat <- as.matrix(distance.mat)

      if (!is.matrix(distance.mat) || !all(dim(distance.mat) == length(treat)) ||
          !all(abs(diag(distance.mat)) < 1e-8) || any(distance.mat < 0) ||
          !check_symmetric(unname(distance.mat))) {
        customStop("'distance.mat' must be one of \"mahalanobis\", \"scaled\", or \"euclidean\" or a square, symmetric, N x N distance matrix when metric = \"energy\".",
                   "makeFrontier()")
      }
    }
    strata <- NULL
  }

  if (is.null(ps)) {
    if (is.null(info[["transform"]])) {
      #Shouldn't happen
      if (metric == "dist" && !is.null(distance.type) && distance.type == "mahalanobis") {
        mah.dist <- distance.mat
      }
      else {
        mah.dist <- MatchIt::mahalanobis_dist(formula, data)
      }
    }
    else {
      if (metric == "dist" && !is.null(distance.type) && distance.type == info[["transform"]]) {
        #If distance.mat is same as matching distance, just reuse it
        mah.dist <- distance.mat
      }
      else {
        mah.dist <- switch(
          info[["transform"]],
          "mahalanobis" = MatchIt::mahalanobis_dist(formula, data),
          "scaled_euclidean" = MatchIt::scaled_euclidean_dist(formula, data),
          "euclidean" = MatchIt::euclidean_dist(formula, data),
          "robust_mahalanobis" = MatchIt::robust_mahalanobis_dist(formula, data)
        )
      }
    }

    ind <- numeric(length(treat))
    for (i in 0:1) ind[treat == i] <- seq_len(sum(treat == i))
    distance <- mah.dist[cbind(ind[matched], ind[matched.to[matched, 1]])]
    attr(distance, "type") <- "mahalanobis"
  }
  else {
    distance <- abs(ps[matched] - ps[matched.to[matched, 1]])
    attr(distance, "type") <- "ps"
  }

  if (verbose) cat("Calculating frontier...\n")

  if (QOI == "FSATE") {
    # frontier <- matchitToFrontierFSATE(distance, matched, matched.to, metric, treat, distance.mat = distance.mat,
    #                                    binnings = binnings)
  }
  else if (QOI == "FSATT") {
    frontier <- matchitToFrontierFSATT(distance, matched, matched.to, metric, treat, distance.mat = distance.mat,
                                       strata = strata, verbose = verbose)
  }

  frontier$distance <- distance

  if (verbose) cat("Done!\n")

  return(frontier)
}

matchitToFrontierFSATT <- function(distance, matched, matched.to, metric, treat.vec, distance.mat = NULL,
                                   strata = NULL, verbose) {

  treated.ind <- which(treat.vec == 1)
  control.ind <- which(treat.vec == 0)

  N <- length(treat.vec)
  N1 <- length(treated.ind)
  N0 <- length(control.ind)
  Nm <- Nm_ <- length(matched)

  inds <- seq_len(length(matched))[-(1:10)]

  if (verbose) {
    pb <- pbapply::startpb(min = 0, max = length(inds))
  }


  #Order (rank) distances to closest matches
  # ranks <- rank(distance, ties.method = "min", na.last = NA)
  ranks <- order(distance)

  #Create list of drop order, starting with dropping no one
  #No ties allowed
  drop.order <- c(list(integer(0)), as.list(matched[ranks[rev(inds)]]))

  #Empty entries to remove (except no drop) (no will be dropped)
  empty <- c(which(lengths(drop.order) == 0)[-1])

  drop.order[empty] <- NULL

  #Xs: how many have been removed at each step
  Xs <- cumsum(lengths(drop.order))

  #Compute imbalance metric across frontier
  Ys <- numeric(length(Xs))

  Y.origin <- NULL

  metric.type <- attr(metric, "type")
  if (metric.type == "dist") {
    #Get Mahalanobis distances between PS-matched pairs
    ind_ <- cbind(match(matched, treated.ind),
                  match(matched.to[matched, 1], control.ind))

    imbalance.distances.full <- numeric(N)
    imbalance.distances.full[matched] <- distance.mat[ind_]

    Ys[1] <- sum(imbalance.distances.full[matched])
    for (i in seq_along(Ys)[-1]) {
      Ys[i] <- Ys[i-1] - sum(imbalance.distances.full[drop.order[[i]]])
      if (verbose) {
        pbapply::setpb(pb, Xs[i])
      }
    }
    Ys <- Ys / (Nm_ - Xs)

  }
  else if (metric.type == "bin") {
    if (startsWith(metric, "l1")) {
      Lstat <- function(diffs) .5*sum(abs(diffs))
    }
    else if (startsWith(metric, "l2")) {
      Lstat <- function(diffs) .5*sqrt(sum(diffs^2))
    }

    #L-stat before matching
    strataholder <- lapply(unique(strata), function(s) which(strata == s))
    diffs <- get.diffs(strataholder, treat.vec, N1, N0)
    Y.origin <- Lstat(diffs)

    #L-stat after matching
    matched.ind <- unname(c(matched, matched.to[matched, 1]))
    strataholder <- lapply(unique(strata[matched.ind]), function(s) matched.ind[strata[matched.ind] == s])
    diffs <- get.diffs(strataholder, treat.vec, Nm, Nm)
    Ys[1] <- Lstat(diffs)

    for (i in seq_along(Ys)[-1]) {
      drop.inds <- drop.order[[i]]

      drop.inds.matched <- c(drop.inds, matched.to[drop.inds, 1])

      #Remove instances of drop.ind.matched
      for (j in drop.inds.matched) {
        strata_with_j <- which(vapply(strataholder, function(s) any(s == j), logical(1L)))[1]
        strataholder[[strata_with_j]] <- strataholder[[strata_with_j]][-which(strataholder[[strata_with_j]] == j)[1]]
      }

      Nm <- Nm_ - Xs[i]

      if (verbose) {
        pbapply::setpb(pb, Xs[i])
      }

      diffs <- get.diffs(strataholder, treat.vec, Nm, Nm)
      Ys[i] <- Lstat(diffs)
    }
  }
  else if (metric.type == "energy") {

    #Energy distance before any matching
    d10 <- distance.mat[treated.ind, control.ind, drop = FALSE]
    Sd10 <- sum(d10)

    d11 <- distance.mat[treated.ind, treated.ind, drop = FALSE]
    Sd11 <- sum(d11)

    d00 <- distance.mat[control.ind, control.ind, drop = FALSE]
    Sd00 <- sum(d00)

    Y.origin <- 2*Sd10/(N1*N0) - Sd11/(N1*N1) - Sd00/(N0*N0)

    #Energy distance after matching
    treated.ind.matched <- matched
    control.ind.matched <- matched.to[matched, 1]

    d10 <- distance.mat[treated.ind.matched, control.ind.matched, drop = FALSE]
    Sd10 <- sum(d10)

    d11 <- distance.mat[treated.ind.matched, treated.ind.matched, drop = FALSE]
    Sd11 <- sum(d11)

    d00 <- distance.mat[control.ind.matched, control.ind.matched, drop = FALSE]
    Sd00 <- sum(d00)

    Ys[1] <- (2*Sd10 - Sd11 - Sd00)/(Nm*Nm)

    for (i in seq_along(Ys)[-1]) {

      drop.inds.matched <- match(drop.order[[i]], treated.ind.matched)

      treated.ind.to.drop <- treated.ind.matched[drop.inds.matched]
      control.ind.to.drop <- control.ind.matched[drop.inds.matched]

      #Remove those units and decrease

      treated.ind.matched <- treated.ind.matched[-drop.inds.matched]
      control.ind.matched <- control.ind.matched[-drop.inds.matched]

      Nm <- length(treated.ind.matched)

      if (verbose) {
        pbapply::setpb(pb, Xs[i])
      }

      #Compute new edist with dropped units removed
      Sd10 <- Sd10 - sum(distance.mat[treated.ind.to.drop, control.ind.matched]) -
        sum(distance.mat[control.ind.to.drop, treated.ind.matched]) -
        sum(distance.mat[treated.ind.to.drop, control.ind.to.drop])

      Sd11 <- Sd11 - 2*sum(distance.mat[treated.ind.to.drop, treated.ind.matched]) -
        sum(distance.mat[treated.ind.to.drop, treated.ind.to.drop])

      Sd00 <- Sd00 - 2*sum(distance.mat[control.ind.to.drop, control.ind.matched]) -
        sum(distance.mat[control.ind.to.drop, control.ind.to.drop])

      Ys[i] <- (2*Sd10 - Sd11 - Sd00)/(Nm*Nm)
    }
  }

  if (verbose) {
    pbapply::setpb(pb, Nm_)
    pbapply::closepb(pb)
  }

  return(list(drop.order = drop.order, Xs = Xs, Ys = Ys, matched.to = matched.to,
              Y.origin = Y.origin))
}

checkArgs.matchit <- function(QOI, metric, matchit, outcome = NULL, ...){

  if (isTRUE(matchit$info$method == "matchingFrontier")) {
    customStop("a frontier cannot be computed on a matchit object resulting from frontier_to_matchit().", 'makeFrontier()')
  }
  if (is.null(matchit$distance) && isTRUE(matchit$info$distance_is_matrix)) {
    customStop("the distance measure used to match cannot have been supplied to matchit() as a matrix.", 'makeFrontier()')
  }
  if (is.null(matchit$distance) && is.null(matchit$info$transform)) {
    customStop("a frontier can only be computed with a matchit object after pair matching on a distance.", 'makeFrontier()')
  }
  if (is.null(matchit$match.matrix) || !is.matrix(matchit$match.matrix) || ncol(matchit$match.matrix) != 1) {
    customStop("a frontier can only be computed with a matchit object after 1:1 pair matching.", 'makeFrontier()')
  }
  if (!is.null(matchit$s.weights) && abs(max(matchit$s.weights) - min(matchit$s.weights)) > sqrt(.Machine$double.eps)) {
    customStop("sampling weights are not supported in the matchit obejct.", 'makeFrontier')
  }
  if (!is.null(matchit$caliper) && "" %in% names(matchit$caliper)) {
    customWarning("a caliper on the propensity score is present in the matchit object; the frontier will begin after applying the caliper.",
                  'makeFrontier()')
  }

  if (!is.null(outcome)) {
    customWarning("the 'outcome' argument is defunct and will be ignored. See ?estimateEffects.", 'makeFrontier()')
  }

}

#Not used
# matchitToFrontierFSATE <- function(distance, matched, matched.to, metric, treat.vec, distance.mat = NULL,
#                                    strataholder = NULL, verbose) {
#   #For when replace = TRUE, and both treated and controls have received a matched (not currently
#   #implemented in MatchIt but might be in the future)
#   treated.ind <- which(treat.vec == 1)
#   control.ind <- which(treat.vec == 0)
#   matched <- which(!is.na(matched.to))
#
#   N <- length(treat.vec)
#   Nm <- length(matched)
#
#   inds <- seq_along(treat.vec)[matched]
#
#   #Order (rank) distances to closest matches
#   ranks <- rank(distance, ties.method = "min", na.last = NA)
#
#   #Create list of drop order, starting with dropping no one
#   #Entries will be empty when there are ties (which are grouped together)
#   drop.order <- c(list(integer(0)), lapply(rev(inds), function(i) inds[ranks == i]))
#
#   #Empty entries to remove (except no drop)
#   empty <- c(which(lengths(drop.order) == 0)[-1], length(matched) + 1)
#
#   #Xs: how many have been removed at each step
#   Xs <- c(0, inds)[-empty]
#
#   drop.order[empty] <- NULL
#
#   #Compute imbalance metric across frontier
#   Ys <- numeric(length(Xs))
#
#   metric.type <- metricType(metric)
#   if (metric.type == "dist") {
#     #Get Mahalanobis distances between PS-matched pairs
#     ind_ <- matrix(nrow = Nm, ncol = 2)
#     matched.treated <- matched %in% treated.ind
#     ind_[matched.treated, ] <- cbind(match(matched[matched.treated], treated.ind),
#                                      match(matched.to[matched.treated], control.ind))
#     ind_[!matched.treated, ] <- cbind(match(matched.to[!matched.treated], treated.ind),
#                                       match(matched[!matched.treated], control.ind))
#
#     imbalance.distances.full <- numeric(N)
#     imbalance.distances.full[matched] <- distance.mat[ind_]
#
#     Ys[1] <- sum(imbalance.distances.full[matched])
#     for (i in seq_along(Ys)[-1]) {
#       Ys[i] <- Ys[i-1] - sum(imbalance.distances.full[drop.order[[i]]])
#     }
#     Ys <- Ys / (Nm - Xs)
#   }
#   else if (metric.type == "bin") {
#     if (startsWith(metric, "l1")) {
#       Lstat <- function(diffs) .5*sum(abs(diffs))
#     }
#     else if (startsWith(metric, "l2")) {
#       Lstat <- function(diffs) .5*sqrt(sum(diffs^2))
#     }
#
#     diffs <- get.diffs(strataholder, treat.vec, Nm, Nm)
#     Ys[1] <- Lstat(diffs)
#
#     for (i in seq_along(Ys)[-1]) {
#       drop.inds <- drop.order[[i]]
#
#       drop.inds.matched <- c(drop.inds, matched.to[drop.inds])
#
#       for (j in drop.inds.matched) {
#         for (k in seq_along(strataholder)) {
#           if (j %in% strataholder[[k]]) {
#             #Remove instance of j in strataholder[[k]]
#             #Note that there may be duplicates of j and one should be dropped per iter
#             strataholder[[k]] <- strataholder[[k]][-which(strataholder[[k]] == j)[1]]
#             break
#           }
#         }
#       }
#
#       Nm_i <- Nm - Xs[i]
#
#       diffs <- get.diffs(strataholder, treat.vec, Nm_i, Nm_i)
#       Ys[i] <- Lstat(diffs)
#     }
#   }
#   else if (metric.type == "energy") {
#
#     treated.ind.matched <- matched
#     control.ind.matched <- matched.to[matched]
#
#     d10 <- distance.mat[treated.ind.matched, control.ind.matched, drop = FALSE]
#     Sd10 <- sum(d10)
#
#     d11 <- distance.mat[treated.ind.matched, treated.ind.matched, drop = FALSE]
#     Sd11 <- sum(d11)
#
#     d00 <- distance.mat[control.ind.matched, control.ind.matched, drop = FALSE]
#     Sd00 <- sum(d00)
#
#     Ys[1] <- (2*Sd10 - Sd11 - Sd00)/(Nm*Nm)
#
#     for (i in seq_along(Ys)[-1]) {
#
#       drop.inds.matched <- match(drop.order[[i]], treated.ind.matched)
#
#       treated.ind.to.drop <- treated.ind.matched[drop.inds.matched]
#       control.ind.to.drop <- control.ind.matched[drop.inds.matched]
#
#       #Remove those units and decrease
#
#       treated.ind.matched <- treated.ind.matched[-drop.inds.matched]
#       control.ind.matched <- control.ind.matched[-drop.inds.matched]
#
#       Nm <- length(treated.ind.matched)
#
#       #Compute new edist with dropped units removed
#       Sd10 <- Sd10 - sum(distance.mat[treated.ind.to.drop, control.ind.matched]) -
#         sum(distance.mat[control.ind.to.drop, treated.ind.matched]) -
#         sum(distance.mat[treated.ind.to.drop, control.ind.to.drop])
#
#       Sd11 <- Sd11 - 2*sum(distance.mat[treated.ind.to.drop, treated.ind.matched]) -
#         sum(distance.mat[treated.ind.to.drop, treated.ind.to.drop])
#
#       Sd00 <- Sd00 - 2*sum(distance.mat[control.ind.to.drop, control.ind.matched]) -
#         sum(distance.mat[control.ind.to.drop, control.ind.to.drop])
#
#       Ys[i] <- (2*Sd10 - Sd11 - Sd00)/(Nm*Nm)
#     }
#   }
#
#   return(list(drop.order = drop.order, Xs = Xs, Ys = Ys, matched.to = matched.to, distance.mat = distance.mat))
# }