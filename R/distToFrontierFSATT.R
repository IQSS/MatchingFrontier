distToFrontierFSATT <- function(distance.mat, treat.vec, verbose, ratio = NULL) {

  treated.ind <- which(treat.vec == 1)
  control.ind <- which(treat.vec == 0)

  N <- length(treat.vec)
  N1 <- length(treated.ind)
  inds1 <- seq_len(N1)

  if (is.null(ratio)) ratio <- 1
  else if (ratio > length(control.ind)) {
    customStop("for pair distance-based frontiers, 'ratio' cannot be larger than the number of control units when QOI = 'FSATT'.",
               "makeFrontier()")
  }

  if (verbose) {
    pb <- pbapply::startpb(min = 0, max = N1)
  }

  #Find closest matches to treated units (rows)
  row.mins.inds <- do.call("rbind", lapply(inds1, function(i) {
    which(distance.mat[i,] <= sort(distance.mat[i,], partial = ratio)[ratio])[1:ratio]
  }))

  #Assign index of who is matched to who
  matched.to <- control.ind[row.mins.inds]

  #Extract distances to closest matches
  min.distances.mat <- matrix(NA_real_, nrow = N1, ncol = ratio)
  for (i in 1:ratio) {
    min.distances.mat[,i] <- distance.mat[cbind(inds1, row.mins.inds[,i])]
  }

  if (ratio == 1) {
    min.distances <- min.distances.mat[,1]
  }
  else {
    min.distances <- rowMeans(min.distances.mat)
  }

  min.distances.full <- rep(NA, N)
  min.distances.full[treated.ind] <- min.distances

  #Order (rank) distances to closest matches
  ranks <- rank(min.distances, ties.method = "min", na.last = NA)

  #Create list of drop order, starting with dropping no one
  #Entries will be empty when there are ties (which are grouped together)
  drop.order <- c(list(integer(0)), lapply(rev(inds1), function(i) treated.ind[ranks == i]))

  #Empty entries to remove (except no drop)
  empty <- c(which(lengths(drop.order) == 0)[-1], N1 + 1)

  #Sort distances to closest matches from smallest to largest
  sorted.min.distances <- c(0, rev(min.distances.full[unlist(drop.order)]))

  #Xs: how many have been removed at each step
  Xs <- c(0, inds1)[-empty]

  #Ys: mean of remaining distances at each step
  Ys <- rev(cumsum(sorted.min.distances))[-empty] / (N1 - Xs)

  drop.order[empty] <- NULL

  matched.to.full <- matrix(NA_integer_, nrow = N, ncol = ratio)
  matched.to.full[treated.ind,] <- matched.to

  if (verbose) {
    pbapply::setpb(pb, N1)
    pbapply::closepb(pb)
  }

  # Checks to confirm monotonically decreasing. Since
  # that's theoretically impossible, if the condition is
  # met, there's a serious bug somewhere in the code.
  if (any(diff(Ys) > 0 )){
    stop('Something is very wrong. Email ngreifer@iq.harvard.edu.')
  }
  return(list(drop.order = drop.order, Xs = Xs, Ys = Ys, matched.to = matched.to.full))
}

