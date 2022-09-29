#Find matches for all units, then discard units with greatest distance
distToFrontierFSATE <- function(distance.mat, treat.vec, verbose, ratio = NULL){

  N <- length(treat.vec)

  treated.ind <- which(treat.vec == 1)
  control.ind <- which(treat.vec == 0)

  inds1 <- seq_along(treated.ind)
  inds0 <- seq_along(control.ind)

  if (verbose) {
    pb <- pbapply::startpb(min = 0, max = N)
  }

  if (is.null(ratio)) ratio <- 1
  else if (ratio > min(length(control.ind), length(treated.ind))) {
    customStop("for pair distance-based frontiers, 'ratio' cannot be larger than the size of the smaller group when QOI = 'FSATE'.",
               "makeFrontier()")
  }

  #Find closest matches to treated units (rows) and control units (cols)
  row.mins.inds <- apply(distance.mat, 1, function(x) which.min(x))
  col.mins.inds <- apply(distance.mat, 2, function(x) which.min(x))

  row.mins.inds <- do.call("rbind", lapply(inds1, function(i) {
    which(distance.mat[i,] <= sort(distance.mat[i,], partial = ratio)[ratio])[1:ratio]
  }))
  col.mins.inds <- do.call("rbind", lapply(inds0, function(i) {
    which(distance.mat[,i] <= sort(distance.mat[,i], partial = ratio)[ratio])[1:ratio]
  }))

  #Assign index of who is matched to who
  matched.to <- matrix(NA, nrow = N, ncol = ratio)
  matched.to[treated.ind,] <- control.ind[row.mins.inds]
  matched.to[control.ind,] <- treated.ind[col.mins.inds]

  #Extract distances to closest matches
  min.distances.mat <- matrix(NA_real_, nrow = N, ncol = ratio)
  for (i in 1:ratio) {
    min.distances.mat[treated.ind, i] <- distance.mat[cbind(inds1, row.mins.inds[,i])]
    min.distances.mat[control.ind, i] <- distance.mat[cbind(col.mins.inds[,i], inds0)]
  }

  if (ratio == 1) {
    min.distances <- min.distances.mat[,1]
  }
  else {
    min.distances <- rowMeans(min.distances.mat)
  }

  inds <- seq_along(treat.vec)

  #Order (rank) distances to closest matches
  ranks <- rank(min.distances, ties.method = "min", na.last = NA)

  #Create list of drop order, starting with dropping no one
  #Entries will be empty when there are ties (which are grouped together)
  drop.order <- c(list(integer(0)), lapply(c(rev(inds)), function(i) which(ranks == i)))

  #Empty entries to remove (except no drop)
  empty <- c(which(lengths(drop.order) == 0)[-1], N + 1)

  #Sort distances to closest matches from smallest to largest
  sorted.min.distances <- c(0, rev(min.distances[unlist(drop.order)]))

  #Xs: how many have been removed at each step
  Xs <- c(0, inds)[-empty]

  #Ys: mean of remaining distances at each step
  Ys <- rev(cumsum(sorted.min.distances))[-empty] / (N - Xs)

  drop.order[empty] <- NULL

  if (verbose) {
    pbapply::setpb(pb, N)
    pbapply::closepb(pb)
  }

  # Checks to confirm monotonically decreasing. Since
  # that's theoretically impossible, if the condition is
  # met, there's a serious bug somewhere in the code.
  if(any(diff(Ys) > 0 )){
    stop('Something is very wrong. Email ngreifer@iq.harvard.edu.')
  }
  return(list(drop.order = drop.order, Xs = Xs, Ys = Ys, matched.to = matched.to))
}

