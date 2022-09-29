binsToFrontierSATT <- function(strata, treat.vec, metric = "l1", verbose, ratio = NULL){

  N1 <- sum(treat.vec == 1)
  N0 <- N0_ <- sum(treat.vec == 0)
  N <- length(treat.vec)

  drop.order <- vector("list", N0)
  Ys <- numeric(N0)

  if (startsWith(metric, "l1")) {
    Lstat <- function(diffs) .5*sum(abs(diffs))
  }
  else if (startsWith(metric, "l2")) {
    Lstat <- function(diffs) .5*sqrt(sum(diffs^2))
  }

  if (verbose) {
    pb <- pbapply::startpb(min = 0, max = N0)
  }

  strataholder <- lapply(unique(strata), function(s) which(strata == s))

  treated.counts <- vapply(strataholder, function(x) {
    sum(treat.vec[x] == 1)
  }, numeric(1L))

  control.counts <- lengths(strataholder) - treated.counts

  treated.props <- treated.counts/N1
  control.props <- control.counts/N0

  diffs <- treated.props - control.props
  Ys[1] <- Lstat(diffs)

  min.Lstat <- Ys[1]

  k <- 1
  repeat {
    k <- k + 1

    altered.control.props <- control.counts/(N0 - 1)

    altered.control.diffs <- altered.control.props - treated.props

    best.control.to.drop <- which.max(altered.control.diffs)

    altered.control.diffs[best.control.to.drop] <- altered.control.diffs[best.control.to.drop] - 1/(N0 - 1)

    L.best.control.to.drop <- Lstat(altered.control.diffs)

    drop.group <- 0
    drop.from <- best.control.to.drop

    dropped.element.ind <- which(treat.vec[strataholder[[drop.from]]] == drop.group)[1]

    drop <- strataholder[[drop.from]][dropped.element.ind]
    strataholder[[drop.from]] <- strataholder[[drop.from]][-dropped.element.ind]

    N0 <- N0 - 1
    control.counts[drop.from] <- control.counts[drop.from] - 1
    control.props <- control.counts/N0
    new.Lstat <- L.best.control.to.drop

    if (N0 == 1) break

    if (verbose) {
      pbapply::setpb(pb, N0_ - N0)
    }

    if (new.Lstat < min.Lstat) min.Lstat <- new.Lstat
    else if (N0 < .9*(N-N1) && new.Lstat - min.Lstat > .3*(Ys[1] - min.Lstat)) break

    Ys[k] <- new.Lstat
    drop.order[[k]] <- drop

    if (new.Lstat < 1e-9) break
  }

  keep <- c(1L, which(lengths(drop.order) > 0))
  drop.order[-keep] <- NULL
  Ys <- Ys[keep]

  Xs <- cumsum(lengths(drop.order))

  if (verbose) {
    pbapply::setpb(pb, N)
    pbapply::closepb(pb)
  }

  return(list(drop.order = drop.order, Xs = Xs, Ys = Ys, Y.origin = Ys[1]))
}
