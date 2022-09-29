#Remove control units to lower between-group energy distance
energyToFrontierSATT <- function(distance.mat, treat.vec, verbose, ratio = NA) {
  #Energy distance formula:
  #2/n1n0 sum(D[t,c]) - 1/n1n1 sum(D[t,t]) - 1/n0n0 sum(D[c,c])

  N <- length(treat.vec)

  ind <- seq_along(treat.vec)
  treated.ind <- ind[treat.vec == 1]
  control.ind <- ind[treat.vec == 0]

  ind.in.vec <- integer(N)
  ind.in.vec[treated.ind] <- seq_along(treated.ind)
  ind.in.vec[control.ind] <- seq_along(control.ind)

  #Compute each unit's contribution to energy dist
  #Find unit that contributes maximally to energy distance
  #Remove that unit

  drop.order <- vector("list", N)
  Ys <- numeric(N)
  Xs <- integer(N)

  N1 <- length(treated.ind)
  N0 <- N0_ <- length(control.ind)

  if (verbose) {
    pb <- pbapply::startpb(min = 0, max = N0)
  }

  d10 <- distance.mat[treated.ind, control.ind, drop = FALSE]
  cSd10 <- col_sums(d10)
  Sd10 <- sum(cSd10)

  d11 <- distance.mat[treated.ind, treated.ind, drop = FALSE]
  Sd11 <- sum(d11)

  d00 <- distance.mat[control.ind, control.ind, drop = FALSE]
  cSd00 <- col_sums(d00)
  Sd00 <- sum(cSd00)
  d00i <- seq_len(ncol(d00))

  Ys[1] <- 2*Sd10/(N1*N0) - Sd11/(N1*N1) - Sd00/(N0*N0)

  min.edist <- Ys[1]

  control.contributions <- (2/(N0*N1) - 2/((N0-1)*N1))*Sd10 +
    (-1/(N0^2) + 1/((N0-1)^2))*Sd00 +
    (2/((N0-1)*N1))*cSd10 -
    (2/((N0-1)^2))*cSd00

  for (k in seq_len(N0)[-1]) {

    #Find which units have the largest contribution to the energy distance
    largest.contribution <- max(control.contributions)

    drop.control <- which(abs(control.contributions - largest.contribution) < 1e-9)

    control.ind.to.drop <- control.ind[drop.control]

    #Remove those units and decrease
    control.ind <- control.ind[-drop.control]
    N0 <- length(control.ind)

    #If removed units are last units, don't remove and stop
    if (N0-1 <= 0) break

    if (verbose) {
      pbapply::setpb(pb, N0_ - N0)
    }

    #Compute new edist with dropped units removed

    d00i_control.dropped <- d00i[drop.control]
    d00i_control.kept <- d00i[-drop.control]

    #d10 contributions
    Sd10 <- Sd10 - sum(d10[, d00i_control.dropped])
    cSd10 <- cSd10[-drop.control]

    #d00 contributions
    Sd00 <- Sd00 - 2*sum(d00[d00i_control.kept, d00i_control.dropped]) -
      sum(d00[d00i_control.dropped, d00i_control.dropped])
    cSd00 <- cSd00[-drop.control] - col_sums(d00[d00i_control.dropped, d00i_control.kept, drop = FALSE])
    d00i <- d00i_control.kept

    edist <- 2*Sd10/(N1*N0) - Sd11/(N1*N1) - Sd00/(N0*N0)

    #After passing sample size threshold of 90% of original N, stop if
    #new edist is larger than smallest edist
    if (edist < min.edist) min.edist <- edist
    else if (N0/N0_ < .9 && edist-min.edist > .2*(Ys[1]-min.edist)) break

    #Record new edist and units dropped
    Ys[k] <- edist
    drop.order[[k]] <- sort(control.ind.to.drop)

    #Compute unit contributions after having dropped units
    control.contributions <- (2/(N0*N1) - 2/((N0-1)*N1))*Sd10 +
      (-1/(N0^2) + 1/((N0-1)^2))*Sd00 +
      (2/((N0-1)*N1))*cSd10 -
      (2/((N0-1)^2))*cSd00
  }

  Ys <- Ys[seq_len(k-1)]
  drop.order <- drop.order[seq_len(k-1)]
  Xs <- cumsum(lengths(drop.order))

  if (verbose) {
    pbapply::setpb(pb, N0_)
    pbapply::closepb(pb)
  }

  return(list(drop.order = drop.order, Xs = Xs, Ys = Ys, Y.origin = Ys[1]))
}
