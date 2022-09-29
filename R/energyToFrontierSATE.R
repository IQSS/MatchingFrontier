energyToFrontierSATE <- function(distance.mat, treat.vec, verbose, ratio = NULL) {
  #Energy distance formula:
  #2/n1n0 sum(D[t,c]) - 1/n1n1 sum(D[t,t]) - 1/n0n0 sum(D[c,c])

  N <- length(treat.vec)

  ind <- seq_along(treat.vec)
  treated.ind <- ind[treat.vec == 1]
  control.ind <- ind[treat.vec == 0]

  #Compute each unit's contribution to energy dist
  #Find unit that contributes maximally to energy distance
  #Remove that unit

  drop.order <- vector("list", N)
  Ys <- numeric(N)
  Xs <- integer(N)

  N1 <- length(treated.ind)
  N0 <- length(control.ind)

  if (verbose) {
    pb <- pbapply::startpb(min = 0, max = N)
  }

  Sdff <- sum(distance.mat)

  df1 <- distance.mat[,treated.ind, drop = FALSE]
  cSdf1 <- col_sums(df1)
  Sdf1 <- sum(cSdf1)

  df0 <- distance.mat[,control.ind, drop = FALSE]
  cSdf0 <- col_sums(df0)
  Sdf0 <- Sdff - Sdf1

  d10 <- df0[treated.ind,, drop = FALSE]
  rSd10 <- row_sums(d10)
  cSd10 <- col_sums(d10)
  Sd10 <- sum(rSd10)

  d11 <- df1[treated.ind,,drop = FALSE]
  cSd11 <- col_sums(d11)
  Sd11 <- sum(cSd11)
  d11i <- seq_len(ncol(d11))

  d00 <- df0[control.ind,, drop = FALSE]
  cSd00 <- col_sums(d00)
  Sd00 <- sum(cSd00)
  d00i <- seq_len(ncol(d00))

  Ys[1] <- 2*Sd10/(N1*N0) - Sd11/(N1*N1) - Sd00/(N0*N0) +
    2*Sdf1/(N1*N) - Sd11/(N1*N1) - Sdff/(N*N) +
    2*Sdf0/(N0*N) - Sd00/(N0*N0) - Sdff/(N*N)

  min.edist <- Ys[1]

  #Difference between edist and edist with each unit
  treated.contributions <- (2/(N1*N0) - 2/((N1-1)*N0))*Sd10 +
    (-1/(N1^2) + 1/((N1-1)^2))*Sd11 +
    (2/((N1-1)*N0))*rSd10 -
    (2/((N1-1)^2))*cSd11 +
    (2/(N1*N) - 2/((N1-1)*N))*Sdf1 +
    (-1/(N1^2) + 1/((N1-1)^2))*Sd11 +
    (2/((N1-1)*N))*cSdf1 -
    (2/((N1-1)^2))*cSd11
  control.contributions <- (2/(N0*N1) - 2/((N0-1)*N1))*Sd10 +
    (-1/(N0^2) + 1/((N0-1)^2))*Sd00 +
    (2/((N0-1)*N1))*cSd10 -
    (2/((N0-1)^2))*cSd00 +
    (2/(N0*N) - 2/((N0-1)*N))*Sdf0 +
    (-1/(N0^2) + 1/((N0-1)^2))*Sd00 +
    (2/((N0-1)*N))*cSdf0 -
    (2/((N0-1)^2))*cSd00

  for (k in seq_len(N)[-1]) {

    #Find which units have the largest contribution to the energy distance
    if (!is.null(ratio)) {
      if (N0 <= N1 * ratio) {
        largest.contribution <- max(treated.contributions)
        drop.treated <- which.min(abs(treated.contributions - largest.contribution))
        drop.control <- integer(0L)
      }
      else {
        largest.contribution <- max(control.contributions)
        drop.treated <- integer(0L)
        drop.control <- which.min(abs(control.contributions - largest.contribution))
      }
    }
    else {
      largest.contribution <- max(max(treated.contributions), max(control.contributions))

      drop.treated <- which(abs(treated.contributions - largest.contribution) < 1e-9)
      drop.control <- which(abs(control.contributions - largest.contribution) < 1e-9)
    }

    treated.ind.to.drop <- treated.ind[drop.treated]
    control.ind.to.drop <- control.ind[drop.control]

    #Remove those units and decrease
    if (length(drop.treated) > 0) {
      treated.ind <- treated.ind[-drop.treated]
      N1 <- length(treated.ind)
    }
    if (length(drop.control) > 0) {
      control.ind <- control.ind[-drop.control]
      N0 <- length(control.ind)
    }

    #If removed units are last units, don't remove and stop
    if (N1-1 <= 0 || N0-1 <= 0) break

    if (verbose) {
      pbapply::setpb(pb, N - (N0 + N1))
    }

    #Compute new edist with dropped units removed
    if (length(drop.treated) > 0) {
      d11i_treated.dropped <- d11i[drop.treated]
      d11i_treated.kept <- d11i[-drop.treated]
    }
    else {
      d11i_treated.dropped <- integer(0L)
      d11i_treated.kept <- d11i
    }
    if (length(drop.control) > 0) {
      d00i_control.dropped <- d00i[drop.control]
      d00i_control.kept <- d00i[-drop.control]
    }
    else {
      d00i_control.dropped <- integer(0L)
      d00i_control.kept <- d00i
    }

    #d10 contributions
    if (length(drop.treated) > 0 && length(drop.control) > 0) {
      Sd10 <- Sd10 - sum(d10[d11i_treated.dropped, d00i_control.dropped]) -
        sum(d10[d11i_treated.dropped, d00i_control.kept]) -
        sum(d10[d11i_treated.kept, d00i_control.dropped])

      rSd10 <- rSd10[-drop.treated] - row_sums(d10[d11i_treated.kept, d00i_control.dropped, drop = FALSE])
      cSd10 <- cSd10[-drop.control] - col_sums(d10[d11i_treated.dropped, d00i_control.kept, drop = FALSE])
    }
    else if (length(drop.treated) > 0) {
      Sd10 <- Sd10 - sum(d10[d11i_treated.dropped, d00i_control.kept])

      rSd10 <- rSd10[-drop.treated]
      cSd10 <- cSd10 - col_sums(d10[d11i_treated.dropped, d00i_control.kept, drop = FALSE])
    }
    else if (length(drop.control) > 0) {
      Sd10 <- Sd10 - sum(d10[d11i_treated.kept, d00i_control.dropped])
      rSd10 <- rSd10 - row_sums(d10[d11i_treated.kept, d00i_control.dropped, drop = FALSE])
      cSd10 <- cSd10[-drop.control]
    }

    #d11 and df1 contributions
    if (length(drop.treated) > 0) {
      Sd11 <- Sd11 - 2*sum(d11[d11i_treated.kept, d11i_treated.dropped]) -
        sum(d11[d11i_treated.dropped, d11i_treated.dropped])
      cSd11 <- cSd11[-drop.treated] - col_sums(d11[d11i_treated.dropped, d11i_treated.kept, drop = FALSE])
      d11i <- d11i_treated.kept

      Sdf1 <- Sdf1 - sum(df1[, d11i_treated.dropped])
      cSdf1 <- cSdf1[-drop.treated]
    }

    #d00 and df0 contributions
    if (length(drop.control) > 0) {
      Sd00 <- Sd00 - 2*sum(d00[d00i_control.kept, d00i_control.dropped]) -
        sum(d00[d00i_control.dropped, d00i_control.dropped])
      cSd00 <- cSd00[-drop.control] - col_sums(d00[d00i_control.dropped, d00i_control.kept, drop = FALSE])
      d00i <- d00i_control.kept

      Sdf0 <- Sdf0 - sum(df0[, d00i_control.dropped])
      cSdf0 <- cSdf0[-drop.control]
    }

    edist <- 2*Sd10/(N1*N0) - Sd11/(N1*N1) - Sd00/(N0*N0) +
      2*Sdf1/(N1*N) - Sd11/(N1*N1) - Sdff/(N*N) +
      2*Sdf0/(N0*N) - Sd00/(N0*N0) - Sdff/(N*N)

    #After passing sample size threshold of 90% of original N, stop if
    #new edist is larger than smallest edist
    if (edist < min.edist) min.edist <- edist
    else if ((N1+N0)/N < .9 && edist-min.edist > .2*(Ys[1]-min.edist)) break

    #Record new edist and units dropped
    Ys[k] <- edist
    drop.order[[k]] <- sort(c(treated.ind.to.drop, control.ind.to.drop))

    treated.contributions <- (2/(N1*N0) - 2/((N1-1)*N0))*Sd10 +
      (-1/(N1^2) + 1/((N1-1)^2))*Sd11 +
      (2/((N1-1)*N0))*rSd10 -
      (2/((N1-1)^2))*cSd11 +
      (2/(N1*N) - 2/((N1-1)*N))*Sdf1 +
      (-1/(N1^2) + 1/((N1-1)^2))*Sd11 +
      (2/((N1-1)*N))*cSdf1 -
      (2/((N1-1)^2))*cSd11
    control.contributions <- (2/(N0*N1) - 2/((N0-1)*N1))*Sd10 +
      (-1/(N0^2) + 1/((N0-1)^2))*Sd00 +
      (2/((N0-1)*N1))*cSd10 -
      (2/((N0-1)^2))*cSd00 +
      (2/(N0*N) - 2/((N0-1)*N))*Sdf0 +
      (-1/(N0^2) + 1/((N0-1)^2))*Sd00 +
      (2/((N0-1)*N))*cSdf0 -
      (2/((N0-1)^2))*cSd00
  }

  Ys <- Ys[seq_len(k-1)]
  drop.order <- drop.order[seq_len(k-1)]
  Xs <- cumsum(lengths(drop.order))

  if (verbose) {
    pbapply::setpb(pb, N)
    pbapply::closepb(pb)
  }

  return(list(drop.order = drop.order, Xs = Xs, Ys = Ys, Y.origin = Ys[1]))
}
