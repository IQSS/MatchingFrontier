summary.frontierEstimates <- function(object, N, Ndrop, ...) {
  if (!inherits(object, "frontierEstimates")) {
    customStop("'object' must be a matchFrontier object, the output of a call to makeFrontier().",
               "summary()")
  }
  if (missing(N) && missing(Ndrop)) {
    N <- c(object$n - object$Xs[1],
           round(median(object$n - object$Xs)),
           object$n - object$Xs[length(object$Xs)])
  }
  else if (!missing(N) && !missing(Ndrop)) {
    customStop("only one of 'N' or 'Ndrop' may be specified.", "summary()")
  }
  else if (!missing(Ndrop)) {
    if (!is.numeric(Ndrop) ||
        any(Ndrop < min(object$Xs)) || any(Ndrop >  max(object$Xs))) {
      customStop(sprintf("'Ndrop' must be between %s and %s.",
                         min(object$Xs), max(object$Xs)), "summary()")
    }
    N <- object$n - Ndrop
  }
  else if (!missing(N)) {
    if (!is.numeric(N) ||
        any(N < min(object$n - object$Xs)) || any(N > max(object$n - object$Xs))) {
      customStop(sprintf("'N' must be between %s and %s.",
                         min(object$n - object$Xs), max(object$n - object$Xs)),
                 "summary()")
    }
  }

  ord <- order(N, decreasing = TRUE)
  N <- N[ord]

  Ns <- object$n - object$Xs
  for (i in seq_along(N)) {
    if (N[i] != object$n) {
      ind <- which.min(abs(Ns - N[i]))
      N[i] <- Ns[ind]
    }
  }
  N <- unique(N)

  selectedEst <- numeric(length(N))
  selectedCIlower <- numeric(length(N))
  selectedCIupper <- numeric(length(N))
  selectedMDlower <- numeric(length(N))
  selectedMDupper <- numeric(length(N))

  for (i in seq_along(N)) {
    if (N[i] == object$n) {
      selectedEst[i] <- object$un$coef
      selectedCIlower[i] <- object$un$CI[1]
      selectedCIupper[i] <- object$un$CI[2]
      if (length(object$un$mod.dependence) != 0) {
        selectedMDlower[i] <- object$un$mod.dependence[1]
        selectedMDupper[i] <- object$un$mod.dependence[2]
      }
    }
    else {
      ind <- which(Ns == N[i])

      selectedEst[i] <- object$coefs[ind]
      selectedCIlower[i] <- object$CIs[[ind]][1]
      selectedCIupper[i] <- object$CIs[[ind]][2]

      if (length(object$mod.dependence) != 0) {
        selectedMDlower[i] <- object$mod.dependence[[ind]][1]
        selectedMDupper[i] <- object$mod.dependence[[ind]][2]
      }
    }
  }

  names(selectedEst) <- if (missing(Ndrop)) paste("at N =", N) else paste("at Ndrop =", object$n - N)
  names(selectedCIlower) <- names(selectedCIupper) <- names(selectedEst)

  out <- list(
    Est = selectedEst,
    CIlower = selectedCIlower,
    CIupper = selectedCIupper,
    CIlevel = names(object$CIs[[1]]))

  if (length(object$mod.dependence) != 0) {
    out$MDlower <- selectedMDlower
    out$MDupper <- selectedMDupper
  }

  class(out) <- "summary.frontierEstimates"
  return(out)
}

print.summary.frontierEstimates <- function(x, digits = 3, ...) {
  cat("Summary of effectEstimates object:\n\n")

  s <- matrix(c(format(x[["Est"]], digits = digits, scientific = FALSE),
                format(c(x[["CIlower"]], x[["CIupper"]]), digits = digits, scientific = FALSE)),
              nrow = length(x[["Est"]]), ncol = 3)

  colnames(s) <- c("Est", paste("CI", x$CIlevel[1]), paste("CI", x$CIlevel[2]))

  if (length(x$MDlower) != 0) {
    s2 <- matrix(format(c(x[["MDlower"]], x[["MDupper"]]), digits = digits, scientific = FALSE),
                 nrow = length(x[["Est"]]), ncol = 2)
    colnames(s2) <- c("MD lower", "MD upper")

    s <- cbind(s, s2)
  }

  rownames(s) <- c(firstup(names(x[["Est"]])))

  print.data.frame(as.data.frame(s))

  invisible(x)
}