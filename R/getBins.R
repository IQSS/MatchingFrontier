getBins <- function(data, match.on, breaks = NULL){

  # Remove dropped covs
  data <- data[match.on]
  for (i in seq_len(ncol(data))) {
    if (is.character(data[[i]])) data[[i]] <- factor(data[[i]])
    if (is.factor(data[[i]])) data[[i]] <- as.numeric(data[[i]])
  }

  #Process breaks; adapted from MatchIt:::cem_matchit()
  if (is.null(breaks)) breaks <- "sturges"

  if (!is.list(breaks)) {
    if (length(breaks) == 1 && (is.null(names(breaks)) || !any(match.on == names(breaks)))) {
      breaks <- setNames(rep(breaks, length(match.on)), match.on)
    }
    breaks <- as.list(breaks)
  }

  if (is.null(names(breaks))) customStop("'breaks' must be a named list of binning values with an element for each numeric variable.", "makeFrontier()")
  bad.names <- setdiff(names(breaks), match.on)
  nb <- length(bad.names)
  if (nb > 0) {
    breaks[bad.names] <- NULL
  }

  bad.breaks <- setNames(rep(FALSE, length(breaks)), names(breaks))
  for (i in names(breaks)) {
    if (length(breaks[[i]]) == 0) {
      breaks[[i]] <- "sturges"
    }
    else if (length(breaks[[i]]) == 1) {
      if (is.character(breaks[[i]])) {

        bad.breaks[i] <- is.na(pmatch(breaks[[i]], c("sturges", "fd", "scott")))
      }
      else if (is.numeric(breaks[[i]])) {
        if (!is.finite(breaks[[i]]) || breaks[[i]] < 0) bad.breaks[i] <- TRUE
        if (breaks[[i]] %in% 0:1) match.on <- setdiff(match.on, i) #Will not be binned
      }
    }
    else {
      bad.breaks[i] <- TRUE
    }
  }
  if (any(bad.breaks)) {
    customStop("each entry in the list supplied to 'breaks' must be a string containing the name of an allowable binning method or a single number corresponding to the number of bins",
               "makeFrontier()")
  }

  #Create bins for numeric variables
  bin.list <- setNames(vector("list", length(match.on)), match.on)
  for (i in match.on) {
    if (!i %in% names(breaks)) {
      if (is.character(data[[i]]) || is.factor(data[[i]])) next
      else breaks[[i]] <- "sturges"
    }

    if (is.character(breaks[[i]])) {
      breaks[[i]] <- match_arg(tolower(breaks[[i]]), c("sturges", "fd", "scott"))
      breaks[[i]] <- switch(breaks[[i]],
                            sturges = nclass.Sturges(data[[i]]),
                            fd = nclass.FD(data[[i]]),
                            scott = nclass.scott(data[[i]]))
      #Breaks is now a single number
    }

    #breaks is number of bins
    bin.list[[i]] <- seq(min(data[[i]]), max(data[[i]]), length = breaks[[i]] + 1)
    bin.list[[i]][c(1, length(bin.list[[i]]))] <- c(-Inf, Inf)

  }

  return(bin.list)
}

assignToBins <- function(data, match.on, bins.list, subset = NULL) {
  if (is.null(subset)) subset <- seq_len(nrow(data))

  data <- data[subset, match.on]

  for (i in match.on) {
    if (!is.character(data[[i]]) && !is.factor(data[[i]]))
      data[[i]] <- findInterval(data[[i]], bins.list[[i]])
  }

  strata <- do.call("paste", c(data, list(sep = "|")))

  return(strata)
}

get.diffs <- function(strataholder, treat.vec, num.treated, num.control){
  #Diffs positive when control proportion larger than treated proportion
  unlist(lapply(strataholder, function(x)
    sum(treat.vec[x] == 0) / num.control - sum(treat.vec[x] == 1) / num.treated))
}

getBinsAtMedian <- function(data, match.on, treat.vec, metric, verbose = TRUE){
  if (startsWith(metric, "l1")) {
    Lstat <- function(diffs) .5*sum(abs(diffs))
  }
  else if (startsWith(metric, "l2")) {
    Lstat <- function(diffs) .5*sqrt(sum(diffs^2))
  }

  num.treated <- sum(treat.vec == 1)
  num.control <- sum(treat.vec == 0)

  n.coarsenings <- 1001

  data <- data[match.on]
  for (i in seq_len(ncol(data))) {
    if (is.character(data[[i]])) data[[i]] <- factor(data[[i]])
    data[[i]] <- as.numeric(data[[i]])
  }

  nunique.covs <- vapply(match.on, function(i) length(unique(data[[i]])), integer(1L))

  bin.lists <- lapply(seq_len(n.coarsenings), function(i) {
    breaks <- setNames(lapply(nunique.covs, function(nu) {
      sample(seq(min(2, nu), min(12, nu)), 1)
    }), match.on)

    getBins(data, match.on, breaks)
  })

  opb <- pbapply::pboptions(type = if (verbose) "timer" else "none")
  on.exit(pbapply::pboptions(opb))

  Lstats <- unlist(pbapply::pblapply(bin.lists, function(b) {
    strata <- assignToBins(data, match.on, b)
    strataholder <- lapply(unique(strata), function(s) which(strata == s))
    Lstat(get.diffs(strataholder, treat.vec, num.treated, num.control))
  }))

  Lstat.med <- sort(Lstats, partial = ceiling(n.coarsenings/2))[ceiling(n.coarsenings/2)]

  return(bin.lists[[which(Lstats == Lstat.med)[1]]])

}
