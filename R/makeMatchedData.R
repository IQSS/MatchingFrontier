makeMatchedData <- function(data, matched.to = NULL, drop.inds = NULL, weights = ".weights",
                            dup = FALSE, with_replacement = TRUE, subclass = ".subclass", id = ".id") {
  if (is.null(matched.to)) {
    if (length(drop.inds) > 0) data <- data[-drop.inds,,drop=FALSE]

    data[[weights]] <- 1

    attr(data, "weights") <- weights
  }
  else if (dup) {
    if (length(drop.inds) != 0) {
      is.na(matched.to[drop.inds,]) <- TRUE
    }

    s <- factor(c(which(!is.na(matched.to[,1])),
                  unlist(lapply(seq_len(ncol(matched.to)),
                                function(i) which(!is.na(matched.to[,i]))))))
    levels(s) <- seq_len(nlevels(s))
    i <- c(which(!is.na(matched.to[,1])),
           unlist(lapply(seq_len(ncol(matched.to)),
                  function(i) matched.to[!is.na(matched.to[,i]), i])))
    w <- c(rep(1, sum(!is.na(matched.to[,1]))),
           unlist(lapply(seq_len(ncol(matched.to)),
                         function(i) rep(1/ncol(matched.to), sum(!is.na(matched.to[,i]))))))

    rn <- rownames(data)
    data <- data[i,]

    data <- setNames(cbind(rn[i], s, w, data),
                        c(id, subclass, weights, names(data)))

    attr(data, "weights") <- weights
    attr(data, "subclass") <- subclass
    attr(data, "id") <- id
  }
  else {

    if (length(drop.inds) > 0) {
      is.na(matched.to)[drop.inds,] <- TRUE
    }

    #Weights; 1 for units that receive a match, 1/ratio for each time unit is
    #used as a match for someone else
    w <- as.numeric(!is.na(matched.to[,1])) + tabulate(matched.to, nbins = nrow(matched.to))/ncol(matched.to)

    data[[weights]] <- w

    if (!with_replacement) {
      matched <- which(!is.na(matched.to[,1]))

      s <- factor(matched)
      levels(s) <- seq_len(nlevels(s))

      data[[subclass]] <- factor(NA, levels = levels(s))

      data[[subclass]][matched] <- s

      for (i in seq_len(ncol(matched.to))) {
        data[[subclass]][matched.to[matched, i]] <- s
      }

    }
    data <- data[w > 0,]

    attr(data, "weights") <- weights
    if (!with_replacement) attr(data, "subclass") <- subclass
  }
  return(data)
}
