generateDataset <- function(frontier.object, N, Ndrop, weights = "weights",
                            dup = FALSE, subclass = "subclass", id = "id") {

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

  d <- frontier.object$data

  matched.to <- frontier.object$matched.to

  if (length(weights) != 1 || !is.character(weights) || is.na(weights)) {
    customStop("the argument to 'weights' must be a string of length 1.",
               "generateDataset()")
  }
  if (weights %in% names(data)) {
    customStop(paste0("\"", weights, "\" is already the name of a variable in the data. Please choose another name for weights using the 'weights' argument."),
               "generateDataset()")
  }

  if (!is.null(matched.to) && dup) {
    if (length(subclass) != 1 || !is.character(subclass) || is.na(subclass)) {
      customStop("the argument to 'subclass' must be a string of length 1.",
                 "generateDataset()")
    }
    if (subclass %in% names(d)) {
      stop(paste0("\"", subclass, "\" is already the name of a variable in the data. Please choose another name for subclass using the 'subclass' argument."), call. = FALSE)
    }

    if (length(id) != 1 || !is.character(id) || is.na(id)) {
      customStop("the argument to 'id' must be a string of length 1.",
                 "generateDataset()")
    }
    if (id %in% names(d)) {
      stop(paste0("\"", id, "\" is already the name of a variable in the data. Please choose another name for id using the 'id' argument."), call. = FALSE)
    }
  }

  d <- makeMatchedData(d, matched.to = matched.to,
                       drop.inds = drop.inds, weights = weights,
                       dup = dup,
                       with_replacement = anyDuplicated(na.omit(as.vector(matched.to))) != 0,
                       subclass = subclass, id = id)

  if (!is.null(matched.to) && dup) {
    # new.cols <- c(attr(d, "id"), attr(d, "subclass"), attr(d, "weights"))

    d[] <- d[order(d[[attr(d, "subclass")]], d[[frontier.object$treatment]],
                   method = "radix", decreasing = c(FALSE, TRUE)),]
    rownames(d) <- NULL

    class(d) <- c("getmatches", class(d))
  }
  else {
    class(d) <- c("matchdata", class(d))
  }

  return(d)
}
