plot.matchFrontier <- function(x, covs = NULL, stat = NULL, n.estimated, axis = "ndrop", ...) {

  if (inherits(x, "MatchItFrontier") && identical(attr(x$distance, "type"), "ps")) {
    allowable_axis <- c("caliper", "ndrop", "n")
  }
  else {
    allowable_axis <- c("ndrop", "n")
  }

  axis <- try(match_arg(tolower(axis), allowable_axis), silent = TRUE)
  if (inherits(axis, "try-error")) {
    customStop(sprintf("'axis' must be one of %s.",
                       word_list(allowable_axis, and.or = "or", quotes = TRUE)),
               "plot()")
  }

  if (axis == "caliper") {
    xlab <- "Caliper"

    #Rescale frontier$Xs to be on the caliper scale
    matched <- which(!is.na(x$matched.to[,1]))
    x$frontier$Xs <- c(vapply(x$frontier$drop.order[-1], function(d) x$distance[match(d[1], matched)], numeric(1L)), min(x$distance))
  }
  else if (axis == "ndrop") {
    xlab <- switch(x$QOI,
                   "SATE" = "Number of units dropped",
                   "FSATE" = "Number of units dropped",
                   "SATT" = "Number of control units dropped",
                   "FSATT" = "Number of treated units dropped")
  }
  else {
    x$frontier$Xs <- x$n - x$frontier$Xs
    xlab <- switch(x$QOI,
                   "SATE" = "Number of units remaining",
                   "FSATE" = "Number of units remaining",
                   "SATT" = "Number of control units remaining",
                   "FSATT" = "Number of treated units remaining")
  }

  ylab <- firstup(metric2info(x$metric))

  plot_geom <- if (length(x$frontier$Xs) == 1) geom_point else geom_line

  p <- ggplot()

  if (!is.null(stat)) {
    stat <- match_arg(stat, c("std-diff", "diff", "ks", "std-mean", "mean", "ks-target", "ess"))
  }

  if (is.null(stat) && is.null(covs)) {
    if (!missing(n.estimated)) {
      customWarning("'n.estimated' is ignored when 'covs' and 'stat' are both NULL.", "plot.matchFrontier()")
    }

    p <- p + plot_geom(data = NULL, mapping = aes(x = x$frontier$Xs, y = x$frontier$Ys), ...)

    if (!is.null(x$frontier$Y.origin)) {
      p <- p + geom_point(data = NULL, aes(x = x$frontier$Xs[1], y = x$frontier$Y.origin))
    }
  }
  else if (!is.null(stat) && stat == "ess") {
    n.steps <-  length(x$frontier$Xs)
    point.inds <- unique(round(seq(1, n.steps, length.out = n.steps)))

    ESS <- function(w) {sum(w)^2/sum(w^2)}
    ess.stat <- do.call("rbind", lapply(point.inds, function(i) {
      this.dat.inds.to.drop <- unlist(x$frontier$drop.order[seq_len(i)])

      m.data <- makeMatchedData(x$data,
                                matched.to = x$matched.to,
                                drop.inds = this.dat.inds.to.drop)

      c("1" = ESS(m.data[[attr(m.data, "weights")]][m.data[[x$treatment]] == 1]),
        "0" = ESS(m.data[[attr(m.data, "weights")]][m.data[[x$treatment]] == 0]))
    }))

    ess.stat <- as.data.frame(ess.stat)

    ess.stat.long <- reshape(ess.stat, direction = "long",
                             varying = c("1", "0"),
                             v.names = "ESS",
                             timevar = "Treatment",
                             times = c("1", "0"))

    ess.point.stat <- data.frame(ESS = vapply(c(1, 0), function(t) sum(x$data[[x$treatment]] == t), numeric(1L)),
                                 Treatment = c("1", "0"))

    p <- p + plot_geom(data = ess.stat.long, mapping = aes(x = c(x$frontier$Xs, x$frontier$Xs),
                                                           y = ESS, linetype = Treatment),
                       color = "black") +
      geom_point(data = ess.point.stat, mapping = aes(x = x$frontier$Xs[1], y = ESS, shape = Treatment),
                 color = "black") +
      scale_y_continuous(limits = c(0, max(ess.point.stat[["ESS"]])))

    ylab <- "Effective sample size"
  }
  else {
    if (is.null(stat)) stat <- "std-diff"

    n.steps <-  length(x$frontier$Xs)
    if (missing(n.estimated)) n.estimated <- 250
    else {
      if (length(n.estimated) != 1 || !is.numeric(n.estimated)) {
        customStop("'n.estimated' must be a single number.", "plot.matchFrontier()")
      }
    }
    point.inds <- unique(round(seq(1, n.steps, length.out = min(n.estimated, n.steps))))

    if (is.null(covs)) {
      data <- as.data.frame(get.covs.matrix(data = x$data[x$match.on]))
    }
    else if (is.character(covs)) {
      if (!all(covs %in% names(x$data))) {
        customStop("all variables in 'covs' must be variables in the dataset supplied to makeFrontier().", "plot.matchFrontier()")
      }
      data <- as.data.frame(get.covs.matrix(data = x$data[covs]))
    }
    else if (inherits(covs, "formula")) {
      tryCatch({
        data <- as.data.frame(get.covs.matrix(delete.response(terms(covs, data = x$data)),
                                              data = x$data))},
        error = function(e) {
          cond <- conditionMessage(e)
          if (startsWith(cond, "object") && endsWith(cond, "not found")) {
            customStop("all variables in 'covs' must be variables in the dataset supplied to makeFrontier().", "plot.matchFrontier()")
          }
          else customStop(e, "plot.matchFrontier()")
        })
    }
    else {
      customStop("'covs' must be a formula or character vector.", "plot.matchFrontier()")
    }

    cov.names <- names(data)

    if (stat == "std-diff") {
      sf <- switch(x$QOI,
                   "SATE" =, "FSATE" = sqrt(.5*(vapply(data[x$data[[x$treatment]] == 1,, drop = FALSE], var, numeric(1L)) +
                                                  vapply(data[x$data[[x$treatment]] == 0,, drop = FALSE], var, numeric(1L)))),
                   "SATT" =, "FSATT" = vapply(data[x$data[[x$treatment]] == 1,,drop = FALSE], sd, numeric(1L)))
      stat.fun <- function(v, t, d, w = NULL) {
        (w_m(d[[v]][d[[t]] == 1], w[d[[t]]==1]) -
           w_m(d[[v]][d[[t]] == 0], w[d[[t]]==0])) / sf[v]
      }

    }
    else if (stat == "diff") {
      stat.fun <- function(v, t, d, w = NULL) {
        (w_m(d[[v]][d[[t]] == 1], w[d[[t]]==1]) -
           w_m(d[[v]][d[[t]] == 0], w[d[[t]]==0]))
      }
    }
    else if (stat == "ks") {
      bin.vars <- vapply(data, function(V) length(unique(V)) <= 2, logical(1L))

      stat.fun <- function(v, t, d, w = NULL) {
        if (bin.vars[v]) {
          abs(w_m(d[[v]][d[[t]] == 1], w[d[[t]] == 1]) -
                w_m(d[[v]][d[[t]] == 0], w[d[[t]] == 0]))
        }
        else {
          w_ks(d[[v]], d[[t]], w)
        }
      }
    }
    else if (stat == "std-mean") {
      center <- switch(x$QOI,
                       "SATE" =, "FSATE" = colMeans(data),
                       "SATT" =, "FSATT" = colMeans(data[x$data[[x$treatment]] == 1,,drop = FALSE]))
      sf <- switch(x$QOI,
                   "SATE" =, "FSATE" = sqrt(.5*(vapply(data[x$data[[x$treatment]] == 1,,drop = FALSE], var, numeric(1L)) +
                                                  vapply(data[x$data[[x$treatment]] == 0,,drop = FALSE], var, numeric(1L)))),
                   "SATT" =, "FSATT" = vapply(data[x$data[[x$treatment]] == 1,,drop = FALSE], sd, numeric(1L)))

      stat.fun <- function(v, t, d, w = NULL) {
        (w_m(d[[v]], w) - center[v]) / sf[v]
      }
    }
    else if (stat == "mean") {
      stat.fun <- function(v, t, d, w = NULL) {
        w_m(d[[v]], w)
      }
    }
    else if (stat == "ks-target") {

      bin.vars <- vapply(data, function(V) length(unique(V)) <= 2, logical(1L))
      if (any(bin.vars)) {
        center <- switch(x$QOI,
                         "SATE" =, "FSATE" = colMeans(data[bin.vars]),
                         "SATT" =, "FSATT" = colMeans(data[x$data[[x$treatment]] == 1, bin.vars, drop = FALSE]))
      }

      stat.fun <- function(v, t, d, w = NULL) {
        if (bin.vars[v]) {
          abs(w_m(d[[v]], w) - center[v])
        }
        else {
          if (is.null(w)) w <- rep(1, nrow(d))
          switch(x$QOI,
                 "SATE" =, "FSATE" = w_ks(c(data[[v]], d[[v]]),
                                          treat = c(rep(1, nrow(data)), rep(0, nrow(d))),
                                          w = c(rep(1, nrow(data)), w)),
                 "SATT" =, "FSATT" = w_ks(c(data[[v]][data[[t]] == 1], d[[v]]),
                                          treat = c(rep(1, sum(data[[t]] == 1)), rep(0, nrow(d))),
                                          w = c(rep(1, sum(data[[t]] == 1)), w)))
        }
      }
    }

    Xs.long <- rep(x$frontier$Xs[point.inds], ncol(data))
    data <- setNames(cbind(data, x$data[[x$treatment]]),
                     c(cov.names, x$treatment))

    cov.stat <- do.call("rbind", lapply(point.inds, function(i) {
      this.dat.inds.to.drop <- unlist(x$frontier$drop.order[seq_len(i)])

      m.data <- makeMatchedData(data,
                                matched.to = x$matched.to,
                                drop.inds = this.dat.inds.to.drop)

      vapply(cov.names, stat.fun, numeric(1L),
             t = x$treatment,
             d = m.data,
             w = m.data[[attr(m.data, "weights")]])
    }))

    cov.stat <- as.data.frame(cov.stat)

    cov.stat.long <- reshape(cov.stat, direction = "long",
                             varying = cov.names,
                             v.names = "Val",
                             timevar = "Covariate",
                             times = cov.names)

    #Unadjusted statistics
    cov.point.stat <- data.frame(Val = vapply(cov.names, stat.fun, numeric(1L),
                                              t = x$treatment,
                                              d = data),
                                 Covariate = cov.names
    )

    if (stat == "mean") {
      center <- switch(x$QOI,
                       "SATE" =, "FSATE" = colMeans(data[cov.names]),
                       "SATT" =, "FSATT" = colMeans(data[x$data[[x$treatment]] == 1, cov.names, drop = FALSE]))
      original.means <- data.frame(Covariate = cov.names, Val = center)

      p <- p + geom_hline(data = original.means,
                          aes(yintercept = Val, color = Covariate),
                          linetype = 2)
    }
    else {
      p <- p + geom_hline(yintercept = 0)
    }

    #Ensure variables appear in order entered
    cov.stat.long[["Covariate"]] <- factor(cov.stat.long[["Covariate"]], levels = cov.names)
    cov.point.stat[["Covariate"]] <- factor(cov.point.stat[["Covariate"]], levels = cov.names)

    p <- p + plot_geom(data = cov.stat.long, mapping = aes(x = Xs.long, y = Val, color = Covariate)) +
      geom_point(data = cov.point.stat, mapping = aes(x = x$frontier$Xs[1], y = Val, color = Covariate))

    ylab <- switch(stat,
                   "std-diff" = "Standardized mean difference",
                   "diff" = "Mean difference",
                   "ks" = "Kolmogorov-Smirnov statistic",
                   "std-mean" = "Standardized mean",
                   "mean" = "Mean",
                   "ks-target" = "Sample-target Kolmogorov-\nSmirnov statistic")
  }

  if (axis == "caliper") {
    p <- p + scale_x_reverse()
  }
  else {
    upper_breaks <- function(y, ...) {
      x$n - scales::breaks_extended()(x$n - y, ...)
    }
    if (axis == "ndrop") {
      p <- p + scale_x_continuous(sec.axis = dup_axis(trans = ~ x$n - .,
                                                      name = sub("dropped", "remaining", xlab),
                                                      breaks = upper_breaks))
    }
    else {
      p <- p + scale_x_continuous(sec.axis = sec_axis(trans = ~ x$n - .,
                                                      name = sub("remaining", "dropped", xlab),
                                                      breaks = upper_breaks),
                                  trans = "reverse")
    }
  }

  p  +
    geom_blank(aes(y = 0)) +
    labs(title = "Frontier Plot", x = xlab, y = ylab) +
    theme_bw() +
    theme(plot.title = element_text(hjust = .5))

}