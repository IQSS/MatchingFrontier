plot.frontierEstimates <- function(x,
                                   band,
                                   band.col = "red",
                                   band.border.col = "red",
                                   band.alpha = .5,
                                   ...){

    if (missing(band)) {
        if (length(x$mod.dependence) > 0) band <- "model-dependence"
        else band <- "confidence"
    }
    else if (length(band) == 0) {
        band <- "none"
    }
    else {
        band <- match_arg(band, c("confidence", "model-dependence", "none"))
    }

    if (band == "model-dependence") {
        if (length(x$mod.dependence) == 0) {
            customStop("'band' cannot be \"model-dependence\" when 'method' was \"none\" in the original call to estimateEffects().",
                       "plot.frontierEstimates")
        }
        band.mins <- pmin(unlist(lapply(x$mod.dependence, function(z) z[1])), x$coefs)
        band.maxs <- pmax(unlist(lapply(x$mod.dependence, function(z) z[2])), x$coefs)

        band.min.un <- min(x$un$mod.dependence[1], x$un$coef)
        band.max.un <- max(x$un$mod.dependence[2], x$un$coef)
    }
    else {
        band.mins <- unlist(lapply(x$CIs, function(z) z[1]))
        band.maxs <- unlist(lapply(x$CIs, function(z) z[2]))

        band.min.un <- x$un$CI[1]
        band.max.un <- x$un$CI[2]
    }

    xlab <- switch(x$QOI,
                   "SATE" = "Number of units dropped",
                   "FSATE" = "Number of units dropped",
                   "SATT" = "Number of control units dropped",
                   "FSATT" = "Number of treated units dropped")

    ylab <- 'Effect estimate'

    sub <- NULL
    if (band == "confidence") {
        CIlevel <- attr(x[["CIs"]], "CIlevel")
        sub <- paste0(round(100*CIlevel, 2), "% CI bands")
    }
    else if (band == "model-dependence") {
        if (x$method == "extreme-bounds") {
            sub <- "Extreme bounds sensitivity bands"
        }
        else if (x$method == "athey-imbens") {
            sub <- "Athey-Imbens sensitivity bands"
        }

    }

    p <- ggplot(mapping = aes(x = x$Xs))

    if (band != "none") {

        p <- p + geom_ribbon(aes(ymin = band.mins, ymax = band.maxs),
                             fill = band.col, color = band.border.col,
                             alpha = band.alpha)

        if (min(x$Xs) == 0) {
          p <- p + geom_errorbar(aes(x = 0, ymin = band.min.un, ymax = band.max.un),
                                 color = "black", width = x$n/100)
        }

    }

    upper_breaks <- function(y, ...) {
      x$n - scales::breaks_extended()(x$n - y, ...)
    }

    p <- p + geom_line(aes(y = x$coefs), ...) +
        scale_x_continuous(sec.axis = dup_axis(trans = ~ x$n - ., name = sub("dropped", "remaining", xlab),
                                               breaks = upper_breaks)) +
        labs(title = "Effects plot", subtitle = sub, x = xlab, y = ylab) +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(hjust = .5))

    if (min(x$Xs) == 0) {
      p <- p + geom_point(aes(x = 0, y = x$un$coef), color = "black")
    }

    p
}
