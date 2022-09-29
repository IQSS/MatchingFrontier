estOneEffect <- function(formula, data, treatment, weights = NULL, subclass = NULL, id = NULL, alpha = .05) {

  data <- process_safe_for_model(data, formula)

  #For now, ignoring clustering by subclass and ID. Seems vcovHC is better for matching
  #w/ replacement.
  id <- NULL

  fit <- do.call("lm", list(formula, data = data, weights = weights), quote = TRUE)

  if (!is.null(alpha)) {
    if (is.null(subclass) && is.null(id)) {
      ci <- safe_ci(fit, treatment, vcov. = sandwich::vcovHC, type = "HC3",
                    alpha = alpha)
    }
    else if (!is.null(subclass)) {
      ci <- safe_ci(fit, treatment, vcov. = sandwich::vcovCL, type = "HC1",
                    cluster = list(subclass), alpha = alpha)
    }
    else {
      ci <- safe_ci(fit, treatment, vcov. = sandwich::vcovCL, type = "HC1",
                    cluster = list(subclass, id), alpha = alpha)
    }
    if (treatment %in% rownames(ci)) {
      out <- c(coef(fit)[treatment], ci[treatment, ])
    }
    else out <- rep(NA_real_, 3)

    names(out) <- c("Estimate", colnames(ci))
  }
  else {
    out <- coef(fit)[treatment]

    names(out) <- "Estimate"
  }
  return(out)
}
