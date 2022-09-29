modelDependence <- function(base.form = NULL,
                            data,
                            treatment,
                            outcome,
                            covariates,
                            weights = NULL,
                            method = c("extreme-bounds", "athey-imbens"),
                            model.dependence.ests = 101,
                            specifications = NULL,
                            cutpoints = NULL,
                            cutpoint.method = c("mean", "median", "segmented"),
                            verbose = TRUE,
                            seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  if (missing(data) || length(data) == 0){
    customStop("a dataset must be supplied.", 'modelDependence()')
  }

  if (is.null(base.form)) {
    if (missing(outcome)) {
      customStop("when 'base.form' is omitted, the 'outcome' argument is required.", "modelDependence()")
    }
    if (missing(treatment) || length(treatment) == 0){
      customStop("when 'base.form' is omitted, the 'treatment' argument is required.", 'modelDependence()')
    }
    base.form <- reformulate(treatment, outcome)
  }
  else {
    base.form <- terms(as.formula(base.form))
    all.base.form.vars <- all.vars(base.form)
    rhs.base.form.vars <- all.vars(delete.response(base.form))

    outcome <- setdiff(all.base.form.vars, rhs.base.form.vars)[1]

    if (length(rhs.base.form.vars) == 1) {
      if (!missing(treatment) && !is.null(treatment) && !identical(treatment, rhs.base.form.vars)) {
        customStop("the treatment variable must be present in 'base.form'.", 'modelDependence()')
      }
      treatment <- rhs.base.form.vars
    }
    else if (missing(treatment) || length(treatment) == 0){
      customStop("a treatment variable must be supplied.", 'modelDependence()')
    }
    else if (!treatment %in% rhs.base.form.vars) {
      customStop("the treatment variable must be present in 'base.form'.", 'modelDependence()')
    }
  }

  if (missing(covariates)) {
    covariates <- setdiff(all.vars(base.form), c(outcome, treatment))
  }
  if (length(covariates) == 0) {
    customStop("the 'covariates' argument is required when 'base.form' does not contain covariates.", "modelDependence()")
  }
  if (inherits(covariates, "formula")) {
    covariates <- all.vars(delete.response(terms(covariates)))
  }
  if (!all(covariates %in% names(data))) {
    customStop("all covariates must be present in 'data'.", 'modelDependence()')
  }

  method <- match_arg(method)

  if (method == "extreme-bounds") {
    if (is.null(model.dependence.ests) && is.null(specifications)) {
      customStop("either 'specifications' or 'model.dependence.ests' must be specified when using the extreme bounds procedure.", "modelDependence()")
    }

    if (is.null(specifications)) {
      specifications <- getSpecifications(base.form, covariates, data, model.dependence.ests)
    }
    else if (!is.list(specifications) ||
             !all(vapply(specifications, function(s) {
               inherits(s, "formula") ||
                 (is.character(s) && length(s) == 1 && is.call(ff <- str2lang(s)) &&
                  is.symbol(c. <- ff[[1L]]) && c. == quote(`~`))
             }, logical(1L)))) {
      customStop("'specifications' must be a list of model formulas.", "modelDependence()")
    }

    if (!is.null(weights) && (!is.numeric(weights) || length(weights) != nrow(data))) {
      customStop("'weights' must be a nmeric vector with a value for each unit in the data.", "modelDependence()")
    }
  }
  else if (method == "athey-imbens") {

    if (!is.null(weights)) {
      if (!all(weights %in% 0:1)) {
        customStop('non-0/1 weights cannot be used with the Athey-Imbens procedure.", "modelDependence()')
      }
      else {
        data <- data[weights > 0,,drop = FALSE]
        weights <- NULL
      }
    }

    cutpoint.method <- match_arg(cutpoint.method)

    for (i in covariates) {
      nu <- length(unique(data[[i]]))
      if (nu == 1L) covariates <- setdiff(covariates, i)
      else if (nu == 2L) data[[i]] <- factor(data[[i]], nmax = 2)
      else if (is.character(data[[i]])) data[[i]] <- factor(data[[i]])
    }

    cutpoints <- setNames(lapply(covariates, function(cov) {
      if (is.factor(data[[cov]])) {
        NA
      }
      else if (!is.null(cutpoints) && cov %in% names(cutpoints)) {
        cutpoints[[cov]]
      }
      else{
        getCutpoint(data, base.form, cov, cutpoint.method)
      }
    }), covariates)
  }

  out <- modelDependenceInternal(data, treatment = treatment,
                                 outcome = outcome,
                                 covariates = covariates,
                                 weights = weights,
                                 base.form = base.form,
                                 method = method,
                                 specifications = specifications,
                                 cutpoints = cutpoints,
                                 verbose = verbose)

  return(out)
}

modelDependenceInternal <- function(data,
                                    treatment,
                                    outcome,
                                    covariates,
                                    weights = NULL,
                                    base.form,
                                    method = c("extreme-bounds", "athey-imbens"),
                                    specifications = NULL,
                                    cutpoints = NULL,
                                    verbose = TRUE) {

  method <- match_arg(method)

  base.coef <- estOneEffect(base.form, data = data,
                            treatment = treatment,
                            # subclass = data$.subclass, id = data$.id,
                            weights = weights, alpha = NULL)["Estimate"]

  if (method == "extreme-bounds") {
    #Needs specifications
    coef.dist <- vapply(specifications, function(s) {
      formula <- as.formula(s)
      # run model
      est <- estOneEffect(formula, data = data,
                          treatment = treatment, weights = weights,
                          # subclass = data$.subclass, id = data$.id,
                          alpha = NULL)
      return(est["Estimate"])
    }, numeric(1L))

    out <- quantile(coef.dist, c(.05, .95))
    attr(out, "specifications") <- lapply(specifications, function(s) {
      if (is.call(s)) deparse1(s) else s
    })
    attr(out, "estimates") <- coef.dist
  }
  else if (method == "athey-imbens") {

    N <- nrow(data)

    theta.Ps <- vapply(covariates, function(cov) {
      # Formula for this iteration; remove cov
      this.form <- update(base.form, formula(paste0(". ~ . -", cov)))

      # Split data
      if (anyNA(cutpoints[[cov]])) {
        split.datasets <- split(data, data[[cov]])
        if (!is.null(weights)) split.weights <- split(weights, data[[cov]])
      }
      else {
        split.datasets <- split(data, data[[cov]] < cutpoints[[cov]])
        if (!is.null(weights)) split.weights <- split(weights, data[[cov]] < cutpoints[[cov]])
      }

      # Get theta_ps
      this.theta.p <- sum(vapply(seq_along(split.datasets), function(i) {
        d <- process_safe_for_model(split.datasets[[i]], this.form)

        if (!is.null(weights)) w <- split.weights[[i]]
        else w <- NULL

        d.est <- estOneEffect(this.form, data = d,
                              treatment = treatment,
                              weights = w, alpha = NULL)["Estimate"]

        return(d.est * (nrow(d)/N))
      }, numeric(1L)))

      as.numeric(this.theta.p)

    }, numeric(1L))

    theta.Ps <- theta.Ps[!is.na(theta.Ps)]

    if (length(theta.Ps) == 0) out <- c(NA_real_, NA_real_)
    else {
      sigma.hat.theta <- sqrt(sum((theta.Ps - base.coef) ^ 2) / length(theta.Ps))
      out <- c(base.coef - sigma.hat.theta, base.coef + sigma.hat.theta)
    }


    attr(out, "estimates") <- theta.Ps
    attr(out, "sigma") <- sigma.hat.theta
  }

  attr(out, "method") <- method
  attr(out, "base.est") <- base.coef
  names(out) <- c("lower", "upper")

  class(out) <- c("modelDependenceBounds", "numeric")

  return(out)
}

print.modelDependenceBounds <- function(x, ...) {
  if (length(x) != 2 || is.null(attr(x, "method"))) {
    print(as.numeric(x), ...)
    return(invisible(as.numeric(x)))
  }
  else {
    method <- attr(x, "method")
    method.info <- switch(method,
                          "extreme-bounds" = "Extreme model dependence bounds:",
                          "athey-imbens" = "Athey-Imbens model dependence bounds:")
    cat(method.info, "\n")
    print(c(x[1:2], diff = unname(diff(x))), ...)
    return(invisible(x))
  }
}

plot.modelDependenceBounds <- function(x, ...) {
  method <- attr(x, "method")
  est <- attr(x, "estimates")
  base.est <- attr(x, "base.est")

  if (method == "extreme-bounds") {
  p <- ggplot() + geom_histogram(aes(x = est), bins = 15, color = "gray60", fill = "gray60") +
    geom_vline(aes(xintercept = x), color = "red") +
    geom_vline(aes(xintercept = base.est), color = "blue") +
    geom_hline(yintercept = 0) +
    theme_bw() +
    theme(plot.title = element_text(hjust = .5)) +
    labs(title = "Extreme model dependence bounds",
         x = "Estimate",
         y = "Count")
  }
  else if (method == "athey-imbens") {
    d <- data.frame(Var = factor(c(names(est), "Original"), levels = c("Original", names(est)[order(est, decreasing = FALSE)])),
                    Est = c(est, base.est))

    p <- ggplot(data = d) +
      geom_point(aes(x = Est, y = Var, color = Var)) +
      geom_vline(xintercept = x, color = "red") +
      geom_vline(aes(xintercept = base.est), color = "blue") +
      scale_color_manual(values = setNames(c("blue", rep("black", length(est))), levels(d$Var))) +
      guides(color = "none") +
      theme_bw() +
      theme(plot.title = element_text(hjust = .5)) +
      labs(title = "Athey-Imbens model dependence bounds",
           x = "Estimate",
           y = "Splitting covariate")
  }

  p
}
