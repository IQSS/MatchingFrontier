estimateEffects <- function(frontier.object,
                            outcome,
                            base.form = NULL,
                            n.estimated = 250,
                            N, Ndrop,
                            method = c("none", "extreme-bounds", "athey-imbens"),
                            model.dependence.ests = 100,
                            specifications = NULL,
                            cutpoints = NULL,
                            cutpoint.method = c("mean", "median", "segmented"),
                            seed = NULL,
                            alpha = 0.05,
                            verbose = TRUE,
                            cl = NULL,
                            ...) {

  set.seed(seed)
  call <- match.call()

  if (!inherits(frontier.object, "matchFrontier")) {
    customStop("'frontier.object' must be a matchFrontier object, the output of a call to makeFrontier().", "estimateEffects()")
  }

  treatment <- frontier.object$treatment
  covariates <- frontier.object$match.on

  numeric.covs <- vapply(covariates, function(i) {
    !is.character(frontier.object$data[[i]]) &&
      !is.factor(frontier.object$data[[i]]) &&
      !is.logical(frontier.object$data[[i]])
  }, logical(1L))
  frontier.object$data[covariates[numeric.covs]] <- scale(frontier.object$data[covariates[numeric.covs]])

  if (is.null(base.form)) {
    if (missing(outcome)) {
      customStop("When 'base.form' is omitted, the 'outcome' argument is required.", "estimateEffects()")
    }
    base.form <- reformulate(treatment, outcome)
  }
  else {
    base.form <- as.formula(base.form)
    outcome <- all.vars(base.form)[1]
  }
  if (!all(setdiff(all.vars(base.form), c(outcome, treatment)) %in% covariates)) {
    customWarning("not all covariates in 'base.form' were matched on in the original call to makeFrontier().", "estimateEffects()")
  }

  # These are the points that we'll estimate
  if (missing(N) && missing(Ndrop)) {
    #No fewer than 5 units per covariate to keep covariances stable
    min.n <- ncol(model.matrix(base.form, frontier.object$data)) - 1
    Ndrop <- c(0, frontier.object$n - 5 * min.n)
  }
  else if (!missing(N) && !missing(Ndrop)) {
    customStop("only one of 'N' or 'Ndrop' may be specified.", "estimateEffects()")
  }
  else if (!missing(Ndrop)) {
    if (!is.numeric(Ndrop) ||
        any(Ndrop < 0) || any(Ndrop > frontier.object$n - 1)) {
      customStop(paste0("'Ndrop' must be between 0 and ", frontier.object$n - 1, "."), "estimateEffects()")
    }
  }
  else if (!missing(N)) {
    if (!is.numeric(N) ||
        any(N < 1) || any(N > frontier.object$n)) {
      customStop(paste0("'N' must be between 1 and ", frontier.object$n, "."), "estimateEffects()")
    }
    Ndrop <- frontier.object$n - N
  }

  minNdrop <- frontier.object$frontier$Xs[which.min(abs(frontier.object$frontier$Xs - min(Ndrop)))]
  maxNdrop <- frontier.object$frontier$Xs[which.min(abs(frontier.object$frontier$Xs - max(Ndrop)))]

  n.steps <- maxNdrop - minNdrop
  if ("prop.estimated" %in% ...names()) {
    prop.estimated <- list(...)$prop.estimated
    if (length(prop.estimated) != 1 || !is.numeric(prop.estimated)) {
      customStop("'prop.estimated' must be a single number.", "estimateEffects()")
    }
    n.estimated <- ceiling(n.steps * prop.estimated)
  }
  else if (length(n.estimated) != 1 || !is.numeric(n.estimated)) {
    customStop("'n.estimated' must be a single number.", "estimateEffects()")
  }

  point.inds <- unique(round(seq(which.min(abs(frontier.object$frontier$Xs - min(Ndrop))),
                                 which.min(abs(frontier.object$frontier$Xs - max(Ndrop))),
                                 length.out = min(n.estimated, n.steps))))

  # Pre-process modelDependence() args
  method <- match_arg(method)

  if (method == "none") {
    mod.dependence <- mod.dependence.un <- NULL
  }
  else if (method == "extreme-bounds") {
    if (is.null(model.dependence.ests) && is.null(specifications)) {
      customStop("either 'specifications' or 'model.dependence.ests' must be specified when using the extreme bounds procedure.", "estimateEffects()")
    }

    if (is.null(specifications)) {
      if (verbose) cat("Getting extreme bounds model specifications...\n")
      specifications <- getSpecifications(base.form, covariates, frontier.object$data, model.dependence.ests)
    }
    else if (!is.list(specifications) ||
             !all(vapply(specifications, function(s) {
               inherits(s, "formula") ||
                 (is.character(s) && length(s) == 1 && is.call(ff <- str2lang(s)) &&
                  is.symbol(c. <- ff[[1L]]) && c. == quote(`~`))
             }, logical(1L)))) {
      customStop("'specifications' must be a list of model formulas.", "modelDependence()")
    }

    mod.dependence <- vector("list", length = length(point.inds))
    attr(method, "model.dependence.ests") <- length(specifications)
  }
  else if (method == "athey-imbens") {
    if (!is.null(frontier.object$matched.to)) {
      customStop("the Athey-Imbens procedure cannot be used with a pair-based frontier.", "estimateEffects()")
    }

    for (i in covariates) {
      if (is.character(frontier.object$data[[i]])) frontier.object$data[[i]] <- factor(frontier.object$data[[i]])
      frontier.object$data[[i]] <- as.numeric(frontier.object$data[[i]])
    }

    if (verbose) cat("Making cutpoints for Athey-Imbens bounds...\n")
    cutpoint.method <- match_arg(cutpoint.method)

    cutpoints <- setNames(lapply(covariates, function(cov) {
      if (is.factor(frontier.object$data[[cov]])) {
        NA
      }
      else if (!is.null(cutpoints) && cov %in% names(cutpoints)) {
        cutpoints[[cov]]
      }
      else{
        getCutpoint(frontier.object$data, base.form, cov, cutpoint.method)
      }
    }), covariates)

    mod.dependence <- vector("list", length = length(point.inds))
    attr(method, "cutpoint.method") <- cutpoint.method
  }

  # Estimate effects
  if (verbose) {
    cat("Estimating effects...\n")
  }

  opb <- pbapply::pboptions(type = if (verbose) "timer" else "none")
  on.exit(pbapply::pboptions(opb))

  #Estimate effect, mod dep, and CI in unadjusted data
  with_replacement <- anyDuplicated(na.omit(frontier.object$matched.to)) != 0

  res <- pbapply::pblapply(c(0L, seq_along(point.inds)), function(i) {
    if (i == 0L) {
      out <- estOneEffect(base.form, frontier.object$data, treatment,
                             alpha = alpha)

      if (method != "none") {
        suppressWarnings({
          out <- c(out, modelDependenceInternal(frontier.object$data,
                                                treatment = treatment,
                                                outcome = outcome,
                                                covariates = covariates,
                                                base.form = base.form,
                                                method = method,
                                                specifications = specifications,
                                                cutpoints = cutpoints,
                                                verbose = FALSE)[1:2])
        })
      }
    }
    else {
      this.dat.inds.to.drop <- unlist(frontier.object$frontier$drop.order[seq_len(point.inds[i])])

      matched.data <- makeMatchedData(frontier.object$data,
                                      matched.to = frontier.object$matched.to,
                                      drop.inds = this.dat.inds.to.drop,
                                      with_replacement = with_replacement)

      out <- estOneEffect(base.form, matched.data, treatment,
                          weights = if (!is.null(attr(matched.data, "weights"))) {
                            matched.data[[attr(matched.data, "weights")]]
                          },
                          subclass = if (!is.null(attr(matched.data, "subclass"))) {
                            matched.data[[attr(matched.data, "subclass")]]
                          },
                          alpha = alpha)

      if (method != "none") {
        suppressWarnings({
          out <- c(out, modelDependenceInternal(matched.data,
                                                treatment = treatment,
                                                outcome = outcome,
                                                covariates = covariates,
                                                weights = matched.data[[attr(matched.data, "weights")]],
                                                base.form = base.form,
                                                method = method,
                                                specifications = specifications,
                                                cutpoints = cutpoints,
                                                verbose = FALSE)[1:2])
        })
      }
    }

    out

  }, cl = cl)

  coef.un <- res[[1]][1]
  CI.un <- res[[1]][2:3]
  mod.dependence.un <- NULL
  if (method != "none") {
    mod.dependence.un <- res[[1]][4:5]
  }

  coefs <- unlist(lapply(res[-1], `[`, 1))
  CIs <- lapply(res[-1], `[`, 2:3)
  attr(CIs, "CIlevel") <- 1 - alpha

  mod.dependence <- NULL
  if (method != "none") {
    mod.dependence <- lapply(res[-1], `[`, 4:5)
  }

  if (verbose) {
    cat("Done!\n")
  }

  out <- list(Xs = frontier.object$frontier$Xs[point.inds],
              coefs = coefs,
              CIs = CIs,
              mod.dependence = mod.dependence,
              QOI = frontier.object$QOI,
              method = method,
              n = frontier.object$n,
              treatment = treatment,
              covariates = covariates,
              base.form = base.form,
              un = list(coef = coef.un,
                        CI = CI.un,
                        mod.dependence = mod.dependence.un),
              call = call)

  if (method == "extreme-bounds") {
    attr(out, "specifications") <- lapply(specifications, function(s) {
      if (is.call(s)) deparse1(s) else s
    })
  }

  class(out) <- "frontierEstimates"
  return(out)
}

print.frontierEstimates <- function(x, ...){
  cat("A frontierEstimates object\n")
  cat(paste0("- quantity of interest: ", x$QOI, "\n"))
  cat(paste0("- model sensitivity method: ", switch(x$method, "none" = "none",
                                                    "extreme-bounds" = "extreme bounds",
                                                    "athey-imbens" = "Athey-Imbens"),
             "\n"))
  if (x$method == "extreme-bounds") {
    cat(paste0("   - number of specifications: ", attr(x$method, "model.dependence.ests"), "\n"))
  }
  else if (x$method == "athey-imbens") {
    cat(paste0("   - cutpoint method: ", switch(attr(x$method, "cutpoint.method"),
                                                "segmented" = "segmented regression",
                                                attr(x$method, "cutpoint.method")), "\n"))
  }
  cat(paste0("- number of estimates: ", length(x$Xs), "\n"))
  cat(paste0("- treatment: ", x$treatment, "\n"))
  cat(paste0("- covariates: ", paste0(x$covariates, collapse = ", "), "\n"))
  cat(paste0("- outcome model: ", deparse1(x$base.form), "\n"))

  invisible(x)
}