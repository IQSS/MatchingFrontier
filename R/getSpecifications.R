getSpecifications <- function(base.form, covariates, data, N){
    #Note: in order for poly_() to work, it must be wrapped in I(),
    #and form.list must be generated in this loop, not lapply().
    form.list <- vector("list", N)
    no.poly.covs <- covariates[vapply(covariates, function(cov) {
        is.character(data[[cov]]) ||
            is.logical(data[[cov]]) ||
            is.factor(data[[cov]]) ||
            length(unique(data[[cov]])) <= 3},
        logical(1L))]

    for (i in seq_len(N)) {
        if (i == 1) {
            form.list[[i]] <- deparse1(base.form)
        }

        #Add polynomials to model
        covs <- sample(covariates, sample(seq_along(covariates), 1))
        cov.polys <- vapply(setdiff(covs, no.poly.covs), function(cov) {
            if (sample(c(TRUE, FALSE), 1)) paste0("I(", cov, "^2) + I(", cov, "^3)")
            else paste0("I(", cov, "^2)")
        }, character(1L))

        if (length(covs) > 1) {
            #Add (a random number of) interactions to model
            possible.interactions <- combn(covs, 2, simplify = FALSE)
            cov.cols <- sample(seq_along(possible.interactions), sample(seq_along(possible.interactions), 1))
            cov.interactions <- vapply(cov.cols, function(cov.ind) {
                paste(possible.interactions[[cov.ind]], collapse = ' * ')
            }, character(1L))

            formula <- paste(deparse1(base.form),
                             '+',
                             paste(c(covs, cov.polys, cov.interactions), collapse = ' + ')
            )
        }
        else {
            formula <- paste(deparse1(base.form),
                             '+',
                             paste(c(covs, cov.polys), collapse = ' + ')
            )
        }

        form.list[[i]] <- formula
    }
    form.list
}
