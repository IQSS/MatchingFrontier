\name{estimateEffects}
\alias{estimateEffects}
\alias{print.frontierEstimates}

\title{
Estimate Effects on the Frontier
}
\description{
\code{estimateEffects()} estimates the effect of the treatment on the supplied outcome at points along the frontier, optionally including model dependence bounds.
}
\usage{
estimateEffects(frontier.object,
                outcome,
                base.form = NULL,
                n.estimated = 250,
                N, Ndrop,
                method = c("none", "extreme-bounds",
                           "athey-imbens"),
                model.dependence.ests = 100,
                specifications = NULL,
                cutpoints = NULL,
                cutpoint.method = c("mean", "median",
                                    "segmented"),
                seed = NULL,
                alpha = 0.05,
                verbose = TRUE,
                cl = NULL,
                ...)

\method{print}{frontierEstimates}(x, ...)
}

\arguments{
  \item{frontier.object}{
a \code{matchFrontier} object; the output of a call to \fun{makeFrontier}.
}
  \item{outcome}{
the name of the outcome variable in the dataset supplied to \fun{makeFrontier}. Not required if \code{base.form} is supplied.
}
  \item{base.form}{
a model formula with the outcome on the left hand side and the treatment (and optionally covariates) on the right hand side. The model is used to estimate treatment effects at each point along the frontier as well as their model sensitivity (if requested). If omitted, will use a simple regression of the supplied outcome variable on the treatment.
}
  \item{n.estimated}{
the number of points along the frontier to estimate. Estimating effects at every point on the frontier can be time-consuming for large frontiers, especially when model dependence bounds are also computed, so using values less than the number of points can save time at the expense of providing less detail in the frontier estimates plot. The default is 250 or the number of points, whichever is smaller.
}
  \item{N, Ndrop}{
the number of units remaining or dropped, respectively, in the sample at the desired points on the frontier for which effects are to be estimated. Should be a vector of 2 values containing the start and end of the range of the desired points. Exactly one of \code{N} or \code{Ndrop} can be specified. If both are omitted, will estimate effects along the full range of the frontier.
}
  \item{method}{
the method of computing model dependence bounds. Can be one of \code{"extreme-bounds"} or \code{"athey-imbens"} for the extreme bounds or Athey-Imbens procedures, respectively, or \code{"none"} to not compute model dependence bounds. Abbreviations allowed. Default is no model dependence bounds. See \fun{modelDependence}. When using \code{metric = 'dist'} in \code{makeFrontier()}, only \code{"extreme-bounds"} is allowed.
}
  \item{model.dependence.ests, specifications, cutpoints, cutpoint.method, seed}{
arguments passed to \fun{modelDependence} when \code{method} is not \code{"none"}.
}
  \item{alpha}{
the alpha value used to select the critical test statistic used in computing confidence intervals. Default is .05 for pointwise 95\% confidence intervals.
}
  \item{verbose}{
\code{logical}; whether to print additional information about the process of estimating effects and sensitivity bounds (if requested). A progress bar will be displayed if \code{TRUE}.
}
  \item{cl}{
a cluster object created by \code{parallel::makeCluster()}, or an integer to indicate number of child-processes (integer values are ignored on Windows) for parallel evaluations. See \pkgfun{pbapply}{pblapply} for details.
}
  \item{x}{
a \code{frontierEstimates} object; the output of a call to \code{estimateEffects()}.
}
  \item{\dots}{
for \code{estimateEffects()}, \code{prop.estimated}, which determines the proportion of points along the frontier to estimate, can be passed instead of \code{n.estimated}. Prior versions of \code{estimateEffects()} used \code{prop.estimated}, and this will continue to be supported.

Ignored for \code{print()}.
}
}
\details{
\code{estimateEffects()} works by creating a dataset at each point along the frontier using the units that remain and fitting the supplied outcome model to those datasets, returning the point estimate, confidence interval bounds, and, if requested, model dependence bounds. The outcome model fit is that supplied to \code{base.form}, and the coefficient on the treatment is used at the treatment effect estimate. Currently, only linear models can be used and the treatment effect corresponds to the difference in means. Models will only be fit for datasets with 15 or more units.

Confidence intervals are computed using HC3 robust standard errors as implemented in \pkg{sandwich}. Note that robust standard errors may not be accurate for matching with replacement; only the Abadie-Imbens (2006) standard errors implemented in the \pkg{Matching} package are accurate. Including covariates in the outcome model improves the performance of the standard errors, and so is recommended here and in general (Hill & Reiter, 2006).

The model dependence bounds are computed using the same machinery as is used with \fun{modelDependence} and uses many of the same arguments.
}
\value{
A \code{frontierEstimates} object with the following components:
\item{Xs}{a vector containing the cumulative number of units dropped at each point on the frontier for which effects were estimated..}
\item{coefs}{a vector containing the treatment effect estimates at each point on the frontier for which effects were estimated.}
\item{CIs}{a list containing the upper and lower confidence interval limits at each point on the frontier for which effects were estimated.}
\item{mod.dependence}{a list containing the upper and lower model dependence bounds at each point on the frontier for which effects were estimated.}
\item{QOI}{the argument supplied to \code{QOI} in the original call to \code{makeFrontier()}.}
\item{method}{the argument supplied to \code{method}.}
\item{n}{the number of units present at the first point in the frontier (i.e., the original sample). For the FSATT, this is the number of treated units; for the SATT, this is the number of control units.}
\item{treatment}{the name of the treatment variable.}
\item{covariates}{the names of the covariates matched on in the original call to \code{makeFrontier()}.}
\item{base.form}{the formula used for the outcome models.}
\item{un}{a list contain three elements computed in the unadjusted sample: the effect estimate (\code{coef}), the confidence interval (\code{CI}), and the model dependence bounds (\code{mod.dependence}).}
\item{call}{the original function call.}

If \code{method = "extreme-bounds"}, the output will additionally have the attribute \code{"specifications"} containing the model specifications that were used in computing the model dependence bounds.
}
\references{
Abadie, A., & Imbens, G. W. (2006). Large Sample Properties of Matching Estimators for Average Treatment Effects. Econometrica, 74(1), 235-267. https://doi.org/10.1111/j.1468-0262.2006.00655.x

Hill, J., & Reiter, J. P. (2006). Interval estimation for treatment effects using propensity score matching. Statistics in Medicine, 25(13), 2230-2256. https://doi.org/10.1002/sim.2277

King, G., Lucas, C., & Nielsen, R. A. (2017). The Balance-Sample Size Frontier in Matching Methods for Causal Inference. \emph{American Journal of Political Science}, 61(2), 473-489. \doi{10.1111/ajps.12272}

}

\seealso{
\fun{makeFrontier} for constructing the frontier, \fun{plot.frontierEstimates} for plotting effect estimates and bounds along the frontier, \fun{generateDataset} for extracting a dataset at one point on the frontier, \fun{modelDependence} for computing model dependence bounds for a single dataset, \code{vignette("estimating-effects", package = "MatchIt")} for instructions on estimating effects after matching.
}
\examples{
data("lalonde", package = "MatchIt")

#Pair distance frontier for FSATT
f1 <- makeFrontier(treat ~ age + educ + married + re74,
                   data = lalonde,
                   QOI = "FSATT",
                   metric = "dist",
                   verbose = FALSE)

f1

e1 <- estimateEffects(f1,
                      base.form = re78 ~ treat + age +
                                    educ + married + re74,
                      n.estimated = 20, #Should be higher
                      method = "extreme-bounds",
                      model.dependence.ests = 51)
e1
plot(e1, band = "model-dependence")
plot(e1, band = "confidence")

summary(e1, Ndrop = c(10, 85))

# Only estimate effects for a subset of the forntier
# (points with remaining sample sizes between 50 and
# 100)
e2 <- estimateEffects(f1,
                      base.form = re78 ~ treat + age +
                                    educ + married + re74,
                      N = c(50, 100))
e2
plot(e2)
}
