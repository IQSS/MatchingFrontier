\name{plot.matchFrontier}
\alias{plot.matchFrontier}

\title{
Plot the Balance-Sample Size Frontier
}
\description{
Plots the balance-sample size frontier for the supplied \code{matchFrontier} object. Can also plot covariate-specific mean differences, means, and other statistics to assess balance on individual covariates across the frontier.
}
\usage{
\method{plot}{matchFrontier}(x, covs = NULL, stat = NULL,
     n.estimated, axis = "ndrop", ...)
}

\arguments{
  \item{x}{
a \code{matchFrontier} object; the output of a call to \fun{makeFrontier}.
}
  \item{covs}{
the names or covariates for which balance statistics are to be displayed, entered either as a character vector or one-sided \code{formula}. If \code{NULL} (the default), will display the frontier for the balance metric used in constructing the frontier if \code{stat = NULL} and otherwise will display the balance measure named in \code{stat} for all matching variables.
}
  \item{stat}{
if \code{covs} is not \code{NULL}, the statistic to display across the frontier. Allowable options include \code{"std-diff"} for standardized mean differences (the default), \code{"diff"} for raw mean differences, \code{"ks"} for Kolmogorov-Smirnov statistics, \code{"std-mean"} for standardized means, \code{"mean"} for raw means, \code{"ks-target"} for sample-target Kolmogorov-Smirnov statistics, and \code{"ess"} for effect sample sizes. See Details. Abbreviations allowed.
}
  \item{n.estimated}{
the number of points along the frontier for which to compute balance statistics. Computing statistics at every point on the frontier can be time-consuming for large frontiers, especially when computing Kolmogorov-Smirnov statistics, so using values less than the number of points can save time at the expense of providing less detail in the frontier plot. The default is 250 or the number of points, whichever is smaller. Ignored when \code{covs} and \code{stat} are both \code{NULL}.
}
  \item{axis}{
whether to display the lower x-axis as the number or units dropped (\code{"ndrop"}) or the number of units remaining (\code{"n"}). Regardless, the other scale will be on the upper axis. When \code{x} is a \code{MatchItFrontier} object (i.e., resulting from \fun{makeFrontier.matchit}), can also be \code{"caliper"}, displaying the size of the hypothetical caliper corresponding to dropping the given dropped units. When \code{"caliper"}, the caliper is scaled by the standard deviation of the propensity scores. Default is \code{"ndrop"}; abbreviations allowed.
}
  \item{\dots}{
if \code{covs} and \code{stat} are both \code{NULL}, further arguments passed to \pkgfun2{ggplot2}{geom_line}{ggplot2::geom_line} to control the appearance of the plot (e.g., \code{color}, \code{size}, etc.). Ignored otherwise.
}
}
\details{
\code{plot.matchFrontier()} uses \pkgfun2{ggplot2}{geom_line}{ggplot2::geom_line} to display the balance-sample size frontier. Unless \code{axis = "caliper"}, the plot has two x-axes: one indicates the number of units dropped, and the other indicates the number of units remaining. Which group this "number of units" refers to depends on the \code{QOI} supplied to the original call to \fun{makeFrontier}.

When \code{covs} and \code{stat} are both \code{NULL}, the y-axis indicates the value of the imbalance metric at the given sample size; otherwise, it indicates the value of the statistic for the covariates supplied to \code{covs} depending on the arguments to \code{stat}.

When \code{stat} is not \code{NULL}, one of seven plots can be produced:
\enumerate{
\item standardized mean differences between treatment groups (\code{stat = "std-diff"}, the default).
\item raw mean differences between treatment groups (\code{stats = "diff"})
\item Kolmogorov-Smirnov statistics between the treatment groups (\code{stat = "ks"})
\item standardized means, which are equivalent to standardized mean differences between the full matched sample and the target sample (\code{stat = "std-mean"})
\item raw means in the matched sample (\code{stat = "mean"})
\item Sample-target Kolmogorov-Smirnov statistics, the Kolmogorov-Smirnov statistic between the full matched sample and the target sample (\code{stat = "ks-target"})
\item effective sample sizes in the treated and control groups (\code{stat = "ess"})
}

The target sample depends on the argument to \code{QOI} in the original call to \fun{makeFrontier}: when \code{"SATE"} or \code{"FSATE"}, the target sample is the full sample prior to matching; when \code{"SATT"} or \code{"FSATT"}, the target sample is the treated group prior to matching. The target sample also determines the standardization factor when computing standardized means or mean differences. For \code{MatchItFrontier} objects, the target sample is the treated group prior to matching (because the FSATT is always the target estimand).

When \code{stat = "mean"}, a dashed line will appear on the plot for each covariate indicating its mean in the target sample. Otherwise, an additional point will be plotted indicating the statistic computed prior to matching; for energy distance- and bin-based frontiers, this is the start of the frontier, but for pair distance-based frontier, the frontier starts after matching, so the additional point corresponds to balance prior to the initial point of the frontier.

When \code{stat = "ess"}, the effective sample size (ESS) is computed for each treatment group as \eqn{(\sum w)^2/\sum w^2} within each treatment group where \eqn{w} are the matching weights at the given point on the frontier. \code{covs} will be ignored because the ESS is only a function of the weights. In some cases, it is possible for the ESS to increase even as the number of units remaining in the sample decreases.
}
\value{
A \code{ggplot} object that can be further manipulated using \pkg{ggplot2} functionality. For example, to zoom in on an area of the plot, \pkgfun{ggplot2}{coord_cartesian} can be used.
}

\seealso{
\fun{makeFrontier}, \fun{makeFrontier.matchit}, \fun{summary.matchFrontier}, \fun{plot.frontierEstimates}
}
\examples{
data("lalonde", package = "MatchIt")

#Pair distance frontier for FSATT
f1 <- makeFrontier(treat ~ age + educ + married + re74,
                   data = lalonde, QOI = "FSATT",
                   metric = "dist", verbose = FALSE)

#Plot the balance metric frontier
plot(f1)

#Plot standardized mean differences
plot(f1,
     covs = ~age + educ + married + re74,
     stat = "std-diff")

#Plot standardized deviations from the mean
#of the treated group in the original
#sample (prior to matching)
plot(f1,
     covs = ~age + educ + re74,
     stat = "std-mean")

#Plot effective samples
plot(f1, stat = "ess")

#Plot covariate means across the frontier;
#make sure variables are on the same scale!
plot(f1,
     covs = ~married + nodegree,
     stat = "mean")

#Plotting a frontier using MatchIt
library("MatchIt")

#1:1 nearest neighbor propensity score matching
#without replacement
m.out <- MatchIt::matchit(
  treat ~ age + educ + married + re74,
  data = lalonde, method = "nearest",
  ratio = 1, replace = FALSE
)
m.out

#Assess the energy distance as pairs are dropped
fm <- makeFrontier(m.out, metric = "energy")

fm

#Plot the balance metric frontier
plot(fm)

#Plot the frontier with caliper width on the x-axis
plot(fm, axis = "caliper")

#Plot Kolmogorov-Smirnov statistics
plot(fm,
     covs = ~age + educ + married + re74,
     stat = "ks")
}
