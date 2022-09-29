\name{makeFrontier.matchit}
\alias{makeFrontier.matchit}

\title{
Compute the balance-sample size frontier with MatchIt
}
\description{
\code{makeFrontier.matchit()} computes the balance-sample size frontier after propensity score matching using \pkg{MatchIt}, in particular to investigate the propensity score paradox described in King and Nielsen (2019). Pairs are dropped based on their propensity score distance, and the frontier concerns an imbalance metric chosen by the user. The paradox occurs when discarding additional pairs increases imbalance, even when only the most distant pairs are dropped. Unlike \fun{makeFrontier}, which uses the same metric for matching and evaluating matches at each point on the frontier, \code{makeFrontier.matchit()} allows one to perform matching based on propensity scores and evaluate the matches at each point on the frontier using a different imbalance metric.
}
\usage{
\method{makeFrontier}{matchit}(x, metric = 'mahal',
                     breaks = NULL, distance.mat = NULL,
                     verbose = TRUE)
}

\arguments{
  \item{x}{
a \code{matchit} object; the output of a call to \pkgfun2{MatchIt}{matchit}{MatchIt::matchit}.
}
  \item{metric}{
the imbalance metric to be minimized; allowable options include \code{'L1'}, \code{'L1median'}, \code{'L2'}, \code{'L2median'}, \code{'mahal'}, \code{'euclid'}, \code{'custom'}, and \code{'energy'}. See Details. Default is \code{'mahal'}.
}
  \item{breaks}{
when \code{metric} is \code{'L1'} or \code{'L2'}, a named list containing a single number giving the number of bins for each or a string corresponding to a method of computing the number of bins for each covariate; for example, \code{list(X1 = 4, X3 = "sturges")}. Allowable strings include \code{"sturges"}, \code{"scott"}, and \code{"fd"}, which use the functions \pkgfun2{grDevices}{nclass}{nclass.Sturges}, \pkgfun2{grDevices}{nclass}{nclass.scott}, and \pkgfun2{grDevices}{nclass}{nclass.FD}, respectively. The default is \code{"sturges"} for variables that are not listed or if no argument is supplied. Can also be a single value to be applied to all numeric variables. Similar to the \code{cutpoints} argument of \code{MatchIt::matchit()} with \code{\link[MatchIt:method_cem]{method = "cem"}}. Ignored for other \code{metric}s.
}
  \item{distance.mat}{
when \code{metric} is \code{'custom'} or \code{'energy'}, a distance matrix used to define the distance between units. For \code{metric = 'custom'}, should be a distance matrix with one row for each treated unit and one column for each control unit. For \code{metric = 'energy'}, should be a full N by N distance matrix with a row and column for each unit. Optional when \code{metric = 'energy'}; if not specified, the Euclidean distance computed on the scaled covariates will be used instead. See Details.
}
  \item{verbose}{
\code{logical}; whether to print additional information about the frontier constructing process.
}
  \item{\dots}{
arguments passed to other methods (ignored).
}
}
\details{
\code{makeFrontier.matchit()} creates a balance-sample size frontier from the supplied \code{matchit} object by dropping pairs one-by-one based on the distance between them and computing a balance statistic at each point. Unlike other frontiers, which are formed by dropping units that lower the imbalance measure the most, \code{matchit} frontiers are formed by dropping pairs with the greatest distance between them, where the distance corresponds to the propensity score difference used to initially create the matches in \code{matchit()}. This is similar to progressively applying a tighter and tighter caliper and considering how balance changes as the caliper is tightened. Because of this, imbalance may increase as pairs are dropped; indeed, this is the phenomenon this function is used to examine.

\code{makeFrontier.matchit()} is only compatible with \code{matchit} objects that result from performing 1:1 matching on the propensity score; matching can have been done with or without replacement. Because the frontier is computed using the existing matched dataset, manually creating a frontier by progressively tightening the caliper may not yield the same results.

Because \code{matchit()} only performs 1:1 matching for the ATT, the QOI is automatically set to the FSATT.

The covariates used to to compute the imbalance metrics are those extracted from the \code{matchit} formula using \fun{all.vars}; that is, if, e.g., \code{log(X)} is present in the model formula, \code{X} will be used in computing the imbalance metric, not \code{log(X)}.
}
\value{
A \code{MatchItfrontier} object, which inherits from \code{matchFrontier}.

\item{frontier}{the matching frontier, which contains four components:
\itemize{
\item \code{drop.order}: a list containing the indices of the units dropped at each point on the frontier. The first entry will be empty, indicating that no units have been dropped.
\item \code{Xs}: a vector containing the cumulative number of units dropped at each point on the frontier. The first value will be 0.
\item \code{Ys}: a vector containing the value of the imbalance metric at each point on the frontier.
\item \code{distance.mat}: when using a pair distance-based frontier or energy distance-based frontier, the distance matrix used to compute the imbalance statistic.
}}
\item{treatment}{the name of the treatment variable.}
\item{QOI}{the QOI; here, \code{"FSATT"}.}
\item{metric}{the argument supplied to \code{metric}.}
\item{dataset}{the dataset containing the units. This is the result of call \code{MatchIt::match.data()} on the \code{matchit} object with \code{drop.unmatched = FALSE}.}
\item{match.on}{a vector of the names of the covariates used in the matching.}
\item{matched.to}{the indices of the units to which each unit is paired. Control units will have \code{NA} values. This is formed by processing the \code{match.matrix} component of the \code{matchit} object.}
\item{distance}{the pairwise distances between each pair of units scaled by the standard deviation of the propensity score; note this is different from (but is computed from) the \code{distance} component in the supplied \code{matchit} object, which contains the propensity scores.}
\item{call}{the original call to \code{makeFrontier()}.}
\item{n}{the number of treated units present at the first point in the frontier.}
}
\references{
King, G., Lucas, C., & Nielsen, R. A. (2017). The Balance-Sample Size Frontier in Matching Methods for Causal Inference. \emph{American Journal of Political Science}, 61(2), 473-489. \doi{10.1111/ajps.12272}

King, G., & Nielsen, R. (2019). Why Propensity Scores Should Not Be Used for Matching. Political Analysis, 1–20. \doi{10.1017/pan.2019.11}
}
\author{
Noah Greifer
}

\seealso{
# Help pages for other matchit methods

\fun{makeFrontier} for creating frontiers without \pkg{MatchIt}

vignette("MatchingFrontier") for a worked example.
}
\examples{
data("lalonde", package = "MatchIt")

#1:1 nearest neighbor propensity score matching
#without replacement
m.out <- matchit(treat ~ age + educ + married + re74,
                   data = lalonde, method = "nearest",
                   ratio = 1, replace = FALSE)
m.out

#Assess the energy distance as pairs are dropped
fm <- makeFrontier(m.out, metric = "energy")

fm
plot(fm)
summary(fm)
}