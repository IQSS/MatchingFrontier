\name{makeFrontier.matchit}
\alias{makeFrontier.matchit}

\title{
Compute the balance-sample size frontier for a \code{matchit} object
}
\description{
\code{makeFrontier()} computes the balance-sample size frontier for a \code{matchit} object aftr 1:1 matching. The frontier corresponds to the relationship between the number of matched pairs dropped and the resulting balance in the matched sample. When the propensity score is used to match, this can also correspond to the realtionship between the width of a hypothetical caliper on the propensity score and the resulting balance. This helps identify the optimal caliper and determine whether the propensity score paradox described in King and Nielsen (2019), in which tightening the caliper worsens balance, is in effect.
}
\usage{
\method{makeFrontier}{matchit}(x,
             metric = 'dist',
             breaks = NULL,
             distance.mat = NULL,
             data = NULL,
             verbose = TRUE, ...)

}
\arguments{
  \item{x}{
a \code{matchit} object; the output of a call to \pkgfun2{MatchIt}{matchit}{MatchIt::matchit} in which 1:1 matching (with or without replacement) was used.
}
  \item{metric}{
the imbalance metric under consideration; allowable options include \code{'dist'}, \code{'L1'}, \code{'L2'}, and \code{'energy'}. See \fun{makeFrontier}. Default is \code{'dist'}.
}
  \item{breaks}{
when \code{metric} is \code{'L1'} or \code{'L2'}, a named list containing a single number giving the number of bins for each or a string corresponding to a method of computing the number of bins for each covariate. See \fun{makeFrontier} for allowable options. Ignored for other \code{metric}s.
}
  \item{distance.mat}{
when \code{metric} is \code{'dist'} or \code{'energy'}, the distance measure used to define the distance between units. The default for \code{metric = 'dist'} is \code{"mahalanobis"} and for \code{metric = 'energy'} is \code{"scaled_euclidean"}. See \fun{makeFrontier}. Abbreviations allowed. Ignored for other \code{metric}s.
}
  \item{data}{
an optional data frame containing the variables used in the original matching. In most cases, does not need to be specified, but it is always safer if it is. Passed to \pkgfun{MatchIt}{match.data}.
}
  \item{verbose}{
\code{logical}; whether to print additional information about the frontier constructing process.
}
  \item{\dots}{
arguments passed to other methods (ignored).
}
}
\details{
\code{makeFrontier.matchit()} creates a balance-sample size "frontier" from the supplied \code{matchit} object by dropping pairs one-by-one based on the distance between them and computing a balance statistic at each point. Unlike other frontiers, which are formed by dropping units that lower the imbalance measure the most, \code{matchit} frontiers are formed by dropping pairs with the greatest distance between them, where the distance corresponds to the distance used to initially create the matches in \code{matchit()} (i.e., the porpensity score difference or Mahalanobis distance, whichever is requested). This is similar to progressively applying a tighter and tighter caliper and considering how balance changes as the caliper is tightened. Because of this, imbalance may increase as pairs are dropped; indeed, this is the phenomenon this function is used to examine.

\code{makeFrontier.matchit()} is only compatible with \code{matchit} objects that result from performing 1:1 matching (with or without replacement), which includes after using \code{method = "nearest"}, \code{method = "optimal"}, or \code{method = "genetic"} in \code{matchit()} with \code{ratio = 1}. The \code{distance} argument to \code{matchit()} cannot have been a matrix, but can be a numeric vector corresponding to propensity scores or a string requesting propensity scores or a distance matrix. When the \code{mahvars} argument is used to perform Mahalanobis distance matching (with a propensity score otherwise estimated), the propensity score difference is used to determine the order the pairs will be dropped.

Because the frontier is computed using the existing matched dataset, manually creating a frontier by progressively tightening the caliper may not yield the same results unless matching with replacement. When a caliper is supplied to \code{matchit()}, the resulting matched sample will have the caliper applied already, so \code{makeFrontier()} will only produce the frontier for calipers more restrictive than the one used in the original matching.

\code{makeFrontier.matchit()} extracts the treatment, covariates, propensity score (if any), and estimand (QOI) from the supplied \code{matchit} object. If Mahalanobis distance matching was used, the Mahalanobis distance between members of each pair is re-computed. The user must specify which imbalance metric is to be used to assess balance in the resulting matched sample (and any other arguments used to compute it). Because both members of each pair are progressively dropped, the estimand after dropping some pairs corresponds to the "feasible" average treatment effect (in the treated); see \fun{makeFrontier} for more details.

The covariates used to compute the imbalance metrics are those extracted from the \code{matchit} formula using \fun{all.vars}; that is, if, e.g., \code{log(X)} is present in the model formula, \code{X} will be used in computing the imbalance metric, not \code{log(X)}.
}
\value{
A \code{MatchItfrontier} object, which inherits from \code{matchFrontier}.

\item{frontier}{the matching frontier, which contains four components:
\itemize{
\item \code{drop.order}: a list containing the indices of the units dropped at each point on the frontier. The first entry will be empty, indicating that no units have been dropped.
\item \code{Xs}: a vector containing the cumulative number of units dropped at each point on the frontier. The first value will be 0.
\item \code{Ys}: a vector containing the value of the imbalance metric at each point on the frontier.
\item \code{Y.origin}: for bin- and energy-based frontiers, the value of the imbalance metric prior to matching (i.e., before the first point on the frontier).
}}
\item{treatment}{the name of the treatment variable.}
\item{QOI}{the QOI; here, \code{"FSATT"}.}
\item{metric}{the argument supplied to \code{metric}.}
\item{data}{the dataset containing the units. This is the result of call \code{MatchIt::match.data()} on the \code{matchit} object with \code{drop.unmatched = FALSE}.}
\item{match.on}{a vector of the names of the covariates used in the matching.}
\item{matched.to}{the indices of the units to which each unit is paired. Control units will have \code{NA} values. This is formed by processing the \code{match.matrix} component of the \code{matchit} object.}
\item{distance}{the pairwise distances between each pair of units use in the original matching. When propensity score matching is used, these values are scaled by the standard deviation of the propensity score. Note this is different from (but is computed from) the \code{distance} component in the supplied \code{matchit} object, which contains the propensity scores, and is totally distinct from the \code{distance.mat} argument.}
\item{call}{the original call to \code{makeFrontier()}.}
\item{n}{the number of treated units present at the first point in the frontier.}
}
\references{
King, G., Lucas, C., & Nielsen, R. A. (2017). The Balance-Sample Size Frontier in Matching Methods for Causal Inference. \emph{American Journal of Political Science}, 61(2), 473-489. \doi{10.1111/ajps.12272}

King, G., & Nielsen, R. (2019). Why Propensity Scores Should Not Be Used for Matching. Political Analysis, 1-20. \doi{10.1017/pan.2019.11}
}
\author{
Noah Greifer
}

\seealso{
# Help pages for other matchit methods

\fun{makeFrontier} for creating frontiers without \pkg{MatchIt}

\fun{plot.matchFrontier} for plotting the frontier.

\fun{estimateEffects} for estimating effects along the frontier.

vignette("MatchingFrontier") for a worked example.
}
\examples{
data("lalonde", package = "MatchIt")

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
plot(fm)
plot(fm, axis = "caliper")
}
