\name{frontier_to_matchit}
\alias{frontier_to_matchit}

\title{
Generate a \code{matchit} object a a point along the frontier
}
\description{
\code{generateDataset()} generates a \code{matchit} object at a given point along the frontier identified either by the number of units remaining or the number of units dropped at that point on the frontier. All \pkg{MatchIt} functions that work with \code{matchit} objects can be used on the output, such as \pkgfun{MatchIt}{summary.matchit} for checking balance.
}
\usage{
frontier_to_matchit(frontier.object, N, Ndrop)
}
\arguments{
  \item{frontier.object}{
a \code{matchFrontier} object; the output of a call to \fun{makeFrontier}.
}
  \item{N}{
the number of units remaining in the sample at the desired point on the frontier. Exactly one of \code{N} or \code{Ndrop} must be specified.
}
  \item{Ndrop}{
the number of units dropped in the sample at the desired point on the frontier. Exactly one of \code{N} or \code{Ndrop} must be specified.
}
}
\details{
The argument to \code{N} or \code{Ndrop} depend on the quantity of interest (QOI) supplied to the original call to \fun{makeFrontier}. When \code{QOI} was \code{"SATE"} or \code{"FSATE"}, they correspond to the total number of units remaining or dropped. When \code{QOI} was \code{"SATT"}, they correspond to the number of control units remaining or dropped (since no treated units are dropped). When \code{QOI} was \code{"FSATT"}, they correspond to the number of treated units remaining or dropped. See also the \code{n} component of the \code{matchFrontier} object at \fun{makeFrontier}. When using \fun{summary.matchFrontier}, the carrot printed underneath the summary table indicates which group \code{N} and \code{Ndrop} refer to.
}

\value{
A \code{matchit} object. See \pkgfun{MatchIt}{matchit} for further details. The following components are included in the output:

\item{match.matrix}{a matrix containing information on which units are matched to which. Only present when using a pair distance=based         frontier.}
\item{weights}{the resulting matching weights. Dropped units will have a weight of 0.}
\item{X}{a data frame of the covariates used in matching.}
\item{call}{the \code{makeFrontier()} call.}
\item{info}{information on the matching method and distance measures used, to be processed by \code{print.matchit()}.}
\item{estimand}{the target estimand. Will be \code{"ATE"} when \code{QOI} is \code{"SATE"} or \code{"FSATE"}, and will be \code{"ATT"} when     \code{QOI} is \code{"SATT"} or \code{"FSATT"}.}
\item{formula}{a formula relating the treatment to the matching covariates.}
\item{treat}{a vector of treatment status converted to zeros (0) and ones (1) if not already in that format.}
\item{nn}{a matrix of the sample sizes of the treated and control groups before and after matching.}
}
\seealso{
\fun{generateDataset}, \pkgfun2{MatchIt}{matchit}{MatchIt::matchit}
}
\examples{
data("lalonde", package = "MatchIt")

#Pair distance frontier for FSATT
f1 <- makeFrontier(treat ~ age + educ + married + re74,
                   data = lalonde, QOI = "FSATT",
                   metric = "dist", verbose = FALSE)
plot(f1)

#Convert to matchit object after dropping 120 treated units
m1 <- frontier_to_matchit(f1, Ndrop = 120)

#Usual MatchIt functions work
m1
summary(m1)
}
