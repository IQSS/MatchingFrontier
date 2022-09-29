\name{summary.frontierEstimates}
\alias{summary.frontierEstimates}
\alias{print.summary.frontierEstimates}

\title{
Summarize a \code{frontierEstimates} object
}
\description{
\code{summary()} displays the effect estimates, confidence bounds, and, if present, model dependence bounds at user-specified points along the forntier.
}
\usage{
\method{summary}{frontierEstimates}(object, N, Ndrop, ...)

\method{print}{summary.frontierEstimates}(x, digits, ...)
}

\arguments{
  \item{object}{
a \code{frontierEstimates} object; the output of a call to \fun{estimateEffects}.
}
  \item{N}{
the number of units remaining in the sample at the desired point(s) on the frontier. Exactly one of \code{N} or \code{Ndrop} can be specified. If both are omitted, will display effect estimates at the first, last, and middle point along the frontier.
}
  \item{Ndrop}{
the number of units dropped in the sample at the desired point(s) on the frontier. Exactly one of \code{N} or \code{Ndrop} can be specified. If both are omitted, will display effect estimates at the first, last, and middle point along the frontier.
}
  \item{x}{
a \code{summary.frontierEstimates} object; the output of a call to \code{summary.frontierEstimates()}.
}
  \item{digits}{
the number of significant digits to display the estimates. Passed to \fun{format}.
}
  \item{\dots}{
ignored.
}
}
\details{
\code{summary()} simply extracts the effect estimates and confidence nad model dependence bounds from the \code{frontierEstimates} objec at the requested pointst; no further calculation is performed. Whenever the number of points along the frontier for which the effect is estimated in the call to \fun{estimateEffects} is less than the total number of points on the frontier, it is possible that the specified points on the frontier do not have effects estimated, in which case the estimates at the nearest points will be displayed.
}
\value{
A \code{summary.frontierEstimates} object, which contains the following components:

\item{Est}{the effect estimates at the user-specified points on the frontier.}
\item{CIlower}{the confidence interval lower bound at the user-specified points on the frontier.}
\item{CIupper}{the confidence interval upper bound at the user-specified points on the frontier.}
\item{CIlevel}{the percentiles corresponding to the confidence level.}
\item{MDlower}{the model dependence lower bound at the user-specified points on the frontier if requested in the call to \fun{estimateEffects}.}
\item{MDlower}{the model dependence upper bound at the user-specified points on the frontier if requested in the call to \fun{estimateEffects}.}
}

\seealso{
\fun{estimateEffects}, \fun{plot.frontierEstimates}
}
\examples{
#See example at help("estimateEffects")
}