\name{summary.matchFrontier}
\alias{summary.matchFrontier}
\alias{print.summary.matchFrontier}

\title{
Summarize a \code{matchFrontier} object
}
\description{
\code{summary()} displays the sample sizes and imbalance metrics at the beginning of the frontier, the end of the frontier, and the point at which the imbalance metric is lowest, or at user-specified points along the frontier.
}
\usage{
\method{summary}{matchFrontier}(object, N, Ndrop, ...)

\method{print}{summary.matchFrontier}(x, digits, ...)
}

\arguments{
  \item{object}{
a \code{matchFrontier} object; the output of a call to \fun{makeFrontier}.
}
  \item{N}{
the number of units remaining in the sample at the desired point(s) on the frontier. Exactly one of \code{N} or \code{Ndrop} can be specified.
}
  \item{Ndrop}{
the number of units dropped in the sample at the desired point(s) on the frontier. Exactly one of \code{N} or \code{Ndrop} can be specified.
}
  \item{x}{
a \code{summary.matchFrontier} object; the output of a call to \code{summary.matchFrontier()}.
}
  \item{digits}{
the number of significant digits to display the imbalance metric. Passed to \fun{format}.
}
  \item{\dots}{
ignored.
}
}
\details{
The lowest value of the imbalance metric may not occur at the end of the frontier (i.e., with the smallest remaining sample size). \code{summary()} can be used to complement \fun{plot.matchFrontier} to identify which sample size yields the least imbalance or to view the values of the imbalance metric at individual points along the frontier.

When \code{object} is a \code{MatchItFrontier} object and propensity score matching was done in the original call to \code{matchit()}, an additiona column, \code{Caliper}, will be displayed. This contains the largest distance between members of each of the remaining pairs at the given remaining sample size, corresponding to the value of the caliper (in standardized units) that would need to be set to arrive at a sample with the given size. (Note that this value is obtained from the frontier, and may not correspond to the caliper actually required to achieve the desired sample size when matching, unless matching with replacement.)

A carrot (i.e., \code{^}) is printed below the column the corresponds to the \code{n} component of the \code{matchFrontier} object. It is this value that the \code{N} and \code{Ndrop} arguments refer to in \code{summary.matchFrontier()}, \fun{frontier_to_matchit}, and \fun{generateDataset}. For example, if the carrot is pointing at the \code{N control} column, then supplying a value of 50 to \code{N} in \fun{generateDataset} will produce a dataset with 50 control units (and however many treated units are present).

Note that when no point on the frontier corresponds to the supplied value of \code{N} or \code{Ndrop}, the nearest point will be used.
}
\value{
A \code{summary.matchFrontier} object, which contains the following components:

\item{Ntreated}{the number of treated units at the start, end, and best point on the frontier (best meaning with the lowest imbalance), or at the user-specified points on the frontier.}
\item{Ncontrol}{the number of control units at the start, end, and best point on the frontier, or at the user-specified points on the frontier.}
\item{N}{the total number of units at the start, end, and best point on the frontier, or at the user-specified points on the frontier.}
\item{Stat}{the value of the imbalance metric at the start, end, and best point on the frontier, or at the user-specified points on the frontier.}
\item{bestind}{the index of the best point on the frontier. \code{NULL} when \code{N} or \code{Ndrop} are specified.}
\item{QOI}{the quantity of interest; the argument to \code{QOI} supplied in the original call to \fun{makeFrontier}.}
\item{metric}{the imbalance metric; the argument to \code{metric} supplied in the original call to \fun{makeFrontier}.}
\item{caliper}{when \code{object} is a \code{MatchItFrontier} object and propensity score matching was done in the original call to \code{matchit()}, the value of the caliper corresponding to the given sample size at the start, end, and best point on the frontier, or at the user-specified points on the frontier.}

}

\seealso{
\fun{makeFrontier}, \fun{plot.matchFrontier}
}
\examples{
# See examples at help("makeFrontier") and
# help("makeFrontier.matchit")
}
