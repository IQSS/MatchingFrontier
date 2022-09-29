\name{generateDataset}
\alias{generateDataset}

\title{
Generate a dataset at a point along the frontier
}
\description{
\code{generateDataset()} generates a dataset at a given point along the frontier identified either by the number of units remaining or the number of units dropped at that point on the frontier. This dataset can then be used in the final analysis. \code{generateDataset()} works just like \pkgfun2{MatchIt}{match.data}{MatchIt::match.data} or \pkgfun2{MatchIt}{match.data}{MatchIt::get_matches} and produces the same output as these functions when they are used on a \code{matchit} object (i.e., a data frame containing the matched units).
}
\usage{
generateDataset(frontier.object, N, Ndrop,
                weights = "weights", dup = FALSE,
                subclass = "subclass", id = "id")
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
  \item{weights}{
a string containing the name that should be given to the variable containing the matching weights in the data frame output. Default is \code{"weights"}.
}
  \item{dup}{
when using a pair distance-based frontier, whether to have a row for each unit (\code{FALSE}), equivalent to using \pkgfun2{MatchIt}{match.data}{MatchIt::match.data}, or a row for each unit for each pair they are a part of (\code{TRUE}), equivalent to using \pkgfun2{MatchIt}{match.data}{MatchIt::get_matches}. When \code{FALSE}, pair membership is omitted. When \code{TRUE}, pair membership and unit ID are stored in additional variables named based on the \code{subclass} and \code{id} arguments, respectively. \code{dup} is ignored for other types of frontiers.
}
  \item{subclass}{
when using a pair distance-based frontier and \code{dup = TRUE}, a string containing the name that should be given to the variable containing the matched pair membership in the data frame output. Default is \code{"subclass"}.
}
  \item{id}{
when using a pair distance-based frontier and \code{dup = TRUE}, a string containing the name that should be given to the variable containing the unit IDs in the data frame output. Default is \code{"id"}. For other types of frontiers or when \code{dup = FALSE}, the units IDs are stored in the row names of the returned data frame.
}
}
\details{
The argument to \code{N} or \code{Ndrop} depend on the quantity of interest (QOI) supplied to the original call to \fun{makeFrontier}. When \code{QOI} was \code{"SATE"} or \code{"FSATE"}, they correspond to the total number of units remaining or dropped. When \code{QOI} was \code{"SATT"}, they correspond to the number of control units remaining or dropped (since no treated units are dropped). When \code{QOI} was \code{"FSATT"}, they correspond to the number of treated units remaining or dropped. See also the \code{n} component of the \code{matchFrontier} object at \fun{makeFrontier}. When using \fun{summary.matchFrontier}, the carrot printed underneath the summary table indicates which group \code{N} and \code{Ndrop} refer to.
}
\value{
A data frame containing the data supplied to the original call to \fun{makeFrontier} with the computed output variables appended as additional columns, named according the arguments above. When using a pair distance-based frontier and \code{dup = TRUE}, \code{generateDataset()} sorts the data by subclass and treatment status; otherwise, the data will be in their original order.

The returned data frame will contain the variables in the original data set and the following columns:

\item{weights}{the computed matching weights. These must be used in effect estimation to correctly incorporate the matching.}
\item{subclass}{matched pair membership. Units with the same value are in the same pair. Only included when using a pair distance-based frontier and \code{dup = TRUE}.}
\item{id}{the ID of each unit, corresponding to the row names in the original data or dataset. Only included when using a pair distance-based frontier and \code{dup = TRUE}. This column can be used to identify which rows belong to the same unit since, e.g., the same control unit may appear multiple times, being matched to multiple treated units.}

These columns will take on the name supplied to the corresponding arguments in the call to \code{generateData()}.

In addition to their base class (e.g., \code{data.frame} or \code{tbl}), returned objects have the class \code{matchdata} or \code{getmatches}. See \pkgfun2{MatchIt}{match.data}{MatchIt::match.data} for details.
}

\seealso{
\fun{frontier_to_matchit} for generating a \code{matchit} object at a point on the frontier, \pkgfun2{MatchIt}{match.data}{MatchIt::match.data} or \pkgfun2{MatchIt}{match.data}{MatchIt::get_matches}
}
\examples{
data("lalonde", package = "MatchIt")

#Pair distance frontier for FSATT
f1 <- makeFrontier(treat ~ age + educ + married + re74,
                   data = lalonde, QOI = "FSATT",
                   metric = "dist", verbose = FALSE)

plot(f1)

md1 <- generateDataset(f1, Ndrop = 120)
names(md1)

md2 <- generateDataset(f1, Ndrop = 120, dup = TRUE)
names(md2)

#The same can be accomplished by fist converting to
#a matchit object and then using match.data() or
#get_matches()
m1 <- frontier_to_matchit(f1, Ndrop = 120)

stopifnot(all.equal(md1, MatchIt::match.data(m1)))
stopifnot(all.equal(md2, MatchIt::get_matches(m1)))
}
