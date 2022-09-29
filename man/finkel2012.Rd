\name{finkel2012}
\alias{finkel2012}
\docType{data}
\title{
Data from Finkel et al. (2012) evaluating Kenya's NCEP II-Uraia
}
\description{
This is a modified dataset from Finkel, Horowitz, and Rojo-Mendoza (2012) examining the effect of the NCEP II-Uraia, a civic engagemnet program ocurring in Kenya in 2007. The treatment (\code{"treat"}) is participation in at least one of the specified civic engagement activities before the 2007 election. Several outcomes were examined in the original paper; here only political knowledge (\code{"polknow"}) is included.

This dataset was used in King and Nielsen (2019) as an example demonstrating the propensity score mathcing paradox.
}
\usage{data("finkel2012")}
\format{
  A data frame with 3141 observations on the following 16 variables.
  \describe{
    \item{\code{treat}}{\code{integer}: treatment (0: untreated, 1: treated)}
    \item{\code{uraiamedia}}{\code{integer}; Uraia media exposure (0: No, 1: Yes)}
    \item{\code{age}}{\code{integer}; age (years)}
    \item{\code{churchgo}}{\code{integer}; frequency of church attendence (0: never, ..., 5: more than once a week)}
    \item{\code{groupactive}}{\code{numeric}; group activity scale, from 0 (no memberships) to 2 (extremely active)}
    \item{\code{income}}{\code{integer}; income level (0: up to kshs 5000, ..., 4: 15,000+)}
    \item{\code{groupleader}}{\code{integer}; leader of any group (0: No, 1: Yes)}
    \item{\code{male}}{\code{integer}; sex (0: female, 1: male)}
    \item{\code{educ}}{\code{integer}; education level (0: no formal schooling, ..., 9: university)}
    \item{\code{religion}}{\code{factor}: religion ("protestant", "catholic", "muslim", "other")}
    \item{\code{media}}{\code{numeric}: media exposure, from 1 (never) to 4 (daily)}
    \item{\code{polinterest}}{\code{numeric}: poltical interest, from 1 (very little interest) to 3 (a great deal of interest)}
    \item{\code{poldiscuss}}{\code{integer}: frequency of polticial discussion in the last year (0: never, ..., 2: several times)}
    \item{\code{civicgroup}}{\code{integer}: participation in a civic group (0: not a member, ..., 2: an active member)}
    \item{\code{polparty}}{\code{integer}: participation in a political party (0: not a member, ..., 2: an active member)}
    \item{\code{polknow}}{\code{integer}: political knowledge (0: minimum knowledge, ..., 4: maximum knowledge)}
  }
}
\details{
Although many of the included variables are ordinal in nature, here they are analyzed as numeric values.
}

\references{
Finkel, S. E., Horowitz, J., & Rojo-Mendoza, R. T. (2012). Civic Education and Democratic Backsliding in the Wake of Kenya's Post-2007 Election Violence. The Journal of Politics. \doi{10.1017/S0022381611001162}

King, G., & Nielsen, R. (2019). Why Propensity Scores Should Not Be Used for Matching. Political Analysis, 1-20. \doi{10.1017/pan.2019.11}
}
\keyword{datasets}
