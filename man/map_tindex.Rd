% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map-tindex.R
\name{map_tindex}
\alias{map_tindex}
\title{Map \code{tindex()}}
\usage{
map_tindex(x, margin)
}
\arguments{
\item{x}{object of type \code{\link[term:term]{term}}.}

\item{margin}{integer. Which index to extract.}
}
\description{
Extract specific elements in the list returned by \code{\link[term:tindex]{term::tindex()}}.
}
\examples{
map_tindex(term::term("term[10,1]", "term[20,1]"), 1)
#> term[1,1] term[2,1]
#>        10        20

}
