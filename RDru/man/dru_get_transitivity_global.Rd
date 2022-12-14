% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dru_get_transitivity_global.R
\name{dru_get_transitivity_global}
\alias{dru_get_transitivity_global}
\title{Return the global transitivity value.}
\usage{
dru_get_transitivity_global(connection, start, end)
}
\arguments{
\item{connection}{A string that contains an address of the Dru instance.}

\item{start}{A number which is the start height of a block
that is included in the search range.}

\item{end}{A number which is the end height of a block
that is included in the search range.}
}
\value{
This function returns a number that is the global transitivity value,
for a given graph
}
\description{
The function sends a request to a Dru instance
and returns the global value of transitivity in the created graph.
}
\details{
Returns the clustering coefficient of the graph
created from the blocks in the range: start - end. \cr
This value is global for the graph.
For node-level clustering coefficient, use dru_get_transitivity function.
}
\examples{
dru_get_transitivity_global("https://dru.bergplace.org/api", 0, 1000)
}
\author{
Mateusz Gabrys
}
