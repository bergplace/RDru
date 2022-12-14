% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dru_get_closeness_max.R
\name{dru_get_closeness_max}
\alias{dru_get_closeness_max}
\title{Return closeness value for a collection of addresses.}
\usage{
dru_get_closeness_max(connection, start, end, directed)
}
\arguments{
\item{connection}{A string that contains an address of the Dru instance.}

\item{start}{A number which is the start height of a block
that is included in the search range.}

\item{end}{A number which is the end height of a block
that is included in the search range.}

\item{directed}{A boolean that specifies if the graph should be considered
directed or undirected.}
}
\value{
This function returns a \code{data.frame} including columns:
\itemize{
\item address
\item closeness_max
}
}
\description{
The function sends a request to a Dru instance
and returns the value of closeness for addresses in a created graph.
}
\details{
Returns the list of addresses and the value of closeness
corresponding to them. \cr
The graph is created from the from the blocks
in the range: start - end. \cr
The graph can be built either as directed or undirected.
}
\examples{
dru_get_closeness_max("https://dru.bergplace.org/api", 0, 1000, "true")
}
\author{
Mateusz Gabrys
}
