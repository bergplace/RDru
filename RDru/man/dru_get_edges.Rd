% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dru_get_edges.R
\name{dru_get_edges}
\alias{dru_get_edges}
\title{Return the list of edges}
\usage{
dru_get_edges(connection, start, end)
}
\arguments{
\item{connection}{A string that contains an address of the Dru instance.}

\item{start}{A number which is the start height of a block
that is included in the search range.}

\item{end}{A number which is the end height of a block
that is included in the search range.}
}
\value{
This function returns a \code{data.frame} including columns:
\itemize{
\item source
\item target
\item value
\item block_height
\item block_time
\item block_datetime
}
}
\description{
The function sends a request to a Dru instance
and returns the list of edges.
}
\details{
Returns the list of edges blocks for a given range of blocks's height.
These edges can be easily imported into graph-processing libraries.
}
\examples{
dru_get_edges("https://dru.bergplace.org/api", 0, 10)
}
\author{
Mateusz Gabrys
}
