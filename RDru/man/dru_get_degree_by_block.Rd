% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dru_get_degree_by_block.R
\name{dru_get_degree_by_block}
\alias{dru_get_degree_by_block}
\title{Return the value of the degree}
\usage{
dru_get_degree_by_block(connection, start, end, address, mode)
}
\arguments{
\item{connection}{A string that contains an address of the Dru instance.}

\item{start}{A number which is the start height of a block
that is included in the search range.}

\item{end}{A number which is the end height of a block
that is included in the search range.}

\item{address}{A string that specifies an address, which values of degree
are shown in the results.}

\item{mode}{A string that specifies the method to calculate the degree.
Three modes are available : \cr
all - total degree of the node,
in - in degree of the node, out - out degree of the node.}
}
\value{
This function returns a list including an address, mode
as well as all of the degrees with their heights.
}
\description{
The function sends a request to a Dru instance
and returns the list of addresses degree values,
corresponding to them. \cr The degree is computed in each block separately.
}
\details{
Returns the list of addresses and the value of the degree
corresponding to them. \cr
The graph is created from the blocks in the range: start - end. \cr
This variant computes the degree in each block separately. \cr
The graph will be built as directed,
but using the mode,
all variants of the measure can be computed: all, in, out.
}
\examples{
dru_get_degree_by_block("https://dru.bergplace.org/api",
0, 10, "t1KstPVzcNEK4ZeauQ6cogoqxQBMDSiRnGr", "all")
}
\author{
Mateusz Gabrys
}
