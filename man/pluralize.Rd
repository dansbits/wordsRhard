% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/pluralize.R
\name{pluralize}
\alias{pluralize}
\title{Pluralize an english noun}
\usage{
pluralize(word, count = NA)
}
\arguments{
\item{word}{The word to be pluralized}

\item{count}{An optional count to determine if pluralization is necessary. The word wil always be pluralized if left blank.}
}
\value{
The pluralized word or the original word if count is 1 or -1.
}
\description{
Pluralize an english noun
}
\examples{
pluralize("chair")    # => "chairs"
pluralize("box", 1)   # => "box"
pluralize("box", 2)   # => "boxes"
}

