% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/targeting.idea.request.R
\name{targeting.idea.request}
\alias{targeting.idea.request}
\title{Adwords Targeting Idea Request}
\usage{
targeting.idea.request(search.parameters, idea.type = "KEYWORD",
  request.type = "IDEAS", requested.attribute.types = c("KEYWORD_TEXT",
  "SEARCH_VOLUME", "CATEGORY_PRODUCTS_AND_SERVICES"), start.index = 0,
  number.of.results = 700)
}
\arguments{
\item{search.parameters}{Search rules applied for the targeting ideas. If empty metadata will be collected with default parameters.}

\item{idea.type}{Limits the request to responses of this idea type. Default to "KEYWORD", the only possibility.}

\item{request.type}{Specify the request type. Can be either "IDEAS" (the default) or "STATS".}

\item{requested.attribute.types}{Request Attributes and associated data for this set of Types.}

\item{start.index}{Index of the first result to return with this request. Default is 0.}

\item{number.of.results}{Maximum number of results to return in this request. Default is 700 (the maximum allowed).}
}
\value{
An XML formatted targeting idea request.
}
\description{
Prepares the body of an XML formatted request for the Adwords targeting idea service.
}
\examples{
request <- targeting.idea.request(search.parameters = list(related.to.query("Mercury", "Venus", "Mars"), language(1000, 1002), network()))
}
\seealso{
get.service
}
