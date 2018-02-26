#' Related to Query Search Parameter
#'
#' Related to query search parameters for a targeting idea.
#' See \url{https://developers.google.com/adwords/api/docs/reference/v201710/TargetingIdeaService.RelatedToQuerySearchParameter}.
#'
#' @param ... One or more keywords.
#'
#' @return An S3 search parameter object.
#' @export
#'
#' @examples
#' # Queries for some planets
#' related.to.query("Mercury", "Venus", "Mars")
related.to.query <- function(...)
{
	x <- as.list(c(...))
	class(x) <- c("search.parameter", "related.to.query")
	attr(x, "rule") <- "RelatedToQuerySearchParameter"
	x
}
