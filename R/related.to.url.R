#' Related to URL Search Parameter
#'
#' Related to URL search parameters for a targeting idea.
#' See \url{https://developers.google.com/adwords/api/docs/reference/v201702/TargetingIdeaService.RelatedToUrlSearchParameter}.
#'
#' @param ... One or more language id.
#' @param ... One or more language id.
#'
#' @return An S3 search parameter object.
#' @export
#'
#' @examples
#' # Queries for some planets
#' related.to.query("Mercury", "Venus", "Mars")
related.to.url <- function(..., include.sub.urls = FALSE)
{
	x <- list(...)
	class(x) <- c("search.parameter", "related.to.url")
	attr(x, "rule") <- "RelatedToUrlSearchParameter"
	attr(x, "include.sub.urls") <- include.sub.urls
	x
}
