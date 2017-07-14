#' Location Search Parameter
#'
#' Location search parameters for a targeting idea.
#' See \url{https://developers.google.com/adwords/api/docs/reference/v201702/TargetingIdeaService.LocationSearchParameter}.
#'
#' @param ... One or more location id.
#'
#' @return An S3 search parameter object.
#' @export
#'
#' @examples
#' # USA and Canada
#' location(2840, 2124)
location <- function(...)
{
	x <- list(...)
	class(x) <- c("search.parameter", "location")
	attr(x, "rule") <- "LocationSearchParameter"
	x
}
