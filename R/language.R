#' Language Search Parameter
#'
#' Language search parameters for a targeting idea.
#' See \url{https://developers.google.com/adwords/api/docs/reference/v201710/TargetingIdeaService.LanguageSearchParameter}.
#'
#' @param ... One or more language id.
#'
#' @return An S3 search parameter object.
#' @export
#'
#' @examples
#' # English and French
#' language(1000, 1002)
language <- function(...)
{
	x <- list(...)
	class(x) <- c("search.parameter", "language")
	attr(x, "rule") <- "LanguageSearchParameter"
	x
}
