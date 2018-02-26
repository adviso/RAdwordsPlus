#' Network Search Parameter
#'
#' Location search parameters for a targeting idea.
#' See \url{https://developers.google.com/adwords/api/docs/reference/v201710/TargetingIdeaService.NetworkSetting}.
#'
#' @param target.google.search Logical. If true, ads will be served with Google.com search results.
#' @param target.search.network Logical. If true, ads will be served on partner sites in the Google Search Network (requires GOOGLE_SEARCH).
#' @param target.content.network Logical. If true, ads will be served on specified placements in the Google Display Network. Placements are specified using Placement criteria.
#' @param target.partner.content.network Logical. If true, ads will be served on the Google Partner Network. This is available to only some specific Google partner accounts.
#'
#' @return An S3 search parameter object.
#' @export
#'
#' @examples
#' # Default network settings
#' network()
network <- function(target.google.search = TRUE, target.search.network = FALSE, target.content.network = FALSE, target.partner.content.network = FALSE)
{
	x <- list(target.google.search = target.google.search, target.search.network = target.search.network, target.content.network = target.content.network, target.partner.content.network = target.partner.content.network)
	class(x) <- c("search.parameter", "network")
	attr(x, "rule") <- "NetworkSearchParameter"
	x
}
