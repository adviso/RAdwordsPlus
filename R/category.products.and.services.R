#' Category Products and Services Search Parameter
#'
#' Category products and services search parameters for a targeting idea.
#' See \url{https://developers.google.com/adwords/api/docs/reference/v201809/TargetingIdeaService.CategoryProductsAndServicesSearchParameter}.
#'
#' @param ... One or more keywords.
#'
#' @return An S3 search parameter object.
#' @export
#'
#' @examples
#' # Search parameter for vertical Advertising & Marketing
#' category.products.and.services(25)
category.products.and.services <- function(...)
{
	x <- as.list(c(...))
	class(x) <- c("search.parameter", "category.products.and.services")
	attr(x, "rule") <- "CategoryProductsAndServicesSearchParameter"
	x
}
