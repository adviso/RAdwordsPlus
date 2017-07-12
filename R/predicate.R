#' Adwords Predicate
#'
#' Specifies how an entity should be filtered.
#' Read \url{https://developers.google.com/adwords/api/docs/reference/v201705/AdGroupAdService.Predicate} for more informations.
#'
#' @param field The field by which to filter the returned data.
#' @param operator The operator to use for filtering the data returned.
#' @param values The values by which to filter the field.
#'
#' @return An S3 object with the predicate data.
#' @export
#'
#' @examples
#' predicate(field = "CustomerId", operator = "EQUALS", values = "123-456-7890")
predicate <- function(field = "Name", operator = "EQUALS", values = "Google")
{
	x <- list(field = field, operator = operator, values = values)
	class(x) <- "predicate"
	x
}
