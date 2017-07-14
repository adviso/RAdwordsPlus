#' Adwords Criterion
#'
#' Creates or coerces S3 objects of type \code{criterion} for the Adwords API.
#' \code{is.criterion} will test if an object is interpretable as a criterion.
#' Criteria can be added to a \code{campaign.estimate.request}.
#' Only locations and languages are supported in the keyword traffic estimator.
#'
#' @param id Amount to bid for the maximum cost per click.
#' @param type TRUE to specify that the amount x must be converted to a microamount, FALSE to avoid any conversion for bids that are already micromounts. Default to FALSE.
#'
#' @return A S3 object of type criterion.
#' @export "criterion"
#'
#' @examples
#' usa <- criterion(2840) # United States
#' english <- criterion("1000", type = "Language") # English
#' french <- as.criterion(1002, type = "Language") # French
#' is.criterion(french)
criterion <- function(id, type = "Location")
{
	if(is.null(id)) stop("id must be a valid identifier (integer or character)")
	if(is.null(type)) stop("type must be either 'Location' or 'Language'")

	x <- mapply(.criterion, id = id, type = type, SIMPLIFY = FALSE)
	if(length(x) == 1) x[[1]] else x
}

.criterion <- function(id, type)
{
	if(!is.numeric(id) & !is.character(id)) stop("id must be a valid identifier (integer or character)")
	if(!is.character(type)) stop("type must be either 'Location' or 'Language'")
	if(!type %in% c("Location", "Language")) stop("type must be either 'Location' or 'Language'")

	x <- as.character(id)
	class(x) <- "criterion"
	attr(x, "type") <- type
	x
}

#' @rdname criterion
#' @export
as.criterion <- function(id, ...)
{
	if(is.null(id)) stop("id must be a character convertible value")
	if(is.na(id)) stop("id must be a character convertible value")

	criterion(id, ...)
}

#' @rdname criterion
#' @export
is.criterion <- function(x)
{
	"criterion" %in% class(x)
}
