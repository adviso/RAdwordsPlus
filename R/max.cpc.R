#' Adwords Maximum Cost per Click
#'
#' Creates or coerces S3 objects of type \code{max.cpc} for the Adwords API.
#' \code{is.max.cpc} will test if an object is interpretable as a max.cpc.
#'
#' @param bid Amount to bid for the maximum cost per click.
#' @param use.micro TRUE to specify that the amount x must be converted to a microamount, FALSE to avoid any conversion for bids that are already micromounts. Default to FALSE.
#'
#' @return A S3 object of type max.cpc.
#' @export "max.cpc"
#'
#' @examples
#' bid1 <- max.cpc(0.99)
#' bid2 <- max.cpc(990000, use.micro = TRUE)
#' bid3 <- as.max.cpc("0.99")
#' is.max.cpc(bid3)
max.cpc <- function(bid, use.micro = FALSE)
{
	if(is.null(bid)) stop("bid must be a numerical value")
	if(is.null(use.micro)) stop("use.micro must be a logical value")

	x <- mapply(.max.cpc, bid = bid, use.micro = use.micro, SIMPLIFY = FALSE)
	if(length(x) == 1) x[[1]] else x
}

.max.cpc <- function(bid, use.micro = TRUE)
{
	if(!is.numeric(bid)) stop("bid must be a numerical value")
	if(!is.logical(use.micro)) stop("use.micro must be a logical value")
	if(is.na(use.micro)) stop("use.micro must be a logical value")

	x <- ifelse(use.micro, bid * 1000000, bid)
	class(x) <- "max.cpc"
	x
}

#' @rdname max.cpc
#' @export
as.max.cpc <- function(bid, ...)
{
	if(is.null(bid)) stop("bid must be a numeric convertible value")
	if(is.na(bid)) stop("bid must be a numeric convertible value")

	max.cpc(as.numeric(bid), ...)
}

#' @rdname max.cpc
#' @export
is.max.cpc <- function(x)
{
	"max.cpc" %in% class(x)
}
