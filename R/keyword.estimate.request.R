#' Adwords Keyword Estimate Request
#'
#' Creates or coerces S3 objects of type \code{keyword.estimate.request} for the Adwords API.
#' \code{is.keyword.estimate.request} will test if an object is interpretable as a keyword.estimate.request
#'
#' @param keyword The keyword objects to prepare into a keyword.estimate.request.
#' @param max.cpc The bid for the keyword.
#' @param is.negative When TRUE, no current ad group ads will appear for searches containing this keyword.
#'
#' @return A S3 of type keyword.estimate.request.
#' @export
#'
#' @examples
#' # Minimal example
#' k1 <- keyword("Example")
#' request1 <- keyword.estimate.request(k1)
#'
#' # More complex example
#' k2 <- keyword(c("Example", "Sample", "Test", "Only a test"), match.type = "PHRASE")
#' request2 <- keyword.estimate.request(k2, max.cpc = max.cpc(c(1, 2, 1, 3), use.micro = TRUE))
#'
#' # Coercing to a keyword.estimate.request
#' k3 <- as.keyword.estimate.request("Convert", is.negative = TRUE, match.type = "EXACT")
#'
#' # Checking a keyword.estimate.request
#' is.keyword.estimate.request(k3)
keyword.estimate.request <- function(keyword, max.cpc = NULL, is.negative = NULL)
{
	max.cpc[is.null(max.cpc)] <- NA
	is.negative[is.null(is.negative)] <- NA
	if(is.null(keyword) | !(is.keyword(keyword) | all(sapply(keyword, is.keyword))))
	{
		stop("argument keyword must contains keyword objects")
	}

	keyword <- if(is.list(keyword)) keyword else list(keyword)
	x <- mapply(.keyword.estimate.request, keyword = keyword, max.cpc = max.cpc, is.negative = is.negative, SIMPLIFY = FALSE)
	if(length(x) == 1) x[[1]] else x
}

.keyword.estimate.request <- function(keyword, max.cpc = NA, is.negative = NA)
{
	x <- keyword
	class(x) <- c("keyword.estimate.request", "keyword")
	if(!is.na(max.cpc)) attr(x, "max.cpc") <- as.max.cpc(max.cpc)
	attr(x, "is.negative") <- is.negative
	x
}

#' @rdname keyword.estimate.request
#' @export "as.keyword.estimate.request"
as.keyword.estimate.request <- function(x, ...)
{
	UseMethod("as.keyword.estimate.request", x)
}

#' @rdname keyword.estimate.request
#' @export
as.keyword.estimate.request.data.frame <- function(x, max.cpc = NULL, is.negative = NULL, ...)
{
	keyword <- as.keyword(x, ...)

	max.cpc <- if(is.null(max.cpc) & !is.null(x[["Max.CPC"]])) x[["Max.CPC"]] else max.cpc
	is.negative <- if(is.null(is.negative) & !is.null(x[["IsNegative"]])) x[["Max.CPC"]] else is.negative

	keyword.estimate.request(keyword = keyword, max.cpc = max.cpc, is.negative = is.negative)
}

#' @rdname keyword.estimate.request
#' @export
as.keyword.estimate.request.default <- function(x, ...)
{
	args = list(...)
	max.cpc = args[["max.cpc"]]
	args[["max.cpc"]] <- NULL
	is.negative <- args[["is.negative"]]
	args[["is.negative"]] <- NULL
	keyword <- do.call(as.keyword, c(x, args))

	keyword.estimate.request(keyword = keyword, max.cpc = max.cpc, is.negative = is.negative)
}

#' @rdname keyword.estimate.request
#' @export
is.keyword.estimate.request <- function(x)
{
	"keyword.estimate.request" %in% class(x)
}
