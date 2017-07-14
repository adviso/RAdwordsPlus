#' Adwords Keyword
#'
#' Creates or coerces S3 objects of type \code{keyword} for the Adwords API.
#' \code{is.keyword} will test if an object is interpretable as a keyword.
#'
#' @param text Text of the keyword.
#' @param match.type Either "BROAD", "PHRASE" or "EXACT".
#' @param keyword.id The ID for the keyword, unsuported in the Adwords keyword traffic estimator service. When converting a data frame, this parameter will override the existing column with keyword ids, remove it if NULL or leave the column intact if missing (which is the default).
#' @param type Placeholder for the keyword type, unsuported for now.
#'
#' @return A S3 object of type keyword.
#' @export
#'
#' @examples
#' # Simplest case
#' k1 <- keyword("Example")
#'
#' # Adding a match type
#' k2 <- keyword("Example", "EXACT")
#'
#' # Creating multiple keywords
#' k3 <- keyword(c("Example", "Sample", "Test", "Only a test"), match.type = "PHRASE")
#'
#' # Creating multiple keywords with different match types
#' k4 <- keyword(c("Example", "Sample", "Test", "Only a test"), match.type = c("PHRASE", "BROAD", "EXACT", "BROAD"))
#'
#' # Coercing to a keyword
#' k5 <- as.keyword(911)
#'
#' # Checking a keyword
#' is.keyword(k5)
keyword <- function(text, match.type = "BROAD", keyword.id = NULL, type = "Keyword")
{
	keyword.id[is.null(keyword.id)] <- NA
	match.type[is.null(match.type)] <- "BROAD"
	match.type[is.na(match.type)] <- "BROAD"
	match.type <- toupper(match.type)
	x <- mapply(.keyword, text = text, match.type = match.type, keyword.id = keyword.id, type = type, SIMPLIFY = FALSE)
	if(length(x) == 1) x[[1]] else x
}

.keyword <- function(text, match.type, keyword.id, type)
{
	x <- text
	class(x) <- c("keyword")
	attr(x, "match.type") <- match.type
	attr(x, "id") <- keyword.id
	attr(x, "type") <- type
	x
}

#' @rdname keyword
#' @export
as.keyword <- function(x, ...)
{
	UseMethod("as.keyword", x)
}

#' @rdname keyword
#' @export
as.keyword.data.frame <- function(x, keyword.id, match.type = NULL, type = "Keyword")
{
	match.type = if(is.null(match.type) & !is.null(x[["Matchtype"]])) x[["Matchtype"]] else match.type
	keyword.id <- if(!missing(keyword.id)) keyword.id else if(!is.null(x[["KeywordID"]])) x[["KeywordID"]] else NULL
	keyword(text = x[["Keyword"]], match.type = match.type, keyword.id = keyword.id, type = type)
}

#' @rdname keyword
#' @export
as.keyword.default <- function(x, ...)
{
	keyword(as.character(x), ...)
}

#' @rdname keyword
#' @export
is.keyword <- function(x)
{
	"keyword" %in% class(x)
}
