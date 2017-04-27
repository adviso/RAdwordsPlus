#' Check Adwords API Response for Error
#'
#' Checks if the Adwords API response is an error.
#'
#' @param x Adwords XML response.
#'
#' @return Logical. TRUE if this XML message is an error.
#' @export
#'
#' @examples
#' xml.error.msg <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><reportDownloadError><ApiError><type>ReportDefinitionError.CUSTOMER_SERVING_TYPE_REPORT_MISMATCH</type><trigger></trigger><fieldPath>selector.reportDefinition</fieldPath></ApiError></reportDownloadError>"
#' is.error(xml.error.msg)
is.error <- function(x)
{
	UseMethod("is.error", x)
}

#' @rdname is.error
#' @export
is.error.list <- function(x, ...)
{
	sapply(x, is.error, ..., simplify = TRUE)
}

#' @rdname is.error
#' @export
is.error.data.frame <- function(x)
{
	FALSE
}

#' @rdname is.error
#' @export
is.error.default <- function(x)
{
	if(!require(xml2)) stop("is.error requires package xml2")

	doc <- read_xml(x)
	xml_ns_strip(doc)

	!is.na(xml_find_first(doc, "/reportDownloadError", xml_ns(doc)))
}
