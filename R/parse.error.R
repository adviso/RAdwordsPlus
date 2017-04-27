#' XML Error Parsing for Adwords
#'
#' Parses an error message in XMl format into a message explaining the error.
#'
#' @param x Adwords API response in XML.
#'
#' @return A data.frame with the estimate results.
#' @export
#' @import xml2
#'
#' @examples
#' # For this exemple to work, you must supply a valid client customer id and your developper token
#' library(RAdwords)
#' TODO
parse.error <- function(x, ...)
{
	if(!require(xml2)) stop("parse.targeting.idea requires package xml2")

	doc <- read_xml(x)
	xml_ns_strip(doc)

	error <- x

	report.error.node <- xml_find_first(doc, "/reportDownloadError", xml_ns(doc))
	api.error.node <- xml_find_first(report.error.node, ".//ApiError", xml_ns(doc))
	if(!is.na(api.error.node))
	{
		error <- xml_text(xml_find_first(api.error.node, ".//type", xml_ns(doc)))
	}

	error
}
