#' Adwords Service Error Handling
#'
#' Checks a service response in the \link{get.service} function.
#' The XML is parsed to detect an error an extract the error message.
#' Then the error message is returned.
#'
#' @param x The result from a service request.
#'
#' @return An error message if an error is found, or NULL if none were found.
#' @export
#' @import xml2
#' @seealso get.service
#'
#' @examples
#' TODO
check.service <- function(x)
{
	if(!require(xml2)) stop("check.service requires package xml2")

	doc <- read_xml(x)
	xml_ns_strip(doc)

	if(!is.na(xml_find_first(doc, ".//faultcode", xml_ns(doc))))
	{
		message <- xml_text(xml_find_first(doc, ".//faultstring", xml_ns(doc)))
		return(message)
	}

	NULL
}
