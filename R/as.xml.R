#' XML Encoding for Adwords Service Requests
#'
#' Encodes the search parameters of an Adwords service request into XMl.
#' Allow the conversion of the data to a SOAP message for the Adwords service.
#'
#' @param x Data to convert to XML.
#'
#' @return An XMLNode encoding the data
#' @export
#' @import XML gtools
#'
#' @examples
#' TODO
as.xml <- function(x)
{
	if(!require(XML)) stop("as.xml requires package XML")
	if(!require(gtools)) stop("as.xml requires package gtools")

	UseMethod("as.xml", x)
}

#' @rdname as.xml
#' @export
as.xml.NULL <- function(x)
{
	NULL
}

#' @rdname as.xml
#' @export
as.xml.list <- function(x)
{
	lapply(x, as.xml)
}

#' @rdname as.xml
#' @export
as.xml.logical <- function(x)
{
	ifelse(x, "true", "false")
}

#' @rdname as.xml
#' @export
as.xml.predicate <- function(x)
{
	field.node <- xmlNode(name = "field", namespace = "ns0", x[["field"]])
	operator.node <- xmlNode(name = "operator", namespace = "ns0", x[["operator"]])
	value.node <- xmlNode(name = "values", namespace = "ns0", x[["values"]])
	xmlNode(name = "predicates", namespace = "ns0", field.node, operator.node, value.node)
}
