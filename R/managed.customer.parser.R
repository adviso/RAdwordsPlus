#' XML Parsing for Adwords Managed Customer
#'
#' Parses an Adwords managed customer service response in XMl format into a list of two data.frames : the customer account entries and the management links.
#'
#' @param x Managed customer response in XML.
#'
#' @return A list of two data.frames with the entries and the links.
#' @export
#' @import xml2
#'
#' @examples
#' # For this exemple to work, you must supply a valid client customer id and your developper token
#' TODO
managed.customer.parser <- function(x, ...)
{
	if(!require(xml2)) stop("managed.customer.parser requires package xml2")

	doc <- read_xml(x)
	xml_ns_strip(doc)

	#TODO manage 0 entries
	entry.nodes <- xml_find_all(doc, ".//ns2:entries", xml_ns(doc))
	entry.df <- if(length(entry.nodes) == 0)
	{
		warning("x is not a valid managed.customer")
		NULL
	}
	else
	{
		entries <- lapply(entry.nodes, managed.customer.entry.parser)
		do.call(rbind, entries)
	}

	link.nodes <- xml_find_all(doc, ".//ns2:links", xml_ns(doc))
	links <- lapply(link.nodes, managed.customer.link.parser)
	link.df <- do.call(rbind, links)

	list(Entries = entry.df, Links = link.df)
}

#' @rdname .customer.parser
#' @export
managed.customer.entry.parser <- function(entry.node)
{
	if(xml_name(entry.node) != "entries") stop("managed.customer.entry.parser only accepts entries nodes")

	field.nodes <- xml_children(entry.node)
	name <- lapply(field.nodes, xml_name)
	value <- lapply(field.nodes, xml_text)
	result <- data.frame(value, stringsAsFactors = FALSE)
	names(result) <- name
	result
}

#' @rdname managed.customer.parser
#' @export
managed.customer.link.parser <- function(entry.node)
{
	if(xml_name(entry.node) != "links") stop("managed.customer.link.parser only accepts entries nodes")

	field.nodes <- xml_children(entry.node)
	name <- lapply(field.nodes, xml_name)
	value <- lapply(field.nodes, xml_text)
	result <- data.frame(value, stringsAsFactors = FALSE)
	names(result) <- name
	result
}
