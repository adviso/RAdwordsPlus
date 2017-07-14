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
parse.managed.customer <- function(x, ...)
{
	if(!require(xml2)) stop("parse.managed.customer requires package xml2")

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
		entries <- lapply(entry.nodes, parse.managed.customer.entry)
		do.call(rbind, entries)
	}

	link.nodes <- xml_find_all(doc, ".//ns2:links", xml_ns(doc))
	links <- lapply(link.nodes, parse.managed.customer.link)
	link.df <- do.call(rbind, links)

	list(Entries = entry.df, Links = link.df)
}

#' @rdname parse.managed.customer
#' @export
parse.managed.customer.entry <- function(entry.node)
{
	if(xml_name(entry.node) != "entries") stop("parse.managed.customer.entry only accepts entries nodes")

	field.nodes <- xml_children(entry.node)
	name <- lapply(field.nodes, xml_name)
	value <- lapply(field.nodes, xml_text)
	result <- data.frame(value, stringsAsFactors = FALSE)
	names(result) <- name
	result
}

#' @rdname parse.managed.customer
#' @export
parse.managed.customer.link <- function(entry.node)
{
	if(xml_name(entry.node) != "links") stop("parse.managed.customer.link only accepts entries nodes")

	field.nodes <- xml_children(entry.node)
	name <- lapply(field.nodes, xml_name)
	value <- lapply(field.nodes, xml_text)
	result <- data.frame(value, stringsAsFactors = FALSE)
	names(result) <- name
	result
}
