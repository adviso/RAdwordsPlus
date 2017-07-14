#' XML Parsing for Adwords Targeting Idea
#'
#' Parses an Adwords targeting idea service response in XMl format into a data.frame.
#'
#' @param x Targeting idea response in XML.
#'
#' @return A list of two data.frames with the entries and the links.
#' @export
#' @import xml2
#'
#' @examples
#' # For this exemple to work, you must supply a valid client customer id and your developper token
#' TODO
parse.targeting.idea <- function(x, ...)
{
	if(!require(xml2)) stop("parse.targeting.idea requires package xml2")

	doc <- read_xml(x)
	xml_ns_strip(doc)

	total <- xml_integer(xml_find_first(doc, ".//totalNumEntries", xml_ns(doc)))
	entry.nodes <- xml_find_all(doc, ".//entries", xml_ns(doc))
	if(length(entry.nodes) == 0)
	{
		warning("x is not a valid targeting.idea")
		NULL
	}
	else
	{
		results <- lapply(entry.nodes, parse.targeting.idea.entry)
		result <- do.call(rbind, results)
		attr(result, "total.number.of.entries") <- total
		result
	}
}

#' @rdname parse.targeting.idea
#' @export
parse.targeting.idea.entry <- function(entry.node)
{
	if(xml_name(entry.node) != "entries") stop("parse.targeting.idea.entry only accepts entries nodes")

	data.nodes <- xml_find_all(entry.node, "data", xml_ns(entry.node))
	results <- lapply(data.nodes, parse.targeting.idea.data)
	result <- data.frame(results)
	result
}

#' @rdname parse.targeting.idea
#' @export
parse.targeting.idea.data <- function(data.node)
{
	if(xml_name(data.node) != "data") stop("parse.targeting.idea.data only accepts data nodes")

	key <- xml_text(xml_find_first(data.node, "key", xml_ns(data.node)))
	value.node <- xml_find_first(data.node, "value", xml_ns(data.node))
	value.text <- xml_text(xml_find_all(value.node, "value", xml_ns(value.node)))
	values <- if(length(value.text) > 0) list(value.text) else list(NA)

	names(values) <- key
	values
}
