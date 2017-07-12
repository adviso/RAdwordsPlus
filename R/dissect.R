#' XML Parsing for Adwords
#'
#' Dissects an Adwords service response in XMl format into the appropriate format for the results.
#'
#' @param x Managed Customer response in XML.
#'
#' @return A data.frame with the estimate results.
#' @export
#' @import xml2
#'
#' @examples
#' # For this exemple to work, you must supply a valid client customer id and your developper token
#' TODO
dissect <- function(x, ...)
{
	UseMethod("dissect", x)
}

#' @rdname dissect
#' @export
dissect.list <- function(x, ...)
{
	lapply(x, dissect, ...)
}

#' @rdname dissect
#' @export
dissect.managed.customer <- function(x, ...)
{
	if(!require(xml2)) stop("dissect.managed.customer requires package xml2")

	doc <- read_xml(x)
	xml_ns_strip(doc)

	entry.nodes <- xml_find_all(doc, ".//ns2:entries", xml_ns(doc))
	entry.df <- if(length(entry.nodes) == 0)
	{
		warning("x is not a valid managed.customer")
		NULL
	}
	else
	{
		entries <- lapply(entry.nodes, dissect.entry)
		do.call(rbind, entries)
	}

	link.nodes <- xml_find_all(doc, ".//ns2:links", xml_ns(doc))
	links <- lapply(link.nodes, dissect.link)
	link.df <- do.call(rbind, links)

	list(Entries = entry.df, Links = link.df)
}

#' @rdname dissect
#' @export
dissect.entry <- function(entry.node)
{
	if(!require(xml2)) stop("dissect.entry requires package xml2")
	if(xml_name(entry.node) != "entries") stop("dissect.entry only accepts entries nodes")

	field.nodes <- xml_children(entry.node)
	name <- lapply(field.nodes, xml_name)
	value <- lapply(field.nodes, xml_text)
	result <- data.frame(value, stringsAsFactors = FALSE)
	names(result) <- name
	result
}

#' @rdname dissect
#' @export
dissect.link <- function(entry.node)
{
	if(!require(xml2)) stop("dissect.link requires package xml2")
	if(xml_name(entry.node) != "links") stop("dissect.link only accepts entries nodes")

	field.nodes <- xml_children(entry.node)
	name <- lapply(field.nodes, xml_name)
	value <- lapply(field.nodes, xml_text)
	result <- data.frame(value, stringsAsFactors = FALSE)
	names(result) <- name
	result
}
