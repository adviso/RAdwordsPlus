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

#' @rdname as.xml
#' @export
as.xml.search.parameter <- function(x)
{
	rule <- attr(x, "rule")
	search.type.node <- xmlNode(name = "SearchParameter.Type", namespace = "ns1", rule)
	class(x) <- setdiff(class(x), "search.parameter")
	content.nodes <- as.xml(x)
	if("XMLNode" %in% class(content.nodes))
	{
		xmlNode(name = "searchParameters", namespace = "ns1", attrs = c("xsi:type" = paste0("ns1:", rule)), search.type.node, content.nodes)
	} else # Else we have a list of nodes
	{
		xmlNode(name = "searchParameters", namespace = "ns1", attrs = c("xsi:type" = paste0("ns1:", rule)), .children = c(list(search.type.node), content.nodes))
	}
}

#' @rdname as.xml
#' @export
as.xml.language <- function(x)
{
	id.nodes <- lapply(x, xmlNode, name = "id", namespace = "ns0")
	lapply(id.nodes, xmlNode, name = "languages", namespace = "ns1", attrs = NULL, namespaceDefinitions = NULL)
}

#' @rdname as.xml
#' @export
as.xml.location <- function(x)
{
	id.nodes <- lapply(x, xmlNode, name = "id", namespace = "ns0")
	lapply(id.nodes, xmlNode, name = "locations", namespace = "ns1", attrs = NULL, namespaceDefinitions = NULL)
}

#' @rdname as.xml
#' @export
as.xml.network <- function(x)
{
	setting.nodes <- mapply(xmlNode, sapply(x, as.xml), name = c("targetGoogleSearch", "targetSearchNetwork", "targetContentNetwork", "targetPartnerSearchNetwork"), namespace = "ns0", SIMPLIFY = FALSE)
	xmlNode(name = "networkSetting", namespace = "ns1", .children = setting.nodes)
}

#' @rdname as.xml
#' @export
as.xml.related.to.query <- function(x)
{
	lapply(x, xmlNode, name = "queries", namespace = "ns1")
}

#' @rdname as.xml
#' @export
as.xml.related.to.url <- function(x)
{
	include.sub.urls <- attr(x, "include.sub.urls")
	c(lapply(x, xmlNode, name = "urls", namespace = "ns1"), list(xmlNode(name = "includeSubUrls", namespace = "ns1", as.xml(include.sub.urls))))
}

#' @rdname as.xml
#' @export
as.xml.keyword.estimate.request <- function(x)
{
	criterion.node <- xmlNode("Keyword", name = "Criterion.Type", namespace = "ns0")
	text.node <- xmlNode(x, name = "text", namespace = "ns0")
	match.node <- xmlNode(attr(x, "match.type"), name = "matchType", namespace = "ns0")

	if(invalid(attr(x, "id")))
	{
		keyword.node <- xmlNode(name = "keyword", namespace = "ns1", criterion.node, text.node, match.node)
	}
	else
	{
		id.node <- xmlNode(attr(x, "id"), name = "id", namespace = "ns0")
		keyword.node <- xmlNode(name = "keyword", namespace = "ns1", id.node, criterion.node, text.node, match.node)
	}

	if(!invalid(attr(x, "max.cpc")))
	{
		bid.node <- as.xml(attr(x, "max.cpc"))
	}

	if(!invalid(attr(x, "is.negative")))
	{
		negative.text <- ifelse(attr(x, "is.negative"), "true", "false")
		negative.node <- xmlNode(negative.text, name = "isNegative", namespace = "ns1")
	}

	if(exists("bid.node"))
	{
		if(exists("negative.node"))
		{
			node <- xmlNode(name = "keywordEstimateRequests", namespace = "ns1", keyword.node, bid.node, negative.node)
		}
		else
		{
			node <- xmlNode(name = "keywordEstimateRequests", namespace = "ns1", keyword.node, bid.node)
		}
	}
	else
	{
		if(exists("negative.node"))
		{
			node <- xmlNode(name = "keywordEstimateRequests", namespace = "ns1", keyword.node, negative.node)
		}
		else
		{
			node <- xmlNode(name = "keywordEstimateRequests", namespace = "ns1", keyword.node)
		}
	}

	node
}

#' @rdname as.xml
#' @export
as.xml.adgroup.estimate.request <- function(x)
{
	node <- xmlNode(name = "adGroupEstimateRequests", namespace = "ns1")

	if(!invalid(attr(x, "id")))
	{
		id.node <- xmlNode(attr(x, "id"), name = "adGroupId", namespace = "ns1")
		node <- addChildren(node = node, kids = list(id.node))
	}

	if(!invalid(attr(x, "max.cpc")))
	{
		bid.node <- as.xml(attr(x, "max.cpc"))
		node <- addChildren(node = node, kids = list(bid.node))
	}

	keyword.nodes <- lapply(x, as.xml)
	node <- addChildren(node = node, kids = keyword.nodes)

	node
}

#' @rdname as.xml
#' @export
as.xml.campaign.estimate.request <- function(x)
{
	node <- xmlNode(name = "campaignEstimateRequests", namespace = "ns1")

	if(!invalid(attr(x, "id")))
	{
		id.node <- xmlNode(attr(x, "id"), name = "campaignId", namespace = "ns1")
		node <- addChildren(node = node, kids = list(id.node))
	}

	adgroup.nodes <- lapply(x, as.xml)
	node <- addChildren(node = node, kids = adgroup.nodes)

	if(!invalid(attr(x, "criteria")))
	{
		criteria.nodes <- lapply(attr(x, "criteria"), as.xml)
		node <- addChildren(node = node, kids = criteria.nodes)
	}

	node
}

#' @rdname as.xml
#' @export
as.xml.max.cpc <- function(amount)
{
	money.node <- xmlNode("Money", name = "ComparableValue.Type", namespace = "ns0")
	amount.node <- xmlNode(format(amount, scientific = FALSE), name = "microAmount", namespace = "ns0")
	xmlNode(name = "maxCpc", namespace = "ns1", money.node, amount.node)
}

#' @rdname as.xml
#' @export
as.xml.criterion <- function(criterion)
{
	id.node <- xmlNode(criterion, name = "id", namespace = "ns0")
	type.node <- xmlNode(attr(criterion, "type"), name = "Criterion.Type", namespace = "ns0")
	xmlNode(name = "criteria", namespace = "ns1", attrs = c("xsi:type" = paste0("ns0:", attr(criterion, "type"))), id.node, type.node)
}
