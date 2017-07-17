#' Adwords Targeting Idea Request
#'
#' Prepares the body of an XML formatted request for the Adwords targeting idea service.
#'
#' @param search.parameters Search rules applied for the targeting ideas. If empty metadata will be collected with default parameters.
#' @param idea.type Limits the request to responses of this idea type. Default to "KEYWORD", the only possibility.
#' @param request.type Specify the request type. Can be either "IDEAS" (the default) of "STATS".
#' @param requested.attribute.types Request Attributes and associated data for this set of Types.
#' @param start.index Index of the first result to return with this request. Default is 0.
#' @param number.of.results Maximum number of results to return in this request. Default is 700 (the maximum allowed).
#'
#' @return An XML formatted targeting idea request.
#' @export
#' @import XML
#' @seealso get.service
#'
#' @examples
#' request <- targeting.idea.request(search.parameters = list(related.to.query("Mercury", "Venus", "Mars"), language(1000, 1002), network()))
targeting.idea.request <- function(search.parameters, idea.type = "KEYWORD", request.type = "IDEAS", requested.attribute.types = c("KEYWORD_TEXT", "SEARCH_VOLUME", "CATEGORY_PRODUCTS_AND_SERVICES"), start.index = 0, number.of.results = 700)
{
	if(!require(XML)) stop("targeting.idea.request requires package XML")

	search.nodes <- as.xml(search.parameters)
	idea.node <- xmlNode(name = "ideaType", namespace = "ns1", idea.type)
	request.node <- xmlNode(name = "requestType", namespace = "ns1", request.type)
	attribute.nodes <- lapply(requested.attribute.types, xmlNode, name = "requestedAttributeTypes", namespace = "ns1")

	start.index.node <- xmlNode(start.index, name = "startIndex", namespace = "ns0")
	number.of.results.node <- xmlNode(number.of.results, name = "numberResults", namespace = "ns0")
	paging.node <- xmlNode(name = "paging", namespace = "ns1", start.index.node, number.of.results.node)

	selector.node <- xmlNode(name = "selector", namespace = "ns1", .children = search.nodes)
	selector.node <- addChildren(selector.node, kids = list(idea.node, request.node))
	selector.node <- addChildren(selector.node, kids = attribute.nodes)
	selector.node <- addChildren(selector.node, kids = list(paging.node))

	get.node <- xmlNode(name = "get", namespace = "ns1", selector.node)
	body.node <- xmlNode(name = "Body", namespace = "ns2", get.node)

	class(body.node) <- c(class(body.node), "adwords.service.request")
	attr(body.node, "service") <- "TargetingIdeaService"
	attr(body.node, "path") <- "https://adwords.google.com/api/adwords/o/"
	attr(body.node, "parser") <- parse.targeting.idea
	body.node
}
