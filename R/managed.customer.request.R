#' Adwords Managed Customer Service Request
#'
#' Prepares the body of an XML formatted request for the Adwords managed customer service.
#' Only data retrieval is implemented for now.
#'
#' @param fields List of fields to select.
#' @param predicates Specifies how an entity (eg. adgroup, campaign, criterion, ad) should be filtered.
#' @param start.index Index of the first result to return with this request. Default is 0.
#' @param number.of.results Maximum number of results to return in this request. Default is 100.
#'
#' @return An XML formatted managed customer request.
#' @export
#' @import XML
#' @seealso get.service
#'
#' @examples
#' # Prepare a request to get all account in a MCC that starts with "USA" and doesn't contain "inc".
#' preds <- list(predicate("Name", "STARTS_WITH_IGNORE_CASE", "USA"), predicate("Name", "DOES_NOT_CONTAIN_IGNORE_CASE", "inc"))
#' request <- managed.customer.request(fields = c("Name", "CustomerId"), predicates = preds)
managed.customer.request <- function(fields = c("Name", "CustomerId"), predicates = NULL, start.index = 0, number.of.results = 100)
{
	if(!require(XML)) stop("managed.customer.request requires package XML")

	fields.nodes = lapply(fields, xmlNode, name = "fields", namespace = "ns0")

	predicate.nodes <- as.xml(predicates)

	start.index.node <- xmlNode(start.index, name = "startIndex", namespace = "ns0")
	number.of.results.node <- xmlNode(number.of.results, name = "numberResults", namespace = "ns0")
	paging.node <- xmlNode(name = "paging", namespace = "ns1", start.index.node, number.of.results.node)

	selector.node <- xmlNode(name = "serviceSelector", namespace = "ns1", .children = fields.nodes)
	if(!is.null(predicate.nodes))
	{
		if("XMLNode" %in% class(predicate.nodes))
		{
			selector.node <- addChildren(selector.node, kids = list(predicate.nodes))
		} else
		{
			selector.node <- addChildren(selector.node, kids = predicate.nodes)
		}
	}
	selector.node <- addChildren(selector.node, kids = list(paging.node))
	get.node <- xmlNode(name = "get", namespace = "ns1", selector.node)
	body.node <- xmlNode(name = "Body", namespace = "ns2", get.node)

	class(body.node) <- c(class(body.node), "adwords.service.request")
	attr(body.node, "service") <- "ManagedCustomerService"
	attr(body.node, "path") <- "https://adwords.google.com/api/adwords/mcm/"
	attr(body.node, "return") <- "managed.customer"
	body.node
}
