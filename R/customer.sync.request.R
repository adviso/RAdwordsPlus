#' Adwords Customer Sync Request
#'
#' Prepares the body of an XML formatted request for the Adwords customer sync service.
#'
#' @param campaign.ids Vector or list of campaign ids for which we want the changes.
#' @param campaign.ids Vector or list of feed ids for which we want the changes.
#' @param min.date.time The min date time of the range for the changes in yyyyMMdd HHmmss <Timezone ID> format. Default to system time if NULL.
#' @param max.date.time The min date time of the range for the changes in yyyyMMdd HHmmss <Timezone ID> format. Default to system time if NULL.
#'
#' @return An XML formatted customer sync request.
#' @export
#' @import XML
#' @seealso get.service
#'
#' @examples
#' request <- customer.sync.request(campaign.ids = c("123456789", "456789123", "789123456"))
customer.sync.request <- function(campaign.ids, feed.ids = NULL, min.date.time = NULL, max.date.time = NULL)
{
	if(!require(XML)) stop("customer.sync.request requires package XML")

	if(is.null(min.date.time))
	{
		min.date.time <- Sys.time() - 24*60*60
	}

	if(is.null(max.date.time))
	{
		max.date.time <- Sys.time()
	}

	min.node <- xmlNode(format(min.date.time, "%Y%m%d %H%M%S"), name = "min", namespace = "ns0")
	max.node <- xmlNode(format(max.date.time, "%Y%m%d %H%M%S"), name = "max", namespace = "ns0")
	range.node <- xmlNode(date.time.range[1], name = "dateTimeRange", namespace = "ns2", .children = list(min.node, max.node))

	campaign.nodes <- lapply(campaign.ids, xmlNode, name = "campaignIds", namespace = "ns2")

	feed.nodes <- lapply(feed.ids, xmlNode, name = "feedIds", namespace = "ns2")

	selector.node <- xmlNode(name = "selector", namespace = "ns1", .children = c(list(range.node), campaign.nodes, feed.nodes))

	get.node <- xmlNode(name = "get", namespace = "ns1", selector.node)
	body.node <- xmlNode(name = "Body", namespace = "ns2", get.node)

	class(body.node) <- c(class(body.node), "adwords.service.request")
	attr(body.node, "service") <- "CustomerSyncService"
	attr(body.node, "path") <- "https://adwords.google.com/api/adwords/ch/"
	attr(body.node, "parser") <- customer.sync.parser
	body.node
}
