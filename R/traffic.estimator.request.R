#' Adwords Traffic Estimator Request
#'
#' Encodes a \code{\link{campaign.estimate.request}} into a an XML formatted request for the Adwords Keyword Traffic Estimator.
#' \code{is.traffic.estimator.request} will test if an object is interpretable as a traffic.estimator.request.
#'
#' @param cer campaign.estimate.request to encode.
#'
#' @return An XML formatted estimate request.
#' @export
#' @import XML
#'
#' @examples
#' # For this exemple to work, you must supply a valid client customer id and your developper token
#' k <- keyword(c("Example", "Test"), match.type = c("EXACT", "PHRASE"))
#' ker <- keyword.estimate.request(k, max.cpc = c(1500000, 2250000))
#' aer <- adgroup.estimate.request(ker)
#' cer <- campaign.estimate.request(aer, campaign.id = "123456789")
#' request <- traffic.estimator.request(cer, cid = "YOUR_CID", dev.token = "YOUR_DEV_TOKEN", user.agent = "AKTE Exemple")
traffic.estimator.request <- function(cer, cid, dev.token, user.agent, api.version = "v201702", validate.only = FALSE, partial.failure = FALSE)
{
	if(!require(XML)) stop("traffic.estimator.request requires package XML")

	selector.node <- xmlNode(name = "selector", namespace = "ns1")
	if(is.campaign.estimate.request(cer))
	{
		campaign.node <- as.xml(cer)
		selector.node <- addChildren(node = selector.node, kids = list(campaign.node))
	}
	else
	{
		campaign.nodes <- lapply(cer, as.xml)
		selector.node <- addChildren(node = selector.node, kids = campaign.nodes)
	}
	platform.node <- xmlNode("false", name = "platformEstimateRequested", namespace = "ns1")
	selector.node <- addChildren(selector.node, kids = list(platform.node))

	get.node <- xmlNode(name = "get", namespace = "ns1", selector.node)
	body.node <- xmlNode(name = "Body", namespace = "ns2", get.node)

	class(body.node) <- c(class(body.node), "adwords.service.request", "traffic.estimator.request")
	attr(body.node, "service") <- "TrafficEstimatorService"
	attr(body.node, "path") <- "https://adwords.google.com/api/adwords/o/"
	attr(body.node, "parser") <- traffic.estimator.parser
	body.node
}

#' @rdname traffic.estimator.request
#' @export
is.traffic.estimator.request <- function(x)
{
	"traffic.estimator.request" %in% class(x)
}
