#' XML Parsing for the Traffic Estimator Service.
#'
#' Parses a \link{traffic.estimator} response in XMl format into a data frame containing the results of the request.
#' The data frame is in long format.
#' Use dcast in package reshape2 with \code{formula = CampaignID + AdgroupID + KeywordID ~ Metric} and \code{value.var = "Value"} to get a data.frame in wide format.
#'
#' There's also a parse function for \link{traffic.estimator.request} to convert the requests a list of \link{campaign.estimate.request}.
#'
#' @param x traffic.estimator response in XML.
#'
#' @return A data.frame with the estimate results.
#' @export
#' @import xml2
#'
#' @examples
#' # For this exemple to work, you must supply a valid client customer id and your developper token
#' library(RAdwords)
#' k <- keyword(c("Example", "Test"), match.type = c("EXACT", "PHRASE"))
#' ker <- keyword.estimate.request(k, max.cpc = c(1500000, 2250000))
#' aer <- adgroup.estimate.request(ker)
#' cer <- campaign.estimate.request(aer, campaign.id = "123456789")
#' request <- traffic.estimator.request(cer)
#' estimate <- get.service(request, cid = "YOUR_CID", auth = doAuth(), dev.token = "YOUR_DEV_TOKEN", user.agent = "AKTE Exemple")
#'
#' # Parse the request to get back the data
#' parse(request)
#'
#' # Parse the estimate to get the results
#' traffic.estimator.parser(estimate)
traffic.estimator.parser <- function(x, ...)
{
	if(!require(xml2)) stop("traffic.estimator.parser requires package xml2")

	doc <- read_xml(x)
	xml_ns_strip(doc)

	campaign.estimates <- xml_find_all(doc, ".//campaignEstimates", xml_ns(doc))
	if(length(campaign.estimates) == 0)
	{
		warning("x is not a valid traffic estimate")
		NULL
	}
	else
	{
		campaign.results <- mapply(campaign.estimate.parser, campaign.estimates, paste0("#", default.id = 1:length(campaign.estimates)), SIMPLIFY = FALSE)
		do.call(rbind, campaign.results)
	}
}

#' @rdname traffic.estimator.parser
#' @export
campaign.estimate.parser <- function(node, default.id = NULL)
{
	if(!require(xml2)) stop("campaign.estimate.parser requires package xml2")
	if(xml_name(node) != "campaignEstimates") stop("campaign.estimate.parser only accepts campaignEstimates nodes")

	node.id <- xml_text(xml_find_first(node, ".//campaignId", xml_ns(node)))
	campaign.id <- if(is.null(node.id) | is.na(node.id)) default.id else node.id

	adgroup.estimates <- xml_find_all(node, ".//adGroupEstimates", xml_ns(node))
	adgroup.results <- mapply(adgroup.estimate.parser, adgroup.estimates, default.id = paste0("#", 1:length(adgroup.estimates)), SIMPLIFY = FALSE)

	data.frame(CampaignID = campaign.id, do.call(rbind, adgroup.results), stringsAsFactors = FALSE)
}

#' @rdname traffic.estimator.parser
#' @export
adgroup.estimate.parser <- function(node, default.id = NULL)
{
	if(!require(xml2)) stop("adgroup.estimate.parser requires package xml2")
	if(xml_name(node) != "adGroupEstimates") stop("adgroup.estimate.parser only accepts adGroupEstimates nodes")

	node.id <- xml_text(xml_find_first(node, ".//adGroupId", xml_ns(node)))
	adgroup.id <- if(is.null(node.id) | is.na(node.id)) default.id else node.id

	keyword.estimates <- xml_find_all(node, ".//keywordEstimates", xml_ns(node))
	keyword.results <- mapply(keyword.estimate.parser, keyword.estimates, default.id = paste0("#", 1:length(keyword.estimates)), SIMPLIFY = FALSE)

	data.frame(AdgroupID = adgroup.id, do.call(rbind, keyword.results), stringsAsFactors = FALSE)
}

#' @rdname traffic.estimator.parser
#' @export
keyword.estimate.parser <- function(node, default.id = NULL)
{
	if(!require(xml2)) stop("keyword.estimate.parser requires package xml2")
	if(xml_name(node) != "keywordEstimates") stop("keyword.estimate.parser only accepts keywordEstimates nodes")

	node.id <- xml_text(xml_find_first(node, ".//criterionId", xml_ns(node)))
	keyword.id <- if(is.null(node.id) | is.na(node.id)) default.id else node.id

	min.node <- xml_find_first(node, ".//min", xml_ns(node))
	min.values <- gsub("Money", "", sapply(xml_children(min.node), xml_text))
	min.values <- ifelse(min.values == "<NA>", NA, as.numeric(min.values))
	min.names <- paste0("Min", capitalize(sapply(xml_children(min.node), xml_name)))

	max.node <- xml_find_first(node, ".//max", xml_ns(node))
	max.values <- gsub("Money", "", sapply(xml_children(max.node), xml_text))
	max.values <- ifelse(max.values == "<NA>", NA, as.numeric(max.values))
	max.names <- paste0("Max", capitalize(sapply(xml_children(max.node), xml_name)))

	data.frame(KeywordID = keyword.id, Metric = c(min.names, max.names), Value = c(min.values, max.values), stringsAsFactors = FALSE)
}
