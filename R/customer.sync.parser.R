#' XML Parsing for Adwords Customer Sync
#'
#' Parses an Adwords customer sync service response in XMl format into a TODO.
#'
#' @param x Customer sync response in XML.
#'
#' @return A list of two data.frames with the entries and the links.
#' @export
#' @import xml2
#'
#' @examples
#' # For this exemple to work, you must supply a valid client customer id and your developper token
#' TODO
customer.sync.parser <- function(x, ...)
{
	if(!require(xml2)) stop("parse.managed.customer requires package xml2")

	doc <- read_xml(x)
	xml_ns_strip(doc)

	#TODO manage 0 data
	changed.campaigns.nodes <- xml_find_all(doc, ".//ns2:changedCampaigns", xml_ns(doc))
	customer.sync <- if(length(changed.campaigns.nodes) == 0)
	{
		warning("x is not a valid customer sync response")
		NULL
	}
	else
	{
		changed.campaigns <- lapply(changed.campaigns.nodes, changed.campaigns.parser)
		do.call(rbind, changed.campaigns)
	}

	customer.sync
}

#' @rdname customer.sync.parser
#' @export
changed.campaigns.parser <- function(node)
{
	if(xml_name(node) != "changedCampaigns") stop("changed.campaigns.parser only accepts changedCampaigns nodes")

	id <- xml_text(xml_find_first(node, ".//ns2:campaignId", xml_ns(node)))
	status <- xml_text(xml_find_first(node, ".//ns2:campaignChangeStatus", xml_ns(node)))

	changed.adgroups.nodes <- xml_find_all(node, ".//ns2:changedAdGroups", xml_ns(node))
	changed.adgroups <- lapply(changed.adgroups.nodes, changed.adgroups.parser)
	changed.adgroups.df <- do.call(rbind, changed.adgroups)

	changes <- data.frame()

	added.campaign.criteria <- xml_text(xml_find_all(node, ".//ns2:addedCampaignCriteria", xml_ns(node)))
	if(length(added.campaign.criteria) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "CAMPAIGN_CRITERIA_ADDED", Id = added.campaign.criteria, stringsAsFactors = FALSE))
	}

	removed.campaign.criteria <- xml_text(xml_find_all(node, ".//ns2:removedCampaignCriteria", xml_ns(node)))
	if(length(removed.campaign.criteria) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "CAMPAIGN_CRITERIA_REMOVED", Id = removed.campaign.criteria, stringsAsFactors = FALSE))
	}

	changed.feeds <- xml_text(xml_find_all(node, ".//ns2:changedFeeds", xml_ns(node)))
	if(length(changed.feeds) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "CAMPAIGN_FEED_CHANGED", Id = changed.feeds, stringsAsFactors = FALSE))
	}

	removed.feeds <- xml_text(xml_find_all(node, ".//ns2:removedFeeds", xml_ns(node)))
	if(length(removed.feeds) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "CAMPAIGN_FEED_REMOVED", Id = removed.feeds, stringsAsFactors = FALSE))
	}

	if(!is.null(changed.adgroups.df))
	{
		changes <- rbind(changed.adgroups.df, data.frame(AdgroupID = NA, AdgroupStatus = NA, changes, stringsAsFactors = FALSE))
	}

	if(is.null(changes))
	{
		data.frame(CampaignID = id, CampaignStatus = status, stringsAsFactors = FALSE)
	}
	else
	{
		data.frame(CampaignID = id, CampaignStatus = status, changes, stringsAsFactors = FALSE)
	}
}

#' @rdname customer.sync.parser
#' @export
changed.adgroups.parser <- function(node)
{
	if(xml_name(node) != "changedAdGroups") stop("changed.adgroups.parser only accepts changedAdGroups nodes")

	id <- xml_text(xml_find_first(node, ".//ns2:adGroupId", xml_ns(node)))
	status <- xml_text(xml_find_first(node, ".//ns2:adGroupChangeStatus", xml_ns(node)))

	changes <- data.frame()

	changed.ads <- xml_text(xml_find_all(node, ".//ns2:changedAds", xml_ns(node)))
	if(length(changed.ads) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "AD_CHANGED", Id = changed.ads, stringsAsFactors = FALSE))
	}

	changed.criteria <- xml_text(xml_find_all(node, ".//ns2:changedCriteria", xml_ns(node)))
	if(length(changed.criteria) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "CRITERION_CHANGED", Id = changed.criteria, stringsAsFactors = FALSE))
	}

	removed.criteria <- xml_text(xml_find_all(node, ".//ns2:removedCriteria", xml_ns(node)))
	if(length(removed.criteria) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "CRITERION_REMOVED", Id = removed.criteria, stringsAsFactors = FALSE))
	}

	changed.feeds <- xml_text(xml_find_all(node, ".//ns2:changedFeeds", xml_ns(node)))
	if(length(changed.feeds) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "FEED_CHANGED", Id = changed.feeds, stringsAsFactors = FALSE))
	}

	removed.feeds <- xml_text(xml_find_all(node, ".//ns2:removedFeeds", xml_ns(node)))
	if(length(removed.feeds) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "FEED_REMOVED", Id = removed.feeds, stringsAsFactors = FALSE))
	}

	changed.bid <- xml_text(xml_find_all(node, ".//ns2:changedAdGroupBidModifierCriteria", xml_ns(node)))
	if(length(changed.bid) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "ADGROUP_BID_MODIFER_CHANGED", Id = changed.bid, stringsAsFactors = FALSE))
	}

	removed.bid <- xml_text(xml_find_all(node, ".//ns2:removedAdGroupBidModifierCriteria", xml_ns(node)))
	if(length(removed.bid) > 0)
	{
		changes <- rbind(changes, data.frame(Modification = "ADGROUP_BID_MODIFER_REMOVED", Id = removed.bid, stringsAsFactors = FALSE))
	}

	if(is.null(changes))
	{
		data.frame(AdgroupID = id, AdgroupStatus = status, stringsAsFactors = FALSE)
	}
	else
	{
		data.frame(AdgroupID = id, AdgroupStatus = status, changes, stringsAsFactors = FALSE)
	}
}
