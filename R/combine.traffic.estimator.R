#' Combine Adwords Traffic Request and Estimate
#'
#' Combines a \link{campaign.estmate.request} and the \link{traffic.estimator} obtained with this campaign.estimate.request into one final data.frame.
#' In the Adwords Traffict Estimate, the requested keywords are not returned.
#' This method parse the returned estimates and join the keywords from the request.
#' The solution uses the fact that the results in the estimate are in the same order as the keywords in the request.
#'
#' @param request An object of type campaign.estmate.request
#' @param estimate An object of type traffic.estimator.
#'
#' @return A data frame with the combined informations.
#' @export
#' @import reshape2
#'
#' @examples
#' # For this exemple to work, you must supply a valid client customer id and your developper token
#' library(RAdwords)
#' k <- keyword(c("Example", "Test"), match.type = c("EXACT", "PHRASE"))
#' ker <- keyword.estimate.request(k, max.cpc = c(1500000, 2250000))
#' aer <- adgroup.estimate.request(ker)
#' cer <- campaign.estimate.request(aer, campaign.id = "123456789")
#' request <- traffic.estimator.request(cer)
#' estimate <- get.service(request, cid = "YOUR_CID", auth = doAuth(), cid = "YOUR_CID", dev.token = "YOUR_DEV_TOKEN", user.agent = "AKTE Exemple")
#' combine.traffic.estimator(cer, estimate)
combine.traffic.estimator <- function(cer, estimate)
{
	if(!require(reshape2)) stop("combine.traffic.estimator requires package reshape2")
	if(!is.campaign.estimate.request(cer)) stop("request must be a campaign.estimate.request")
	if(!is.data.frame(estimate)) stop("estimate must be a data.frame")

	# Estimates are in long table format and must be converted to wide table format first
	wide.estimate <- dcast(estimate, formula = CampaignID + AdgroupID + KeywordID ~ Metric, value.var = "Value")
	cer.df <- as.data.frame(cer)
	keywords <- cer.df[, "Keyword"]
	match.types <- cer.df[, "Matchtype"]
	max.cpc <- cer.df[, "Max.CPC"]
	data.frame(Keyword = keywords, Matchtype = match.types, Max.CPC = max.cpc, wide.estimate, stringsAsFactors = FALSE)
}
