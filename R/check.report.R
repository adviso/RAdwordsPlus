#' Adwords Report Error Handling
#'
#' Checks a report response in the \link{get.report} function.
#' If the response is a data.frame then the request worked.
#' If the response is still in XML than an error occured.
#' The XML is parsed to extract and return the error message.
#'
#' @param x The result from a report request.
#'
#' @return An error message if an error is found, or NULL if none were found.
#' @export
#' @import xml2
#' @seealso get.report
#'
#' @examples
#' # Try to get a non existing report.
#' library(RAdwords)
#' report <- "INVALID_REPORT"
#' cid <- "XXX-XXX-XXXX"
#' auth <- doAuth()
#' fields <- c("AccountDescriptiveName", "AdGroupId", "AdGroupName", "AdGroupStatus", "CampaignId", "CampaignName", "CampaignStatus", "KeywordId", "KeywordTextMatchingQuery", "QueryMatchTypeWithVariant", "Impressions", "Clicks", "Conversions", "Cost", "AveragePosition")
#' report <- get.report(report, cid, auth, fields, date = "LAST_14_DAYS")
check.report <- function(x)
{
	if(!require(xml2)) stop("check.report requires package xml2")

	if(!is.data.frame(x))
	{
		doc <- read_xml(x)
		xml_ns_strip(doc)

		if(!is.na(xml_find_first(doc, "/reportDownloadError", xml_ns(doc))))
		{
			report.error.node <- xml_find_first(doc, "/reportDownloadError", xml_ns(doc))
			api.error.node <- xml_find_first(report.error.node, ".//ApiError", xml_ns(doc))
			if(!is.na(api.error.node))
			{
				message <- xml_text(xml_find_first(api.error.node, ".//type", xml_ns(doc)))
				return(message)
			}
		}
	}

	NULL
}
