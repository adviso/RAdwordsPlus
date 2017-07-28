#' Retrieve an Adwords Report
#'
#' Downloads an Adwords report. Wrapper function around the \link[RAdwords]{statement} and \link[RAdwords]{getData} functions.
#' Allows for some additional feature with the dates.
#'
#' @param report Name of the report. The list of reports can be found at \url{https://developers.google.com/adwords/api/docs/appendix/reports}.
#' @param cid Customer client ids (or vector of). If more than one CID is provided, the results will be concatenated.
#' @param auth Google AdWords authentification token.
#' @param fields Metrics to select in the report. This can be a mix of attributes, segments and metrics. Default to "AccountDescriptiveName", "Impressions", "Clicks", "Cost", "Date".
#' @param date Either a date range (see \url{https://developers.google.com/adwords/api/docs/guides/reporting#date-ranges}) or a vector of length two with the date interval (start date first, end date last). Default to last fourteen days ("LAST_14_DAYS"). If NULL the during clause will be omitted, getting data for the whole time. Note that this works only if no Date or Week columns are specified (see \url{https://developers.google.com/adwords/api/docs/guides/awql}).
#' @param ... Extra parameters for the RAdwords::statement function
#'
#' @return The report as a data.table with the selected columns.
#' @export
#' @import RAdwords
#' @importFrom data.table rbindlist
#'
#' @examples
#' # Download the search query performance report for the last 14 days.
#' library(RAdwords)
#' report <- "SEARCH_QUERY_PERFORMANCE_REPORT"
#' cid <- c('XXX-XXX-XXXX', 'YYY-YYY-YYYY')
#' auth <- doAuth()
#' fields <- c("AccountDescriptiveName", "AdGroupId", "AdGroupName", "AdGroupStatus", "CampaignId", "CampaignName", "CampaignStatus", "KeywordId", "KeywordTextMatchingQuery", "QueryMatchTypeWithVariant", "Impressions", "Clicks", "Conversions", "Cost", "AveragePosition")
#' report <- get.report(report, cid, auth, fields, date = "LAST_14_DAYS")
get.report <- function(report, cid, auth, fields = c("AccountDescriptiveName", "Impressions", "Clicks", "Cost", "Date"), date = "LAST_14_DAYS", ...)
{
	if(!require(RAdwords)) stop("adwords.report requires package RAdwords, use install_github('jburkhardt/RAdwords')")
	if(!require(data.table)) stop("adwords.report requires package data.table, use install.packages(data.table)")

	query <- RAdwordsPlus::statement(report = report, fields = fields, date = date, ...)
	response <- lapply(cid, getData, google_auth = auth, statement = query)
	error <- is.error(response)
	if(any(error))
	{
		error.index <- which.max(error)
		error.msg <- parse.error(response[[error.index]])
		stop(error.msg)
	}
	data <- rbindlist(response)
	data
}

