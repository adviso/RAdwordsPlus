#' Retrieve an Adwords Report
#'
#' Downloads an Adwords report. Wrapper function around the \link[RAdwords]{statement} and \link[RAdwords]{getData} functions.
#'
#' @param cid Customer client ids (or vector of).
#' @param auth Google AdWords authentification token.
#' @param report Name of the report. Default to the account performance report ("ACCOUNT_PERFORMANCE_REPORT").
#' @param metrics Metrics to select in the report. Default to "AccountDescriptiveName", "Impressions", "Clicks", "Cost", "Date".
#' @param date Either a date range (see \url{https://developers.google.com/adwords/api/docs/guides/reporting#date-ranges}) or a vector of length two with the date interval (start date first, end date last). Default to last fourteen days ("LAST_14_DAYS")
#' @param ... Extra parameters for the RAdwords::statement function
#'
#' @return The report as a data.table with the selected columns.
#' @export
#' @import RAdwords data.table
#'
#' @examples
#' # Download the search query performance report for January 2015
#' auth <- doAuth()
#' report <- "SEARCH_QUERY_PERFORMANCE_REPORT"
#' select <- c("AccountDescriptiveName", "AdGroupId", "AdGroupName", "AdGroupStatus", "CampaignId", "CampaignName", "CampaignStatus", "KeywordId", "KeywordTextMatchingQuery", "QueryMatchTypeWithVariant", "Impressions", "Clicks", "Conversions", "Cost", "AveragePosition")
#' cid <- c('XXX-XXX-XXXX', 'YYY-YYY-YYYY')
#' report <- adwords.report(cid, auth, report, metrics, date = "LAST_14_DAYS")
adwords.report <- function(cid, auth, report = "ACCOUNT_PERFORMANCE_REPORT", metrics = c("AccountDescriptiveName", "Impressions", "Clicks", "Cost", "Date"), date = "LAST_14_DAYS", ...)
{
	if(!require(RAdwords)) stop("adwords.report requires package RAdwords, use install_github('jburkhardt/RAdwords')")
	if(!require(data.table)) stop("adwords.report requires package data.table, use install.packages(data.table)")

	query <- RAdwordsPlus::statement(report = report, metrics = metrics, date = date, ...)
	clients.data <- lapply(cid, getData, google_auth = auth, statement = query)
	data <- tryCatch({rbindlist(clients.data)}, error = function(e){print(clients.data); print(e)})
	data
}

