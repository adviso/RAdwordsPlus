#' Prepare an Adwards Query Language Statement
#'
#' Builds an Adwards Query Language Statement.
#'
#' @param report Name of the report. Default to the account performance report ("ACCOUNT_PERFORMANCE_REPORT").
#' @param fields Fields to select in the report. This can be a mix of attributes, segments and metrics. Default to "AccountDescriptiveName", "Impressions", "Clicks", "Cost", "Date".
#' @param date Either a date range (see \url{https://developers.google.com/adwords/api/docs/guides/reporting#date-ranges}) or a vector of length two with the date interval (start date first, end date last). Default to last fourteen days ("LAST_14_DAYS")
#' @param where Conditions to use in the statement, like restrictions on a metric
#'
#' @return A character string with the content of the query.
#' @export
#'
#' @examples
#' statement(report = "ACCOUNT_PERFORMANCE_REPORT", fields = c("AccountDescriptiveName", "Impressions", "Clicks", "Cost", "Date"), date = "LAST_14_DAYS")
statement <- function(report = "ACCOUNT_PERFORMANCE_REPORT", fields = c("AccountDescriptiveName", "Impressions", "Clicks", "Cost", "Date"), date = "LAST_14_DAYS", where)
{
	query <- paste0("__rdquery=SELECT+", paste(fields, collapse = ","))
	query <- paste0(query, "+FROM+", report)

	if(!missing(where))
	{
		query <- paste0(query, "+WHERE+", where)
	}

	during <- date
	if(length(date) == 2)
	{
		start <- gsub("-", "", date[1])
		end <- gsub("-", "", date[2])
		during <- paste(start, end, sep = ",")
	}

	if(report != "LABEL_REPORT")
	{
		query <- paste0(query, "+DURING+", during)
	}

	query <- paste0(query, "&__fmt=CSV")
	attr(query, "reportType") <- report
	query
}
