adwords.reports.201609.files <- list.files(system.file(package = "RAdwords", "extdata/api201609/"))
adwords.reports.201607.files <- list.files(system.file(package = "RAdwords", "extdata/api201607/"))
adwords.reports.201605.files <- list.files(system.file(package = "RAdwords", "extdata/api201605/"))

report.type <- function(report.file = "account-performance-report.csv")
{
	report.type <- sub(".csv", "", report.file)
	report.type <- gsub("-", "_", report.type)
	report.type <- toupper(report.type)
	report.type
}

report.name <- function(report.file = "account-performance-report.csv")
{
	report.name <- sub(".csv", "", report.file)
	report.name <- gsub("-", " ", report.name)
	report.name <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", report.name, perl = TRUE)
	report.name
}

adwords.reports.201609 <- data.frame(API = "201609", Name = report.type(adwords.reports.201609.files), Display.Name = report.name(adwords.reports.201609.files), stringsAsFactors = FALSE)
adwords.reports.201607 <- data.frame(API = "201607", Name = report.type(adwords.reports.201607.files), Display.Name = report.name(adwords.reports.201607.files), stringsAsFactors = FALSE)
adwords.reports.201605 <- data.frame(API = "201605", Name = report.type(adwords.reports.201605.files), Display.Name = report.name(adwords.reports.201605.files), stringsAsFactors = FALSE)

adwords.reports <- rbind(adwords.reports.201609, adwords.reports.201607, adwords.reports.201605)

devtools::use_data(adwords.reports, overwrite = TRUE)
