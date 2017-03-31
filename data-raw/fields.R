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

report.fields <- function(report.file)
{
	report.path <- file.path(system.file(package = "RAdwords"), "extdata", "api201609", report.file)
	report.fields <- read.csv(report.path, encoding = "UTF-8", stringsAsFactors = FALSE)
	data.frame(Report = report.type(report.file), report.fields, stringsAsFactors = FALSE)
}

adwords.fields.201609 <- data.frame(API = "201609", do.call(rbind, lapply(adwords.reports.201609.files, report.fields)), stringsAsFactors = FALSE)
adwords.fields.201607 <- data.frame(API = "201607", do.call(rbind, lapply(adwords.reports.201607.files, report.fields)), stringsAsFactors = FALSE)
adwords.fields.201605 <- data.frame(API = "201605", do.call(rbind, lapply(adwords.reports.201605.files, report.fields)), stringsAsFactors = FALSE)

adwords.fields <- rbind(adwords.fields.201609, adwords.fields.201607, adwords.fields.201605)

devtools::use_data(adwords.fields, overwrite = TRUE)
