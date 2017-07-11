report.name <- function(report.file = "account-performance-report.csv")
{
	name <- sub(".csv", "", report.file)
	name <- gsub("-", "_", name)
	name <- toupper(name)
	name
}

display.name <- function(report.file = "account-performance-report.csv")
{
	name <- sub(".csv", "", report.file)
	name <- gsub("-", " ", name)
	name <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", name, perl = TRUE)
	name
}

folders <- list.files(system.file(package = "RAdwords", "extdata"))
files <- lapply(folders, function(folder){list.files(system.file(package = "RAdwords", file.path("extdata", folder)))})
adwords.reports <- do.call(rbind, mapply(function(folder, file){data.frame(API = gsub("api", "", folder), Name = report.name(file), Display.Name = display.name(file), stringsAsFactors = FALSE)}, folders, files, SIMPLIFY = FALSE))
row.names(adwords.reports) <- NULL

devtools::use_data(adwords.reports, overwrite = TRUE)
