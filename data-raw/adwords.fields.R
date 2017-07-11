report.name <- function(report.file = "account-performance-report.csv")
{
	name <- sub(".csv", "", report.file)
	name <- gsub("-", "_", name)
	name <- toupper(name)
	name
}

report.fields <- function(folder, file)
{
	report.path <- file.path(system.file(package = "RAdwords"), "extdata", folder, file)
	report.fields <- read.csv(report.path, encoding = "UTF-8", stringsAsFactors = FALSE)
	data.frame(API = gsub("api",  "", folder), Report = report.name(file), report.fields, stringsAsFactors = FALSE)
}

folders <- list.files(system.file(package = "RAdwords", "extdata"))
files <- lapply(folders, function(folder){list.files(system.file(package = "RAdwords", file.path("extdata", folder)))})
adwords.fields <- do.call(rbind, mapply(function(folder, folder.files){do.call(rbind, lapply(folder.files, report.fields, folder = folder))}, folders, files, SIMPLIFY = FALSE))
row.names(adwords.fields) <- NULL

devtools::use_data(adwords.fields, overwrite = TRUE)
