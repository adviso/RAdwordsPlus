#' @export
as.data.frame.adgroup.estimate.request <- function(x, ...)
{
	m <- apply(t(sapply(x, as.list)), 2, unlist)
	id <- attr(x, "id")
	if(is.null(id)) id <- NA
	max.cpc <- attr(x, "max.cpc")
	if(is.null(max.cpc)) max.cpc <- NA
	data.frame(AdgroupID = rep(id, length(x)), AdgroupMaxCPC = rep(max.cpc, length(x)), m, stringsAsFactors = FALSE)
}

#' @export
as.data.frame.campaign.estimate.request <- function(x, ...)
{
	df <- do.call(rbind, lapply(x, as.data.frame))
	id <- attr(x, "id")
	if(is.null(id)) id <- NA
	data.frame(CampaignID = rep(id, nrow(df)), df)
}
