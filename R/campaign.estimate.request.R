#' Adwords Campaign Estimate Request
#'
#' Creates or coerces S3 objects of type \code{campaign.estimate.request} for the Adwords API.
#' \code{is.campaign.estimate.request} will test if an object is interpretable as a campaign.estimate.request.
#'
#' @param aer The adgroup.estimate.request objects to put into a campaign.estimate.request.
#' @param campaign.id The ID for the campaign. When converting a data frame, this parameter will override the existing column with campaign ids, remove it if NULL or leave the column intact if missing (which is the default).
#' @param criteria List of criterion.
#'
#' @return A S3 of type campaign.estimate.request.
#' @export
#'
#' @examples
#' # Minimal example
#' k1 <- keyword("Example")
#' ker1 <- keyword.estimate.request(k1)
#' aer1 <- adgroup.estimate.request(ker1)
#' cer1 <- campaign.estimate.request(aer1)
#'
#' # More complex example
#' k2 <- keyword(c("Example", "Sample", "Test", "Only a test"), match.type = "PHRASE")
#' ker2 <- keyword.estimate.request(k2, max.cpc = max.cpc(c(1, 2, 1, 3), use.micro = TRUE))
#' aer2 <- adgroup.estimate.request(ker2, adgroup.id = 12345)
#' usa <- criterion(2840) # Location for the USA
#' english <- criterion(1000, "Language") # Language is english
#' cer2 <- campaign.estimate.request(aer2, campaign.id = "123456789", criteria = list(usa, english))
#'
#' # Coercing to a campaign.estimate.request
#' cer3 <- as.campaign.estimate.request("Convert", is.negative = TRUE, match.type = "EXACT", max.cpc = 500000, campaign.id = 6789)
#'
#' # Checking a campaign.estimate.request
#' is.campaign.estimate.request(cer3)
campaign.estimate.request <- function(aer, campaign.id = NULL, criteria = NULL)
{
	if(is.null(campaign.id)) campaign.id <- NA
	if(is.null(criteria) & !(is.criterion(criteria) | all(sapply(criteria, is.criterion))))
	{
		stop("argument criteria must contains criterion objects")
	}
	if(is.null(aer) | !(is.adgroup.estimate.request(aer) | all(sapply(aer, is.adgroup.estimate.request))))
	{
		stop("argument aer must contains adgroup.estimate.request objects")
	}

	x <- if(is.adgroup.estimate.request(aer)) list(aer) else aer
	class(x) <- "campaign.estimate.request"
	attr(x, "id") <- campaign.id
	attr(x, "criteria") <- criteria
	x
}

#' @rdname campaign.estimate.request
#' @export
as.campaign.estimate.request <- function(x, ...)
{
	UseMethod("as.campaign.estimate.request", x)
}

#' @rdname campaign.estimate.request
#' @export
as.campaign.estimate.request.data.frame <- function(x, campaign.id, criteria = NULL, ...)
{
	aer <- as.adgroup.estimate.request(x, campaign.id, ...)

	campaign.id <- if(!missing(campaign.id)) campaign.id else if(!is.null(x[["CampaignID"]])) x[["CampaignID"]] else NULL

	if(length(unique(campaign.id)) > 1)
	{
		campaigns <- split(aer, names(aer))
		mapply(campaign.estimate.request, campaigns, campaign.id = names(campaigns), SIMPLIFY = FALSE)
	}
	else
	{
		campaign.estimate.request(aer = aer, campaign.id = unique(campaign.id), criteria = criteria)
	}
}

#' @rdname campaign.estimate.request
#' @export
as.campaign.estimate.request.default <- function(x, campaign.id = NULL, criteria = NULL, ...)
{
	aer <- sapply(x, as.adgroup.estimate.request, ..., simplify = FALSE)

	campaign.estimate.request(aer = aer, campaign.id = campaign.id, criteria = criteria)
}

#' @rdname campaign.estimate.request
#' @export
is.campaign.estimate.request <- function(x)
{
	"campaign.estimate.request" %in% class(x)
}
