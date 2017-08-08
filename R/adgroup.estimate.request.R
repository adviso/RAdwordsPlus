#' Adwords Adgroup Estimate Request
#'
#' Creates or coerces S3 objects of type \code{adgroup.estimate.request} for the Adwords API.
#' \code{is.adgroup.estimate.request} will test if an object is interpretable as an adgroup.estimate.request.
#' When coercing to an adgroup.estimate.request, max.cpc will be applied to the adgroup.estimate.request.
#' To supply a max.cpc to the keywords, use argument keyword.max.cpc instead.
#'
#' @param ker The keyword.estimate.request objects to put into a adgroup.estimate.request.
#' @param adgroup.id The ID for the adgroup. When converting a data frame, this parameter will override the existing column with adgroup ids, remove it if NULL or leave the column intact if missing (which is the default).

#' @param max.cpc The bid for the adgroup.
#'
#' @return A S3 of type adgroup.estimate.request.
#' @export
#'
#' @examples
#' # Minimal example
#' k1 <- keyword("Example")
#' ker1 <- keyword.estimate.request(k1)
#' aer1 <- adgroup.estimate.request(ker1)
#'
#' # More complex example
#' k2 <- keyword(c("Example", "Sample", "Test", "Only a test"), match.type = "PHRASE")
#' ker2 <- keyword.estimate.request(k2, max.cpc = max.cpc(c(1, 2, 1, 3), use.micro = TRUE))
#' aer2 <- adgroup.estimate.request(ker2, adgroup.id = 12345)
#'
#' # Coercing to an adgroup.estimate.request
#' aer3 <- as.adgroup.estimate.request("Convert", is.negative = TRUE, match.type = "EXACT", max.cpc = 500000)
#'
#' # Checking an adgroup.estimate.request
#' is.adgroup.estimate.request(aer3)
adgroup.estimate.request <- function(ker, adgroup.id = NULL, max.cpc = NULL)
{
	adgroup.id[is.null(adgroup.id)] <- NA
	max.cpc[is.null(max.cpc)] <- NA
	if(is.null(ker) | !(is.keyword.estimate.request(ker) | all(sapply(ker, is.keyword.estimate.request))))
	{
		stop("argument ker must contains keyword.estimate.request objects")
	}
	duplicates <- duplicated(as.character(ker))
	if(any(duplicates))
	{
		count <- sum(duplicates)
		warning(paste(count, "duplicated", ifelse(count == 1, "keyword", "keywords"), "removed from adgroup.estimate.request"))
		ker <- ker[!duplicates]
	}

	x <- if(is.keyword.estimate.request(ker)) list(ker) else ker
	class(x) <- "adgroup.estimate.request"
	if(!invalid(adgroup.id)) attr(x, "id") <- adgroup.id
	if(!invalid(max.cpc)) attr(x, "max.cpc") <- as.max.cpc(max.cpc)
	x
}

#' @rdname adgroup.estimate.request
#' @export
as.adgroup.estimate.request <- function(x, ...)
{
	UseMethod("as.adgroup.estimate.request", x)
}

#' @rdname adgroup.estimate.request
#' @export
as.adgroup.estimate.request.data.frame <- function(x, adgroup.id, campaign.id, ...)
{
	ker <- as.keyword.estimate.request(x, ...)

	adgroup.id <- if(!missing(adgroup.id)) adgroup.id else if(!is.null(x[["AdgroupID"]])) x[["AdgroupID"]] else NULL
	campaign.id <- if(!missing(campaign.id)) campaign.id else if(!is.null(x[["CampaignID"]])) x[["CampaignID"]] else NULL

	if(length(adgroup.id) > 1 & length(campaign.id) > 1)
	{
		if(length(adgroup.id) != length(campaign.id)) stop("adgroup.id and campaign.id must have the same length")
	}

	id <- if(length(adgroup.id) > 1) adgroup.id else campaign.id

	if(length(id) > 1 & length(id) == length(ker))
	{
		adgroups <- split(ker, id)
		if(length(adgroup.id) > 1)
		{
			aer <- mapply(adgroup.estimate.request, adgroups, adgroup.id = names(adgroups), SIMPLIFY = FALSE)
			if(length(campaign.id) > 1)
			{
				i <- match(names(aer), adgroup.id)
				names(aer) <- campaign.id[i]
			}
			aer
		}
		else
		{
			lapply(adgroups, adgroup.estimate.request)
		}
	}
	else
	{
		adgroup.estimate.request(ker = ker, adgroup.id = adgroup.id)
	}
}

#' @rdname adgroup.estimate.request
#' @export
as.adgroup.estimate.request.default <- function(x, ...)
{
	args = list(...)
	adgroup.id <- args[["adgroup.id"]]
	args[["adgroup.id"]] <- NULL
	max.cpc = args[["max.cpc"]]
	args[["max.cpc"]] <- NULL
	keyword.max.cpc = args[["keyword.max.cpc"]]
	args[["keyword.max.cpc"]] <- NULL
	ker <- do.call(as.keyword.estimate.request, c(x, args, max.cpc = keyword.max.cpc))

	adgroup.estimate.request(ker = ker, adgroup.id = adgroup.id, max.cpc = max.cpc)
}

#' @rdname adgroup.estimate.request
#' @export
is.adgroup.estimate.request <- function(x)
{
	"adgroup.estimate.request" %in% class(x)
}
