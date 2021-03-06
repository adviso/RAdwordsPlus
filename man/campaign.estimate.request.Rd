% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/campaign.estimate.request.R
\name{campaign.estimate.request}
\alias{campaign.estimate.request}
\alias{as.campaign.estimate.request}
\alias{as.campaign.estimate.request.data.frame}
\alias{as.campaign.estimate.request.default}
\alias{is.campaign.estimate.request}
\title{Adwords Campaign Estimate Request}
\usage{
campaign.estimate.request(aer, campaign.id = NULL, criteria = NULL)

as.campaign.estimate.request(x, ...)

\method{as.campaign.estimate.request}{data.frame}(x, campaign.id,
  criteria = NULL, ...)

\method{as.campaign.estimate.request}{default}(x, campaign.id = NULL,
  criteria = NULL, ...)

is.campaign.estimate.request(x)
}
\arguments{
\item{aer}{The adgroup.estimate.request objects to put into a campaign.estimate.request.}

\item{campaign.id}{The ID for the campaign. When converting a data frame, this parameter will override the existing column with campaign ids, remove it if NULL or leave the column intact if missing (which is the default).}

\item{criteria}{List of criterion.}
}
\value{
A S3 of type campaign.estimate.request.
}
\description{
Creates or coerces S3 objects of type \code{campaign.estimate.request} for the Adwords API.
\code{is.campaign.estimate.request} will test if an object is interpretable as a campaign.estimate.request.
}
\examples{
# Minimal example
k1 <- keyword("Example")
ker1 <- keyword.estimate.request(k1)
aer1 <- adgroup.estimate.request(ker1)
cer1 <- campaign.estimate.request(aer1)

# More complex example
k2 <- keyword(c("Example", "Sample", "Test", "Only a test"), match.type = "PHRASE")
ker2 <- keyword.estimate.request(k2, max.cpc = max.cpc(c(1, 2, 1, 3), use.micro = TRUE))
aer2 <- adgroup.estimate.request(ker2, adgroup.id = 12345)
usa <- criterion(2840) # Location for the USA
english <- criterion(1000, "Language") # Language is english
cer2 <- campaign.estimate.request(aer2, campaign.id = "123456789", criteria = list(usa, english))

# Coercing to a campaign.estimate.request
cer3 <- as.campaign.estimate.request("Convert", is.negative = TRUE, match.type = "EXACT", max.cpc = 500000, campaign.id = 6789)

# Checking a campaign.estimate.request
is.campaign.estimate.request(cer3)
}
