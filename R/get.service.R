#' Call an Adwords Service
#'
#' Sends a request for a Google Adwords service.
#'
#' @param request XMLNode for the body of an XML encoded Adwords service request.
#' @param cid client customer id.
#' @param auth Google authentification made with \code{\link{doAuth}}.
#' @param user.agent User agent string for your script, read \url{http://googleadsdeveloper.blogspot.ca/2013/11/please-set-user-agent-or-application.html} for more information.
#' @param api.version version of the Adwords API to use, must be the same as in the request. Default to version v201702.
#' @param validate.only Test the API call without actually executing the call against real data. Default to FALSE.
#' @param partial.failure If true service will carry out the operations that had no errors. Default to FALSE.
#' @param verbose TRUE to turn on the verbose mode of the \code{\link{getURL}} call. Default to FALSE.
#'
#' @return The XML response for the service request.
#' @export
#' @import RAdwords XML curl
#' @seealso \code{\link{managed.customer.request}}
#'
#' @examples
#' # For this exemple to work, you must supply a valid client customer id and your developper token
#' data <- get.service(request, cid = cid, auth = doAuth(), user.agent = user.agent)
get.service <- function(request, cid, auth, user.agent, api.version = "v201702", validate.only = FALSE, partial.failure = FALSE, verbose = FALSE)
{
	if(!require(RAdwords)) stop("get.service requires package RAdwords")
	if(!require(XML)) stop("get.service requires package XML")
	if(!require(curl)) stop("get.service requires package curl")

	if(!"adwords.service.request" %in% class(request)) stop("argument request is not a valid Adwords service request")
	service <- attr(request, "service")
	path <- attr(request, "path")

	access <- auth[["access"]]
	if(as.numeric(Sys.time()) - 3600 >= access$timeStamp)
	{
		access <- refreshToken(auth)
	}
	authorization <- paste(access[["token_type"]], access[["access_token"]])
	credlist <- auth[["credentials"]]
	dev.token <- auth$credentials$auth.developerToken

	# Prepare the header for the service request
	namespace.def <-c(tns = paste0("https://adwords.google.com/api/adwords/cm/", api.version))
	cid.node <- xmlNode(cid, name = "clientCustomerId", namespace = "tns", namespaceDefinitions = namespace.def)
	dev.node <- xmlNode(dev.token, name = "developerToken", namespace = "tns", namespaceDefinitions = namespace.def)
	agent.node <- xmlNode(user.agent, name = "userAgent", namespace = "tns", namespaceDefinitions = namespace.def)
	validate.node <- xmlNode(ifelse(validate.only, "true", "false"), name = "validateOnly", namespace = "tns", namespaceDefinitions = namespace.def)
	partial.node <- xmlNode(ifelse(partial.failure, "true", "false"), name = "partialFailure", namespace = "tns", namespaceDefinitions = namespace.def)
	request.node <- xmlNode(name = "RequestHeader", namespace = "tns", cid.node, dev.node, agent.node, validate.node, partial.node)
	header.node <- xmlNode(name = "Header", namespace = "SOAP-ENV", request.node)

	# Finalize the request by combining the header and the body
	envelope.node <- xmlNode("Envelope", namespace = "SOAP-ENV", namespaceDefinitions = c(tns = paste0(path, api.version), ns0 = paste0("https://adwords.google.com/api/adwords/cm/", api.version), ns1 = paste0(path, api.version), ns2 = "http://schemas.xmlsoap.org/soap/envelope/", xsi = "http://www.w3.org/2001/XMLSchema-instance", 'SOAP-ENV' = "http://schemas.xmlsoap.org/soap/envelope/"), header.node, request)
	full.request <- saveXML(envelope.node)

	# Retreive the service data
	response <- RCurl::getURL(paste0(path, api.version, "/", service, "?wsdl"),
						  httpheader = c(Authorization = authorization,
						  			   developerToken = credlist[["auth.developerToken"]],
						  			   clientCustomerId = cid,
						  			   includeZeroImpressions = FALSE),
						  postfields = full.request,
						  verbose = verbose, ssl.verifypeer = TRUE)
	parser <- attr(request, "parser")
	parser(response)
}
