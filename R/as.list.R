#' @export
as.list.keyword <- function(x)
{
	list(Keyword = as.character(x), Matchtype = attr(x, "match.type"), KeywordID = attr(x, "id"), Keywordtype = attr(x, "type"))
}

#' @export
as.list.keyword.estimate.request <- function(x)
{
	list(Keyword = as.character(x), Matchtype = attr(x, "match.type"), KeywordID = attr(x, "id"), Keywordtype = attr(x, "type"), Max.CPC = as.numeric(attr(x, "max.cpc")), IsNegative = attr(x, "is.negative"))
}
