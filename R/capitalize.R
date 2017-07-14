#' Capitalize a string
#'
#' Puts the first letter of a string in uppercase.
#' Used in some of the parse functions.
#' Inspiration drawn from the examples of the \link{toupper} function.
#'
#' @param x string to capitalize
#'
#' @return Capitalized string
#' @export
#' @seealso \link{parse}
#'
#' @examples
#' capitalize("capital")
capitalize <- function(x)
{
	paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}
