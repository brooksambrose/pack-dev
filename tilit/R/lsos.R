#' Title https://stackoverflow.com/a/47001817
#'
#' @param pos
#' @param pattern
#' @param order.by
#' @param decreasing
#' @param head
#' @param n
#'
#' @return
#' @export
#'
#' @examples
.ls.objects <- function (pos = 1, pattern, order.by,
												 decreasing=FALSE, head=FALSE, n=5) {
	napply <- function(names, fn) sapply(names, function(x)
		fn(get(x, pos = pos)))
	names <- ls(pos = pos, pattern = pattern)
	obj.class <- napply(names, function(x) as.character(class(x))[1])
	obj.mode <- napply(names, mode)
	obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
	obj.prettysize <- napply(names, function(x) {
		format(utils::object.size(x),  units = "auto") })
	obj.size <- napply(names, utils::object.size)

	obj.dim <- t(napply(names, function(x)
		as.numeric(dim(x))[1:2]))
	vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
	obj.dim[vec, 1] <- napply(names, length)[vec]
	out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
	names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
	if (!missing(order.by))
		out <- out[order(out[[order.by]], decreasing=decreasing), ]
	if (head)
		out <- head(out, n)

	return(out)
}

#' Title
#'
#' @param ...
#' @param n
#'
#' @return
#' @export
#'
#' @examples
lsos <- function(..., n=10) {
	.ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
