rect.dendrogram <- function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2,
                           cluster = NULL, horiz = FALSE, density = NULL, angle = 45,
                           text = NULL, text_cex = 1, text_col = 1, xpd = TRUE, lower_rect, hpk=NULL,
                           ...
)
{
  if (!is.dendrogram(tree))
    stop("x is not a dendrogram object.")
  if (length(h) > 1L | length(k) > 1L)
    stop("'k' and 'h' must be a scalar(i.e.: of length 1)")
  if(is.null(hpk)) {tree_heights <- heights_per_k.dendrogram(tree)[-1]} else {tree_heights<-hpk[-1]}
  tree_order <- order.dendrogram(tree)
  if (!is.null(h)) {
    if (!is.null(k))
      stop("specify exactly one of 'k' and 'h'")
    ss_ks <- tree_heights < h
    k <- min(as.numeric(names(ss_ks))[ss_ks])
    k <- max(k, 2)
  }
  else if (is.null(k))
    stop("specify exactly one of 'k' and 'h'")
  if (k < 2 | k > length(tree_heights))
    stop(gettextf("k must be between 2 and %d", length(tree_heights)),
         domain = NA)
  if (is.null(cluster))
    cluster <- cutree(tree, k = k)
  clustab <- table(cluster)[unique(cluster[tree_order])]
  m <- c(0, cumsum(clustab))
  if (!is.null(x)) {
    if (!is.null(which))
      stop("specify exactly one of 'which' and 'x'")
    which <- x
    for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
  }
  else if (is.null(which))
    which <- 1L:k
  if (any(which > k))
    stop(gettextf("all elements of 'which' must be between 1 and %d",
                  k), domain = NA)
  border <- rep_len(border, length(which))
  retval <- list()
  old_xpd <- par()["xpd"]
  par(xpd = xpd)
  for (n in seq_along(which)) {
    if (!horiz) {
      xleft = m[which[n]] + 0.66
      if (missing(lower_rect))
        lower_rect <- par("usr")[3L] - strheight("W") *
          (max(nchar(labels(tree))) + 1)
      ybottom = lower_rect
      xright = m[which[n] + 1] + 0.33
      ytop = tree_heights[names(tree_heights) == k]
    }
    else {
      ybottom = m[which[n]] + 0.66
      if (missing(lower_rect))
        lower_rect <- par("usr")[2L] + strwidth("X") *
          (max(nchar(labels(tree))) + 1)
      xright = lower_rect
      ytop = m[which[n] + 1] + 0.33
      xleft = tree_heights[names(tree_heights) == k]
    }
    rect(xleft, ybottom, xright, ytop, border = border[n],
         density = density, angle = angle, ...)
    if (!is.null(text))
      text((m[which[n]] + m[which[n] + 1] + 1)/2, grconvertY(grconvertY(par("usr")[3L],
                                                                        "user", "ndc") + 0.02, "ndc", "user"), text[n],
           cex = text_cex, col = text_col)
    retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
  }
  par(xpd = old_xpd)
  invisible(retval)
}
