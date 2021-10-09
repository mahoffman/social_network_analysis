plot_blockmodel <- function (x, ...) 
{
  require(sna)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  n <- dim(x$blocked.data)[2]
  m <- stackcount(x$blocked.data)
  if (!is.null(x$plabels)) 
    plab <- x$plabels
  else plab <- (1:n)[x$order.vector]
  if (!is.null(x$glabels)) 
    glab <- x$glabels
  else glab <- 1:m
  par(mfrow = c(floor(sqrt(m)), ceiling(m/floor(sqrt(m)))))
  if (m > 1) 
    for (i in 1:m) {
      plot.sociomatrix(x$blocked.data[i, , ], labels = list(plab, 
                                                            plab), main = paste("Relation - ", glab[i]), 
                       drawlines = FALSE)
      for (j in 2:n) if (x$block.membership[j] != x$block.membership[j - 
                                                                     1]) 
        abline(v = j - 0.5, h = j - 0.5, lty = 3)
    }
  else {
    plot_socio_mat(x$blocked.data, labels = list(plab, 
                                                   plab), main = paste("Relation - ", glab[1]), drawlines = FALSE, ...)
    for (j in 2:n) if (x$block.membership[j] != x$block.membership[j - 
                                                                   1]) 
      abline(v = j - 0.5, h = j - 0.5, lty = 3)
  }
}

plot_socio_mat <- function (x, labels = NULL, drawlab = TRUE, diaglab = F, drawlines = TRUE, 
          xlab = NULL, ylab = NULL, cex.lab = 0.3, font.lab = 1, col.lab = 1, 
          scale.values = TRUE, cell.col = gray, ...) 
{
  if ((!inherits(x, c("matrix", "array", "data.frame"))) || 
      (length(dim(x)) > 2)) 
    x <- as.sociomatrix.sna(x)
  if (is.list(x)) 
    x <- x[[1]]
  n <- dim(x)[1]
  o <- dim(x)[2]
  if (is.null(labels)) 
    labels <- list(NULL, NULL)
  if (is.null(labels[[1]])) {
    if (is.null(rownames(x))) 
      labels[[1]] <- 1:dim(x)[1]
    else labels[[1]] <- rownames(x)
  }
  if (is.null(labels[[2]])) {
    if (is.null(colnames(x))) 
      labels[[2]] <- 1:dim(x)[2]
    else labels[[2]] <- colnames(x)
  }
  if (scale.values) 
    d <- 1 - (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                           min(x, na.rm = TRUE))
  else d <- x
  if (is.null(xlab)) 
    xlab <- ""
  if (is.null(ylab)) 
    ylab <- ""
  plot(1, 1, xlim = c(-5, o + 1), ylim = c(n + 1, -5), type = "n", 
       axes = FALSE, xlab = xlab, ylab = ylab, ...)
  for (i in 1:n) for (j in 1:o) rect(j - 0.5, i + 0.5, j + 
                                       0.5, i - 0.5, col = cell.col(d[i, j]), xpd = TRUE, border = drawlines)
  rect(0.5, 0.5, o + 0.5, n + 0.5, col = NA, xpd = TRUE)
  if (drawlab) {
    text(rep(1.4, n), 1:n, labels[[1]], cex = cex.lab, font = font.lab, 
         col = col.lab, pos = 2)
    text(0:(o-1), rep(0, o), labels[[2]], cex = cex.lab, font = font.lab, 
         col = col.lab, srt = 45, pos = 4)
  }
  if ((n == o) & (drawlab) & (diaglab)) 
    if (all(labels[[1]] == labels[[2]])) 
      text(1:o, 1:n, labels[[1]], cex = cex.lab, font = font.lab, 
           col = col.lab)
}