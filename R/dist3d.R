#' 3D histogram distribution of two variables
#' @description Draw a 3D histogram distribution of points.
#' @param x,y vectors with x and y values.
#' @param break.func a function to compute the vector of breakpoints (see ?hist).
#' @param breaks an integer specifying the number of breaks.
#' @param col.bar color for bars.
#' @param colvar the variable used for coloring points
#' @param col.point Color palette to be used for the colvar variable.
#' If col is NULL and colvar is specified, then a red-yellow-blue colorscheme (jet.col) will be used.
#' If col is NULL and colvar is not specified, then col will be grey.
#' @param clab Only if colkey = TRUE, the label to be written on top of the color key.
#' @param theta,phi The angles defining the viewing direction
#' @param ... other arguments to be passed to plot3D::hist3D()
#' @seealso \code{\link[plot3D]{hist3D}}, \code{\link[graphics]{hist}}
#' @examples
#' data(quakes)
#' dist3d(quakes$long, quakes$lat, colvar=quakes$depth,
#'    breaks =30)
#' @export
dist3d <- function(x, y, break.func = c("Sturges", "scott", "FD"), breaks = NULL,
                        col.bar = "white",
                        colvar = NULL, col.point = plot3D::gg.col(100),
                        clab=NULL, phi = 5, theta = 25, ...){

  # Compute the number of classes for a histogram
  break.func <- break.func [1]
  if(is.null(breaks)){
    x.breaks <- switch(break.func,
                       Sturges = grDevices::nclass.Sturges(x),
                       scott = grDevices::nclass.scott(x),
                       FD = grDevices::nclass.FD(x))
    y.breaks <- switch(break.func,
                       Sturges = grDevices::nclass.Sturges(y),
                       scott = grDevices::nclass.scott(y),
                       FD = grDevices::nclass.FD(y))
  } else x.breaks <- y.breaks <- breaks

  # Cut x and y variables in bins for counting
  x.bin <- seq(min(x), max(x), length.out = x.breaks)
  y.bin <- seq(min(y), max(y), length.out = y.breaks)
  xy <- table(cut(x, x.bin), cut(y, y.bin))
  z <- xy

  xmid <- 0.5*(x.bin[-1] + x.bin[-length(x.bin)])
  ymid <- 0.5*(y.bin[-1] + y.bin[-length(y.bin)])

  oldmar <- par("mar")
  par (mar = par("mar") + c(0, 0, 0, 2))
  plot3D::hist3D(x = xmid, y = ymid, z = xy, ...,
         zlim = c(-max(z)/2, max(z)), zlab = "counts", bty= "g",
         phi = phi, theta = theta,
         shade = 0.2, col = col.bar, border = "black",
         d = 1, ticktype = "detailed")

  plot3D::scatter3D(x, y,
            z = rep(-max(z)/2, length.out = length(x)),
            colvar = colvar, col = col.point,
            add = TRUE, pch = 18, clab = clab,
            colkey = list(length = 0.5, width = 0.5,
                          dist = 0.05, cex.axis = 0.8, cex.clab = 0.8)
  )
  par(mar = oldmar)

}
