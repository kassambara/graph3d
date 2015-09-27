#' @include rgl_init.R
NULL
#' RGL scatter plot
#'
#' @description
#' RGL scatter plot. \cr\cr
#' \itemize{
#' \item rgl_scatter(): quick scatter plot using rgl
#' \item rgl_add_axes(): Add axes to an RGL plot
#' \item get_colors(): Get colors for the different levels of a factor variable.
#' \item rgl_add_ellipses(): Add an ellipse of concentration.
#' \item rgl_add_linear_planes(): Add regression planes
#' }
#'
#' @param x,y,z the coordinates of the points to draw.
#'  Both y and z can be left at NULL. x can be a data frame or a formula
#'  zvar ~ xvar + yvar
#' @param axis.col colors for the 3 axes.
#' @param show.axis.lab If TRUE show axis labels.
#' @param xlab,ylab,zlab labels to be used for x, y, and z axes, respectively.
#' @param show.plane If TRUE, show axis planes at y = 0.
#' @param plane.col the color to be used for plane. Default is "grey".
#' @param show.bbox add the bounding box decoration.
#' @param bbox.col the bounding box colors. The first color is the
#' the background color; the second color is the color of tick marks
#' @param show.ticks if TRUE, show ticks
#' @param xlim,ylim,zlim the limits for x, y and z axes, respectively
#' @examples
#'\donttest{
#' # Load dataset
#' data(iris)
#' x <- iris$Sepal.Length
#' y <- iris$Petal.Length
#' z <- iris$Sepal.Width
#'
#' library("rgl")
#'
#' # Quick scatter plots
#' rgl_scatter(iris, groups = iris$Species)
#' # Scale data
#' rgl_scatter(iris, groups = iris$Species, data.scale = TRUE)
#' # Remove the bounding box
#' rgl_scatter(iris, groups = iris$Species, show.bbox = FALSE)
#' # Add ellipses
#' rgl_scatter(iris, groups = iris$Species,
#'    show.bbox = FALSE, add.ellipse = TRUE)
#'
#' # Scatter plot with axes
#' # +++++++++++++++++
#' rgl_init()
#' rgl.spheres(x, y, z, r = 0.2, color = "yellow")
#' rgl_add_axes(x, y, z)
#'
#' # Change the color by groups
#' # +++++++++++++++++
#'  rgl_init()
#'  rgl.spheres(x, y, z, r = 0.2, color = get_colors(iris$Species))
#'  rgl_add_axes(x, y, z, show.bbox = TRUE)
#'  aspect3d(1,1,1)
#'
#'  # Add ellipses
#'  # ++++++++++++++++++
#'  rgl_init()
#'  rgl.spheres(x, y, z, r = 0.2, color = "#D95F02")
#'  rgl_add_axes(x, y, z, show.bbox = TRUE)
#'  rgl_add_ellipses(x, y, z, ellipse.col = "#D95F02")
#'  aspect3d(1,1,1)
#'
#'  # Regression planes
#'  # +++++++++++++++++++
#'  rgl_init()
#'  rgl.spheres(x, y, z, r = 0.2,
#'      color = get_colors(iris$Species, c("red", "blue", "green")))
#'  rgl_add_axes(x, y, z, show.bbox = TRUE)
#'  rgl_add_linear_planes(x, y, z, groups = iris$Species,
#'      surface.col = c("red", "blue", "green"))
#'  aspect3d(1,1,1)
#'
#'}
#'
#' @name rgl_scatter
#' @rdname rgl_scatter
#' @param sphere.size the size of the spheres
#' @param point.col the color to be used for the points when groups = NULL
#' @param add.ellipse logical value. If TRUE, an ellipse is added around points
#' @param add.reg.planes logical value. If TRUE add regression planes
#' @param show.group.labels logical value. If TRUE, group labels are shown
#' @param data.scale logical value. If TRUE, the coordinates (x, y, z) is scaled
#'  so that the minimum of each axis = 0 and the maximum = 1
#' @param ... other arguments to be passed to the function rgl_add_axis(),
#' rgl_add_ellipses() or rgl_add_linear_planes().
#' @export
rgl_scatter <- function(x, y = NULL, z = NULL, groups = NULL, sphere.size = 1,
                        point.col = "grey", group.col = NULL,
                        add.ellipse = FALSE, add.reg.planes = FALSE,
                        show.group.labels = TRUE, data.scale = FALSE,
                        show.bbox = TRUE,
                        ...){

  .check_rgl()
  # Environment setup
  rgl_init()
  # suspend updating while making multiple changes to the scene
  rgl::par3d(skipRedraw= TRUE )

  # Coordinates
  xyz <- grDevices::xyz.coords(x, y, z)
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z

  # Clean the data
  valid <- if (is.null(groups)) complete.cases(x, y, z)
  else complete.cases(x, y, z, groups)
  x <- x[valid]
  y <- y[valid]
  z <- z[valid]

  if(data.scale){
    x <-(x-min(x))/(max(x)-min(x))
    y <-(y-min(y))/(max(y)-min(y))
    z <-(z-min(z))/(max(z)-min(z))
  }

  # Compute spheres size
  if(data.scale) size <- sphere.size*((100/length(x))^(1/3)) * 0.015
  else size <- sphere.size*((100/length(x))^(1/3))* 0.1

  # color
  color <- point.col
  if(!is.null(groups)){
    groups <- groups[valid]
    color <- get_colors(groups, group.col)
  }

  # Plot the data
  if (size > 0.01) rgl::rgl.spheres(x, y, z, color = color, radius = size)
  else rgl::rgl.points(x, y, z, color = color)
  rgl_add_axes(x, y, z, show.bbox = show.bbox,...) # Add axes

  # Add ellipse
  ellipse.col <- point.col[1]
  if(!is.null(groups)) ellipse.col <- group.col
  if(add.ellipse) rgl_add_ellipses(x, y, z, groups = groups, ellipse.col = ellipse.col, ...)

  # Add regression planes
  surface.col <- point.col[1]
  if(!is.null(groups)) surface.col <- group.col
  if(add.reg.planes) rgl_add_linear_planes(x, y, z, groups = groups, surface.col = surface.col, ...)

  # Add group labels
  if(!is.null(groups) & show.group.labels)
    .rgl_group_labels(x, y, z, groups = groups, group.col = group.col)


  # Finalize the plot
  rgl::aspect3d( c( 1, 1, 1 ) )
  rgl::par3d(skipRedraw = FALSE)
  rgl::rgl.bringtotop()
}


#' @rdname rgl_scatter
#' @export
rgl_add_axes <- function(x, y = NULL, z = NULL, axis.col = c("grey", "grey", "grey"),
                         show.axis.lab = FALSE, xlab = NULL, ylab = NULL, zlab = NULL,
                         show.plane = TRUE, plane.col = "grey",
                         show.bbox = TRUE, bbox.col = c("#333377","black"),
                         show.ticks = FALSE,
                         xlim = NULL, ylim = NULL, zlim = NULL, ...)
{

  .check_rgl()

  # Coordinates
  xyz <- grDevices::xyz.coords(x, y, z)
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z

  # Axis limits
  if(is.null(xlim)) xlim <- c(-max(abs(x)), max(abs(x))) * 1.1
  if(is.null(ylim)) ylim <- c(-max(abs(y)), max(abs(y))) * 1.1
  if(is.null(zlim)) zlim <- c(-max(abs(z)), max(abs(z))) * 1.1

  # Add axis
  if(length(axis.col) < 3) axis.col <- rep(axis.col, 3)
  rgl::rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col[1])
  rgl::rgl.lines(c(0,0), ylim, c(0, 0), color = axis.col[2])
  rgl::rgl.lines(c(0,0), c(0, 0), zlim, color = axis.col[3])

  # Add a point at the end of each axes to specify the direction
  rgl::rgl.points(xlim[2],  0, 0, color = axis.col[1], size = 3)
  rgl::rgl.points(0, ylim[2], 0, color = axis.col[2], size = 3)
  rgl::rgl.points(0, 0, zlim[2], color = axis.col[3], size = 3)

  # Add labels
  if(show.axis.lab){
    if(is.null(xlab)) xlab = deparse(substitute(x))
    if(is.null(ylab)) ylab = deparse(substitute(y))
    if(is.null(zlab)) zlab = deparse(substitute(z))
    rgl::rgl.texts(xlim[2],  0, 0, xlab, color = axis.col[1], adj = c(0.5, -0.8), size = 2)
    rgl::rgl.texts(0, ylim[2], 0, ylab, color = axis.col[2],  adj = c(0.5, -0.8), size = 2)
    rgl::rgl.texts(0, 0, zlim[2], zlab,  color = axis.col[3], adj = c(0.5, 0.8), size = 2)
  }

  # Add plane
  if(show.plane) {
    xlim <- xlim/1.1
    zlim <- zlim/1.1

    rgl::rgl.quads(
      rbind( c( xlim[1], 0, zlim[1]), c( xlim[1], 0, zlim[2] ),
             c( xlim[2], 0, zlim[2] ), c( xlim[2], 0, zlim[1] )),
      col= plane.col, alpha= 0.4)
  }

  # Add bounding box decoration
  if(show.bbox){
    rgl::rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5,
             emission=bbox.col[1], specular=bbox.col[1], shininess=5,
             xlen = 3, ylen = 3, zlen = 3)
  }

  # Show ticks
  if(show.ticks){
    rgl::axis3d('x',pos=c( NA, 0, 0 ), col = axis.col[1])
    rgl::axis3d('y',pos=c( 0, NA, 0 ), col = axis.col[2])
    rgl::axis3d('z',pos=c( 0, 0, NA ), col = axis.col[3])
  }

  rgl::rgl.bringtotop()
}






#' @param group.col a vector containing the names of colors to be used for each group
#' @rdname rgl_scatter
#' @export
get_colors <- function(groups, group.col = palette()[-1]){
  if(is.null(group.col)) group.col <- palette()[-1]
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col))
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}


#' @param groups a factor variable specifing the groups of the observations
#' @param conf.level The confidence level of a simultaneous confidence region.
#'  This is used to control the size of the ellipsoid.
#' @param ellipse.col a vector of color(s) to be used for the ellipse(s)
#' @param ellipse.fill a logical value. If TRUE, fills the plotted ellipse(s) with ellipse.col
#' @param ellipse.grid a logical value. If TRUE, adds grid lines on the ellipse(s)
#' @rdname rgl_scatter
#' @export
rgl_add_ellipses <- function(x, y = NULL, z = NULL, groups = NULL, conf.level = 0.95,
                             ellipse.col = NULL, ellipse.fill = TRUE, ellipse.grid = FALSE, ...)
{
  .check_rgl()

  # Coordinates
  xyz <- grDevices::xyz.coords(x, y, z)
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z
  if(is.null(groups)) groups <- rep(1, length(x))

  # plot
  groups <- as.factor(groups)
  levs <- levels(groups)

  if(is.null(ellipse.col)) ellipse.col<- palette()[-1]
  if(length(levs)> length(ellipse.col)) ellipse.col <- rep(ellipse.col, length(levs))
  for (i in 1:length(levs)) {
    group <- levs[i]
    selected <- groups == group
    xx <- x[selected]
    yy <- y[selected]
    zz <- z[selected]
    ellips <- rgl::ellipse3d(cov(cbind(xx,yy,zz)),
                        centre=c(mean(xx), mean(yy), mean(zz)), level = conf.level)

    if(ellipse.fill) rgl::shade3d(ellips, col = ellipse.col[i],
                             alpha = 0.1, lit = FALSE)
    if(ellipse.grid) rgl::wire3d(ellips, col = ellipse.col[i],  lit = FALSE)

  }

  rgl::rgl.bringtotop()
}


#' @param surface.col color to be used for regression plane surfaces.
#' @param grid.col color for grids in the regression planes
#' @param plane.fill logical value; if TRUE the regression planes are filled
#' @param plane.grid logical value. If TRUE grids are shown
#' @param droplines logical values. If TRUE, droplines are shown from regression surfaces to the points.
#' @param smooth logical value. If TRUE, smoothed regression planes are drawn.
#' @rdname rgl_scatter
#' @export
rgl_add_linear_planes <- function(x, y = NULL, z=NULL, groups = NULL, surface.col = "steelblue",
                              grid.col = "black", plane.fill = TRUE, plane.grid = FALSE,
                              droplines = TRUE, smooth = FALSE, ...){

  .check_rgl()
  # Coordinates
  xyz <- grDevices::xyz.coords(x, y, z)
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z


  if(is.null(groups))
    .add_rgl_planes(x, y, z, surface.col = surface.col, grid.col = grid.col,
                    plane.fill = plane.fill, plane.grid = plane.grid, droplines = droplines, smooth= smooth)
  else{

    groups <- as.factor(groups)
    levs <- levels(groups)

    if(is.null(surface.col)) surface.col<- palette()[-1]
    if(length(levs) > length(surface.col)) surface.col <- rep(surface.col, length(levs))
    if(!plane.fill) grid.col = surface.col

    for (i in 1:length(levs)) {
      group <- levs[i]
      selected <- groups == group
      xx <- x[selected]
      yy <- y[selected]
      zz <- z[selected]

      .add_rgl_planes(xx, yy, zz, surface.col = surface.col[i], grid.col = grid.col,
                      plane.fill = plane.fill, plane.grid = plane.grid, droplines = droplines, smooth = smooth)


    }

  }

  rgl::rgl.bringtotop()
}

# helper function to addrgl planes
.add_rgl_planes <- function(x, y, z, surface.col = "steelblue",
                            grid.col = "black", plane.fill = TRUE, plane.grid = FALSE,
                            droplines = TRUE, smooth = FALSE){

  # Compute the linear regression (y = ax + bz + d)
  if(smooth) fit = mgcv::gam(y ~ s(x, z))
  else fit <- lm(y ~ x + z)
  # predict values on regular xz grid
  grid.lines = 26
  x.pred <- seq(min(x), max(x), length.out = grid.lines)
  z.pred <- seq(min(z), max(z), length.out = grid.lines)
  xz <- expand.grid( x = x.pred, z = z.pred)

  y.pred <- matrix(predict(fit, newdata = xz),
                   nrow = grid.lines, ncol = grid.lines)

  if (plane.fill)
    rgl::rgl.surface(x.pred, z.pred, y.pred, color = surface.col,
                alpha = 0.5, lit = FALSE)
  if(plane.grid)
    rgl::rgl.surface(x.pred, z.pred, y.pred, color = grid.col[1],
                alpha = 0.5, lit = FALSE, front = "lines", back = "lines")
  if(droplines){
    # fitted points for droplines to surface
    fitted <- fitted(fit)
    rgl::rgl.lines(as.vector(rbind(x, x)), as.vector(rbind(y, fitted)),
              as.vector(rbind(z, z)), color = surface.col[1])
  }


}



# Show the label of each group
#  groups a factor variable specifing the groups of the observations
#  group.col a vector of color(s) to be used for the groups
#  cex the size of text
# ... other arguments to be passed to text3d
.rgl_group_labels <- function(x, y = NULL, z = NULL,
                             groups, group.col = "black", cex = 2, ...)
{
  # Coordinates
  xyz <- xyz.coords(x, y, z)
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z

  groups <- as.factor(groups)
  levs <- levels(groups)

  if(is.null(group.col)) group.col<- palette()[-1]
  if(length(levs) > length(group.col)) group.col <- rep(group.col, length(levs))

  for (i in 1:length(levs)) {
    group <- levs[i]
    selected <- groups == group
    xx <- x[selected]
    yy <- y[selected]
    zz <- z[selected]
    rgl::texts3d(mean(xx),mean(yy), mean(zz), group, col= group.col[i], cex = cex, ...)
  }

}

.check_rgl <- function(){
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl package needed for this function to work. Please install it.")
}
