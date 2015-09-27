#' Build and customize 3D graphs in R
#' @name graph3d
#' @aliases graph3d
#' @docType package
#' @description Utilities for easily building and customizing 3D graphs in R.
#' @examples
#' \donttest{
#' # Load dataset
#' data(iris)
#' x <- iris$Sepal.Length
#' y <- iris$Petal.Length
#' z <- iris$Sepal.Width
#'
#' library("rgl")
#'
#' # Quick scatter plots
#' # ++++++++++++++++
#' rgl_scatter(iris, groups = iris$Species)
#' # Scale data
#' rgl_scatter(iris, groups = iris$Species, data.scale = TRUE)
#' # Remove the bounding box
#' rgl_scatter(iris, groups = iris$Species, show.bbox = FALSE)
#' # Add ellipses
#' rgl_scatter(iris, groups = iris$Species,
#'    show.bbox = FALSE, add.ellipse = TRUE)
#'
#' # Build RGL scatter plot
#' # +++++++++++++++++++
#' rgl_init()
#' rgl.spheres(x, y, z, r = 0.2, color = get_colors(iris$Species))
#' rgl_add_axes(x, y, z, show.bbox = TRUE)
#' rgl_add_ellipses(x, y, z, groups = iris$Species)
#' aspect3d(1,1,1)
#' }
#'
NULL
