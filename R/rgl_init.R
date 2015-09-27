#' Initialize RGL device
#'
#' @description Creates a new RGL device if requested or if there
#'  is no opened device
#'
#' @param new.device a logical value. If TRUE, creates a new RGL device
#' @param bg the background color of the device
#' @param width the width of the device
#' @examples
#' \donttest{
#' # Load dataset
#' data(iris)
#' x <- iris$Sepal.Length
#' y <- iris$Petal.Length
#' z <- iris$Sepal.Width
#' # Scatter plot
#' require("rgl")
#' rgl_init()
#' rgl.spheres(x, y, z, r = 0.2, color = "grey")
#' }
#' @export
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) {

  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl package needed for this function to work. Please install it.")

  if( new.device | rgl::rgl.cur() == 0 ) {
    rgl::rgl.open()
    rgl::par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl::rgl.bg(color = bg )
  }
  rgl::rgl.clear(type = c("shapes", "bboxdeco"))
  rgl::rgl.viewpoint(theta = 45, phi = 30, fov= 60)
}
