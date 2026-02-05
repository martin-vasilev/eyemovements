
#' Converts pixel distances to degrees of visual angle (x- and y-axis)
#'
#' Computes the degrees of visual angle (dva) corresponding to pixel distances
#' along the horizontal (x) and vertical (y) screen axes, given the physical
#' screen dimensions, screen resolution, and viewing distance.
#'
#' The function first computes the degrees of visual angle corresponding to a
#' single pixel on each axis using the formula:
#'
#' \deqn{
#' \mathrm{deg\_per\_px} =
#' \frac{\arctan\left(\frac{0.5 \cdot \mathrm{screen\_size}}{\mathrm{viewing\_distance}}\right)
#' \cdot 180 / \pi}{0.5 \cdot \mathrm{resolution}}
#' }
#'
#' Pixel sizes are then converted to degrees of visual angle by simple
#' multiplication.
#'
#' @param screen_size_cm Numeric vector of length 2 giving the physical screen
#'   size in centimetres for the x (horizontal) and y (vertical) axes.
#'   Defaults to \code{c(x = 53, y = 30)}.
#'
#' @param resolution_px Numeric vector of length 2 giving the screen resolution
#'   in pixels for the x (horizontal) and y (vertical) axes.
#'   Defaults to \code{c(x = 1920, y = 1080)}.
#'
#' @param viewing_dist_cm Viewing distance in centimetres.
#'   Defaults to \code{80}.
#'
#' @param size_px Numeric vector of length 2 giving the stimulus size in pixels
#'   along the x and y axes.
#'   Defaults to \code{c(x = 12, y = 12)}.
#'
#' @return
#' A data frame with one row per axis (\code{"x"} and \code{"y"}) and the
#' following columns:
#' \describe{
#'   \item{axis}{Screen axis (\code{"x"} or \code{"y"})}
#'   \item{screen_cm}{Physical screen size (in cm)}
#'   \item{resolution_px}{Screen resolution (in pixels)}
#'   \item{deg_per_px}{Degrees of visual angle corresponding to a single pixel}
#'   \item{size_px}{Stimulus size in pixels}
#'   \item{size_deg}{Stimulus size in degrees of visual angle}
#' }
#'
#' @details
#' The function assumes that the stimulus lies near the centre of the screen and
#' that visual angles are sufficiently small for the per-pixel approximation to
#' be valid. For large eccentricities, a full position-dependent transformation
#' should be used instead.
#'
#' @examples
#' # Horizontal and vertical degrees of visual angle for a rectangular stimulus
#' visAngle_xy(
#'   screen_size_cm = c(x = 53, y = 30),
#'   resolution_px  = c(x = 1920, y = 1080),
#'   viewing_dist_cm = 80,
#'   size_px = c(x = 30, y = 15)
#' )
#'
#' @seealso
#' \code{\link{atan2}}
#'
#' @export

VisualAngle <- function(
    screen_size_cm = c(x = 53, y = 30),
    resolution_px  = c(x = 1920, y = 1080),
    viewing_dist_cm = 82,
    size_px = c(x = 1, y = 1)
) {

  # Calculate degrees per pixel for each axis
  deg_per_px <- (atan2(0.5 * screen_size_cm, viewing_dist_cm) * 180 / pi) /
    (0.5 * resolution_px)

  size_deg <- size_px * deg_per_px

  out <- data.frame(
    axis = c("x", "y"),
    screen_cm = screen_size_cm,
    resolution_px = resolution_px,
    deg_per_px = deg_per_px,
    size_px = size_px,
    size_deg = size_deg,
    row.names = NULL
  )

  return(out)
}
