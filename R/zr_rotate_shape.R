#' Rotate polygon by specified angle
#'
#' The `zr_rotate_shape()` function takes a one-row special feature object and rotates the geometry about the centroid or a specified point.
#'
#' @param shape a polygon you want to rotate. Must be a special feature object with only one row.
#' @param angle_degrees The angle (in degrees) that you want to rotate the shape
#' @param center The center about which to rotate in the form of XY coordinates.
#' Default is null,  When NULL, rotation is about the centroid.
#'
#' @return
#' Returns the simple feature shape object with rotated geometry
#' @export
#'
zr_rotate_shape <- function(shape, angle_degrees, center = NULL) {
  # Convert angle to radians
  rad <- angle_degrees * pi / 180

  # Create the rotation matrix
  rotation_matrix <- matrix(c(cos(rad), -sin(rad), sin(rad), cos(rad)), ncol = 2)

  # If no center is provided, use the centroid
  if (is.null(center)) {
    center <- sf::st_centroid(shape)
    center <- sf::st_coordinates(center)[1, ]
  }

  coords <- shape |>
    sf::st_union() |>
    sf::st_cast("POLYGON") |>
    sf::st_coordinates()

  # this checks to see if there are any holes in the polygon
  # if so, it will chosse the polygon with with most coordinates as
  # the main polygon to rotate
  if (length(unique(coords[,"L2"])) > 1){
    l2_val <- c()
    len <- c()
    for (i in unique(coords[,"L2"])){
      l2_val <- c(l2_val, i)
      len <- c(len, nrow(coords[coords[,"L2"] == i,]))
    }

    val <- l2_val[which(len == max(len))]
    coords <-coords[coords[,"L2"] == val,]
  }


  coords <- coords[1:nrow(coords),1:2]

  new_coords <- (sweep(coords, 2, as.vector(center), "-") %*% rotation_matrix) |>
    sweep(2,as.vector(center),"+")

  # Convert back to the same geometry type
  new_geom <- sf::st_set_crs(sf::st_sfc(sf::st_polygon(list(new_coords))), sf::st_crs(shape))

  return(new_geom)
}
