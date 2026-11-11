### compare all watershed parts as one geometry in metres

# this geometry is for checks only; extraction coordinates stay unchanged
watershed_comparison_geometry <- function(geometry) {
  geometry <- sf::st_geometry(geometry)
  stopifnot(
    length(geometry) > 0L,
    !any(sf::st_is_empty(geometry)), all(sf::st_is_valid(geometry)),
    all(sf::st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))
  )

  # combine every polygon component after projection
  # this keeps one comparison feature for multipart watersheds
  projected <- sf::st_transform(geometry, 6933)
  components <- suppressWarnings(sf::st_cast(projected, "POLYGON"))
  expected <- sum(vapply(projected, function(shape) {
    if (inherits(shape, "MULTIPOLYGON")) length(shape) else 1L
  }, integer(1)))
  vertices <- function(shapes) sum(vapply(shapes, function(shape) {
    nrow(sf::st_coordinates(shape))
  }, integer(1)))
  stopifnot(
    length(components) == expected,
    vertices(components) == vertices(projected)
  )
  combined <- sf::st_union(components)
  stopifnot(
    length(combined) == 1L,
    !sf::st_is_empty(combined), sf::st_is_valid(combined),
    as.numeric(sf::st_area(combined)) > 0
  )
  combined
}
