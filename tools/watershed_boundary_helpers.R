### compare continuous great-circle boundaries without editing source coordinates

# retain the original coordinate precision when creating comparison edges
watershed_edge_geography <- function(from, to) {
  s2::as_s2_geography(sprintf(
    "LINESTRING (%.17g %.17g, %.17g %.17g)",
    from[, 1L], from[, 2L], to[, 1L], to[, 2L]
  ))
}

watershed_boundary_edges <- function(boundary) {
  lines <- sf::st_cast(sf::st_as_sfc(boundary), "LINESTRING")
  stopifnot(length(lines) > 0L, all(vapply(lines, nrow, integer(1)) >= 2L))
  from <- do.call(rbind, lapply(lines, function(line) line[-nrow(line), 1:2, drop = FALSE]))
  to <- do.call(rbind, lapply(lines, function(line) line[-1L, 1:2, drop = FALSE]))
  list(from = from, to = to, edges = watershed_edge_geography(from, to), rings = length(lines))
}

### subdivide only an exact meridian chain at existing source vertices

watershed_meridian_chain <- function(source, from, to) {
  stopifnot(from[1L] == to[1L], from[2L] != to[2L], all(abs(c(from[2L], to[2L])) < 89))
  direction <- sign(to[2L] - from[2L])
  current <- from
  result_from <- result_to <- list()
  used <- integer()
  for (step in seq_len(64L)) {
    on_meridian <- source$from[, 1L] == current[1L] & source$to[, 1L] == current[1L]
    starts <- which(on_meridian &
      source$from[, 2L] == current[2L] &
      direction * (source$to[, 2L] - current[2L]) > 0 &
      direction * (to[2L] - source$to[, 2L]) >= 0)
    ends <- which(on_meridian &
      source$to[, 2L] == current[2L] &
      direction * (source$from[, 2L] - current[2L]) > 0 &
      direction * (to[2L] - source$from[, 2L]) >= 0)
    candidates <- setdiff(c(starts, ends), used)
    if (length(candidates) != 1L) stop("an ambiguous meridian chain requires review")
    index <- candidates[1L]
    next_point <- if (index %in% starts) source$to[index, ] else source$from[index, ]
    result_from[[length(result_from) + 1L]] <- current
    result_to[[length(result_to) + 1L]] <- next_point
    used <- c(used, index)
    current <- next_point
    if (identical(unname(current), unname(to))) return(list(
      from = do.call(rbind, result_from), to = do.call(rbind, result_to)
    ))
  }
  stop("the meridian chain exceeds the bounded comparison limit")
}

### endpoint bounds and complete source-edge coverage certify the whole boundary

watershed_boundary_certificate <- function(original, parts, tolerance_m = .001) {
  stopifnot(
    isTRUE(sf::sf_use_s2()), length(tolerance_m) == 1L,
    is.finite(tolerance_m), tolerance_m > 1e-6, tolerance_m <= .001
  )
  original <- sf::st_geometry(original)
  parts <- sf::st_geometry(parts)
  stopifnot(
    length(original) == 1L, length(parts) > 0L,
    identical(sf::st_crs(original)$epsg, 4326L), identical(sf::st_crs(parts)$epsg, 4326L),
    all(sf::st_is_valid(original)), all(sf::st_is_valid(parts)),
    !any(sf::st_is_empty(original)), !any(sf::st_is_empty(parts))
  )
  original_bytes <- sf::st_as_binary(original, EWKB = TRUE)
  part_bytes <- sf::st_as_binary(parts, EWKB = TRUE)
  source <- sf::st_as_s2(original)
  joined <- s2::s2_union_agg(sf::st_as_s2(parts))

  # filter only zero-area line output in this comparison copy
  polygon <- s2::s2_rebuild(joined, options = s2::s2_options(dimensions = "polygon"))
  filtered_area <- s2::s2_area(s2::s2_sym_difference(joined, polygon))
  stopifnot(
    length(filtered_area) == 1L, is.finite(filtered_area), filtered_area == 0,
    s2::s2_area(source) > 0, s2::s2_area(polygon) > 0
  )
  source_edges <- watershed_boundary_edges(s2::s2_boundary(source))
  query_edges <- watershed_boundary_edges(s2::s2_boundary(polygon))
  stopifnot(source_edges$rings > 0L, query_edges$rings > 0L)
  numerical_allowance <- 1e-6
  endpoint_limit <- (tolerance_m - numerical_allowance) / 4
  points <- function(a) s2::as_s2_geography(s2::s2_lnglat(a[, 1L], a[, 2L]))
  match_edges <- function(query) {
    midpoint <- s2::s2_interpolate_normalized(query$edges, rep(.5, length(query$edges)))
    nearest <- s2::s2_closest_feature(midpoint, source_edges$edges)
    stopifnot(!anyNA(nearest))
    from <- points(query$from)
    to <- points(query$to)
    error <- pmax(
      s2::s2_distance(from, source_edges$edges[nearest]),
      s2::s2_distance(to, source_edges$edges[nearest])
    )
    stopifnot(all(is.finite(error)))
    list(nearest = nearest, from = from, to = to, error = error)
  }
  matched <- match_edges(query_edges)
  unclear <- which(matched$error > endpoint_limit)
  analytical_splits <- 0L
  if (length(unclear)) {
    if (length(unclear) > 64L) stop("too many unresolved boundary edges require review")
    from <- query_edges$from[-unclear, , drop = FALSE]
    to <- query_edges$to[-unclear, , drop = FALSE]
    for (index in unclear) {
      chain <- watershed_meridian_chain(source_edges, query_edges$from[index, ], query_edges$to[index, ])
      analytical_splits <- analytical_splits + nrow(chain$from) - 1L
      from <- rbind(from, chain$from)
      to <- rbind(to, chain$to)
    }
    query_edges$from <- from
    query_edges$to <- to
    query_edges$edges <- watershed_edge_geography(from, to)
    matched <- match_edges(query_edges)
  }
  stopifnot(all(matched$error <= endpoint_limit))
  source_lengths <- s2::s2_length(source_edges$edges)
  query_lengths <- s2::s2_length(query_edges$edges)
  angle <- max(source_lengths, query_lengths) / s2::s2_earth_radius_meters()
  stopifnot(is.finite(angle), angle < pi / 2)

  # closest endpoint fractions must cover each complete original edge
  first <- s2::s2_project_normalized(source_edges$edges[matched$nearest], matched$from)
  last <- s2::s2_project_normalized(source_edges$edges[matched$nearest], matched$to)
  stopifnot(
    all(is.finite(first)), all(is.finite(last)),
    all(first >= 0 & first <= 1), all(last >= 0 & last <= 1)
  )
  groups <- split(seq_along(matched$nearest), factor(matched$nearest, levels = seq_along(source_lengths)))
  gaps <- vapply(groups, function(index) {
    if (!length(index)) return(1)
    low <- pmin(first[index], last[index])
    high <- pmax(first[index], last[index])
    interval_order <- order(low)
    low <- low[interval_order]
    high <- cummax(high[interval_order])
    max(c(low[1L], low[-1L] - high[-length(high)], 1 - tail(high, 1L), 0))
  }, numeric(1))
  gap_m <- max(gaps * source_lengths)
  endpoint_m <- max(matched$error)

  # normalized chords bound the full minor arc between the matched endpoints
  directed_bound <- 2 * endpoint_m / cos(angle / 2)
  bound <- directed_bound + gap_m + numerical_allowance
  stopifnot(
    is.finite(bound), bound <= tolerance_m,
    identical(original_bytes, sf::st_as_binary(original, EWKB = TRUE)),
    identical(part_bytes, sf::st_as_binary(parts, EWKB = TRUE)), isTRUE(sf::sf_use_s2())
  )
  list(
    passed = TRUE, method = "continuous great-circle edge bound with complete source-edge coverage",
    boundary_upper_bound_m = bound, tolerance_m = tolerance_m,
    maximum_endpoint_error_m = endpoint_m, maximum_uncovered_interval_m = gap_m,
    directed_arc_bound_m = directed_bound, numerical_allowance_m = numerical_allowance,
    maximum_minor_arc_radians = angle, analytical_meridian_splits = analytical_splits,
    source_edges = length(source_lengths), query_subedges = length(query_lengths),
    source_rings = source_edges$rings, query_rings = query_edges$rings,
    zero_area_qa_filter_difference_m2 = filtered_area,
    original_and_query_wkb_unchanged = TRUE, s2_remained_enabled = TRUE
  )
}
