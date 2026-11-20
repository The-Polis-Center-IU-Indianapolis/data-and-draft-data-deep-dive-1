# Place any selected neighborhoods in two label columns outside the map.
# Each column follows north-to-south order. Use a small selection for readable labels.
place_map_labels <- function(areas, bounds, wrap_width = 16) {
  points <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(areas)))
  width <- bounds["xmax"] - bounds["xmin"]
  height <- bounds["ymax"] - bounds["ymin"]
  middle <- mean(bounds[c("xmin", "xmax")])

  sf::st_drop_geometry(areas) |>
    dplyr::mutate(
      x = points[, 1], y = points[, 2],
      label = vapply(gsub("/", " / ", neighborhood, fixed = TRUE),
                     function(name) paste(strwrap(name, width = wrap_width), collapse = "\n"),
                     character(1)),
      side = dplyr::if_else(x < middle, "left", "right")
    ) |>
    dplyr::group_by(side) |>
    dplyr::arrange(y, .by_group = TRUE) |>
    dplyr::mutate(
      label_x = dplyr::if_else(side == "left", bounds["xmin"] - 0.04 * width,
                        bounds["xmax"] + 0.04 * width),
      label_y = if (dplyr::n() == 1L) y else
        seq(bounds["ymin"] + 0.08 * height, bounds["ymax"] - 0.08 * height,
            length.out = dplyr::n()),
      align = dplyr::if_else(side == "left", 1, 0)
    ) |>
    dplyr::ungroup()
}
