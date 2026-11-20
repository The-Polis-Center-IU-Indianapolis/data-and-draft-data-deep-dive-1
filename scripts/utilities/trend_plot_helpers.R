# The 2014, 2019, and 2024 releases cover separate five-year periods.
event_plot_theme <- function(base_size = 17) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#777777"),
      text = ggplot2::element_text(color = "#222222"),
      axis.text = ggplot2::element_text(color = "#222222"),
      axis.text.x = ggplot2::element_text(size = 11),
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 15),
      legend.key.width = grid::unit(1.1, "cm")
    )
}

acs_release_points <- function(data, years = c(2014, 2019, 2024),
                               size = 2, large_size = 3.5) {
  list(
    ggplot2::geom_point(
      data = data, ggplot2::aes(year, value, fill = neighborhood, shape = neighborhood),
      color = "black", size = size, stroke = 0.6, inherit.aes = FALSE
    ),
    ggplot2::geom_point(
      data = dplyr::filter(data, year %in% years),
      ggplot2::aes(year, value, fill = neighborhood, shape = neighborhood),
      color = "black", size = large_size, stroke = 0.6,
      inherit.aes = FALSE, show.legend = FALSE
    )
  )
}

acs_year_axis <- function(years = 2011:2024, bold_years = c(2014, 2019, 2024)) {
  labels <- parse(text = ifelse(years %in% bold_years,
                                paste0("bold(\"", years, "\")"), as.character(years)))
  ggplot2::scale_x_continuous(
    limits = range(years), breaks = years, labels = labels,
    expand = ggplot2::expansion(mult = 0.05), oob = scales::oob_keep
  )
}

# These bars show each estimate plus or minus its 90% margin of error.
# Missing margins stay missing; they are not replaced with zero.
acs_error_bars <- function(data, years = c(2014, 2019, 2024), show = TRUE,
                           width = 0.14, linewidth = 0.5) {
  if (!show) return(NULL)
  ggplot2::geom_errorbar(
    data = dplyr::filter(data, year %in% years, is.finite(value),
                         is.finite(value_moe), value_moe >= 0),
    ggplot2::aes(year, ymin = value - value_moe, ymax = value + value_moe,
                 color = neighborhood),
    width = width, linewidth = linewidth, inherit.aes = FALSE, show.legend = FALSE
  )
}

# Labels show descriptive change, not a test of statistical significance.
# An asterisk marks a comparison that had missing source values.
acs_change_labels <- function(data, comparisons, indicator, from_year = 2019,
                              to_year = 2024, neighborhoods = NULL, show = TRUE,
                              size = 3.5, nudge_x = -0.15) {
  if (!show) return(NULL)

  endpoints <- data |>
    dplyr::filter(is.finite(value)) |>
    dplyr::group_by(neighborhood) |>
    dplyr::slice_max(year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(neighborhood, year, value)

  labels <- comparisons |>
    dplyr::filter(
      .data$indicator == .env$indicator,
      .data$from_year == .env$from_year, .data$to_year == .env$to_year,
      is.finite(change)
    ) |>
    dplyr::inner_join(endpoints, by = "neighborhood")
  if (!is.null(neighborhoods)) {
    labels <- dplyr::filter(labels, neighborhood %in% neighborhoods)
  }
  labels <- labels |>
    dplyr::mutate(label = paste0(
      sprintf("%+.1f", change),
      dplyr::if_else(change_unit == "percentage points", " pp", "%"),
      " (", from_year, "-", to_year, ")", dplyr::if_else(has_missing, "*", "")
    ))

  ggrepel::geom_label_repel(
    data = labels, ggplot2::aes(year, value, label = label, color = neighborhood),
    nudge_x = nudge_x, hjust = 1, direction = "y", size = size,
    xlim = range(data$year, na.rm = TRUE), seed = 1, max.overlaps = Inf,
    force = 3, force_pull = 0.05, max.time = 2, max.iter = 100000,
    min.segment.length = 0, box.padding = 0.35, point.padding = 0.2,
    fill = "white", label.size = 0, inherit.aes = FALSE, show.legend = FALSE
  )
}
