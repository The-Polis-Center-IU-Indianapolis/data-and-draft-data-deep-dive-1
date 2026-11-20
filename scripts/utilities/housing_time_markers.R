# Historical time markers for the 2011-2024 plots.
# Sources: HUD Indianapolis housing analyses (2019 and 2023), FHCCI (2023),
# and Federal Reserve's eviction-moratorium chronology (2022).
housing_time_markers <- function(label_size = 11, top_margin = 52, show_bands = TRUE) {
  # These positions match the fixed 2011-2024 x-axis and its 5% padding.
  event_years <- c(2012, 2017.5, 2020, 2022.5)
  anchor_x <- (event_years - 2010.35) / 14.3
  label_x <- c(0.11, anchor_x[2], anchor_x[3], 0.89)
  labels <- c("Great Recession\nrecovery",
              "Housing supply\ntransitions",
              "COVID",
              "Rent increases +\nHousing Shortages")
  # Keep the center visible, then fade to white over one year at either edge.
  bands <- data.frame(start = c(2011, 2016, 2021), end = c(2013, 2019, 2024),
                      fill = c("#DCE7EE", "#E5E2EA", "#F0E6D4"))
  shading <- lapply(seq_len(nrow(bands)), function(i) {
    edges <- c(seq(bands$start[i] - 1, bands$start[i], length.out = 31),
               bands$end[i], seq(bands$end[i], bands$end[i] + 1, length.out = 31)[-1])
    opacity <- c(seq(0, 0.55, length.out = 30), 0.55,
                 seq(0.55, 0, length.out = 30))
    ggplot2::annotate("rect", xmin = head(edges, -1), xmax = tail(edges, -1),
                      ymin = -Inf, ymax = Inf, fill = bands$fill[i], alpha = opacity)
  })
  if (!show_bands) {
    shading <- NULL
    anchor_x <- anchor_x[3]
    label_x <- label_x[3]
    labels <- labels[3]
  }
  leaders <- grid::segmentsGrob(
    x0 = anchor_x, x1 = label_x,
    y0 = grid::unit(1, "npc"),
    y1 = grid::unit(1, "npc") + grid::unit(3, "mm"),
    gp = grid::gpar(col = "#777777", lwd = 0.6)
  )
  text <- grid::textGrob(
    labels, x = label_x,
    y = grid::unit(1, "npc") + grid::unit(4, "mm"), just = "bottom",
    gp = grid::gpar(col = "black", fontsize = label_size, lineheight = 0.95)
  )
  list(
    shading,
    ggplot2::geom_vline(xintercept = 2020, color = "#777777",
                        linewidth = 0.45, linetype = "dotted"),
    ggplot2::annotation_custom(grid::grobTree(leaders, text)),
    ggplot2::coord_cartesian(clip = "off"),
    ggplot2::theme(
      plot.margin = ggplot2::margin(top_margin, 8, 8, 8)
    )
  )
}
