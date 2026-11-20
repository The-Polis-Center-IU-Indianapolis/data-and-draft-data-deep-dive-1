
# !!!Let's explore neighborhoods and indicators!!!
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(gt)

# Load some custom helper functions.
source("scripts/utilities/place_map_labels.R")
source("scripts/utilities/housing_time_markers.R")
source("scripts/utilities/trend_plot_helpers.R")



# Always start by looking at the data first. :)



# Read the prepared ACS table
acs <- read_csv(
  file = "data/data_processed/indyvitals_neighborhood_acs_2011_2024.csv",
  col_types = cols(neighborhood_id = col_character())
  )
changes <- read_csv(
  "data/data_processed/indyvitals_neighborhood_acs_changes.csv",
  col_types = cols(neighborhood_id = col_character())
)

View(acs)
# dim(acs)
# glimpse(acs)
# ?glimpse

# List out neighborhoods
sort(unique(acs$neighborhood[acs$geo_type == "neighborhood"]))

# List All available numeric fields
names(select(acs, where(is.numeric)))


# Choose our four neighborhoods
chosen_neighborhoods <- c(
  "Meridian Hills/Williams Creek",
  "East Warren",
  "Arlington Woods",
  "Near Southside"
)

# Pick our indicator and name it

# First example: household income in (2024 dollars)
# To switch examples, replace all four settings with one complete example below
indicator <- "household_income_median_weighted_real"
indicator_label <- "Median Household Income (2024)"
axis_labels <- scales::label_dollar(accuracy = 1)
indicator_limits <- NULL


# Second example: Share of Owners Occupying
# indicator <- "share_owner_occupied"
# indicator_label <- "Share of Owners Occupying"
# axis_labels <- scales::label_percent(accuracy = 1)
# indicator_limits <- c(0, 1)





# Now let's plot!




# Prep neighborhood trend data
selected <- acs |>
  dplyr::filter(
    geo_type == "neighborhood",
    neighborhood %in% chosen_neighborhoods
  ) |>
  dplyr::mutate(
    neighborhood = factor(neighborhood, levels = chosen_neighborhoods),
    value = .data[[indicator]],
    value_moe = .data[[paste0(indicator, "_moe")]]
  )

# Prep Marion county trend data
county <- acs |>
  filter(geo_type == "county") |>
  transmute(year, value = .data[[indicator]],
            value_moe = .data[[paste0(indicator, "_moe")]], neighborhood = "Marion County")

trend_data <- bind_rows(county, selected)

# Prep some variables for color and shape
# SAVI blue, SAVI coral, SAVI gold, and SAVI green
neighborhood_colors <- setNames(
  c("#0068AC", "#F0506C", "#FFB607", "#9BD16D")[seq_along(chosen_neighborhoods)],
  chosen_neighborhoods
)
neighborhood_shapes <- setNames(c(21, 22, 24, 23)[seq_along(chosen_neighborhoods)],
                                chosen_neighborhoods)

# Prep Legend breaks and labels
legend_breaks <- c(chosen_neighborhoods, "Marion County")
legend_labels <- gsub("/", "/\n", legend_breaks, fixed = TRUE)


# Make the plot!
trend_plot <- ggplot(selected, aes(year, value, group = neighborhood)) +
  event_plot_theme() +
  # Draw historical context behind the data. ACS points cover five-year periods
  housing_time_markers() + # custom function to add the plot backgrounds
  # Draw the County first so neighborhood lines remain visible at crossings
  geom_line(
    data = county,
    aes(color = neighborhood),
    linewidth = 1.3
  ) +
  geom_line(
    aes(color = neighborhood),
    linewidth = 1.3
  ) +
  # Set show = TRUE to add 90% error bars at the larger points.
  acs_error_bars(trend_data, show = TRUE) +
  acs_release_points(trend_data) + # Set show = TRUE for change labels; add neighborhoods = c("East Warren") to select lines.
  # pp means percentage points; * means source values were missing.
  acs_change_labels(trend_data, changes, indicator, from_year = 2019, to_year = 2024, show = FALSE) +
  # Edit displayed colors and fills and shapes
  scale_color_manual(name = NULL, values = c(neighborhood_colors, "Marion County" = "#000000"),
                     limits = legend_breaks, breaks = legend_breaks, labels = legend_labels) +
  scale_fill_manual(name = NULL, values = c(neighborhood_colors, "Marion County" = "#000000"),
                    limits = legend_breaks, breaks = legend_breaks, labels = legend_labels) +
  scale_shape_manual(name = NULL, values = c(neighborhood_shapes, "Marion County" = 21),
                     limits = legend_breaks, breaks = legend_breaks, labels = legend_labels) +
  # Edit legend
  guides(color = guide_legend(ncol = 1),
         fill = guide_legend(ncol = 1),
         shape = guide_legend(ncol = 1)) +
  # Edit scales
  acs_year_axis() +
  scale_y_continuous(labels = axis_labels, limits = indicator_limits, oob = scales::oob_keep) +
  # Edits axis labels
  labs(x = NULL, y = indicator_label)

print(trend_plot)

# example for saving the plots
# ggsave(file.path("products/plots", paste0(indicator, ".png")), trend_plot,
#        width = 12, height = 5.5, dpi = 600, bg = "white")




# Now let's map!




# Read the cleaned neighborhood boundaries.
neighborhoods <- read_sf(
  "data/data_collected/spatial/indy-vitals/Indy_Neighborhoods.gpkg",
  layer = "neighborhoods"
)

# Join neighborhood boundaries and the ACS data
map_data <- neighborhoods |>
  left_join(filter(acs, geo_type == "neighborhood", year == 2024), by = "neighborhood_id")

# Get the selected neighborhoods on their own
highlighted <- filter(map_data, neighborhood %in% chosen_neighborhoods)

# Position labels and connect them to neighborhoods with lead lines
bounds <- st_bbox(neighborhoods)
map_width <- bounds["xmax"] - bounds["xmin"]
map_height <- bounds["ymax"] - bounds["ymin"]
labels <- place_map_labels(highlighted, bounds) # custom function does heavy lifting!

# Build the map!

county_map <- ggplot(map_data) +
  # Add various spatial geometries like boundaries and segments and label texts
  geom_sf(aes(fill = .data[[indicator]]), color = "#777777", linewidth = 0.3) +
  # Outline the same neighborhoods chosen for the trend plot
  geom_sf(data = highlighted, fill = NA, color = "white", linewidth = 1.8) +
  geom_sf(data = highlighted, aes(color = neighborhood), fill = NA, linewidth = 1) +
  geom_segment(data = labels,
               aes(x = x, y = y, xend = label_x, yend = label_y, color = neighborhood),
               linewidth = 0.6) +
  geom_label(data = labels,
             aes(label_x, label_y, label = label, hjust = align),
             color = "black", fill = "white", linewidth = 0, size = 5, lineheight = 0.95) +
  # Edit the colors of the neighborhoods
  scale_color_manual(values = neighborhood_colors, guide = "none") +
  # Edit the fill for the tracts
  scale_fill_gradient(low = "#eee5f2", high = "#7b3294", labels = axis_labels,
                      limits = indicator_limits, breaks = scales::breaks_pretty(n = 3),
                      na.value = "#dddddd") +
  coord_sf(xlim = c(bounds["xmin"] - 0.50 * map_width,
                  bounds["xmax"] + 0.50 * map_width),
          ylim = c(bounds["ymin"] - 0.18 * map_height, bounds["ymax"] + 0.20 * map_height),
          expand = FALSE, clip = "off") +
  # Edit legend
  guides(fill = guide_colorbar(title.position = "top", barwidth = grid::unit(3.5, "in"))) +
  labs(fill = indicator_label) +
  # Apply a theme present
  theme_void(base_size = 18) +
  # Customize the theme further
  theme(legend.position = "bottom", legend.text = element_text(size = 20))

print(county_map)

# ggsave(file.path("products/maps", paste0(indicator, ".png")), county_map,
#        width = 7, height = 6, dpi = 600, bg = "white")



# Now let's make a table!

# Change these years to compare a different pair of ACS estimates.
from_year <- 2011
to_year <- 2024

# Get the indicator estimate for both years.
table_data <- acs |>
  filter(
    year %in% c(from_year, to_year),
    (geo_type == "neighborhood" & neighborhood %in% chosen_neighborhoods) |
      geo_type == "county"
  ) |>
  transmute(
    Neighborhood = if_else(geo_type == "county", "Marion County", neighborhood),
    year,
    Estimate = .data[[indicator]]
  ) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = Estimate,
    names_glue = "{.value}_{year}"
  ) |>
  mutate(
    Percent_Change = if_else(
      .data[[paste0("Estimate_", from_year)]] == 0,
      NA_real_,
      (.data[[paste0("Estimate_", to_year)]] -
         .data[[paste0("Estimate_", from_year)]]) /
        .data[[paste0("Estimate_", from_year)]]
    ),
    # Percentage_Point_Change = 100 * (
    #   .data[[paste0("Estimate_", to_year)]] -
    #     .data[[paste0("Estimate_", from_year)]]
    # )
  ) |>
  arrange(match(Neighborhood, c(chosen_neighborhoods, "Marion County")))

estimate_columns <- c(
  paste0("Estimate_", from_year), paste0("Estimate_", to_year),
  "Percent_Change"
  # "Percentage_Point_Change"
)

column_labels <- stats::setNames(
  c(
    paste(from_year, "estimate"), paste(to_year, "estimate"),
    "Percent change"
    # "Percentage-point difference"
  ),
  estimate_columns
)

indicator_table <- table_data |>
  gt::gt() |>
  gt::tab_header(
    title = gt::md(paste("**", indicator_label, "**", sep = "")),
    subtitle = paste("Estimates and change from", from_year, "to", to_year)
  ) |>
  gt::fmt_number(
    columns = dplyr::all_of(c(paste0("Estimate_", from_year),
                              paste0("Estimate_", to_year))),
    decimals = 1
  ) |>
  gt::fmt_percent(columns = Percent_Change, decimals = 1) |>
  # gt::fmt_number(columns = Percentage_Point_Change, decimals = 1, suffix = " pp") |>
  gt::cols_label(Neighborhood = "Neighborhood", .list = column_labels) |>
  gt::tab_options(
    table.font.size = 14,
    heading.title.font.size = 18,
    column_labels.font.weight = "bold"
  )

indicator_table
