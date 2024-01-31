# load packages ---------
library(tidyverse)
library(tigris)
# library(tidycensus)
library(sf)

# # source api key -----------
# source(here::here("slides", "sections", "KEYS.R"))
# census_api_key(censusKEY)
#
# # read in precip data --------
# precip_data <- read_csv(here::here("slides", "data", "county-jan19-dec23-precip.csv"), skip = 4)# |>
#
# # get shape data using {tidycensus} -----------
# # us_counties <- get_acs(
# #   geography = "county",
# #   variables = "B19013_001",
# #   year = 2021,
# #   geometry = TRUE)
#
# us_counties <- get_estimates(
#   geography = "county",
#   product = "population",
#   year = 2022,
#   geometry = TRUE
# ) |>
#   filter(variable == "POPESTIMATE")
#
# # wrangle census data ----------
# # us_counties_wrangled <- us_counties |>
# #   separate_wider_delim(cols = NAME, delim = ", ", names = c("county", "state")) |>
# #   filter(!state %in% c("Alaska", "Hawaii", "Puerto Rico", "District of Columbia")) |>
# #   mutate(county = str_replace(string = county, pattern = " city", replacement = " City")) |>
# #   select(GEOID, county, state, geometry)
#
# us_counties_wrangled <- us_counties |>
#   separate_wider_delim(cols = NAME, delim = ", ", names = c("county", "state")) |>
#   filter(!state %in% c("Alaska", "Hawaii", "Puerto Rico", "District of Columbia")) |>
#   # filter(str_detect(string = county, pattern = "Planning Region", negate = TRUE),
#   #        str_detect(string = county, pattern = "Planning Regio", negate = TRUE)) |>
#   mutate(county = str_replace(string = county, pattern = " city", replacement = " City")) |>
#   select(GEOID, county, state, geometry)
#
# # wrangle precipitation data ---------
# precip_wrangled <- precip_data |>
#   janitor::clean_names() |>
#   rename(county = name) |>
#   filter(!county %in% c("Washington, D.C.")) |>
#   mutate(county = str_replace(string = county, pattern = "Dona Ana County", replacement = "Doña Ana County")) |>
#   mutate(value = as.numeric(value),
#          x1901_2000_mean = as.numeric(x1901_2000_mean)) |>
#   mutate(perc_change = ((value - x1901_2000_mean)/x1901_2000_mean)*100) |>
#   select(id, state, county, mean_1901_2000 = x1901_2000_mean, precip = value, perc_change, anomaly_1901_2000_base_period)
#
# # join dfs (see https://github.com/tidyverse/ggplot2/issues/3936) -------
# joined_precip_geom <- full_join(precip_wrangled, us_counties_wrangled) |>
#   select(GEOID, id, state, county, mean_1901_2000, precip, perc_change, anomaly_1901_2000_base_period, geometry)
#
# # make sf object again -----------
# joined_precip_geom_sf <- st_as_sf(joined_precip_geom)
#
# # precip_test <- precip_wrangled |> select(county, state, id)
# # counties_test <- us_counties_wrangled |> select(county, state, GEOID)
# # difference <- anti_join(counties_test, precip_test)
#
# # get state lines -----
# usa <- st_as_sf(maps::map("state", fill=TRUE, plot = FALSE))
#
# # plot ------
# base_plot <- ggplot(joined_precip_geom_sf) +
#   geom_sf(color = "#2b2b2b",  size = 0.125) +
#   geom_sf(aes(fill = perc_change), linewidth = 0.1) +
#   labs(title = "5-year precipitation compared with the 20th century average",
#        caption = "Source: National Centers for Environmental Information") +
#   theme_void() +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     plot.caption = element_text(face = "italic", margin = margin(t = 20, r = 20, b = 0, l = 0))
#   )
#
# base_plot

# ----------------------------------------------------------------

county_geo <- tigris::counties(class = "sf", cb = TRUE) |>
  shift_geometry()

county_geo_wrangled <- county_geo |>
  janitor::clean_names() |>
  rename(county = namelsad, state = state_name) |>
  filter(!state %in% c("Alaska", "Hawaii", "District of Columbia",
                       "United States Virgin Islands", "Puerto Rico", "American Samoa",
                       "Commonwealth of the Northern Mariana Islands", "Guam")) |>
  mutate(county = str_replace(string = county, pattern = " city", replacement = " City")) # |>
  # mutate(county = case_when(
  #   county == "Greater Bridgeport Planning Region" ~ "Fairfield County",
  #   #county == "Capitol Planning Region" ~ "Hartford County",
  #   county == "Northwest Hills Planning Region" ~ "Litchfield County",
  #   county == "Lower Connecticut River Valley Planning Region" ~ "Middlesex County",
  #   county == "Northwest Hills Planning Region" ~ "New Haven County",
  #   county == "Southeastern Connecticut Planning Region" ~ "New London County",
  #   #county == "Capitol Planning Region" ~ "Tolland County",
  #   county == "Northeastern Connecticut Planning Region" ~ "Windham County"
  #   ))

precip_data <- read_csv(here::here("slides", "data", "county-jan19-dec23-precip.csv"), skip = 4)

precip_wrangled <- precip_data |>
  janitor::clean_names() |>
  rename(county = name) |>
  filter(!county %in% c("Washington, D.C.")) |>
  mutate(county = str_replace(string = county, pattern = "Dona Ana County", replacement = "Doña Ana County")) |>
  mutate(value = as.numeric(value),
         x1901_2000_mean = as.numeric(x1901_2000_mean)) |>
  mutate(perc_change = ((value - x1901_2000_mean)/x1901_2000_mean)*100) |>
  select(id, state, county, mean_1901_2000 = x1901_2000_mean, precip = value, perc_change, anomaly_1901_2000_base_period)

# join dfs (see https://github.com/tidyverse/ggplot2/issues/3936 & https://map-rfun.library.duke.edu/032_thematic_mapping_geom_sf.html) -------
joined_precip_geom <- full_join(county_geo_wrangled, precip_wrangled) #|>
  #drop_na(perc_change)

# plot base map ------
base_map <- ggplot(joined_precip_geom) +
  geom_sf(aes(fill = perc_change), linewidth = 0.1) +
  labs(title = "5-year precipitation compared with the 20th century average",
       subtitle = "January 2019 - December 2023",
       caption = "Source: National Centers for Environmental Information") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(face = "italic", margin = margin(t = 20, r = 10, b = 0, l = 0))
  )

base_map

# palette -------
my_brew_palette11 <- RColorBrewer::brewer.pal(n = 11, name = 'BrBG')
my_brew_palette10 <- RColorBrewer::brewer.pal(n = 10, name = 'BrBG')

# cont --------
base_map +
scale_fill_gradientn(colors = my_brew_palette11,
                     labels = scales::label_percent(scale = 1),
                     breaks = scales::breaks_width(width = 10)) +
  guides(fill = guide_colorbar(label.hjust = 0.5,
                               barwidth = 15, barheight = 0.75))

# updated --------
base_map +
  scale_fill_gradientn(colors = my_brew_palette11,
                       labels = scales::label_percent(scale = 1),
                       breaks = scales::breaks_width(width = 10),
                       values = scales::rescale(x = c(
                         min(na.omit(joined_precip_geom)$perc_change),
                         0,
                         max(na.omit(joined_precip_geom)$perc_change)))) +
  guides(fill = guide_colorbar(label.hjust = 0.5,
                               barwidth = 15, barheight = 0.75))

# classed -----
base_map +
  scale_fill_stepsn(colors = my_brew_palette10,
                    labels = scales::label_percent(scale = 1)) +
  guides(fill = guide_colorsteps(barwidth = 25, barheight = 0.75))

# breaks 10 -----
base_map +
  scale_fill_stepsn(colors = my_brew_palette10,
                    labels = scales::label_percent(scale = 1),
                    breaks = scales::breaks_width(width = 10)) +
  guides(fill = guide_colorsteps(barwidth = 25, barheight = 0.75))

# breaks 5 -----
base_map +
  scale_fill_stepsn(colors = my_brew_palette10,
                    labels = scales::label_percent(scale = 1),
                    values = scales::rescale(x = c(
                      min(na.omit(joined_precip_geom)$perc_change),
                      0,
                      max(na.omit(joined_precip_geom)$perc_change))),
                    breaks = scales::breaks_width(width = 5)) +
  guides(fill = guide_colorsteps(barwidth = 25, barheight = 0.75))





ct <- joined_precip_geom |>
  filter(state == "Connecticut")
