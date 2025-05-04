library(sf)
library(tidyverse)
library(gganimate)
library(rvest)

ele_map <- read_sf("data/2025/AUS-March-2025-esri/AUS_ELB_region.shp") |>
  mutate(name = toupper(Elect_div)) |>
  select(name, geometry) |>
  rmapshaper::ms_simplify(keep = 0.001,
                          keep_shapes = FALSE)
# page <- read_html("https://www.abc.net.au/news/elections/federal/2025/results")
# xml2::write_html(page, "data/2025/abc-results-page.html")
page <- read_html("data/2025/abc-results-page.html")

electorates <- page |>
  html_elements("section") |>
  pluck(9) |>
  html_elements(".Electorate_electorate__T3ed3")

ele_names_states <- electorates |>
  html_elements("h2") |>
  html_text()

states <- ele_names_states |>
  word(-1) |>
  str_remove("\\n")

ele_names <- ele_names_states |>
  str_remove(states) |>
  str_trim()

preds <- electorates |>
  html_elements(".GainRetain_gainRetainContainer__DD7HJ") |>
  html_elements(".GainRetain_gainRetain__7WUkv") |>
  html_element("span") |>
  html_text()

party <- preds |>
  str_extract("(.+) ([:alpha:]+)", group = 1)

cond <- preds |>
  str_extract("(.+) ([:alpha:]+)", group = 2)

res <- tibble(name = toupper(ele_names), state = states, party = party, cond = cond)
res

ozmap <- left_join(ele_map, res, by = "name") |>
  # remove the islands ouside main land
  st_crop(xmin = 100, xmax = 168, ymin = -43.74, ymax = -9.12)

hex <- geogrid::calculate_grid(ozmap, grid_type = "hexagonal", seed = 1, verbose = TRUE, learning_rate = 0.03)
hexmap <- geogrid::assign_polygons(ozmap, hex)

ggplot(hexmap) + geom_sf(aes(fill = state)) +
  geom_text(aes(x = V1, y = V2, label = name), size = 2)


swap_ele <- function(map, name1, name2) {
  hexmap_final <- map
  i <- which(map$name == name1)
  j <- which(map$name == name2)

  hexmap_final$name[i] <- name2
  hexmap_final$state[i] <- map$state[j]
  hexmap_final$party[i] <- map$party[j]
  hexmap_final$cond[i] <- map$cond[j]
  hexmap_final$name[j] <- name1
  hexmap_final$state[j] <- map$state[i]
  hexmap_final$party[j] <- map$party[i]
  hexmap_final$cond[j] <- map$cond[i]
  hexmap_final
}

hexmap_final <- hexmap |>
  swap_ele("ISAACS", "GREY") |>
  swap_ele("ADELAIDE", "WANNON") |>
  swap_ele("BRADDON", "KINGSTON") |>
  swap_ele("KINGSTON", "CORANGAMITE") |>
  swap_ele("LYONS", "HOLT") |>
  swap_ele("GIPPSLAND", "BASS") |>
  swap_ele("CANBERRA", "FARRER")|>
  swap_ele("BRADFIELD", "LINGIARI") |>
  swap_ele("KINGSTON", "BRADDON")|>
  swap_ele("KINGSTON", "CORANGAMITE")


ggplot(hexmap_final) + geom_sf(aes(fill = state)) +
  geom_text(aes(x = V1, y = V2, label = name), size = 2)

# saveRDS(hexmap_final, "data/2025/hexmap_final.rds")

shift <- tibble(
  x = c(9, 3, 0, 3, -2, 0, -2, -4),
  y = c(-8, 0, 3, 3, -14, -4, -1, -1),
  state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
) |>
  st_as_sf(coords = c("x", "y")) |>
  as_tibble()

join_wide_maps <- hexmap_final |>
  rename(hex = geometry) |>
  as_tibble()  |>
  left_join(shift, by = "state") |>
  mutate(hex = hex + geometry) |>
  select(-geometry) |>
  left_join(as_tibble(ozmap), by = join_by(name, state, party, cond)) |>
  rename(orig = geometry)

ggplot(join_wide_maps) + geom_sf(aes(geometry = hex, fill = state))
ggplot(join_wide_maps) + geom_sf(aes(geometry = orig, fill = state))
ggplot(ozmap) + geom_sf(aes(geometry, fill = state))

join_long_maps <- bind_rows(ozmap |>
                              mutate(type = "orig") |>
                              st_as_sf() |>
                              st_transform(crs = 4326),
                            join_wide_maps |>
                              rename(geometry = hex) |>
                              mutate(type = "hex") |>
                              st_as_sf() |>
                              st_set_crs(4326)) |>
  mutate(prediction = fct_collapse(party,
                                   Labor = "Labor",
                                   #Undecided = "",
                                   Coalition = c("Liberal", "LNP", "National"),
                                   Greens = "Greens",
                                   Other = c("Centre Alliance", "Independent", "Katter's Aus"))) |>
  mutate(type = factor(type, levels = c("orig", "hex")))

ggplot(join_long_maps, aes(group = name)) +
  geom_sf(aes(geometry = geometry, fill = prediction),
          color = "black") +
  labs(caption = "Source: Predictions from ABC News on 04/05/2025",
       fill = "Prediction") +
  #facet_wrap(~type) +
  theme_void(base_size = 18) +
  scale_fill_manual(values = c("#e11f30", "#0952bf", "green4", "black"),
                    limits = c("Labor", "Coalition", "Greens", "Other")) +
  transition_states(type)

gganimate::anim_save("hex-animate-2025.gif")
