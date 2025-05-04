library(sf)
library(tidyverse)
library(gganimate)
ozmap <- readRDS("data/2022/ozced.rds")

# note: pivot_longer has error:
# ! Can't combine `hex` <sfc_GEOMETRY> and `geometry` <sfc_GEOMETRY>.
# so use approach below
hexmap <- ozmap %>%
  select(-geometry) %>%
  rename(geometry = hex) %>%
  mutate(type = "hex")
longmap <- ozmap %>%
  select(-hex) %>%
  mutate(type = "choropleth") %>%
  bind_rows(hexmap) %>%
  mutate(prediction = fct_collapse(prediction,
                                   Labor = "ALP",
                                   Undecided = "",
                                   Coalition = c("LIB", "LNP", "NAT"),
                                   Green = "GRN",
                                   Other = c("CA", "IND", "KAP")))

# Prediction taken from guardian
# https://www.theguardian.com/australia-news/ng-interactive/2022/may/21/australia-election-2022-results-live-tracker-australian-federal-poll-who-won-is-winning-track-the-votes-seat-counts-electoral-commission-aec-latest-seat-count
ggplot(longmap) +
  geom_sf(aes(geometry = geometry, fill = prediction),
          color = "black") +
  #facet_wrap(~type) +
  theme_void() +
  labs(fill = "Prediction") +
  scale_fill_manual(values = c("#e11f30", "#0952bf", "green4", "black", "grey"),
                    limits = c("Labor", "Coalition", "Green", "Other", "Undecided")) +
  transition_states(type)
