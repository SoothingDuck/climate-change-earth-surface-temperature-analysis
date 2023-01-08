library(tidyverse)
library(lubridate)
library(tidyquant)
library(viridis)

## Global
df_global <- read_csv("DATA/GlobalTemperatures.csv")

# Line
df_global %>%
  pivot_longer(-dt) %>%
  filter(! is.na(value)) %>%
  ggplot() +
  geom_line(aes(x = dt, y = value)) +
  facet_wrap(~ name, scales = "free_x") +
  expand_limits(y = 0)

# Line per month <- Best
df_global %>%
  select(-contains("Uncertainty")) %>%
  filter(year(dt) >= 1850) %>%
  pivot_longer(-dt) %>%
  filter(! is.na(value)) %>%
  mutate(
    Year = year(dt),
    Month = month(dt)
  ) %>%
  ggplot() +
  geom_line(aes(x = Year, y = value, color = name)) +
  facet_wrap(~ Month, scales = "free_x") +
  expand_limits(y = 0) +
  scale_color_tq()

# Heatmap
df_global %>%
  select(-contains("Uncertainty")) %>%
  filter(year(dt) >= 1900) %>%
  pivot_longer(-dt) %>%
  filter(! is.na(value)) %>%
  mutate(
    Year = year(dt),
    Month = month(dt)
  ) %>%
  filter(name == "LandAverageTemperature") %>%
  arrange(dt) %>%
  group_by(Month) %>%
  mutate(
    first_value = value %>% pluck(1),
    pct_evolution = (value-first_value)/first_value
  ) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Month, fill = value)) +
  scale_fill_viridis(option = "C")

## Global by Country
df_global_country <- read_csv("DATA/GlobalLandTemperaturesByCountry.csv")


df_global_country %>% distinct(Country)

df_global_country %>%
  pivot_longer(cols = contains("Temperature")) %>%
  filter(! is.na(value)) %>%
  filter(Country %in% c("France")) %>%
  filter(name == "AverageTemperature") %>%
  filter(year(dt) >= 1800) %>%
  mutate(Month = factor(month(dt))) %>%
  mutate(Year = year(dt)) %>%
  ggplot() +
  geom_tile(aes(y = Year, x = Month, fill = value), color = "black") +
  scale_fill_viridis(option = "turbo")

## Moyenne de l'année
df_global_country %>%
  pivot_longer(cols = contains("Temperature")) %>%
  filter(! is.na(value)) %>%
  filter(Country %in% c("France")) %>%
  filter(name == "AverageTemperature") %>%
  filter(year(dt) >= 1850) %>%
#  filter(year(dt) <= 1900) %>%
  mutate(Month = factor(month(dt))) %>%
  mutate(Year = year(dt)) %>%
  ## group_by(Year) %>%
  ## summarise(mean_temp = mean(value)) %>%
  ## ungroup() %>%
  ggplot() +
  geom_line(aes(x = Year, y = value)) +
  geom_smooth(aes(x = Year, y = value)) +
  facet_grid(Month ~ ., scales = "free") +
  scale_color_tq()

## Map proj
library(sf)
library(gganimate)

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")


## Temperature en 1900
df_year_country <- df_global_country %>%
  pivot_longer(cols = contains("Temperature")) %>%
  filter(! is.na(value)) %>%
  filter(name == "AverageTemperature") %>%
  mutate(Year = year(dt)) %>%
  group_by(Year, Country) %>%
  summarise(MeanTemp = mean(value)) %>%
  ungroup() %>%
  mutate(Country = case_when(
           Country == "Central African Republic" ~ "Central African Rep.",
           Country == "Congo (Democratic Republic Of The)" ~ "Dem. Rep. Congo",
           TRUE ~ Country
  ))

df_global_country %>%
  distinct(Country) %>%
  filter(grepl("Congo", Country))

world %>%
  left_join(df_year_country, by = c("name" = "Country")) %>%
  filter(Year >= 1950) %>%
  ggplot() +
  geom_sf(aes(fill = MeanTemp)) +
  theme_tq() +
  scale_fill_viridis_c(option = "turbo") +
  transition_time(Year) +
  ease_aes('linear')
