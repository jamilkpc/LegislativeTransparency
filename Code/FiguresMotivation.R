library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)
library(haven)
library(countrycode)

dfT <- read.csv('2PLtransparency.csv') %>% select(-X)
df <- read.csv('export-country-compare.csv', skip = 17) %>% 
  select(ISO.Code, Country) %>% 
  rename(iso = ISO.Code) %>% 
  left_join(dfT) %>% 
  drop_na %>% 
  mutate(ccodealp = countrycode::countrycode(iso,'iso2c','iso3c'))

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- merge(world, df, by.x = "iso_a3", by.y = "ccodealp", all.x = TRUE)

ggplot(data = world) +
  geom_sf(aes(fill = LegislativeTransparency2)) +  # Use `value` from your data as fill
  scale_fill_viridis_c(option = "magma", na.value = "lightgrey") +  # Color palette and NA color
  theme_minimal() +  # Clean theme
  labs(fill = "Transparency Index", title = "Legislative Transparency Map") +
  theme(
    panel.grid = element_blank(),  # Remove grid lines
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank()   # Remove axis ticks
  )

qog <- read_dta('~/Downloads/qog_std_cs_jan24_stata14.dta') %>% 
  select(ccodealp, ipu_l_s, wdi_pop, wdi_gdpcappppcon2017,bmr_dem) %>% 
  mutate(proximity = ipu_l_s/wdi_pop^(1/3),
         ipu_l_s = log(ipu_l_s),
         wdi_pop = log(wdi_pop),
         wdi_lgdppc = log(wdi_gdpcappppcon2017)) %>% 
  left_join(df %>% select(ccodealp, LegislativeTransparency2)) %>% 
  drop_na

summary(lm(LegislativeTransparency2 ~ proximity, data = qog))
summary(lm(LegislativeTransparency2 ~ bmr_dem, data = qog))
summary(lm(LegislativeTransparency2 ~ wdi_lgdppc, data = qog))


ggplot(qog, aes(x = proximity, y = LegislativeTransparency2)) +
  geom_point(color = "black", size = 2) +   # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line without confidence interval
  labs(
    x = "Proximity of Representation", 
    y = "Legislative Transparency"
  ) +
  theme_minimal()

ggplot(qog, aes(x = wdi_lgdppc, y = LegislativeTransparency2)) +
  geom_point(color = "black", size = 2) +   # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line without confidence interval
  labs(
    x = "log(GDPpc)", 
    y = "Legislative Transparency"
  ) +
  theme_minimal()

ggplot(qog, aes(x = bmr_dem, y = LegislativeTransparency2)) +
  geom_point(color = "black", size = 2) +   # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line without confidence interval
  labs(
    x = "log(GDPpc)", 
    y = "Legislative Transparency"
  ) +
  theme_minimal()
