# ............................................
# Prep data for Albemarle County Equity Atlas
#   with magisterial/precinct district overlays
# Date: 2022-02-10
# Author: mpc
# ............................................


# ............................................
# Set up ----
library(tidyverse)
library(sf)

# pull in regional equity atlas
load("app_data_2021.Rdata")


# ............................................
# ACS/equity atlas data ----
## reduce ----
# just albemarle, just tracts and block groups
data <- all_data %>% 
  filter(GEO_LEVEL %in% c("Block Group", "Census Tract"), COUNTYFP == "003")
data <- st_transform(data, 4326)

ac_geo <- counties_geo %>% 
  filter(COUNTYFP == "003")
ac_geo <- st_transform(ac_geo, 4326)

mcd <- mcd_sf %>% 
  filter(COUNTYFP == "003")
parks <- parks_sf %>% 
  filter(FIPS == "003")
sabselem <- sabselem_sf %>% 
  filter(county == "003")
schools <- schools_sf %>% 
  filter(CNTY == "51003")

rm(list = c("race_comp", "ind_choices_county", "counties_geo",
            "years", "year_ind", "race_vars", "counties", "all_data",
            "mcd_sf", "parks_sf", "sabselem_sf", "schools_sf"))

# ............................................
# add proposed districts ----
# districts
d1 <- st_read("options/Magisterial_Option_1/Magisterial_Option_1.shp")
d1 <- st_transform(d1, 4326)

d2 <- st_read("options/Magisterial_Option_2/Magisterial_Option_2.shp")
d2 <- st_transform(d2, 4326)

d3 <- st_read("options/Magisterial_Option_3/Magisterial_Option_3.shp")
d3 <- st_transform(d3, 4326)


# precincts
p1 <- st_read("options/Precincts_Option_1/Precincts_Option_1.shp")
p1 <- st_transform(p1, 4326)

p2 <- st_read("options/Precincts_Option_2/Precincts_Option_2.shp")
p2 <- st_transform(p2, 4326)

p3 <- st_read("options/Precincts_Option_3/Precincts_Option_3.shp")
p3 <- st_transform(p3, 4326)


# ............................................
# save atlas data/boundaries ----
save.image(file = "data/app_data.Rdata") 


# ............................................
# read dem summary data ----
library(readxl)
library(janitor)
library(plotly)

crd <- read_excel("options/Proposed 2020 Magisterial District and Voting Precincts Demographics_Final.xlsx",
                  sheet = 2)

# prep dem summary data ----
crd <- crd %>% clean_names()

# crd %>% 
#   ggplot(aes(x = option, y = totalhisp, color = option)) +
#   geom_point()

# create/plot counts ----
crd_counts_long <-
  crd %>% select(option, district, total_pop, totalhisp, totalnh,
                 whitenh, blacknh, aiannh, asiannh, hpinh, othernh, mltmnnh) %>% 
  pivot_longer(-c(option, district), names_to = "population", values_to = "counts")

crd_counts_long <- crd_counts_long %>% 
  mutate(population = factor(population, 
                             levels = c("total_pop", "totalnh", "totalhisp",
                                        "whitenh", "blacknh", "asiannh",
                                        "othernh", "aiannh", "mltmnnh", "hpinh"),
                             labels = c("Total Population", "Non-Hispanic, All Races",
                                        "Hispanic, All Races", "White, Non-Hispanic",
                                        "Black, Non-Hispanic", "Asian, Non-Hispanic",
                                        "Other Race, Non-Hispanic", "American Indian, Alaskan Native, NH",
                                        "Multiracial, Non-Hispanic", "Hawaiian, Pacific Islander, NH"))
         )

counts <- crd_counts_long %>% 
  ggplot(aes(x = fct_rev(option), y = counts, color = option, label = district)) +
  geom_point() +
  scale_color_manual(values = c("grey", "purple", "red", "#ff7f00")) +
  coord_flip() +
  facet_wrap(~population, scales = "free_x") +
  labs(y = "Population Counts", x = "") +
  guides(color = "none")

ggplotly(counts, tooltip = c("y", "color", "label")) %>% 
  layout(showlegend = FALSE)

# create/plot percents ----
crd_percent_long <-
  crd %>% select(option, district, totalhisp_percent, totalnh_percent,
                 whitenh_percent, blacknh_percent, aiannh_percent, 
                 asiannh_percent, hpinh_percent, othernh_percent, 
                 mltmnnh_percent) %>% 
  pivot_longer(-c(option, district), names_to = "population", values_to = "percent")

crd_percent_long <- crd_percent_long %>% 
  mutate(population = factor(population, 
                             levels = c("totalnh_percent", "totalhisp_percent", 
                                        "whitenh_percent", "blacknh_percent", 
                                        "asiannh_percent", "othernh_percent", 
                                        "aiannh_percent", "mltmnnh_percent", 
                                        "hpinh_percent"),
                             labels = c("Non-Hispanic, All Races",
                                        "Hispanic, All Races", "White, Non-Hispanic",
                                        "Black, Non-Hispanic", "Asian, Non-Hispanic",
                                        "Other Race, Non-Hispanic", "American Indian, Alaskan Native, NH",
                                        "Multiracial, Non-Hispanic", "Hawaiian, Pacific Islander, NH"))
  )

percent <- crd_percent_long %>% 
  ggplot(aes(x = fct_rev(option), y = percent, color = option, label = district)) +
  geom_point() +
  scale_color_manual(values = c("grey", "purple", "red", "#ff7f00")) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.1)) +
  coord_flip() +
  facet_wrap(~population, scales = "free_x") +
  labs(y = "Population Percents", x = "") +
  guides(color = "none")

ggplotly(percent, tooltip = c("y", "color", "label")) %>% 
  layout(showlegend = FALSE)

# ............................................
# save dem summary ----
save(crd_counts_long, crd_percent_long, file = "data/summaries.Rdata") 
# load("data/summaries.Rdata")


# ............................................
# add census block data ----
library(tigris)

# from census: downloaded from data.census.gov
# https://data.census.gov/cedsci/table?g=0500000US51003%241000000&y=2020&d=DEC%20Redistricting%20Data%20%28PL%2094-171%29&tid=DECENNIALPL2020.P2

table <- read_csv("decennial_redistricting/DECENNIALPL2020.P2_data_with_overlays_2022-02-19T130008.csv")

crd_data <- table %>% 
  filter(GEO_ID != "id") %>% 
  select(GEO_ID, NAME, total = P2_001N, hispanic = P2_002N,
         nonhispanic = P2_003N, whitenh = P2_005N, 
         blacknh = P2_006N, aiannh = P2_007N, asiannh = P2_008N,
         nhpinh = P2_009N, othernh = P2_010N, multinh = P2_011N) %>% 
  mutate(across(total:multinh, as.numeric))

summary(crd_data)

# create proportions (make printable names)
crd_data <- crd_data %>% 
  mutate(`Percent Hispanic` = (hispanic/total)*100,
         `Percent White` = (whitenh/total)*100,
         `Percent Black` = (blacknh/total)*100,
         `Percent Asian` = (aiannh/total)*100,
         p_asiannh = asiannh/total,
         p_nhpinh = nhpinh/total,
         p_othernh = othernh/total,
         p_multinh = multinh/total) %>% 
  rename(`Total Population` = total)

# for benchmarking
crd_totals <- crd_data %>% 
  summarize(countypop = sum(`Total Population`),
            pop_hispanic = sum(hispanic),
            pop_nonhispanic = sum(nonhispanic),
            pop_whitenh = sum(whitenh), 
            pop_blacknh = sum(blacknh),
            pop_aiannh = sum(aiannh),
            pop_asiannh = sum(asiannh),
            pop_nhpinh = sum(nhpinh),
            pop_othernh = sum(othernh),
            pop_multnh = sum(multinh))

# add geography ----
blocks <- blocks(state = "51", county = "003", year = 2020)

blocks <- blocks %>% 
  mutate(GEO_ID = paste0("1000000US", GEOID20))

crd_data_geo <- left_join(blocks, crd_data)
crd_data_geo <- st_transform(crd_data_geo, crs = 4326)

# Save
saveRDS(crd_data_geo, file = "data/crd_data_geo.RDS")
# crd_geo <- readRDS("data/crd_data_geo.RDS")
