## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(tidyr)
library(sf)
library(geodimension)

names(layer_us_city)

check_key(layer_us_city, key = c("name", "statefp"))

check_key(layer_us_city, key = c("geoid"))

## -----------------------------------------------------------------------------
get_geometry(layer_us_city)

city <-
  geolevel(name = "city",
           layer = layer_us_city,
           key = c("geoid"))

## ----message=FALSE, warning=FALSE---------------------------------------------
get_geometry(layer_us_county)

county <-
  geolevel(name = "county",
           layer = layer_us_county,
           key = c("geoid")) %>%
  complete_point_geometry()

## ----message=FALSE, warning=FALSE---------------------------------------------
us_state_point <-
  coordinates_to_geometry(layer_us_state,
                          lon_lat = c("intptlon", "intptlat"))

state <-
  geolevel(name = "state",
           layer = layer_us_state,
           key = c("statefp")) %>%
  add_geometry(layer = us_state_point)

region <-
  geolevel(name = "region",
           layer = layer_us_region,
           key = c("regionce"))

division <-
  geolevel(name = "division",
           layer = layer_us_division,
           key = c("divisionce"))

nation <-
  geolevel(name = "nation",
           layer = layer_us_nation,
           key = c("name"))

## -----------------------------------------------------------------------------
gd <-
  geodimension(name = "gd_us",
               level = region) %>%
  add_level(division) %>%
  add_level(state) %>%
  add_level(nation) %>%
  add_level(city) %>%
  add_level(county)

## ----message=FALSE, warning=FALSE---------------------------------------------
gd <- gd %>%
  relate_levels(lower_level_name = "state",
                lower_level_attributes = c("division"),
                upper_level_name = "division") %>%
  relate_levels(lower_level_name = "division",
                upper_level_name = "region",
                by_geography = TRUE) %>%
  relate_levels(lower_level_name = "region",
                upper_level_name = "nation")

## -----------------------------------------------------------------------------
gd <- gd %>%
  relate_levels(lower_level_name = "city",
                lower_level_attributes = c("statefp"),
                upper_level_name = "state") %>%
  relate_levels(lower_level_name = "county",
                lower_level_attributes = c("statefp"),
                upper_level_name = "state")

## ----message=FALSE, warning=FALSE---------------------------------------------
gd <- gd %>%
  relate_levels(lower_level_name = "city",
                upper_level_name = "county",
                by_geography = TRUE)

## -----------------------------------------------------------------------------
nrow(
  gd %>% get_unrelated_instances(lower_level_name = "city",
                                 upper_level_name = "county")
)

## -----------------------------------------------------------------------------
nrow(
  gd %>% get_unrelated_instances(lower_level_name = "state",
                                 upper_level_name = "division")
)

## ---- results = "asis", echo = FALSE------------------------------------------
t <-gd %>% get_unrelated_instances(lower_level_name = "state",
                                 upper_level_name = "division")
pander::pandoc.table(t, split.table = Inf)

## -----------------------------------------------------------------------------
gd <- gd %>%
  relate_levels(lower_level_name = "state",
                upper_level_name = "nation")

## -----------------------------------------------------------------------------
gd %>%
  get_level_names()

## -----------------------------------------------------------------------------
gds <- gd %>%
  select_levels(level_names = c("state", "division", "region", "nation"))

gds %>%
  get_level_names()

## -----------------------------------------------------------------------------
ld <- gd %>%
  get_level_data(level_name = "division")
names(ld)

ld <- gd %>%
  get_level_data(level_name = "division",
                 inherited = TRUE)
names(ld)


## -----------------------------------------------------------------------------
gd %>%
  get_higher_level_names(level_name = "state",
                         indirect_levels = TRUE)

ld <- gd %>%
  get_level_data(level_name = "state",
                 inherited = TRUE)
names(ld)


## ---- fig.width=7, fig.height=2.5---------------------------------------------
gd %>%
  get_level_geometries(level_name = "state")

ll <- gd %>%
  get_level_layer(level_name = "state",
                  geometry = "polygon",
                  only_key = TRUE)

plot(ll)

