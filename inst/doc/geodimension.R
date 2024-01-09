## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(geodimension)

layer_us_place <- gd_us |>
  get_level_layer("place")

layer_us_county <-
  dplyr::inner_join(
    get_level_data_geo(gd_us, "county"),
    get_level_layer(gd_us, "county"),
    by = c("geoid", "statefp", "name", "type")
  ) |>
  sf::st_as_sf()

layer_us_state <-
  dplyr::inner_join(
    get_level_data_geo(gd_us, "state"),
    get_level_layer(gd_us, "state"),
    by = c("statefp", "division", "region", "stusps", "name")
  ) |>
  sf::st_as_sf()

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(geodimension::us_division, split.table = Inf)

## -----------------------------------------------------------------------------
names(layer_us_place)

check_key(layer_us_place, key = c("name", "statefp"))

check_key(layer_us_place, key = "geoid")

## -----------------------------------------------------------------------------
get_geometry(layer_us_place)

place <-
  geolevel(name = "place",
           layer = layer_us_place,
           key = "geoid")

## -----------------------------------------------------------------------------
check_key(layer_us_county, key = c("name", "statefp"))

check_key(layer_us_county, key = "geoid")

get_geometry(layer_us_county)

county <-
  geolevel(name = "county",
           layer = layer_us_county,
           key = c("geoid")) |>
  add_geometry(coordinates_to_geometry(layer_us_county))

## -----------------------------------------------------------------------------
us_state_point <-
  coordinates_to_geometry(layer_us_state,
                          lon_lat = c("intptlon", "intptlat"))

state <-
  geolevel(name = "state",
           layer = layer_us_state,
           key = "statefp") |>
  add_geometry(layer = us_state_point)

## -----------------------------------------------------------------------------
division <-
  geolevel(
    name = "division",
    layer = us_division,
    attributes = c("country", "region_code", "division_name"),
    key = "division_code"
  ) |>
  add_geometry(layer = layer_us_state,
               layer_key = "division") |>
  complete_point_geometry()

region <-
  geolevel(
    name = "region",
    layer = us_division,
    attributes = c("country", "region_name"),
    key = "region_code"
  ) |>
  add_geometry(layer = layer_us_state,
               layer_key = "region") |>
  complete_point_geometry()

## -----------------------------------------------------------------------------
country <-
  geolevel(
    name = "country",
    layer = get_level_layer(region),
    attributes = "country",
    key = "country"
  ) |>
  complete_point_geometry()

## -----------------------------------------------------------------------------
gd <-
  geodimension(name = "gd_us",
               level = region,
               snake_case = TRUE) |>
  add_level(division) |>
  add_level(state) |>
  add_level(country) |>
  add_level(place) |>
  add_level(county)

## -----------------------------------------------------------------------------
gd <- gd |>
  relate_levels(
    lower_level_name = "state",
    lower_level_attributes = "division",
    upper_level_name = "division"
  ) |>
  relate_levels(
    lower_level_name = "division",
    lower_level_attributes = "region_code",
    upper_level_name = "region"
  ) |>
  relate_levels(
    lower_level_name = "region",
    lower_level_attributes = "country",
    upper_level_name = "country"
  )

## -----------------------------------------------------------------------------
gd <- gd |>
  relate_levels(
    lower_level_name = "place",
    lower_level_attributes = "county_geoid",
    upper_level_name = "county"
  ) |>
  relate_levels(
    lower_level_name = "county",
    lower_level_attributes = "statefp",
    upper_level_name = "state"
  )

## -----------------------------------------------------------------------------
gd_2 <- gd |>
  relate_levels(lower_level_name = "place",
                upper_level_name = "county",
                by_geography = TRUE)

## -----------------------------------------------------------------------------
nrow(get_unrelated_instances(gd_2,
                             lower_level_name = "place",
                             upper_level_name = "county"))

## -----------------------------------------------------------------------------
gd |>
  get_level_names()

## -----------------------------------------------------------------------------
gds <- gd |>
  select_levels(level_names = c("state", "division", "region", "country"))

gds |>
  get_level_names()

## -----------------------------------------------------------------------------
ld <- gd |>
  get_level_data(level_name = "state")
names(ld)

ld <- gd |>
  get_level_data(level_name = "state",
                 inherited = TRUE)
names(ld)


## -----------------------------------------------------------------------------
gd |>
  get_higher_level_names(level_name = "state",
                         indirect_levels = TRUE)

## -----------------------------------------------------------------------------
ld_place <- gd |>
  get_level_data(level_name = "place")
nrow(ld_place)

ld_place <- ld_place |>
  dplyr::filter(type == "city")

gd <- gd |>
  set_level_data(level_name = "place",
                 data = ld_place)
ld_place_2 <- gd |>
  get_level_data(level_name = "place")
nrow(ld_place_2)

## -----------------------------------------------------------------------------
gd |>
  get_level_geometries(level_name = "division")

ll <- gd |>
  get_level_layer(level_name = "division",
                  geometry = "polygon",
                  only_key = TRUE)

plot(sf::st_shift_longitude(ll))

## ----results = "asis"---------------------------------------------------------
ld_geo <- gd |>
  get_level_data_geo(level_name = "division")

pander::pandoc.table(ld_geo, split.table = Inf)

