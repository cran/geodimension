context("test add_geometry")

library(sf) # It has to be included even if it is not used directly.

test_that("add_geometry works", {
  us_state_point <-
    coordinates_to_geometry(layer_us_state,
                            lon_lat = c("intptlon", "intptlat"))

  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = c("geoid")) %>%
    add_geometry(layer = us_state_point)

  expect_equal(names(state$geometry$polygon),
               c("state_key", "Shape"))

  expect_equal(names(state$geometry$point),
               c("state_key", "geometry"))
})
