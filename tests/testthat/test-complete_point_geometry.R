context("test complete_point_geometry")

library(sf) # It has to be included even if it is not used directly.

test_that("complete_point_geometry works", {
  state <-
    geolevel(name = "state",
             layer = layer_us_state,
             key = c("geoid")) %>%
    complete_point_geometry()

  expect_equal(names(state$geometry$polygon),
               c("state_key", "Shape"))

  expect_equal(names(state$geometry$point),
               c("state_key", "Shape"))
})
