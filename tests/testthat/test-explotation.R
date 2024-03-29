test_that("get_level_geometries()", {
  expect_equal(gd_us |>
                 get_level_geometries(level_name = "state"),
               c("point", "polygon"))

})

test_that("get_level_names()", {
  expect_equal(
    gd_us |>
      get_level_names(),
    c("country", "county", "division", "place", "region", "state")
  )

})


test_that("get_level_data()", {
  ld_1 <- gd_us |>
    get_level_data(level_name = "county")

  ld_2 <- gd_us |>
    get_level_data(level_name = "county",
                   inherited = TRUE)

  expect_equal(nrow(ld_1),
               nrow(ld_2))

  expect_equal(names(ld_1),
               c("geoid", "statefp", "name", "type"))

  expect_equal(
    names(ld_2),
    c(
      "county_geoid",
      "county_statefp",
      "county_name",
      "county_type",
      "state_division",
      "state_region",
      "state_stusps",
      "state_name",
      "division_country",
      "division_region_code",
      "division_name",
      "region_country",
      "region_name"
    )
  )

})


test_that("get_level_layer()", {
  ll_1 <- gd_us |>
    get_level_layer(level_name = "county",
                    geometry = "polygon")

  ll_2 <- gd_us |>
    get_level_layer(level_name = "county",
                    geometry = "polygon",
                    inherited = TRUE)

  expect_equal(nrow(ll_1),
               nrow(ll_2))

  expect_equal(names(ll_1),
               c("geoid", "statefp", "name", "type", "geom"))

  expect_equal(
    names(ll_2),
    c(
      "county_geoid",
      "county_statefp",
      "county_name",
      "county_type",
      "state_division",
      "state_region",
      "state_stusps",
      "state_name",
      "division_country",
      "division_region_code",
      "division_name",
      "region_country",
      "region_name",
      "geom"
    )
  )

})



test_that("get_level_data_geo()", {
  ld_1 <- gd_us |>
    get_level_data_geo(level_name = "county")

  ld_2 <- gd_us |>
    get_level_data_geo(level_name = "county",
                   inherited = TRUE)

  expect_equal(nrow(ld_1),
               nrow(ld_2))

  expect_equal(names(ld_1),
               c("geoid", "statefp", "name", "type", "intptlon", "intptlat"))

  expect_equal(
    names(ld_2),
    c(
      "county_geoid",
      "county_statefp",
      "county_name",
      "county_type",
      "state_division",
      "state_region",
      "state_stusps",
      "state_name",
      "division_country",
      "division_region_code",
      "division_name",
      "region_country",
      "region_name",
      "intptlon",
      "intptlat"
    )
  )

})


test_that("get_level_data_geo()", {
  ld <- gd_us |>
    get_level_data(level_name = "county",
                   inherited = TRUE)

  gd_us_2 <- gd_us |>
    set_level_data(level_name = "county",
                   data = ld)

  ld_2 <- gd_us_2 |>
    get_level_data(level_name = "county")


  expect_equal(ld,
               ld_2)

  expect_equal({
    res <- tryCatch({
      ld_1 <- gd_us |>
        get_level_data(level_name = "county")
      ld_1$statefp <- NULL
      gd_us <- gd_us |>
        set_level_data(level_name = "county",
                       data = ld_1)
    },
    error = function(e)
      1
    )
    res
  },
  1)

  expect_equal({
    res <- tryCatch({
      ld_1 <- gd_us |>
        get_level_data(level_name = "county")
      ld_1$geoid <- NULL
      gd_us <- gd_us |>
        set_level_data(level_name = "county",
                       data = ld_1)
    },
    error = function(e)
      1
    )
    res
  },
  1)


})
