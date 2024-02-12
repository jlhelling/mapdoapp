test_that("data_get_dgo_in_region works", {
  url <- params_url_remonterletemps(lng=6.869433, lat=45.923690, zoom = 12)
  expect_true(inherits(url, "character"),
              "character url loaded")
})
