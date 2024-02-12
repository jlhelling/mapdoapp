test_that("sld_get_quantile_metric works", {
  data <- sld_get_quantile_metric(selected_region_id = 11,
                                  selected_metric = "active_channel_width")
  expect_true(inherits(data, "numeric"),
              "numeric data loaded")
  expect_true(length(data) > 0,
              info = "Data is not empty")
})
