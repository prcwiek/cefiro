context("c_scale_offset test")

ds <- c_mseries(mdata = winddata, date_col = "DateTime",
                ws_col = c("WS125", "WS77"), dir_col = c("WD125"),
                ws_h = c(125, 77), dir_h = c(125),
                name = "Testing",
                tzone = "CET")


test_that("Check if output is a c_mseries", {
  expect_equal(is_c_mseries(c_scale_offset(ds, "WS125", scale = 1.07, offset = 0)), TRUE)
})

test_that("Check correct output", {
  expect_equal(round(mean(c_scale_offset(ds, "WS125", scale = 1.07, offset = 0)), 6), 8.593278)
})
