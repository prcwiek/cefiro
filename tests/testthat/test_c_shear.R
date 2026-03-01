context("c_shear test")

x1_output <- c(0.1554842, 0.5106801, 0.5573740, 0.3089242,
              0.2942975, 0.2613140, 0.2100865, 0.2795167,
              0.3335790, 0.3374990, 0.2944314, 0.2968228,
              0.2280736, 0.2203485, 0.1606154, 0.1458233)

x2_output <- c(3786, 2490, 1839, 2989, 5141, 6656, 8221, 5778,
               5646, 9440, 10413, 12092, 11725, 7844, 4467,4289)

x3_output <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
               "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

ds <- c_mseries(mdata = winddata, date_col = "DateTime",
               ws_col = c("WS125", "WS77", "WS44"), dir_col = c("WD77", "WD125"),
               ws_h = c(125, 77, 44), dir_h = c(77, 125),
               name = "Testing",
               tzone = "CET")

test_that("Check if output is a data frame", {
  expect_equal(is.data.frame(c_shear(ds, ws_signals = c("WS77", "WS125"))), TRUE)
})

test_that("Check correct output", {
  expect_equal(length(c_shear(ds, ws_signals = c("WS77", "WS125"))$shear), 16)
  expect_equal(round(c_shear(ds, ws_signals = c("WS77", "WS125"))$shear, 7), x1_output)
  expect_equal(round(c_shear(ds, ws_signals = c("WS125", "WS77"))$shear, 7), x1_output)
  expect_equal(c_shear(ds, ws_signals = c("WS77", "WS125"))$records, x2_output)
  expect_equal(c_shear(ds, ws_signals = c("WS125", "WS77"))$records, x2_output)
  expect_equal(c_shear(ds, ws_signals = c("WS77", "WS125"), numeric_directions = FALSE)$sector, x3_output)
})

