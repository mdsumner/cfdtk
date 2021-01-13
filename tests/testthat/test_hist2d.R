# library(testthat)



test_that('hist2d categorises data correctly', {

  test_data <- iris %>% dplyr::select(x = Sepal.Length, y = Sepal.Width)

  x_binwidth <- 5
  x_breaks <- seq(from = plyr::round_any(min(test_data$x), x_binwidth, floor),
                  to = plyr::round_any(max(test_data$x), x_binwidth, ceiling),
                  by = x_binwidth)

  y_binwidth <- 3
  y_breaks <- seq(from = plyr::round_any(min(test_data$y), y_binwidth, floor),
                  to = plyr::round_any(max(test_data$y), y_binwidth, ceiling),
                  by = y_binwidth)


  h <- hist2d(xy = test_data,
              x_breaks = x_breaks,
              y_breaks = y_breaks)

  x_counts <- h$freq2D %>% colSums
  y_counts <- h$freq2D %>% rowSums

  # calulate histogram values
  hx <- hist(x = test_data$x, breaks = x_breaks, plot = FALSE)
  hy <- hist(x = test_data$y, breaks = y_breaks, plot = FALSE)
  # plot(hx)
  # plot(hy)

  expect_equal(x_counts, hx$counts)

  expect_equal(y_counts, hy$counts)
})



test_that('hist2d errors when data exists outside of bounds', {

  test_data_bounds <- tibble(x = 1:4, y = 1:4) %>% as.matrix

  expect_error(hist2d(xy = test_data_bounds,
                      x_breaks = c(   2.5, 5),
                      y_breaks = c(0, 2.5, 5)),
               "Min.*outside x_breaks")

  expect_error(hist2d(xy = test_data_bounds,
                      x_breaks = c(0, 2.5    ),
                      y_breaks = c(0, 2.5, 5)),
               "Max.*outside x_breaks")

  expect_error(hist2d(xy = test_data_bounds,
                      x_breaks = c(0, 2.5, 5),
                      y_breaks = c(   2.5, 5)),
               "Min.*outside y_breaks")

  expect_error(hist2d(xy = test_data_bounds,
                      x_breaks = c(0, 2.5, 5),
                      y_breaks = c(0, 2.5   )),
               "Max.*outside y_breaks")

  expect_error(hist2d(xy = test_data_bounds,
                      x_breaks = c(   2.5, 5),
                      y_breaks = c(0, 2.5   )),
               "Min.*outside x_breaks.*Max.*outside y_breaks")
})


