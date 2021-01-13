# common data

dates <- seq(from = as.Date("1961-07-01"), length.out = 9, by = "days")
values <- seq(from = 1.1, length.out = 9, by = 1.1)



# date_vector_exceeds_threshold

test_that('date_vector_exceeds_threshold works for expected data', {
  expect_equal(date_vector_exceeds_threshold(dates, values, threshold = 5), dates[5])
})

test_that('date_vector_exceeds_threshold errors when dates and values are different lengths', {
  expect_error(date_vector_exceeds_threshold(dates, head(values, -1), threshold = 5), "Lengths.*do not match")
})

test_that('date_vector_exceeds_threshold returns correct date when first value exceeds threshold', {
  expect_equal(date_vector_exceeds_threshold(dates, values, threshold = 0), dates[1])
})

test_that('date_vector_exceeds_threshold returns NA when threshold is not exceeded', {
  expect_equal(is.na(date_vector_exceeds_threshold(dates, values, threshold = 50)), TRUE)
})

test_that('date_vector_exceeds_threshold treats exceeds as greater than (not equal to or greater than)', {
  expect_equal(date_vector_exceeds_threshold(dates, values, threshold = 2.2), dates[3])
})



# last_date_vector_under_threshold

test_that('last_date_vector_under_threshold works for expected data', {
  expect_equal(last_date_vector_under_threshold(dates, values, threshold = 5), as.Date("1961-07-04"))
})

test_that('last_date_vector_under_threshold errors when dates and values are different lengths', {
  expect_error(last_date_vector_under_threshold(dates, head(values, -1), threshold = 5), "Lengths.*do not match")
})

test_that('last_date_vector_under_threshold returns last date when all values are under threshold', {
  expect_equal(last_date_vector_under_threshold(dates, values, threshold = 50), tail(dates, 1))
})

test_that('last_date_vector_under_threshold returns NA when value never under threshold', {
  expect_equal(is.na(last_date_vector_under_threshold(dates, values, threshold = 0)), TRUE)
})

test_that('last_date_vector_under_threshold treats under as less than (not less than or equal to)', {
  expect_equal(last_date_vector_under_threshold(dates, values, threshold = 3.3), dates[2])
})


# Austral_season_month

test_that('Austral_season_month works for all dates in a standard year', {
  all_dates <- seq.Date(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = "day")
  expected_values <- c(rep( 7, 31), # Jan
                       rep( 8, 28), # Feb
                       rep( 9, 31), # Mar
                       rep(10, 30), # Apr
                       rep(11, 31), # May
                       rep(12, 30), # Jun
                       rep( 1, 31), # Jul
                       rep( 2, 31), # Aug
                       rep( 3, 30), # Sep
                       rep( 4, 31), # Oct
                       rep( 5, 30), # Nov
                       rep( 6, 31)) # Dec
  expect_equal(Austral_season_month(all_dates), expected_values)
})


test_that('Austral_season_month works for all dates in a leap year', {
  all_dates_ly <- seq.Date(from = as.Date("2016-01-01"), to = as.Date("2016-12-31"), by = "day")
  expected_values <- c(rep( 7, 31), # Jan
                       rep( 8, 29), # Feb
                       rep( 9, 31), # Mar
                       rep(10, 30), # Apr
                       rep(11, 31), # May
                       rep(12, 30), # Jun
                       rep( 1, 31), # Jul
                       rep( 2, 31), # Aug
                       rep( 3, 30), # Sep
                       rep( 4, 31), # Oct
                       rep( 5, 30), # Nov
                       rep( 6, 31)) # Dec
  expect_equal(Austral_season_month(all_dates_ly), expected_values)
})


test_that("is_A_to_B month check functions are correct for all months", {
  mapped <- tibble(calendar_month       = c(   1L,    2L,    3L,    4L,    5L,    6L,    7L,    8L,    9L,   10L,   11L,   12L),
                   Austral_season_month = c(   7L,    8L,    9L,   10L,   11L,   12L,    1L,    2L,    3L,    4L,    5L,    6L),
                   is_Mar_to_Apr        = c(FALSE, FALSE,  TRUE,  TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                   is_Sep_to_Apr        = c( TRUE,  TRUE,  TRUE,  TRUE, FALSE, FALSE, FALSE, FALSE,  TRUE,  TRUE,  TRUE,  TRUE),
                   is_Oct_to_Apr        = c( TRUE,  TRUE,  TRUE,  TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE,  TRUE,  TRUE))

  for (i in 1:nrow(mapped)) {
    expect_equal(is_Mar_to_Apr(Austral_season_month = mapped[i,]$Austral_season_month), mapped[i,]$is_Mar_to_Apr)
    expect_equal(is_Sep_to_Apr(Austral_season_month = mapped[i,]$Austral_season_month), mapped[i,]$is_Sep_to_Apr)
    expect_equal(is_Oct_to_Apr(Austral_season_month = mapped[i,]$Austral_season_month), mapped[i,]$is_Oct_to_Apr)
  }
})
