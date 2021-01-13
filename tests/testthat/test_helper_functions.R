

test_that(
  "seconds in month works for non leap years",
  {
    N_SECONDS_IN_DAY <- 86400L  # 24 * 60 * 60
    x <- seq(lubridate::ymd("2011-01-01"), lubridate::ymd("2011-12-01"), "1 month")
    expected <- c(
      Jan = 31L, Feb = 28L, Mar = 31L,
      Apr = 30L, May = 31L, Jun = 30L,
      Jul = 31L, Aug = 31L, Sep = 30L,
      Oct = 31L, Nov = 30L, Dec = 31L
    ) * N_SECONDS_IN_DAY
    expect_that(seconds_in_month(x), equals(expected))
  }
)

test_that(
  "seconds in month works for leap years",
  {
    N_SECONDS_IN_DAY <- 86400L  # 24 * 60 * 60
    x <- seq(lubridate::ymd("2012-01-01"), lubridate::ymd("2012-12-01"), "1 month")
    expected <- c(
      Jan = 31L, Feb = 29L, Mar = 31L,
      Apr = 30L, May = 31L, Jun = 30L,
      Jul = 31L, Aug = 31L, Sep = 30L,
      Oct = 31L, Nov = 30L, Dec = 31L
    ) * N_SECONDS_IN_DAY
    expect_that(seconds_in_month(x), equals(expected))
  }
)

test_that("seconds per month works for non leap years", {
  x <- seq(lubridate::ymd("2011-01-01"), lubridate::ymd("2011-12-01"), "1 month")
  N_SECONDS_IN_DAY <- 86400L  # 24 * 60 * 60
  expected <- c(
    Jan = 31L, Feb = 28L, Mar = 31L,
    Apr = 30L, May = 31L, Jun = 30L,
    Jul = 31L, Aug = 31L, Sep = 30L,
    Oct = 31L, Nov = 30L, Dec = 31L
  ) * N_SECONDS_IN_DAY
  expect_that(seconds_per_month(lubridate::year(x), lubridate::month(x)), equals(expected))
})

test_that("seconds per month works for leap years", {
  x <- seq(lubridate::ymd("2012-01-01"), lubridate::ymd("2012-12-01"), "1 month")
  N_SECONDS_IN_DAY <- 86400L  # 24 * 60 * 60
  expected <- c(
    Jan = 31L, Feb = 29L, Mar = 31L,
    Apr = 30L, May = 31L, Jun = 30L,
    Jul = 31L, Aug = 31L, Sep = 30L,
    Oct = 31L, Nov = 30L, Dec = 31L
  ) * N_SECONDS_IN_DAY
  expect_that(seconds_per_month(lubridate::year(x), lubridate::month(x)), equals(expected))
})



test_that('is_leap_year works for different years', {
  expect_equal(is_leap_year(1900), FALSE)
  expect_equal(is_leap_year(2000), TRUE)
  expect_equal(is_leap_year(2001), FALSE)
  expect_equal(is_leap_year(2004), TRUE)
  expect_equal(is_leap_year(2100), FALSE)
})


test_that('yday_ly works for different years', {

  date_values <- seq.Date(from = as.Date('2004-01-01'),
                          to = as.Date('2008-01-01'),
                          by = '1 day')

  test_df <- data.frame(date = date_values,
                        doy = yday_ly(date_values))

  ## leap day is the same as previous day
  expect_equal(
    test_df[test_df$date == '2004-02-28',]$doy,
    test_df[test_df$date == '2004-02-29',]$doy)

  ## last day of year is the same for leap year and normal year
  expect_equal(
    test_df[test_df$date == '2004-12-31',]$doy,
    test_df[test_df$date == '2005-12-31',]$doy)
})


test_that('is_zero_range_vector works for different vectors', {

  # vector of length 1
  expect_equal(is_zero_range_vector(c(123456)), TRUE)

  expect_equal(is_zero_range_vector(c(1, 2, 3)), FALSE)
  expect_equal(is_zero_range_vector(c(3, 3, 3)), TRUE)

  expect_equal(is_zero_range_vector(c(3.14, 3.14, 3.14)), TRUE)
  expect_equal(is_zero_range_vector(c(3.14, 3.14, 3.1400001)), FALSE)
})
