context('Help functions')

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
                        doy = yday_static(date_values))

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

  expect_equal(is_zero_range_vector(c(1, 2, 3)), FALSE)
  expect_equal(is_zero_range_vector(c(3, 3, 3)), TRUE)

  expect_equal(is_zero_range_vector(c(3.14, 3.14, 3.14)), TRUE)
  expect_equal(is_zero_range_vector(c(3.14, 3.14, 3.1400001)), FALSE)
})
