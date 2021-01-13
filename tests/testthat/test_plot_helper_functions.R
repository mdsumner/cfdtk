context('Plot helper functions')

test_that('get_scale_limits_from_values works for different parameter combinations', {

  sample_values_positive <-
    c(14.62, 14.5, 15.56, 16.72, 15.6, 14.13, 15.66, 15.67, 15.98,
      17, 18.81, 16.87, 17.36, 17.02, 16.45, 15.67, 16.09, 16.81, 18.61,
      17.08, 16.04, 16.57, 16.52, 17.86, 19.65, 19.83, 19.55, 19.58,
      17.37, 17.43, 18.58, 16.94, 16.32, 17.2, 17.58, 16.35, 15.91,
      17.6, 15.33, 15.86, 15.15, 15.67, 15.14, 16.03, 18.47, 15.3,
      14.66, 15.84, 17.89, 15.8, 17.22, 17.55, 17.79, 18.14, 17.76,
      16.67, 16.68, 17.79, 19.49, 17.88, 15.12, 15.89, 16.49, 14.27,
      15.47, 15.92, 15.88, 16.03, 16.74, 17.84, 17.54, 18.14, 15.82,
      15.92, 15.08, 16.08, 16.47, 17.49, 17.29, 17, 17.01, 16.55, 14.81,
      14.22, 15.33, 15.93, 15.97, 16.25, 17.37, 18.04, 20.5, 20.46,
      15.75, 15.04, 16.74, 15.77, 16.52, 18.75, 20.16, 18.34, 16.11,
      16.31, 18.21, 15.2, 15.52, 16.85, 17.49, 15.76, 15.87, 15.85,
      16.97, 16.68, 17.19, 17.85, 15.51, 15.44, 16.27, 17.56, 13.89,
      15.36, 13.98, 15.44, 17.58, 17.46, 15.27, 14.53, 14.86, 14.46,
      14.83, 15.28, 14.81, 13.95, 13.59, 14.36, 14.95, 14.99, 15.36,
      14.01, 14.62, 15.22, 15.82, 16.01, 16.21, 15.86, 12.98, 12.98,
      11.89, 11.53, 13.27, 13.67, 12.71, 12.4, 12.63, 12.94, 14.84,
      14.1, 14.46, 15.4, 16.05, 15.61, 16.46, 14.69, 15.33, 14.63,
      14.2, 12.57, 12.46, 12.36, 13.41, 12.39, 13.42, 12.56, 12.41,
      12.44, 12.55, 13.49, 13.05, 12.51, 12.83, 13.5, 14.71, 12.99,
      13.47, 13.19, 13.62, 14.14, 14.19, 12.46, 11.52, 11.47, 12.37,
      12.73, 11.38, 10.57, 10.64, 10.84, 11.19, 12.2, 13.13, 13.54,
      12.95, 12.72, 14.33, 14.18, 13.58, 13.76, 12.4, 12.85, 13.5,
      13.04, 12.93, 12.77, 12.66, 13.86, 13.18, 10.77, 11.99, 12.45,
      11.83, 12.36, 11.4, 8.43, 8.92, 11.31, 12.48, 13.21, 11.91, 12.69,
      13.18, 14.9, 13.16, 12.66, 11.28, 10.99, 11.63, 11.42, 11.1,
      10.34, 12.12, 13.48, 15.03, 14.59, 10.76, 11.29, 10.99, 11.17,
      11.23, 11.91, 12.68, 14.38, 15.42, 15.55, 13.09, 11.19, 11.18,
      11.67, 12.23, 11.58, 11.09, 10.99, 12.18, 11.85, 12.13, 12.46,
      12.96, 12.89, 12.41, 12.75, 12.4, 12.91, 13.73, 12.74, 14.03,
      15.02, 14.18, 12.66, 12.43, 12.94, 12.05, 12.45, 11.49, 12.09,
      12.38, 12.27, 12.81, 12.98, 12.7, 13, 13.42, 13.98, 14.32, 14.77,
      14.76, 14.34, 13.91, 15.43, 15.42, 14.89, 14.72, 13.91, 14.64,
      14.97, 14.06, 14.87, 16.09, 18.78, 15.76, 14, 14.42, 16.1, 16.69,
      16.6, 15.22, 13.68, 13.5, 14.11, 14.36, 14.4, 14.04, 14.48, 16.18,
      16.31, 13.9, 13.49, 14.58, 14.81, 14.97, 16.04, 16.69, 13.69,
      14.72, 14.82, 16.54, 16.62, 18.48, 16.5, 15.05, 15.83, 16.01,
      17.18, 16.23, 15.33, 15.6, 15.37, 16.14, 16.68, 15.83, 16.11,
      17.57, 15.45, 14.97, 14.23, 15.29, 17.57, 16.43, 15.65, 17.34,
      15.14, 15.87, 16.07, 18.73, 16.59, 18.81, 18.45, 19.24)

  sample_values_negative <-
    c(-20.81, -21.36, -15.71, -18.72, -16.54, -17.34, -17.68, -16.72,
      -17.34, -20.75, -18.38, -17.76, -17.93, -19.04, -13.85, -14.82,
      -16.35, -17.97, -20.51, -15.85, -15.92, -17.04, -18.17, -18.2,
      -18.63, -17.29, -15.21, -16.15, -16.09, -14.63, -18.49, -21.09,
      -24.53, -21.5, -18.05, -18.34, -19.45, -23.07, -23.99, -17.72,
      -16.12, -17.72, -19.13, -16.63, -14.67, -14.63, -15.78, -17.99,
      -22.9, -22.25, -23.22, -17.08, -16.84, -16.02, -15.91, -14.58,
      -17.7, -16.83, -13.19, -13.79, -16.46, -18.46, -18.86, -19.21,
      -19.51, -20.15, -19.86, -21.02, -13.98, -14.94, -13.37, -14.87,
      -15.64, -18.72, -16.8, -14.26, -15.29, -16.4, -17.07, -14.99,
      -15.55, -14.38, -15.75, -15.32, -13.46, -10.94, -12.46, -12.88,
      -12.89, -14.87, -12.49, -11.95, -11.53, -15.18, -14.89, -15.98,
      -19.39, -13.17, -13.68, -13.72, -9.78, -12.47, -12.69, -16.07,
      -14.64, -14.76, -15.84, -15.99, -10.05, -10.2, -12.54, -12.47,
      -13.58, -13.38, -14.53, -14.58, -10.99, -11.84, -13.96, -14.86,
      -14.04, -12.93, -12.94, -13.49, -12.68, -12.5, -12.79, -11.28,
      -11.7, -11, -10.17, -10.09, -11.71, -15.19, -15.69, -12.86, -11.11,
      -11.3, -13.42, -11.36, -10.91, -10.76, -11.11, -11.05, -12.73,
      -12.9, -10.4, -10.36, -11.38, -11.62, -9.07, -11.54, -13.25,
      -10.24, -12.1, -14.56, -13.65, -15.21, -13.42, -13.14, -13.02,
      -12.56, -12.96, -10.19, -8.27, -10.29, -9.73, -9.57, -9.82, -12,
      -11.2, -12.41, -13.97, -12.85, -12.59, -12.27, -9.03, -11.36,
      -12.91, -11.92, -11.57, -10.06, -9.84, -10.74, -11.54, -11.23,
      -10.62, -12.65, -11.96, -12.05, -12.26, -12.91, -11.18, -11.16,
      -11.76, -10.2, -10.1, -9.91, -10.5, -11.2, -10.71, -10.41, -9.72,
      -9.23, -11.69, -12.4, -11.63, -9.51, -10.14, -9.97, -10.75, -10.23,
      -9.32, -11.01, -10.74, -11.45, -10.44, -9.86, -10.09, -11.6,
      -11.72, -11.52, -12.47, -10.82, -11.15, -13.54, -14.05, -9.81,
      -9.28, -8.9, -9.14, -8.16, -7.95, -6.79, -9.23, -11.49, -11.49,
      -11.09, -9.56, -9.46, -10.62, -11.28, -12.34, -11.89, -10.97,
      -11.11, -8.3, -9.47, -12.76, -12.38, -11.44, -11.24, -12.91,
      -12.59, -13.14, -11.88, -13.1, -14, -10.78, -10.88, -11.54, -14.5,
      -8.65, -9.23, -13.66, -17.41, -12.95, -8.78, -10.21, -10.42,
      -14.47, -17.18, -16.83, -13.22, -10.86, -11.24, -10.99, -11.1,
      -9.6, -12.27, -9.55, -10.63, -15.24, -10.7, -12.38, -11.05, -12.09,
      -9.63, -11.5, -10.43, -8.38, -8.69, -10.68, -13.42, -16.39, -15.1,
      -14.09, -13.45, -13.06, -12.97, -11.82, -12.2, -12.01, -10.65,
      -11.92, -11.88, -13.91, -17.31, -15.65, -11.4, -11.3, -13.13,
      -12.27, -8.64, -9.4, -11.63, -12.18, -11.84, -11.84, -12.38,
      -12.53, -9.55, -12.8, -13.22, -13.68, -14.31, -14.44, -15.82,
      -13.44, -13.76, -13.03, -12.26, -12.99, -12.88, -12.16, -12.35,
      -11.46, -12.68, -13.2, -11.1, -11.6, -12.12, -12.25, -10.77,
      -11.95, -13.01, -13.07, -13.57, -12.95, -13.19, -14.09, -13.39,
      -15.86, -15.68, -13.65, -14.73, -14.58, -13.81, -12.34, -12.16,
      -13.79, -14.59, -15.74, -13.35, -13.23)

  sample_values_both <-
    c(-6.54, -5.18, -2.98, 0.14, 6.86, 7.14, 1.62, -0.78, 4.42, 4.74,
      4.58, 7.74, 15.98, 10.9, 4.66, -0.86, -1.06, 3.26, 12.38, 14.1,
      10.58, 12.7, 10.18, 3.54, 4.86, 10.58, 9.66, 21.66, 25.34, 19.14,
      17.26, 15.34, 15.06, 11.02, 15.1, 13.46, 15.98, 24.1, 7.42, 2.74,
      5.34, 3.26, 7.06, 11.38, 13.22, 12.46, 10.94, 15.58, 14.58, 14.14,
      14.98, 14.62, 16.62, 10.7, 7.18, 13.46, 10.38, 16.14, 12.22,
      9.82, 16.1, 14.14, -1.7, 1.38, 4.1, 3.7, 5.14, 11.58, 13.14,
      15.18, 13.94, 14.18, 5.46, -0.1, 5.98, 2.86, 6.3, 14.02, 11.62,
      21.02, 14.38, 8.98, 10.94, 10.62, 7.1, 4.54, 7.5, 9.1, 12.46,
      14.5, 16.5, 16.66, 5.82, 3.02, -4.62, -2.82, 3.62, 0.74, 2.86,
      5.14, 6.98, 4.9, 8.22, 7.82, -2.26, 4.58, 6.3, 5.5, 2.86, 4.22,
      8.46, 8.5, 9.42, 5.82, -2.46, -9.02, -11.78, -9.5, -7.22, -2.1,
      -1.3, 0.86, 2.22, 1.14, -2.3, 1.66, -0.3, 0.34, 1.3, -2.02, -3.66,
      -2.74, -1.5, 0.26, -3.26, -4.82, -3.62, -6.38, 1.42, 3.78, 3.94,
      4.42, 6.42, 6.06, 4.3, -1.18, -5.38, -5.66, -5.98, -5.58, -0.74,
      1.86, 2.14, 3.86, -1.46, -2.18, -5.78, -9.38, -12.22, -17.86,
      -13.5, -10.74, -15.82, -7.86, -7.3, -8.82, -5.38, -4.38, -9.62,
      -2.7, 0.38, -2.26, -7.9, -13.62, -9.1, -8.78, -10.22, -10.38,
      -6.62, -0.42, -1.62, -9.02, -14.3, -9.66, -12.82, -13.78, -14.18,
      -12.1, -8.26, -6.9, -5.58, -6.1, -7.58, -13.78, -14.38, -12.38,
      -12.58, -16.5, -9.26, -3.82, -3.1, -1.78, -12.18, -15.38, -11.42,
      -9.14, -9.66, -10.98, -8.06, -8.02, -10.26, -8.58, -8.14, -8.5,
      -11.54, -11.1, -6.3, -0.74, -12.38, -9.5, -9.3, -6.06, -6.42,
      -7.62, -5.3, -6.34, 1.42, -5.62, -7.46, -6.86, -4.18, -5.1, -11.5,
      -7.58, -3.1, -12.82, -14.22, -10.22, -5.42, -7.66, -13.54, -10.82,
      -11.18, -10.22, -5.06, -4.98, -10.82, -9.62, -5.58, -6.78, -9.82,
      -8.9, -12.58, -2.9, 5.26, -6.82, -10.02, -8.18, -3.82, -6.34,
      -8.18, -6.18, -7.3, -6.1, -3.46, -7.38, -4.46, -10.98, -16.7,
      -8.3, -9.1, -9.9, -2.38, -9.66, -10.18, -11.3, -7.1, -10.42,
      -14.62, -15.86, -14.06, -13.62, -9.74, -5.74, -8.06, -11.42,
      -9.3, -9.78, -7.14, -9.42, -6.1, -4.58, -1.06, -7.1, -6.54, -5.46,
      -7.14, -9.06, -6.34, -3.34, -3.86, -1.14, 3.78, 5.46, -0.54,
      3.14, -4.62, -7.98, -9.1, -1.42, 3.62, 5.5, 9.26, 5.86, 1.94,
      1.5, 3.5, 5.3, 5.3, 2.98, 2.46, 7.7, 14.26, 9.06, 4.5, 0.9, 2.62,
      4.42, 5.94, 8.1, 7.3, 4.82, 0.38, -1.62, -0.5, 3.66, 2.42, 0.26,
      3.18, 4.34, -0.42, -0.94, 5.86, 1.98, 1.42, 4.86, 0.22, 2.38,
      7.82, 19.98, 3.38, 4.66, 1.9, 0.34, 3.5, 6.34, 20.26, 7.82, 2.58,
      6.98, 8.5, 9.78, 11.06, 15.42, 19.58)

  # ggplot(tibble(x = seq_along(sample_values_positive), y = sample_values_positive), aes(x=x,y=y)) + geom_point() + scale_y_continuous(breaks = seq(-50, 50, 1))
  # ggplot(tibble(x = seq_along(sample_values_negative), y = sample_values_negative), aes(x=x,y=y)) + geom_point() + scale_y_continuous(breaks = seq(-50, 50, 1))
  # ggplot(tibble(x = seq_along(sample_values_both), y = sample_values_both), aes(x=x,y=y)) + geom_point() + scale_y_continuous(breaks = seq(-50, 50, 1))


  ####
  context('All params FALSE, changing interval')
  ####

  expect_equal(
    get_scale_limits_from_values(values = sample_values_positive,
                                 interval = 0.1,
                                 snap_lower_to_zero = FALSE,
                                 snap_upper_to_zero = FALSE,
                                 symmetrical = FALSE),
    c(8.4, 20.5))

  expect_equal(
    get_scale_limits_from_values(values = sample_values_positive,
                                 interval = 1,
                                 snap_lower_to_zero = FALSE,
                                 snap_upper_to_zero = FALSE,
                                 symmetrical = FALSE),
    c(8, 21))

  expect_equal(
    get_scale_limits_from_values(values = sample_values_positive,
                                 interval = 5,
                                 snap_lower_to_zero = FALSE,
                                 snap_upper_to_zero = FALSE,
                                 symmetrical = FALSE),
    c(5, 25))



  ####
  ####
  context('snap_lower_to_zero')
  ####
  ####

  # affects positive values
  expect_equal(
    get_scale_limits_from_values(values = sample_values_positive,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = FALSE,
                                 symmetrical = FALSE),
    c(0, 21))

  # all negative values unaffected
  expect_equal(
    get_scale_limits_from_values(values = sample_values_negative,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = FALSE,
                                 symmetrical = FALSE),
    c(-25, -6))

  # scale with both positive and negative values unaffected
  expect_equal(
    get_scale_limits_from_values(values = sample_values_both,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = FALSE,
                                 symmetrical = FALSE),
    c(-18, 26))



  ####
  ####
  context('snap_upper_to_zero')
  ####
  ####

  # all positive values unaffected
  expect_equal(
    get_scale_limits_from_values(values = sample_values_positive,
                                 interval = 1,
                                 snap_lower_to_zero = FALSE,
                                 snap_upper_to_zero = TRUE,
                                 symmetrical = FALSE),
    c(8, 21))

  # affects all negative values
  expect_equal(
    get_scale_limits_from_values(values = sample_values_negative,
                                 interval = 1,
                                 snap_lower_to_zero = FALSE,
                                 snap_upper_to_zero = TRUE,
                                 symmetrical = FALSE),
    c(-25, 0))

  # scale with both positive and negative values unaffected
  expect_equal(
    get_scale_limits_from_values(values = sample_values_both,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = FALSE,
                                 symmetrical = FALSE),
    c(-18, 26))


  ####
  ####
  context('snap_lower_to_zero and snap_upper_to_zero both true')
  ####
  ####

  # affects all positive values
  expect_equal(
    get_scale_limits_from_values(values = sample_values_positive,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = TRUE,
                                 symmetrical = FALSE),
    c(0, 21))

  # affects all negative values
  expect_equal(
    get_scale_limits_from_values(values = sample_values_negative,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = TRUE,
                                 symmetrical = FALSE),
    c(-25, 0))

  # scale with both positive and negative values unaffected
  expect_equal(
    get_scale_limits_from_values(values = sample_values_both,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = TRUE,
                                 symmetrical = FALSE),
    c(-18, 26))


  ####
  ####
  context('symmetrical')
  ####
  ####

  # affects all positive values
  expect_equal(
    get_scale_limits_from_values(values = sample_values_positive,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = TRUE,
                                 symmetrical = TRUE),
    c(-21, 21))

  # affects all negative values
  expect_equal(
    get_scale_limits_from_values(values = sample_values_negative,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = TRUE,
                                 symmetrical = TRUE),
    c(-25, 25))

  # affects scale with both positive and negative values
  expect_equal(
    get_scale_limits_from_values(values = sample_values_both,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = TRUE,
                                 symmetrical = TRUE),
    c(-26, 26))



  ####
  ####
  context('when symmetrical chosen, snaps have no affect')
  ####
  ####

  # affects scale with both positive and negative values
  expect_equal(
    get_scale_limits_from_values(values = sample_values_both,
                                 interval = 1,
                                 snap_lower_to_zero = FALSE,
                                 snap_upper_to_zero = TRUE,
                                 symmetrical = TRUE),
    c(-26, 26))

  # affects scale with both positive and negative values
  expect_equal(
    get_scale_limits_from_values(values = sample_values_both,
                                 interval = 1,
                                 snap_lower_to_zero = TRUE,
                                 snap_upper_to_zero = FALSE,
                                 symmetrical = TRUE),
    c(-26, 26))

  # affects scale with both positive and negative values
  expect_equal(
    get_scale_limits_from_values(values = sample_values_both,
                                 interval = 1,
                                 snap_lower_to_zero = FALSE,
                                 snap_upper_to_zero = FALSE,
                                 symmetrical = TRUE),
    c(-26, 26))
})

