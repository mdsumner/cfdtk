test_that('logarray works', {
  expect_equal(logarray(letters), "a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z")  #
})


test_that('logpipe displays message', {
  expect_output(object = mtcars %>% logpipe('test log pipe') %>% group_by(cyl) %>% tally(),
                regexp = "test log pipe")
})


test_that('logpipe passes original value through pipe', {
  expect_equal(object = mtcars %>% logpipe('test log pipe') %>% group_by(cyl) %>% tally(),
               expected = mtcars %>% group_by(cyl) %>% tally())
})


test_that('logtime_init returns a list with an element named init of class POSIXct', {
  time_log <- logtime_init()
  expect_named(time_log, 'init')
  expect_s3_class(time_log$init, 'POSIXct')
})


test_that('logtime adds _start and _end entries for boolean approach', {
  time_log <- logtime_init()

  time_log <- logtime(time_log, 'test', .start = TRUE)
  expect_named(time_log, c('init', 'test_start'))
  expect_s3_class(time_log[['test_start']], 'POSIXct')

  time_log <- logtime(time_log, 'test', .end = TRUE)
  expect_named(time_log, c('init', 'test_start', 'test_end'))
  expect_s3_class(time_log[['test_end']], 'POSIXct')
})


test_that('logtime adds _start and _end entries for log_type approach', {
  time_log <- logtime_init()

  time_log <- logtime(time_log, 'test', log_type = 'start')
  expect_named(time_log, c('init', 'test_start'))
  expect_s3_class(time_log[['test_start']], 'POSIXct')

  time_log <- logtime(time_log, 'test', log_type = 'end')
  expect_named(time_log, c('init', 'test_start', 'test_end'))
  expect_s3_class(time_log[['test_end']], 'POSIXct')
})


test_that('logtime errors when both or neither .start and .end are provided', {
  time_log <- logtime_init()
  expect_error(logtime(time_log, 'test', .start = TRUE, .end = TRUE), 'one and only one of .start and .end must be TRUE')
  expect_error(logtime(time_log, 'test'), 'one and only one of .start and .end must be TRUE')
})


test_that('logtime errors when log_type is neither start or end', {
  time_log <- logtime_init()
  expect_error(logtime(time_log, 'test', log_type = 'foobar'), "log_type must be one of the following:")
})


test_that('logtime_print_diff returns a human readable diff', {
  # fake a logtime
  time_log <- logtime_init()
  time_log[['test_start']] <- lubridate::make_datetime(2000, 12, 25, 6,  0,  0, tz = 'Australia/Hobart')
  time_log[['test_end']]   <- lubridate::make_datetime(2000, 12, 25, 23, 2, 37, tz = 'Australia/Hobart')

  d <- logtime_print_diff(time_log, 'test')
  expect_type(d, 'character')
  expect_equal(d, "17.04 hours")
})


test_that('logtime_secs_diff returns a diff in seconds (for short / same day)', {
  # fake a logtime
  time_log <- logtime_init()
  time_log[['test_start']] <- lubridate::make_datetime(2000, 12, 25, 6, 0, 0, tz = 'Australia/Hobart')
  time_log[['test_end']]   <- lubridate::make_datetime(2000, 12, 25, 6, 0, 37, tz = 'Australia/Hobart')

  d <- logtime_secs_diff(time_log, 'test')
  expect_s3_class(d, 'difftime')
  expect_equal(as.integer(d), 37)
})


test_that('logtime_secs_diff returns a diff in seconds (for long duration)', {
  # fake a logtime
  time_log <- logtime_init()
  time_log[['test_start']] <- lubridate::make_datetime(2000, 12, 25, 6, 6, 40, tz = 'Australia/Hobart')
  time_log[['test_end']]   <- lubridate::make_datetime(2000, 12, 27, 3, 4, 37, tz = 'Australia/Hobart')

  d <- logtime_secs_diff(time_log, 'test')
  expect_s3_class(d, 'difftime')
  expect_equal(as.integer(d), 161877)
})


test_that('logtime_diff logs a warning if end not present', {
  # fake a logtime
  time_log <- logtime_init()
  time_log[['test_start']] <- lubridate::make_datetime(2000, 12, 25, 6,  0,  0, tz = 'Australia/Hobart')
  expect_output(cfdtk:::logtime_diff(time_log, 'test'), 'WARNING::end logtime for test is NULL')
})


test_that('logtime_diff logs a warning if start not present', {
  # fake a logtime
  time_log <- logtime_init()
  time_log[['test_end']] <- lubridate::make_datetime(2000, 12, 25, 6,  0,  0, tz = 'Australia/Hobart')
  expect_output(cfdtk:::logtime_diff(time_log, 'test'), 'WARNING::start logtime for test is NULL')
})


test_that('progress_of_loop correctly identifies position in loop', {
  expect_equal(progress_of_loop('i', letters), "(9/26) i")  # i is 9th letter
  expect_equal(progress_of_loop('May', month.name, FUN = str_to_upper), "(5/12) MAY")  # i is 9th letter
})



let_array <- function() {
  sapply(1:27, function(x) glue("/path/to/files/data_{x}.ext") %>% toString, simplify = TRUE)
}

test_that('loginfo_progress logs entry', {
  array <- let_array()
  expect_output(loginfo_progress("/path/to/files/data_12.ext", array), "INFO::\\(12\\/27\\) \\/path\\/to\\/files\\/data\\_12\\.ext")
})


test_that('loginfo_progress logs every nth entry', {
  array <- let_array()
  expect_output(loginfo_progress("/path/to/files/data_12.ext", array, n = 5), NA)
  expect_output(loginfo_progress("/path/to/files/data_15.ext", array, n = 5), "INFO::\\(15\\/27\\) \\/path\\/to\\/files\\/data\\_15\\.ext")
})

test_that('loginfo_progress always logs first and last entry', {
  array <- let_array()
  expect_output(loginfo_progress("/path/to/files/data_1.ext", array, n = 5), "INFO::\\(1\\/27\\) \\/path\\/to\\/files\\/data\\_1\\.ext")
  expect_output(loginfo_progress("/path/to/files/data_2.ext", array, n = 5), NA)
  expect_output(loginfo_progress("/path/to/files/data_26.ext", array, n = 5), NA)
  expect_output(loginfo_progress("/path/to/files/data_27.ext", array, n = 5), "INFO::\\(27\\/27\\) \\/path\\/to\\/files\\/data\\_27\\.ext")
})


test_that('loginfo_progress logs dirname only on first loop if FUN is basename', {
  array <- let_array()
  expect_output(loginfo_progress("/path/to/files/data_1.ext", array, FUN = basename), "INFO::\\/path\\/to\\/files\\n")
  expect_output(loginfo_progress("/path/to/files/data_1.ext", array, FUN = basename), "INFO::\\(1\\/27\\) data\\_1\\.ext")
  expect_output(loginfo_progress("/path/to/files/data_6.ext", array, FUN = basename), "INFO::\\(6\\/27\\) data\\_6\\.ext")

  expect_output(loginfo_progress("/path/to/files/data_6.ext", array, FUN = base::basename), "INFO::\\(6\\/27\\) data\\_6\\.ext")
})








