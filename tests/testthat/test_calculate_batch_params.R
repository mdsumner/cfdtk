test_that('calculate_batch_params contains all expected attributes', {
  expect_equal(
    calculate_batch_params(total_cells = 51,
                           batch_size = 5,
                           batch_start_number = 1,
                           batch_limit = NA,
                           silent = TRUE) %>% names %>% sort,
    expected = c("batch_start",
                 "batch_end",
                 "total_cells",
                 "total_cells_in_batches",
                 "batch_size",
                 "batch_start_number",
                 "batch_limit",
                 "num_batches") %>% sort
  )
})

test_that('num_batches is calculated correctly for one big batch', {
  expect_equal(
    calculate_batch_params(total_cells = 51,
                           batch_size = 100,
                           batch_start_number = 1,
                           batch_limit = NA,
                           silent = TRUE) %>% .[['num_batches']],
    expected = 1
  )
})

test_that('num_batches is calculated correctly for normal batching', {
  expect_equal(
    calculate_batch_params(total_cells = 51,
                           batch_size = 10,
                           batch_start_number = 1,
                           batch_limit = NA,
                           silent = TRUE) %>% .[['num_batches']],
    expected = 6
  )
})

test_that('total_cells attributes are correct for one big batch', {
  x <- calculate_batch_params(total_cells = 51,
                              batch_size = 100,
                              batch_start_number = 1,
                              batch_limit = NA,
                              silent = TRUE)

  expect_equal(x[['total_cells']], expected = 51)
  expect_equal(x[['total_cells_in_batches']], expected = 51)
})

test_that('total_cells attributes are correct for normal batching, starting from the start', {
  x <- calculate_batch_params(total_cells = 51,
                              batch_size = 5,
                              batch_start_number = 1,
                              batch_limit = NA,
                              silent = TRUE)

  expect_equal(x[['total_cells']], expected = 51)
  expect_equal(x[['total_cells_in_batches']], expected = 51)
})

test_that('total_cells attributes are correct for normal batching, not starting from the first', {
  x <- calculate_batch_params(total_cells = 51,
                              batch_size = 5,
                              batch_start_number = 2,
                              batch_limit = NA,
                              silent = TRUE)

  expect_equal(x[['total_cells']], expected = 51)
  expect_equal(x[['total_cells_in_batches']], expected = 51 - 5)
})

test_that('total_cells attributes are correct for normal batching, starting from the first with a batch limit', {
  x <- calculate_batch_params(total_cells = 51,
                              batch_size = 5,
                              batch_start_number = 1,
                              batch_limit = 3,
                              silent = TRUE)

  expect_equal(x[['total_cells']], expected = 51)
  expect_equal(x[['total_cells_in_batches']], expected = 3*5)
})

test_that('total_cells attributes are correct for normal batching, not starting from the first and with a batch limit', {
  x <- calculate_batch_params(total_cells = 51,
                              batch_size = 5,
                              batch_start_number = 5,
                              batch_limit = 2,
                              silent = TRUE)

  expect_equal(x[['total_cells']], expected = 51)
  expect_equal(x[['total_cells_in_batches']], expected = 2*5)
})

test_that('total_cells attributes are correct for a batch with only one cell at the end of a group', {
  x <- calculate_batch_params(total_cells = 101,
                              batch_size = 100,
                              batch_start_number = 2,
                              batch_limit = NA,
                              silent = TRUE)

  expect_equal(x[['total_cells']], expected = 101)
  expect_equal(x[['total_cells_in_batches']], expected = 1)
})
