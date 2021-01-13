test_that('generate_batch_start_vector and generate_batch_end_vector work for different combinations of parameters', {

  # Only one batch (exact batch size)

  expect_equal(
    cfdtk:::generate_batch_start_vector(total_cells = 51, batch_size = 51),
    c(1)
  )

  expect_equal(
    cfdtk:::generate_batch_end_vector(total_cells = 51, batch_size = 51),
    c(51)
  )


  # Only one batch (batch size > total cells)

  expect_equal(
    cfdtk:::generate_batch_start_vector(total_cells = 51, batch_size = 100),
    c(1)
  )

  expect_equal(
    cfdtk:::generate_batch_end_vector(total_cells = 51, batch_size = 100),
    c(51)
  )


  # Multiple batches, exact numbers

  expect_equal(
    cfdtk:::generate_batch_start_vector(total_cells = 50, batch_size = 5),
    c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46)
  )

  expect_equal(
    cfdtk:::generate_batch_end_vector(total_cells = 50, batch_size = 5),
    c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  )


  # Multiple batches, exact numbers (one cell left over)

  expect_equal(
    cfdtk:::generate_batch_start_vector(total_cells = 51, batch_size = 5),
    c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51)
  )

  expect_equal(
    cfdtk:::generate_batch_end_vector(total_cells = 51, batch_size = 5),
    c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 51)
  )


  # Multiple batches, exact numbers (many cells left over)

  expect_equal(
    cfdtk:::generate_batch_start_vector(total_cells = 106, batch_size = 10),
    c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101)
  )

  expect_equal(
    cfdtk:::generate_batch_end_vector(total_cells = 106, batch_size = 10),
    c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 106)
  )
})
