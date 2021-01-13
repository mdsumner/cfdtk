

test_that("col2hex returns correct hex codes", {
  expect_equal(
    col2hex(c('dodgerblue', 'dodgerblue4', 'firebrick', 'firebrick4', 'forestgreen')),
    c("#1E90FF", "#104E8B", "#B22222", "#8B1A1A", "#228B22")
  )
})


test_that("label_to_code behaves correctly", {
  # standard formats
  expect_equal(label_to_code('Tasmania East Coast'), "tasmania_east_coast")
  expect_equal(label_to_code('ALLCAPS'), "allcaps")

  # special characters
  expect_equal(label_to_code("This/That won't be the Other"), "this_that_wont_be_the_other")

  # only ever one underscore in a row
  expect_equal(label_to_code('Description - Extra Info'), "description_extra_info")
  expect_equal(label_to_code('Description - (Extra Info)'), "description_extra_info")

  # check no end with underscore
  expect_equal(label_to_code('Description (Extra Info)'), "description_extra_info")

  # check no start with underscore
  expect_equal(label_to_code('(underSCORE)'), "underscore")
})


test_that('headtail combines the head and tail of a dataframe', {
  h <- head(iris, n = 3)
  t <- tail(iris, n = 3)
  expect_equal(headtail(iris, n = 3), bind_rows(h, t))
})
