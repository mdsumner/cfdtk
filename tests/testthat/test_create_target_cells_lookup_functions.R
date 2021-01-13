
test_that("pick_preferred_domain_from_lookup works for different scenarios", {
  expect_equal(cfdtk:::pick_preferred_domain_from_lookup(preferred_domains = "TEST-001",
                                                         polygon_code = 'irrelevant',
                                                         target_domains_files = NULL),
               "TEST-001")

  expect_equal(cfdtk:::pick_preferred_domain_from_lookup(preferred_domains = list(),
                                                         polygon_code = 'irrelevant',
                                                         target_domains_files = list("TEST-002" = "/file/path")),
               "TEST-002")

  # malformed... there are more different domains in the preferred domains list than there are in the target domain files
  expect_equal(cfdtk:::pick_preferred_domain_from_lookup(preferred_domains = list("abc" = "TEST-010", "def" = "TEST-020", "xyz" = "TEST-030"),
                                                         polygon_code = 'def',
                                                         target_domains_files = list("TEST-030" = "/file/path")),
               "TEST-030")

  expect_equal(cfdtk:::pick_preferred_domain_from_lookup(preferred_domains = list("abc" = "TEST-100", "def" = "TEST-200", "xyz" = "TEST-300"),
                                                         polygon_code = 'def',
                                                         target_domains_files = list("TEST-100" = "/file/path/100.nc",
                                                                                     "TEST-200" = "/file/path/200.nc",
                                                                                     "TEST-300" = "/file/path/300.nc")),
               "TEST-200")
})

