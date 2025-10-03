
# test basic API call for KPI
test_that("kpi() returns a Kolada envelope", {
  testthat::skip_if_offline("api.kolada.se")
  res <- kpi(title = "skola", per_page = 1)

  expect_type(res, "list")
  expect_true(all(c("values", "count") %in% names(res)))
  expect_s3_class(as.data.frame(res$values), "data.frame")
})

# test API limit parameter
test_that("kpi() respects or caps the per_page limit", {
  res <- kpi(per_page = 5)
  expect_lte(length(res$values), 5000)
})


# test big query handling
test_that("kpi() handles a big query with high per_page", {
  testthat::skip_if_offline("api.kolada.se")
  res <- kpi(per_page = 1000)

  expect_type(res, "list")
  expect_true(length(res$values$id) == 1000)
})

# test municipality() with a known ID
test_that("municipality() returns correct municipality title for Stockholm", {
  testthat::skip_if_offline("api.kolada.se")
  res <- municipality("0180")

  expect_type(res, "list")
  expect_equal(res$values$title[1], "Stockholm")
})

# test municipality() with a invalid ID
test_that("municipality() with invalid ID returns no results", {
  testthat::skip_if_offline("api.kolada.se")
  res <- municipality("9999")

  expect_true(length(res$values) == 0)
})


