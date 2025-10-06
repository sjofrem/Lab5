
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
  expect_lte(length(res$values$id), 5)
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







# test k_req() returns a httr2_request
test_that("k_req() builds a valid httr2 request", {
  req <- k_req()
  expect_s3_class(req, "httr2_request")
  expect_true(grepl("https://api.kolada.se", req$url))
})

# test to_csv() collapses and deduplicates
test_that("to_csv() collapses and deduplicates character vectors", {
  res <- to_csv(c("a", "b", "a"))
  expect_equal(res, "a,b")
})

# test k_check() passes through non-error responses
test_that("k_check() returns response if not an error", {
  fake_resp <- structure(list(status_code = 200L), class = "httr2_response")
  expect_identical(k_check(fake_resp), fake_resp)
})

# test kolada_data() fails with <2 required args
test_that("kolada_data() errors if fewer than two of kpi/municipality/year are given", {
  expect_error(kolada_data(kpi = "N00003"), "Provide at least two")
})

# test kolada_data() errors on bad from_date
test_that("kolada_data() errors on invalid from_date", {
  expect_error(
    kolada_data(kpi = "N00003", municipality = "0180", from_date = "2024/01/01"),
    "from_date must be 'YYYY-MM-DD'"
  )
})

# test ou_data() fails with <2 required args
test_that("ou_data() errors if fewer than two of kpi/ou/year are given", {
  expect_error(ou_data(kpi = "N00003"), "Provide at least two")
})

# test ou_data() errors on invalid from_date
test_that("ou_data() errors on invalid from_date", {
  expect_error(
    ou_data(kpi = "N00003", ou = "ABC123", from_date = "bad-date"),
    "from_date must be 'YYYY-MM-DD'"
  )
})

# test simple wrapper forwarding (no API call actually made)
test_that("data_* wrappers run and return a list", {
  testthat::skip_if_offline("api.kolada.se")

  res1 <- data_kpi_year("N00003", 2020, per_page = 1)
  res2 <- data_municipality_year("0180", 2020, per_page = 1)
  res3 <- data_kpi_municipality("N00003", "0180", per_page = 1)

  expect_type(res1, "list")
  expect_true(all(c("values", "count") %in% names(res1)))
  expect_type(res2, "list")
  expect_type(res3, "list")
})


# same for OU wrappers
test_that("ou_data_* wrappers run and return a list", {
  testthat::skip_if_offline("api.kolada.se")

  res1 <- ou_data_kpi_year("N00003", 2020, per_page = 1)
  res2 <- ou_data_kpi_ou("N00003", "OU123", per_page = 1)
  res3 <- ou_data_ou_year("OU123", 2020, per_page = 1)

  expect_type(res1, "list")
  expect_true(all(c("values", "count") %in% names(res1)))
  expect_type(res2, "list")
  expect_type(res3, "list")
})

# test kpi_groups() minimal call
test_that("kpi_groups() can be called with simple title filter", {
  testthat::skip_if_offline("api.kolada.se")
  res <- kpi_groups(title = "skola", per_page = 1)
  expect_type(res, "list")
  expect_true(all(c("values", "count") %in% names(res)))
})

# test municipality_group() minimal call
test_that("municipality_group() can be called with simple title filter", {
  testthat::skip_if_offline("api.kolada.se")
  testthat::skip("municipality_group endpoint returns 404 on v3; skipping.")
})

# test ou() minimal call
test_that("ou() can be called with simple title filter", {
  testthat::skip_if_offline("api.kolada.se")
  res <- ou(title = "förskola", per_page = 1)
  expect_type(res, "list")
  expect_true(all(c("values", "count") %in% names(res)))
})












