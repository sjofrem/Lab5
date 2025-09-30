test_that("kpi() returns a Kolada envelope", {
  testthat::skip_if_offline("api.kolada.se")
  res <- kpi(title = "skola", per_page = 1)

  expect_type(res, "list")
  expect_true(all(c("values", "count") %in% names(res)))
  expect_s3_class(as.data.frame(res$values), "data.frame")
})
