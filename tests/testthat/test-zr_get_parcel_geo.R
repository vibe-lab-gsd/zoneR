test_that("result is sf object", {
  parcel_file <- system.file("extdata/Paradise.parcel", package = "zoneR")
  result <- zr_get_parcel_geo(parcel_file)
  expect_s3_class(result, "sf")
  # expect_true(inherits(result, "sf"))
})
