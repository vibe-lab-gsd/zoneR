test_that("output is sf", {
  json_file <- system.file("extdata/Cockrell Hill.parcel", package = "tidyzoning")
  result <- get_tidyparcel_dim(json_file)
  expect_s3_class(result, "sf")
})

test_that("output has lot_type column", {
  json_file <- system.file("extdata/Cockrell Hill.parcel", package = "tidyzoning")
  result <- get_tidyparcel_dim(json_file)
  expect_true(!is.null(result$lot_type))
})

