#!/usr/bin/env Rscript
# run-checks.r
# Usage: Rscript run-checks.r [bldg_file] [parcel_files] [zoning_files]

args <- commandArgs(trailingOnly = TRUE)

bldg_file <- if (length(args) >= 1) args[1] else "inst/extdata/2_fam.bldg"
parcel_files <- if (length(args) >= 2) args[2] else "inst/extdata/Paradise.parcel"
zoning_files <- if (length(args) >= 3) args[3] else "inst/extdata/Paradise.zoning"

if (!requireNamespace("pkgload", quietly = TRUE)) install.packages("pkgload", repos = "https://cran.rstudio.com")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf", repos = "https://cran.rstudio.com")

pkgload::load_all('.')

cat("Running zr_run_zoning_checks with:\n",
    "  bldg_file: ", bldg_file, "\n",
    "  parcel_files: ", parcel_files, "\n",
    "  zoning_files: ", zoning_files, "\n\n")

res <- zr_run_zoning_checks(
  bldg_file = bldg_file,
  parcel_files = parcel_files,
  zoning_files = zoning_files,
  detailed_check = FALSE,
  print_checkpoints = TRUE,
  save_to = NULL
)

print(table(res$allowed))

# try to save results as GeoJSON
out_file <- "zr_output.geojson"
tryCatch({
  sf::write_sf(res, out_file)
  cat("Saved output to", out_file, "\n")
}, error = function(e){
  cat("Could not save output:", conditionMessage(e), "\n")
})
