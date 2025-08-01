correct_file <- zr_example_files("Paradise.zoning")
incorrect_file <- zr_example_files("Paradise.parcel")

# correct OZFS
zr_ozfs_validate(correct_file)

zr_ozfs_is_valid(correct_file)

# incorrect OZFS
zr_ozfs_is_valid(incorrect_file)
