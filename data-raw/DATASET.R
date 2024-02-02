## code to prepare `DATASET` dataset goes here
grdi_schema <- yaml::yaml.load_file(input = "data-raw/schema.yaml")
usethis::use_data(grdi_schema, overwrite = TRUE)
