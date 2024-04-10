## code to prepare `DATASET` dataset goes here
grdi_yaml_url <- "https://raw.githubusercontent.com/cidgoh/pathogen-genomics-package/main/templates/grdi/schema.yaml"
download.file(url = grdi_yaml_url, 
	      destfile = "data-raw/schema.yaml", 
	      method = "wget")
grdi_schema <- yaml::yaml.load_file(input = "data-raw/schema.yaml")
usethis::use_data(grdi_schema, overwrite = TRUE)
