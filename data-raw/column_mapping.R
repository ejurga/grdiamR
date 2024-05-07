library(tidyverse)
library(grdiamR)
library(DBI)

# Function to try and map cols in the vmr to the grdi columns
map_grdi_to_vmr_col <- function(grdi_col, vmr_cols){
  message("working on ", grdi_col)
  res <- c()
  dist <- 0
  while (length(res) == 0 | dist == 0.3) {
    res <- agrep(x=vmr_cols$column_name, grdi_col, max.distance = dist)
    dist <- dist + 0.05
  }
  df <- vmr_cols[res,]
  results <- list(table = df$table_name, column = df$column_name)
  return(results)
}

# Connect to the database
vmr <-
  dbConnect(drv = RPostgres::Postgres(),
            user = "emil",
            dbname = "vmr_test")
all_fields <- names(grdi$fields)

# Get all the columns in the database!
q <- dbSendQuery(vmr, "select table_name, column_name, data_type from information_schema.columns
                 WHERE table_schema = 'public'")
vmr_cols <- dbFetch(q) %>% as_tibble()

# Make the list of results
for_yaml <- list()
for (f in all_fields){
  for_yaml[[f]] <- map_grdi_to_vmr_col(f, vmr_cols = vmr_cols)
}

# Save to a temporary file, this file must be edited, it is not perfect.
tmp_file <- tempfile(fileext = ".yaml")
yaml::write_yaml(for_yaml, tmp_file)
message("draft yaml written to: ", tmp_file,
        ". Go and review it before committing it to something")

vmr_map <- yaml::read_yaml("data-raw/column_mapping.yaml")
usethis::use_data(vmr_map, overwrite = TRUE)
