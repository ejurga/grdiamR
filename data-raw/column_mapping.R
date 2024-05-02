require(tidyverse)

column_mapping <-
  list(
    sample_collected_by =
      list(
        db_tab  = "collection_information",
        db_col  = "sample_collected_by"
      ),
    purpose_of_sampling =
      list(
        db_tab  = "sample_purpose",
        db_col  = "purpose_of_sampling"
      ),
    presampling_activity =
      list(
        db_tab  = "sample_activity",
        db_col  = "presampling_activity"
      ),
    specimen_processing =
      list(
        db_tab  = "collection_information",
        db_col  = "specimen_processing"
      ),
    `geo_loc_name (country)` =
      list(
        db_tab  = "geo_loc",
        db_col  = "geo_loc_name_country"
      ),
    `geo_loc_name (state/province/region)` =
      list(
        db_tab  = "geo_loc",
        db_col  = "geo_loc_name_state_province_region"
      ),
    `food_product_origin_geo_loc_name (country)` =
      list(
        db_tab  = "food_data",
        db_col  = "food_product_origin_country"
      ),
    `host_origin_geo_loc_name (country)` =
      list(
        db_tab  = "hosts",
        db_col  = "host_origin_geo_loc_name_country"
      ),
    sample_collection_date_precision =
      list(
        db_tab  = "collection_information",
        db_col  = "sample_collection_date_precision"
      ),
    environmental_site =
      list(
        db_tab  = "environment_data_site",
        db_col  = "environmental_site"
      ),
    water_depth_units =
      list(
        db_tab  = NA,
        db_col  = NA
      ),
    sediment_depth_units =
      list(
        db_tab  = NA,
        db_col  = NA
      ),
    air_temperature_units =
      list(
        db_tab  = NA,
        db_col  = NA
      ),
    water_temperature_units =
      list(
        db_tab  = NA,
        db_col  = NA
      ),
    weather_type =
      list(
        db_tab  = "environment_data_weather_type",
        db_col  = "weather_type"
      ),
    available_data_types =
      list(
        db_tab  = "environment_data_available_data_type",
        db_col  = "available_data_type"
      ),
    animal_or_plant_population =
      list(
        db_tab  = "environment_data_animal_plant",
        db_col  = "animal_or_plant_population"
      ),
    environmental_material =
      list(
        db_tab  = "environment_data_material",
        db_col  = "environmental_material"
      ),
    anatomical_material =
      list(
        db_tab  = "anatomical_data_material",
        db_col  = "anatomical_material"
      ),
    body_product =
      list(
        db_tab  = "anatomical_data_body",
        db_col  = "body_product"
      ),
    anatomical_part =
      list(
        db_tab  = "anatomical_data_part",
        db_col  = "anatomical_part"
      ),
    anatomical_region =
      list(
        db_tab  = "anatomical_data",
        db_col  = "anatomical_region"
      ),
    food_product =
      list(
        db_tab  = "food_data_product",
        db_col  = "food_product"
      ),
    food_product_properties =
      list(
        db_tab  = "food_data_product_property",
        db_col  = "food_product_property"
      ),
    animal_source_of_food =
      list(
        db_tab  = "food_data_source",
        db_col  = "animal_source_of_food"
      ),
    food_product_production_stream =
      list(
        db_tab  = "food_data",
        db_col  = "food_product_production_stream"
      ),
    collection_device =
      list(
        db_tab  = "collection_information",
        db_col  = "collection_device"
      ),
    collection_method =
      list(
        db_tab  = "collection_information",
        db_col  = "collection_method"
      ),
    food_packaging =
      list(
        db_tab  = "food_data_packaging",
        db_col  = "food_packaging"
      ),
    `host (common name)` =
      list(
        db_tab  = "hosts",
        db_col  = "host_common_name"
      ),
    `host (scientific name)` =
      list(
        db_tab  = "hosts",
        db_col  = "host_scientific_name"
      ),
    `host (food production name)` =
      list(
        db_tab  = "hosts",
        db_col  = "host_food_production_name"
      ),
    host_age_bin =
      list(
        db_tab  = "hosts",
        db_col  = "host_age_bin"
      ),
    organism =
      list(
        db_tab  = "isolates",
        db_col  = "organism"
      ),
    taxonomic_identification_process =
      list(
        db_tab  = "isolates",
        db_col  = "taxonomic_identification_process"
      ),
    sequenced_by =
      list(
        db_tab  = "sequencing",
        db_col  = "sequenced_by"
      ),
    purpose_of_sequencing =
      list(
        db_tab  = "sequencing_purpose",
        db_col  = "purpose_of_sequencing"
      ),
    sequencing_platform =
      list(
        db_tab  = "sequencing",
        db_col  = "sequencing_platform"
      ),
    sequencing_instrument =
      list(
        db_tab  = "sequencing",
        db_col  = "sequencing_instrument"
      ),
    sequencing_assay_type =
      list(
        db_tab  = NA,
        db_col  = NA
      ),
    sequence_submitted_by =
      list(
        db_tab  = "public_repository_information",
        db_col  = "sequence_submitted_by"
      ),
    attribute_package =
      list(
        db_tab  = "public_repository_information",
        db_col  = "attribute_package"
      ),
    stage_of_production =
      list(
        db_tab  = "risk_assessment",
        db_col  = "stage_of_production"
      ),
    experimental_intervention =
      list(
        db_tab  = "risk_activity",
        db_col  = "experimental_intervention"
      ),
    AMR_testing_by =
      list(
        db_tab  = "amr_info",
        db_col  = "amr_testing_by"
      ),
    antimicrobial_phenotype =
      list(
        db_tab  = "amr_antibiotics_profile",
        db_col  = "antimicrobial_phenotype"
      ),
    antimicrobial_measurement_units =
      list(
        db_tab  = "amr_antibiotics_profile",
        db_col  = "measurement_units"
      ),
    antimicrobial_measurement_sign =
      list(
        db_tab  = "amr_antibiotics_profile",
        db_col  = "measurement_sign"
      ),
    antimicrobial_laboratory_typing_method =
      list(
        db_tab  = "amr_antibiotics_profile",
        db_col  = "laboratory_typing_method"
      ),
    antimicrobial_laboratory_typing_platform =
      list(
        db_tab  = "amr_antibiotics_profile",
        db_col  = "laboratory_typing_platform"
      ),
    antimicrobial_vendor_name =
      list(
        db_tab  = "amr_antibiotics_profile",
        db_col  = "vendor_name"
      ),
    antimicrobial_testing_standard =
      list(
        db_tab  = "amr_antibiotics_profile",
        db_col  = "testing_standard"
      )
    )



# Test it
vmr <-
  DBI::dbConnect(drv = RPostgres::Postgres(),
                 dbname = "vmr_test",
                 user = "emil")

fk <- DBI::dbReadTable(vmr, "foreign_keys") %>% as_tibble()

err <- c()
for (col in names(column_mapping)){
  x <- column_mapping[[col]]
  y <- fk %>% filter(table_name==x$db_tab, column_name==x$db_col)
  if (nrow(y)!=1){
    message("error on ", col)
    err <- c(err, col)
  }
}
error_list <- column_mapping[match(err, names(column_mapping))]

usethis::use_data(column_mapping, overwrite = TRUE)
