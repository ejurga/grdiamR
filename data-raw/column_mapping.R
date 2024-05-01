column_mapping <-
    bind_rows(
      tibble(
        col     = "sample_collected_by",
        db_tab  = "collection_information",
        db_col  = "sample_collected_by"
      ),
      tibble(
        col     = "purpose_of_sampling",
        db_tab  = "sample_purpose",
        db_col  = "purpose_of_sampling"
      ),
      tibble(
        col     = "presampling_activity",
        db_tab  = "sample_activity",
        db_col  = "presampling_activity"
      ),
      tibble(
        col     = "specimen_processing",
        db_tab  = "collection_information",
        db_col  = "specimen_processing"
      ),
      tibble(
        col     = "geo_loc_name (country)",
        db_tab  = "geo_loc",
        db_col  = "geo_loc_name_country"
      ),
      tibble(
        col     = "geo_loc_name (state/province/region)",
        db_tab  = "geo_loc",
        db_col  = "geo_loc_name_state_province_region"
      ),
      tibble(
        col     = "food_product_origin_geo_loc_name (country)",
        db_tab  = "food_product_origin_country",
        db_col  = "food_data"
      ),
      tibble(
        col     = "host_origin_geo_loc_name (country)",
        db_tab  = "hosts",
        db_col  = "host_origin_geo_loc_name_country"
      ),
      tibble(
        col     = "sample_collection_date_precision",
        db_tab  = "collection_information",
        db_col  = "sample_collection_date_precision"
      ),
      tibble(
        col     = "environmental_site",
        db_tab  = "environmental_data_site",
        db_col  = "environmental_site"
      ),
      tibble(
        col     = "water_depth_units",
        db_tab  = NA,
        db_col  = NA
      ),
      tibble(
        col     = "sediment_depth_units",
        db_tab  = NA,
        db_col  = NA
      ),
      tibble(
        col     = "air_temperature_units",
        db_tab  = NA,
        db_col  = NA
      ),
      tibble(
        col     = "water_temperature_units",
        db_tab  = NA,
        db_col  = NA
      ),
      tibble(
        col     = "weather_type",
        db_tab  = "environment_data_weather_type",
        db_col  = "weather_type"
      ),
      tibble(
        col     = "available_data_types",
        db_tab  = "environment_data_available_data_type",
        db_col  = "available_data_type"
      ),
      tibble(
        col     = "animal_or_plant_population",
        db_tab  = "environment_data_available_data_type",
        db_col  = "animal_or_plant_population"
      ),
      tibble(
        col     = "environmental_material",
        db_tab  = "environment_data_material",
        db_col  = "environmental_material"
      ),
      tibble(
        col     = "anatomical_material",
        db_tab  = "anatomical_data_material",
        db_col  = "anatomical_material"
      ),
      tibble(
        col     = "body_product",
        db_tab  = "anatomical_data_body",
        db_col  = "body_product"
      ),
      tibble(
        col     = "anatomical_part",
        db_tab  = "anatomical_data_part",
        db_col  = "anatomical_part"
      ),
      tibble(
        col     = "anatomical_region",
        db_tab  = "anatomical_data_part",
        db_col  = "anatomical_region"
      ),
      tibble(
        col     = "food_product",
        db_tab  = "food_data_product",
        db_col  = "food_product"
      ),
      tibble(
        col     = "food_product_properties",
        db_tab  = "food_data_product_property",
        db_col  = "food_product_property"
      ),
      tibble(
        col     = "animal_source_of_food",
        db_tab  = "food_data_source",
        db_col  = "food_data_souce"
      ),
      tibble(
        col     = "food_product_production_stream",
        db_tab  = "food_data",
        db_col  = "food_product_production_stream"
      ),
      tibble(
        col     = "collection_device",
        db_tab  = "collection_information",
        db_col  = "collection_device"
      ),
      tibble(
        col     = "collection_method",
        db_tab  = "collection_information",
        db_col  = "collection_method"
      ),
      tibble(
        col     = "food_packaging",
        db_tab  = "food_data_packaging",
        db_col  = "food_packaging"
      ),
      tibble(
        col     = "host (common name)",
        db_tab  = "hosts",
        db_col  = "host_common_name"
      ),
      tibble(
        col     = "host (scientific name)",
        db_tab  = "hosts",
        db_col  = "host_scientific_name"
      ),
      tibble(
        col     = "host (food production name)",
        db_tab  = "hosts",
        db_col  = "host_food_production_name"
      ),
      tibble(
        col     = "host_age_bin",
        db_tab  = "hosts",
        db_col  = "host_age_bin"
      ),
      tibble(
        col     = "organism",
        db_tab  = "isolates",
        db_col  = "organism"
      ),
      tibble(
        col     = "taxonomic_identification_process",
        db_tab  = "isolates",
        db_col  = "taxonomic_identification_process"
      ),
      tibble(
        col     = "sequenced_by",
        db_tab  = "sequencing",
        db_col  = "sequenced_by"
      ),
      tibble(
        col     = "purpose_of_sequencing",
        db_tab  = "sequencing_purpose",
        db_col  = "purpose_of_sequencing"
      ),
      tibble(
        col     = "sequencing_platform",
        db_tab  = "sequencing",
        db_col  = "sequencing_platform"
      ),
      tibble(
        col     = "sequencing_instrument",
        db_tab  = "sequencing",
        db_col  = "sequencing_instrument"
      ),
      tibble(
        col     = "sequencing_assay_type",
        db_tab  = NA,
        db_col  = NA
      ),
      tibble(
        col     = "sequence_submitted_by",
        db_tab  = "public_repository_information",
        db_col  = "sequence_submitted_by"
      ),
      tibble(
        col     = "attribute_package",
        db_tab  = "public_repository_information",
        db_col  = "attribute_package"
      ),
      tibble(
        col     = "stage_of_production",
        db_tab  = "risk_assessment",
        db_col  = "stage_of_production"
      ),
      tibble(
        col     = "experimental_intervention",
        db_tab  = "risk_assessment",
        db_col  = "experimental_intervention"
      ),
      tibble(
        col     = "AMR_testing_by",
        db_tab  = "amr_info",
        db_col  = "amr_testing_by"
      ),
      tibble(
        col     = "antimicrobial_phenotype",
        db_tab  = "amr_antibiotices_profile",
        db_col  = "antimicrobial_phenotype"
      ),
      tibble(
        col     = "antimicrobial_measurement_units",
        db_tab  = "amr_antibiotices_profile",
        db_col  = "measurement_units"
      ),
      tibble(
        col     = "antimicrobial_measurement_sign",
        db_tab  = "amr_antibiotices_profile",
        db_col  = "measurement_sign"
      ),
      tibble(
        col     = "antimicrobial_laboratory_typing_method",
        db_tab  = "amr_antibiotices_profile",
        db_col  = "laboratory_typing_method"
      ),
      tibble(
        col     = "antimicrobial_laboratory_typing_platform",
        db_tab  = "amr_antibiotices_profile",
        db_col  = "laboratory_typing_platform"
      ),
      tibble(
        col     = "antimicrobial_vendor_name",
        db_tab  = "amr_antibiotices_profile",
        db_col  = "vendor_name"
      ),
      tibble(
        col     = "antimicrobial_testing_standard",
        db_tab  = "amr_antibiotices_profile",
        db_col  = "testing_standard"
      )
    )
usethis::use_data(column_mapping, overwrite = TRUE)
