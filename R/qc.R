require(tidyverse)

#' Internal: compare a single field and return decrepancies.
#'
#' Used in [compare_lookup_tables]
compare_one_lookup_field <- function(df, field){

  f.df <- df %>% filter(Field==field)

  s.df <-
    f.df %>%
    group_by(Term, Id) %>%
    summarise(present.in = list(Table), .groups = "drop" ) %>%
    mutate(n = lengths(present.in))

  errors.df <-
    s.df %>%
    filter(n<3)

  if (nrow(errors.df)!=0){
    errors.df$Field <- field
    return(errors.df)
    }
}

#' Return a df of ontology terms and fields from all three sources
#'
#' Get a datagrame with the ontology terms, ids, and fields of the
#' GRDI standard, from the Master excel sheet, the DataHarmonizer YAML,
#' and the Excel template
#'
#' @export
get_terms_from_excel_and_yaml_sources <- function(){

  data("terms_from_master_sheet")
  data("excel_lookup_values")

  yaml <-
    get_all_field_ontology_terms() %>%
    separate_ontology_terms(Terms) %>%
    filter(!is.na(Id)) %>%
    mutate(Table = "yaml")

  excel_template <-
    excel_lookup_values %>%
    filter(Field != "antimicrobial_agent_name") %>%
    select(-version) %>%
    mutate(Table = "template")

  master <-
    terms_from_master_sheet %>%
    select(Field, Term, Ontology.Identifier) %>%
    rename(Id = Ontology.Identifier) %>%
    mutate(Table = "master")

  df <-
    bind_rows(yaml, master, excel_template) %>%
    filter(!Id %in% extract_ont_id(get_null_value()))

  return(df)

}

#' Check for discrepancies in the various forms of the GRDI template
#'
#' There are 3 sources of information for the GRDI ontology: the Master
#' Template, the Excel Spreadsheet template, and the yaml for the
#' Data Harmonizer. Use this function to check for discrepancies between
#' the three.
#'
#' The function does it all, including data import.
#'
#' @return A dataframe of discrepancies.
#'
#' @export
compare_lookup_tables <- function(){

  df <- get_terms_from_excel_and_yaml_sources()

  # Temporary fixes
  df$Field = str_replace(df$Field, " menu", "")
  df$Field = str_replace(df$Field, "^production_stream$", "food_product_production_stream")
  df$Field = str_replace(df$Field, "[ _]{0,1}geo_loc ", "_geo_loc_name ")

  results <- list()
  for (field in unique(df$Field)){
    results[[field]] <- compare_one_lookup_field(df, field)
  }

  discrepancies <- bind_rows(results)

  return(discrepancies)

}
