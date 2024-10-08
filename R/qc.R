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
    filter( !(is.na(Id) | Id == "Not Applicable") )

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

  df <-
    df %>%
    filter(!Id %in% extract_ont_id(get_null_value()))

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

norun <- function(){
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
}

#' Test for terms that are duplicated or misnamed
#'
#' Test for duplicated or misnamed ontology terms between the 3 forms of the
#' GRDI template: the Excel Template , the YAML, and the Master Reference Guide.
#'
#' @return A dataframe of ontology terms and the tables they are found in
#'
#' @export
test_for_term_descrepancies <- function(){
  res <-
    get_terms_from_excel_and_yaml_sources() %>%
    group_by(Field, Term, Id) %>%
    summarise(Tables = list(unique(Table)), .groups = "drop")  %>%
    mutate(n.tab = lengths(Tables)) %>%
    rowwise() %>%
    mutate(Tables = paste0(Tables, collapse = ", "))
  return(res)
}

