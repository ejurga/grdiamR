#' Search for a valid ontology term from a GRDI-AMR2 field
#'
#' @param field The GRDI field to be searched over
#' @param x search term to be passed onto agrep
#' @param ... other arguments passed onto [agrep()]
#'
#' @export
grep_field_val <- function(field, x, ...) {
  values <- grdi$fields[[field]]$values
  result <- agrep(x = values, pattern = x, value = TRUE, ...)
  return(result)
}

#' Get a vector of GRDI fields that have pick-lists
#'
#' To get a list of the GRDI fields that have mandatory values (ontologies)
#' to be selected
#'
#' @export
get_fields_with_menus <- function(){
  slots <- vector()
  for (fn in names(grdi$fields)){
    if ( grdi$fields[[fn]]$pick_list==TRUE ){
      slots <- c(slots, fn)
    }
  }
  return(slots)
}

#' Replace values in a column with GRDI ontologies
#'
#' Given a column in a dataframe, this function will find the supplied term
#' in the data and replace it with an appropriate ontology from the GRDI
#' standard. Supports fuzzy matching via [agrep()]
#'
#' @param df Dataframe
#' @param col_name column name of the dataframe to replace over
#' @param term_query For looking up the ontology term we want to replace with.
#' @param data_query The value in the data that we want replace
#' @param term_query_dist Distance passed onto [agrep()], for fuzzy matching
#'  the desired ontology term
#' @param data_query_data Distance passed onto [agrep()], for fuzzy matching
#'  the value in the data.
#' @param select_GRDI_term If multiple results for an ontology term are returned,
#'  you may select one manually.
#'
#' @export
replace_with_GRDI_term <- function(df, col_name, term_query, data_query = NULL,
                                   term_query_dist = 0, data_query_dist = 0,
                                   select_GRDI_term = NULL){

  if (is.null(data_query)){
    data_query <- term_query
  }

  grdi_val <- grep_field_val(col_name = col_name,
                            x = term_query,
                            max.distance = term_query_dist)

  if (length(grdi_val) == 0){
    message("No grdi term found for query ", term_query)
    message("exiting")

  } else if (length(grdi_val) > 1) {
    message("Found GRDI terms for query ", term_query, ": ", paste(grdi_val, collapse = ', '))
    if (is.null(select_GRDI_term)){
      message("exiting")
    } else {
      grdi_val <- grdi_val[select_GRDI_term]
      message("Selecting term: ", grdi_val)
    }
  }

  if (length(grdi_val) == 1){
    x <- agrep(x = df[[col_name]], data_query, max.distance = data_query_dist)
    if (length(x)==0){
      message("No matches in data for query ", data_query)
      message("exiting")
    } else {
      n <- length(df[[col_name]][x])
      message("Replacing ", n,
              " instances of data values ",
              paste(unique(df[[col_name]][x]), collapse = ', '),
              " --> ",
              grdi_val)
      df[[col_name]][x] <- grdi_val
      return(df)
    }
  }
}


#' Get a GRDI ontology null value
#'
#' Returns a valid NULL value. If no query is supplied to search for a specific
#' ontology term, than a list of all the possible NULL values are supplied.
#'
#' @param x query to search for a specific ontology term
#'
#' @export
get_null_value <- function(x=NULL){
  nulls <- names(grdi_schema$enums$`null value menu`$permissible_values)
  if (is.null(x)){
    return(nulls)
  } else {
    value <- grep(x = nulls, pattern = x, value = TRUE)
    if (length(value)==0){
      stop("No value found for query ", x, " in null value menu")
    } else {
      return(value)
    }
  }
}

#' Return the description and comments tags of a GRDI field
#'
#' @param field GRDI field to query.
#'
#' @export
get_info <- function(field){
  f <- grdi$fields[[field]]
  cat("Description: ", str_wrap(f$description, width = 80), "\n")
  cat("Comments: ", str_wrap(f$comments, width = 80), "\n")
}

#' Get a list of all the ontology terms used in the GRDI standard.
#'
#' @export
get_all_field_ontology_terms <- function(){
  fields <- get_fields_with_menus()
  onts <- vector()
  for (field in fields){
    onts <- c(onts, grdi$fields[[field]]$values)
  }
  return(unique(onts))
}

#' Search for an ontology term across all the possible GRDI fields.
#'
#' In case you want to search for a specific ontology term across all the
#' GRDI fields. Will print out the matches and the field to which that
#' ontology term belongs.
#'
#' @param x query to search the ontology terms with.
#' @param ... passed onto [agrep()] for fuzzy matching.
#'
#' @export
agrep_across_all_field_terms <- function(x, ...) {
  # Get a list of the field names and their associated terms.
  fields <- get_fields_with_menus()
  # Search for the user-input across all the field terms, and print to return
  for ( field in fields ){
    results <- agrep(pattern = x, x = grdi$fields[[field]]$values, value = TRUE, ...)
    if (length(results)>0){
      print(paste("Field:", field, "-->", results))
    }
  }
}
