# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

get_fields_with_menus <- function(){
  slots <- vector()
  for (fn in names(grdi$fields)){
    if ( grdi$fields[[fn]]$pick_list==TRUE ){
      slots <- c(slots, fn)
    }
  }
  return(slots)
}

grep_field_val <- function(field, x, ...) {
  values <- grdi$fields[[field]]$values
  result <- agrep(x = values, pattern = x, value = TRUE, ...)
  return(result)
}

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

get_info <- function(field){
  f <- grdi$fields[[field]]
  cat("Description: ", str_wrap(f$description, width = 80), "\n")
  cat("Comments: ", str_wrap(f$comments, width = 80), "\n")
}

get_all_field_ontology_terms <- function(){
  fields <- get_fields_with_menus()
  onts <- vector()
  for (field in fields){
    onts <- c(onts, grdi$fields[[field]]$values)
  }
  return(unique(onts))
}

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
