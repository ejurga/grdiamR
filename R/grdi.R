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

get_slots_with_menus <- function(){
  slots_with_menus <- list()
  for (slot_name in names(get_slots())){
    menu_names <- get_slot_menu_names(slot_name)
    vals_no_text <- menu_names[ !(menu_names == "WhitespaceMinimizedString" |
                                    menu_names == "null value menu") ]
    if (length(vals_no_text > 1)){
      slots_with_menus[[slot_name]] <- vals_no_text
    }
  }
  return(names(slots_with_menus))
}

grep_slot_val <- function(col_name, x, ...) {
  values <- get_slot_values(slot_name = col_name)
  result <- agrep(x = values, pattern = x, value = TRUE, ...)
  return(result)
}

replace_with_GRDI_term <- function(df, col_name, term_query, data_query = NULL,
                                   term_query_dist = 0, data_query_dist = 0,
                                   select_GRDI_term = NULL){

  if (is.null(data_query)){
    data_query <- term_query
  }

  grdi_val <- grep_slot_val(col_name = col_name,
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

get_info <- function(slot){
  slots <- get_slots()
  x <- names(slots)==slot
  slot <- slots[x][[1]]
  cat("Description: ", str_wrap(slot$description, width = 80), "\n")
  cat("Comments: ", str_wrap(slot$comments,  width = 80), "\n")
}

get_all_slot_values <- function(){
  slots <- get_slots()
  all_slots <- list()
  for (slot in names(slots)){
    vals <- get_slot_values(slot_name = slot)
    all_slots[[slot]] <- vals
  }
  return(all_slots)
}

agrep_across_all_field_terms <- function(x, ...) {
  # Get a list of the field names and their associated terms.
  all_slots <- get_all_slot_values()
  # Search for the user-input across all the field terms, and print to return
  for ( slot in names(all_slots) ){
    results <- agrep(pattern = x, x = all_slots[[slot]], ...)
    if (length(results)>0){
      print(paste("Field:", slot, "-->", all_slots[[slot]][results]))
    }
  }
}

get_unique_ontologies <- function(){
  slot_vals <- get_all_slot_values()
  onts <- unique(unname(unlist(x)))
  onts <- onts[!onts=="WhitespaceMinimizedString"]
}
