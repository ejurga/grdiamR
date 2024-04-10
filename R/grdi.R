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

get_slots <- function(){
  data("grdi_schema")
  # Get lot (column) names
  slots <- grdi_schema[["slots"]]
  # Regex to id all the columns that have to do with AMR
  amr_regexes <-
    c("_resistance_phenotype", "_measurement*", "_laboratory_typing_*",
      "_vendor_name", "_testing_standard*", "_breakpoint")
  # Get index of AMR slots
  amr_index <- unlist(lapply(FUN = grep, X = amr_regexes, x = names(slots)))
  # Remove AMR slots
  slots_no_amr <- slots[-amr_index]
  # Return slots schema with no AMR slots
  return(slots_no_amr)
}

get_slots_by_flag <- function(flag = c('recommended', 'required')) {
  slots <- get_slots()
  slot_list <- vector()
  for (slot in names(slots)){
    # In this schema, the "Required" or "Recommended" flags are set by their
    # presence only, so a NULL value means they are FALSE
    if (!is.null(slots[[slot]][[flag]])){
      slot_list <- append(slot_list, slot)
    }
  }
  return(slots[slot_list])
}

get_slot_menu_names <- function(slot_name){
  slots <- get_slots()
  slot <- slots[[slot_name]]
  menus <- unlist(list(slot[["any_of"]], slot[["range"]]))
  return(unname(menus))
}

get_slot_values <- function(slot_name) {
  menus <- get_slot_menu_names(slot_name)
  # Get the values of the menu
  values <- vector()
  for (menu in menus){
    # If one of these, just add them to the values list.
    if (menu == "WhitespaceMinimizedString") {
      values <- append(values, menu)
    # Otherwise, descend into the schema and return the menu items
    } else {
      values <-
        append(values,
               names(grdi_schema$enums[[menu]][["permissible_values"]]))
    }
  }
  return(values)
}

get_categories <- function(){
  x <- grdi_schema$classes$GRDI$slot_usage
  fields <- names(x)
  cats <- vector()
  for (field in fields){
    category <- x[[field]]$slot_group
    cats <- append(cats, category)
  }
  categories <- list()
  for ( a in unique(cats) ){
    categories[[a]] <- fields[cats==a]
  }
  return(categories)
}

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
  x
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
