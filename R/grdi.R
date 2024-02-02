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

get_slot_values <- function(slot_name) {
  # Get the full slots from the schema
  slots <- get_slots()
  # Get just the one slot
  slot <- slots[[slot_name]]
  # Get the "any_of" slot, or the "range" slot which if present contains the
  # menu
  menus <- unlist(list(slot[["any_of"]], slot[["range"]]))
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

get_slots_with_menus <- function(){
  slots_with_menus <- list()
  for (slot in names(get_slots())){
    vals <- get_slot_values(slot)
    vals_no_text <- vals[ !(vals == "WhitespaceMinimizedString" | vals == "null value menu") ]
    if (length(vals_no_text > 1)){
      slots_with_menus[[slot]] <- vals_no_text
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

