library(tidyverse)
# To generate the easier to use grdi list used in the functions
get_slots <- function(grdi_schema){
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
  # Now, just add back in the ampicillin ones.
  amp_slots <- slots[grep(x = names(slots), "ampicillin_")]
  slots_w_amp <- append(slots_no_amr, amp_slots)
  return(slots_w_amp)
}

get_slot_values <- function(slot) {
  menus = unlist(list(slot$any_of, slot$range))
  # Get the values of the menu
  values <- vector()
  for (menu in menus){
    # If one of these, just add them to the values list.
    if (menu == "WhitespaceMinimizedString" | menu == "date") {
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

get_flag <- function(slot){

  if ( length(slot$required)>0 ) {
    flag <- "required"
  } else if ( length(slot$recommended)>0 ){
    flag <- "recommended"
  } else {
    flag <- "optional"
  }
  return(flag)
}

make_field <- function(slot){
  values <- get_slot_values(slot)
  x <- list(
    description = slot$description,
    comments = slot$comments,
    group = grdi_schema$classes$GRDI$slot_usage[[slot$name]]$slot_group,
    flag = get_flag(slot),
    pick_list = !any(values %in% c("WhitespaceMinimizedString", "date")),
    values = values
   )
  return(x)
}

create_grdi <- function(grdi_schema){

  slots <- get_slots(grdi_schema)

  fields <- list()
  groups <- c()
  for (slot in slots){

    # get rid of the pesky ampicillin name
    if (grepl(x = slot$name, "ampicillin")){
      name <- sub(x=slot$name, "ampicillin", "antimicrobial")
    } else {
      name <- slot$name
    }

    fields[[name]] <- make_field(slot)
    groups <- c(groups,
                grdi_schema$classes$GRDI$slot_usage[[slot$name]]$slot_group)
  }

  grdi <-
    list(
      info = list(
        version = grdi_schema$version
      ),
      groups = unique(groups),
      fields = fields
  )

  return(grdi)
}

# To import the raw YAML file
grdi_yaml_url <- "https://raw.githubusercontent.com/cidgoh/pathogen-genomics-package/main/templates/grdi/schema.yaml"
download.file(url = grdi_yaml_url,
	      destfile = "data-raw/schema.yaml",
	      method = "wget")
grdi_schema <- yaml::yaml.load_file(input = "data-raw/schema.yaml")
usethis::use_data(grdi_schema, overwrite = TRUE, internal = TRUE)
grdi <- create_grdi(grdi_schema)
usethis::use_data(grdi, overwrite = TRUE)
