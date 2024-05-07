#' Maps GRDI cols to their respective Tables and Fields in the VRM
#'
#' The VMR adheres as closely as possible to the naming conventions of the
#' GRDI data specfication. This function takes a YAML file that maps the
#' GRDI columns to the VMR's respective representation of these fields
#' in their table and column. This YAML must be maintained seperately,
#' and is found in `data-raw/` of this package's source.
#'
#' @export
vmr_map_as_dataframe <- function(){
  data("vmr_map")
  x <- as_tibble(transpose(vmr_map))
  x$grdi_col <- names(x$table)
  x$table <- unname(unlist(x$table))
  x$column <- unname(unlist(x$column))
  x <- select(x, grdi_col, table, column)
  return(x)
}



#' Renaming a GRDI formatted dataframe to a VMR table
#'
#' Take a dataframe imported from a GRDI specification template and
#' convert it into a VMR table ready for insertion into the database.
#' This function depends on an updated mapping YAML.
#'
#' @param db A DBI connection to the VMR
#' @param df Dataframe, formatted according to the GRDI spec
#' @param vmr_table The table in the VMR database to convert into
#'
#' @return A dataframe with columns that match the fields of the selected
#'  table in the VMR
#'
#' @export
select_and_rename_to_vmr <- function(db, df, vmr_table){

  # Retrieve the GRDI col to VMR mapping
  full_map <- vmr_map_as_dataframe()
  table_map <-
    full_map %>%
    filter(table==vmr_table)

  # Get all the fields for the selected table; we need this for troubleshooting.
  table_fields <- dbListFields(db, vmr_table)
  table_fields <- table_fields[table_fields!="id"]

  # First, test to make sure that all the columns are either
  # GRDI names columns, some kind of recognized column name in
  # the VMR mapping scheme, or are at least part of the selected
  # tables fields
  t <- colnames(df) %in% c(full_map$grdi_col, table_fields, full_map$column)
  if (!all(t)){
    stop("Unsure how to process columns: ", paste(colnames(df)[!t], sep = " ,"))
  }

  # Select only relevant columns, using both VMR and GRDI fields
  df.s <-
    df %>%
    select(any_of(table_map$grdi_col), any_of(table_fields))

  # Rename GRDI fields to VMR fields
  grdi_cols <- colnames(df.s)
  for (i in seq(length(grdi_cols))){
    # Get the grdi col
    grdi_col <- grdi_cols[i]
    # This uses the mapping table to execute the renaming
    new_col <- table_map$column[table_map$grdi_col==grdi_col]
    # If there is no result:
    if (length(new_col)==0){
      # If the result is part of the vmr table fields, keep this
      # field
      if (grdi_col %in% table_fields){
        next()
      # If not, send a message.
      } else {
        message("No mapping for ", grdi_col)
        next()
      }
    }
    # Send a message if a renaming occurred.
    if (grdi_col!=new_col) message("Replacing ", grdi_col, " -> ", new_col)
    # Set the result to the vector; even if there is no renaming.
    grdi_cols[i] <- new_col
  }

  # Here, we are testing that all the the fields in the VMR are found int
  # the df we supplied to the function. If not, send a message; it is
  # not necessarily a problem if we are missing columns, they should be
  # set to null in the database if not present.
  t_in_g <- table_fields %in% grdi_cols
  if (!all(t_in_g)) {
    message("missing vmr fields: ", paste(table_fields[!t_in_g], sep = ", "))
  }

  # Rename the columns
  colnames(df.s) <- grdi_cols

  # Return the dataframe, but make sure the ID column is first.
  return(select(df.s, matches("id"), everything()))
}


#' Convert GRDI ontology term into VMR ontology indexes
#'
#' Ontology terms in the VMR are indexed in its ontology_terms
#' table. To insert these terms into their respective columns, they
#' must be converted into these index ids. This function handles this
#' from within R.
#'
#' @param ont_table Ontology term index table from the VMR pulled as dataframe
#' @x Vector of ontology terms in the form of "term name [ONT:00000000]"
#'
#' @export
replace_with_vmr_ont_ids <- function(ont_table, x){
  ids <- extract_ont_id(x)
  res <- factor(ids,
                levels = ont_table$ontology_id,
                labels = ont_table$id) |> as.integer()
  return(res)
}
