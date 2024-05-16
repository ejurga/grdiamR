#' Wrapper for [DBI::dbSendStatement] and [DBI::dbBind]
#'
#' For when you just want to insert something. Sends a message of the
#' affected rows.
#'
#' @param db A [DBI] connection
#' @param sql An sql statement, as a string. Use "$n" as placeholders for the
#'  parameters
#' @param params A list of the corresponding parameters.
#'
#' @export
insertBind <- function(db, sql, params, verbose = FALSE){
  q <- dbSendStatement(db, sql)
  dbBind(q, params)
  n <- dbGetRowsAffected(q)
  if (verbose == TRUE) message("Inserted ", n)
  dbClearResult(q)
}

#' Wrapper for [DBI::dbSendQuery], [DBI::dbBind], and [DBI::dbFetch]
#'
#' An operation sufficiently common I wrapped it into a function
#'
#' @param db A [DBI] connection
#' @param sql An sql statement, as a string. Use "$n" as placeholders for the
#'  parameters
#' @param params A list of the corresponding parameters.
#'
#' @return A dataframe (tibble) of the results of the query
#'
#' @export
sendBindFetch <- function(db, sql, params, verbose = FALSE){
  q <- dbSendQuery(db, sql)
  dbBind(q, params)
  res <- dbFetch(q)  %>% as_tibble()
  if (verbose == TRUE) message("Got ", dbGetRowCount(q), " records")
  dbClearResult(q)
  return(res)
}

#' Maps GRDI cols to their respective Tables and Fields in the VRM
#'
#' The VMR adheres as closely as possible to the naming conventions of the
#' GRDI data specification. This function takes a YAML file that maps the
#' GRDI columns to the VMR's respective representation of these fields
#' in their table and column. This YAML must be maintained separately,
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

#' Convert GRDI ontology term into VMR ontology indexes
#'
#' Ontology terms in the VMR are indexed in its ontology_terms
#' table. To insert these terms into their respective columns, they
#' must be converted into these index ids. This function handles this
#' from within R by querying the ontology_term table.
#'
#' @param db [DBI] connection to the VMR
#' @param x Vector of ontology terms in the form of "term name [ONT:00000000]"
#'
#' @export
convert_GRDI_ont_to_vmr_ids <- function(db, x){

  ids <- extract_ont_id(x)
  fac <- factor(ids)
  levels(fac)

  q <-
    dbSendQuery(db,
    "SELECT id, ontology_id FROM ontology_terms where ontology_id = $1")
  dbBind(q, list(levels(fac)))
  res_df <- dbFetch(q)
  dbClearResult(q)

  res <-
    factor(fac, levels = res_df$ontology_id, labels = res_df$id) |>
    as.character() |> as.integer()

  return(res)

}

#' If a list of parameters for [DBI::dbBind] contains NULL, set to NA
#'
#' For [DBI::dbBind], the parameters argument takes a list of the values
#' to bind to the placeholders. The vectors comprising this list must all
#' be the same length. Any argument that has not been initialized must be set
#' to a vector of NAs, of same length as the other vectors.
#'
#' @param params List of vectors to pass onto [DBI::dbBind]
#'
replace_null_params_with_na <- function(params){
  is.0 <- lengths(params)==0
  if (any(is.0)) {
    message("Setting ", sum(is.0), " field to all null")
    params[is.0] <- list(rep(NA, length(params[[1]])))
  }
  return(params)
}

#' Populate a "multi-choice" table in the VMR
#'
#' Currently, the GRDI schema allows some fields to be populated with multiple
#' values. These fields are implemented in the database as their own tables.
#' These tables are all similar in their structure, so this convenience
#' function quickly populates these tables given the values in the GRDI fields.
#' The function assumes that the values are separated wither with a "," or
#' "|".
#'
#' @param db [DBI] connection to VMR
#' @param main_table VMR table that the multi-choice table is related to.
#' @param df The working dataframe that at least contains the GRDI column, as
#'  well as the id field that the multi-choice table will use as a foreign key.
#' @param grdi_col str, the name of the GRDI column that can take on multiple
#'  values
#' @param col_table str, the name of the corresponding VMR table that
#' implements the multiple choice functionality of the GRDI field.
#'
#' @export
populate_multi_choice_table <-
  function(db, df, main_table, grdi_col, col_table){

  dflong <-
    df %>%
    select(all_of(c("id", grdi_col))) %>%
    separate_longer_delim(all_of(grdi_col), "\\s[,|]\\s{0,1}")

  fk <- dbReadTable(db, "foreign_keys") %>% as_tibble()

  fk_sub <-
    fk %>%
    filter(table_name==col_table)

  # Do we need to convert to ontology IDs
  if ("ontology_term_id" %in% fk_sub$foreign_column_name){
    dflong[[grdi_col]] <-
      convert_GRDI_ont_to_vmr_ids(db, x = dflong[[grdi_col]])
  }

  fk_to_main <- fk_sub$column_name[fk_sub$foreign_table_name==main_table]

  q_sql <-
    glue::glue_sql(
    "INSERT INTO {`col_table`} ({`fk_to_main`}, {`grdi_col`})
    VALUES
    ($1, $2)", .con = db)

  message("Inserting into ", col_table)
  q <- dbSendStatement(db, q_sql)
  params = list()
  params[[1]] <- dflong$id
  params[[2]] <- dflong[[grdi_col]]
  dbBind(q, params)
  message("Inserted ", dbGetRowsAffected(q))
  dbClearResult(q)
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

#' Convenience function to update alternative isolate IDS with notes
#'
#'
insert_alternative_isolate_ids <- function(db, sample_ids, iso_ids, alt_ids, notes){
  insertBind(
    db,
    "INSERT INTO alternative_isolate_ids (
      isolate_id, alternative_isolate_id, note
    ) VALUES (
      (SELECT i.id
        FROM isolates AS i
        LEFT JOIN samples AS s ON i.sample_id = s.id
        WHERE s.sample_collector_sample_id = $1
        AND i.isolate_id = $2),
      $3, $4)
    ON CONFLICT ON CONSTRAINT alt_iso_ids_keep_unique DO NOTHING",
    list(sample_ids, iso_ids, alt_ids, notes))
}
