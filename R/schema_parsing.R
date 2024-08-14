#' Get ontology values from the excel sheet.
#'
#' @export
excel_lookup_values <- function(template_file){

  #Get the version
  version <-
    sub(x = template_file, "^.+(v[0-9]{2}\\.[0-9]{1,2}\\.[0-9]{1,2})\\.xlsm$", "\\1")
  # Rename file
  new_file <- tempfile(fileext = ".xlsx")
  file.copy(template_file, new_file)
  # Read into dataframe
  df <- openxlsx::read.xlsx(xlsxFile = new_file,
                            startRow = 2,
                            colNames = TRUE, sep.names = ' ',
                            sheet = "Vocabulary") %>% as_tibble()
  # Format
  excel_lookup_values <-
    df %>%
    pivot_longer(cols = everything(),
    names_to = "Field", values_to = "Terms",
    values_drop_na = TRUE) %>%
    separate_ontology_terms(Terms) %>%
    mutate(version = version)

  return(excel_lookup_values)
}
