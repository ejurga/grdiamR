xcel_template_link <- "https://raw.githubusercontent.com/cidgoh/GRDI_AMR_One_Health/main/Template/GRDI_Harmonization-Template_v10.0.0.xlsm"
template_file <- tempfile(fileext = ".xlsx")
download.file(url = xcel_template_link,
              destfile = template_file,
              method = "wget")
df <- openxlsx::read.xlsx(xlsxFile = template_file,
                          startRow = 2,
                          colNames = TRUE, sep.names = ' ',
                          sheet = "Vocabulary") %>% as_tibble()
excel_lookup_values <-
  df %>%
  pivot_longer(cols = everything(),
  names_to = "Field", values_to = "Terms",
  values_drop_na = TRUE) %>%
  grdiamR::separate_ontology_terms(Terms)
usethis::use_data(excel_lookup_values, overwrite = TRUE)
