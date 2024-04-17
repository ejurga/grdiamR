require(tidyverse)

master_reference_link <- "https://raw.githubusercontent.com/cidgoh/GRDI_AMR_One_Health/main/Reference%20Guide/GRDI_Master-Reference-Guide_v10.0.0.xlsx"
master_ref_file <- tempfile(fileext = ".xlsx")
download.file(url = master_reference_link,
              destfile = master_ref_file,
              method = "wget")
df <- openxlsx::read.xlsx(xlsxFile = master_ref_file,
                          startRow = 1,
                          sheet = "Term Reference Guide")
terms_from_master_sheet <-
  df %>%
  as_tibble() %>%
  select(Field, Term, Ontology.Identifier, Definition) %>%
  filter(!is.na(Term))
usethis::use_data(terms_from_master_sheet)
