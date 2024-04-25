require(tidyverse)
# Link to repo
one_health_repo <- "~/Software/GRDI_AMR_One_Health"
# Get the master reference guide
master_ref_file <-
  list.files(paste0(one_health_repo, "/Reference Guide"), pattern = ".xlsx",
             full.names = TRUE)
#Get the version
version <-
  sub(x = master_ref_file, "^.+(v[0-9]{2}\\.[0-9]{1,2}\\.[0-9]{1,2})\\.xlsx$", "\\1")
# Read into dataframe
df <- openxlsx::read.xlsx(xlsxFile = master_ref_file,
                          startRow = 1,
                          sheet = "Term Reference Guide")
# Format
terms_from_master_sheet <-
  df %>%
  as_tibble() %>%
  select(Field, Term, Ontology.Identifier, Definition,
         contains("Deprecated")) %>%
  filter(!is.na(Term)) %>%
  mutate(Field = trimws(Field, which = "both"),
         Term = trimws(Term, which = "both"),
         Ontology.Identifier = trimws(Ontology.Identifier),
         version = version)
# Save the data
usethis::use_data(terms_from_master_sheet, overwrite = TRUE)
