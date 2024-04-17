library(tidyverse)
library(grdiamR)
library(DBI)

db <- dbConnect(RPostgres::Postgres(), dbname = "vmr_test")


all_terms <- get_all_field_ontology_terms()

for (term in all_terms){
  get_info(term)
}



dbReadTable(db, "ontology_terms") %>% as_tibble()

ont_terms <- get_all_field_ontology_terms()

df <- tibble(vals = ont_terms)

GRDI ontology?:wq

owl_

t <- rdflib::rdf_parse("~/Databases/ontologies/genepio/src/ontology/genepio-full.owl")

t <- rdflib::rdf_query(rdf = t, query = "SELECT ?subject ?predicate ?object WHERE { ?subject ?predicate ?object . }", data.frame = TRUE)


t %>% filter(grepl(x = predicate, "description"))
t %>% filter(grepl(x = predicate, "about"))
t %>% filter(grepl(x = object, "Public"))


unique(t$predicate)

view(t)

ont_df <-
  df %>%
  separate_wider_regex(col = vals,
                       patterns = c(term = "^.+",
                                    "\\s{0,1}\\[",
                                    id = "[A-Za-z]+[_:][A-Z0-9]+",
                                    "\\]"),
                       too_few = "align_start") %>%
  mutate(term = trimws(term, "right")) %>%
  filter(!is.na(id))


exdf <- read_csv("~/Documents/genepio_export.csv")

exont <- gsub(x = exdf$Entity, ".+/([A-Z_a-z]+_[A-Z0-9]+)$", "\\1")
exont <- gsub(x = exont, "([A-Z_a-z]+)_([A-Z0-9]+)$", "\\1:\\2")

gpio_ids <- rols::termId(gpio_terms)
exont[!exont %in% gpio_ids]

length(exont)

7736 + 960

Term(gpio, "FOODON:02000303")
Term(foodon, "FOODON:02000303")


gpio_terms

ontologyIndex::get_owl


grep(x = gp_obo$name, "Public", value = T)


library(rols)
library(ontologyIndex)

gp_obo <- ontologyIndex::get_OBO("~/Databases/ontologies/genepio/src/ontology/genepio-full.obo")


any(names(gp_obo$id)=="GENEPIO:0100551")

x <- ontologyIndex::get_descendants(gp_obo, roots = "BFO:0000001")

Term("genepio", "GENEPIO:0100551")
Term("genepio", "GENEPIO:0001643")

length(x)

grep(x = gp_obo$name, "Agri-Food Canada", value = T)

ontologyIndex::minimal_aset(gp_obo)



foodon <- Ontology("foodon")
foodon_terms <- Terms(foodon)
x <- Term(foodon, "UO:0010029")
Term(foodon, "FOODON:03411301")
eu <- Term(foodon, "FOODON_03520366")
liter <- Term(foodon, "UO_0000099")
liter@has_children

rols::Te

ont_df$


showMethods("Terms")
getMethod("Term", signature = "character")
getMethod("Terms", signature = "Ontology")

phac_ont <- "GENEPIO:0100551"
gpio <- Ontology("genepio")
gpio_terms <- Terms(gpio)
gpio_terms[[phac_ont]]
gpio_terms[[1]]

gpio_terms[["GAZ00004682"]]

Term("genepio", "GAZ:00004682")

gpio@numberOfTerms

debugonce(rols::Term)

gpio_terms

debugonce(rols::Terms)

debugonce(rols:::termFromJson)

debugonce(rols::Term)
debugonce(httr2::req_perform)
Term(gpio, phac_ont)
Term(gpio, "CHEBI:35727")
Term(gpio, "GENEPIO_0001510")
Term(gpio, "OBI:0000759")
x <- Term(gpio, "NCIT_C49493")


gp_obo$id[!gp_obo$id %in% names(gpio_terms@x)]


rols_terms <- names(gpio_terms@x)
new_obo <- names(gp_obo$id)

x <- rols_terms %in% new_obo
rols_terms[!x]

"OBI:0000652"

grep(x = new_obo, "0000652", values = T)

new_obo

length(gp_obo$id)




gp_obo$id

length(gpio_terms@x)

gpio
gpio_terms

gpio_terms[["FOODON:00001181"]]
Term("genepio", "FOODON:00001181")



https://www.ebi.ac.uk/ols4/api/ontologies/genepio/terms/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252FCHEBI_35727
https://www.ebi.ac.uk/ols4/api/ontologies/genepio/terms/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252FGENEPIO_0100551
https://www.ebi.ac.uk/ols4/api/ontologies/genepio/individuals/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252FGENEPIO_0100551


organisation_ont <- "OBI:0000245"
org_term <- gpio_terms[[organisation_ont]]
org_children <- children(org_term)
org_children[[phac_ont]]


org_term <- gpio_terms[["GENEPIO_0001510"]]





gpio_terms@x

l <- list()
for (n in seq(1, length(gpio_terms))){
  term <- gpio_terms[n]
  l[[n]] <-
    tibble(ID = rols::termId(term),
           Label = rols::termLabel(term),
           Description = list(rols::termDesc(term)),
           Synonyms = rols::termSynonym(term),
           isObsolete = rols::isObsolete(term))
}
df <- bind_rows(l)

rols::

org_terms <- rols::children(t)

org_terms[[24]]

  t <- rols::Term(gpio, "OBI:0000245")

df %>% filter(grepl(x=Label, "organization"))




rols::Term(gpio, "GENEPIO:0100551")

as.data.frame(gpio_terms[3])

length(gpio_terms)


