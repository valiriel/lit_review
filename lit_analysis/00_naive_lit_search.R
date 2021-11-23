remotes::install_github("rmetaverse/metaverse"); remotes::install_github("elizagrames/litsearchr", ref="main")
library(litsearchr); library(ggraph); library(igraph); library(tidyverse)

#' *formulate naive search*
#' pick your research question and key concepts, 
#' string from Lira et al., 2019 review + biodiversity lag and biodiversity legacy

naive_search_string <- 
'("extinction debt*" OR "species credit*" OR "colonization credit*" OR "colonisation credit*" OR 
  "immigration credit*" OR "ecosystem function debt*" OR "ecosystem service debt*" OR 
  "ecosystem function credit*" OR "ecosystem service credit*" OR "ecological lags*" OR 
  "biodiversity lag*" OR "species lags" OR "landscape legacy")'
write_file(naive_search_string, file = "lit_data/naive_lit_data/naive_search_string.txt")

#' *select multiple databases*
#' https://www.webofscience.com/
#' https://base-search.net/Search/Advanced

#######################################################################################

#' import naive search and remove duplicates
naive_import <- import_results(directory = "lit_data/00_naive_lit_data/naive_search_data", verbose = TRUE)
print(paste0("imported n. ", nrow(naive_import)))

naive_deduplicated <- remove_duplicates(naive_import, field = "title", method = "exact")
  print(paste0("deduplicated_exact n. ", nrow(naive_deduplicated)))
  
naive_deduplicated <- remove_duplicates(naive_deduplicated, field = "title", method = "string_osa")
  print(paste0("deduplicated_string n. ", nrow(naive_deduplicated)))

#' extract keywords based on number of occurrence
rake_keywords <- extract_terms(text = paste(naive_deduplicated$title, naive_deduplicated$abstract, naive_deduplicated$keywords),
                               method = "fakerake", min_freq = 10, ngrams = TRUE, min_n = 2,
                               language = "English", stopwords = get_stopwords("English")) %>% as_tibble()

# remove terms that are too broad, save to csv (ver1) and then save for re-import (ver2)
write.csv(rake_keywords, file = "lit_data/00_naive_lit_data/naive_search_keywords_ver1.csv")

#' obsolete or too broad terms manually removed, such as keywords related to specific 
#' habitat or taxonomic groups, broad words ("biodiversity", population size) and unrelated connecting words
rake_keywords <- read.csv("lit_data/00_naive_lit_data/naive_search_keywords_ver2.csv")[,-1]

#' create document-feature matrix
naive_dfm <- create_dfm(elements = paste(naive_deduplicated$title, naive_deduplicated$abstract),
                        features = rake_keywords)

#' create a keyword co-occurrence network
naive_graph <- create_network(search_dfm = naive_dfm, min_studies = 5, min_occ = 5)

#' evaluate keyword importance and clean
nodes_rank <- strength(naive_graph)

terms_rank <- tibble(term=names(nodes_rank), strength=nodes_rank, row.names=NULL) %>%
                  mutate(rank=rank(strength, ties.method="min")) %>% arrange(strength)

#' Compare if any of the search terms were missing from initial search

naive_search_string
terms_rank$term

#' These terms could be included to catch some more articles...

#' "community lag" OR "emigration debt" OR "delayed species extinction" OR "species debt" OR "delayed species colonisation*" OR "delayed species colonization"
