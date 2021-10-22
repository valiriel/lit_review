library(tidyverse); library(revtools); library(litsearchr)

#' import search and remove duplicates
import <- import_results(directory = "lit_data/01_final_lit_search", verbose = TRUE)
print(paste0("imported n. ", nrow(import)))

deduplicated <- remove_duplicates(import, field = "title", method = "exact")
print(paste0("deduplicated_exact n. ", nrow(deduplicated)))

deduplicated <- remove_duplicates(deduplicated, field = "title", method = "string_osa")
print(paste0("deduplicated_string n. ", nrow(deduplicated)))

#' screen lit search

# save a copy of raw data, 830 articles
write.csv(deduplicated, file="lit_data/02_title_abstract_screening/00_lit_not_screened.csv")

# screen titles 
screen_titles(deduplicated) 
# manually move saved file from main dir to lit_data/02_title_abstract_screening as 01_lit_title_screened.csv
data <- read.csv("lit_data/02_title_abstract_screening/01_lit_title_screened.csv") 

# retain selection from above, remove excluded
data <- data %>% filter(screened_titles != "excluded") 
table(data$screened_titles)
table(data$source_type)
table(data$year)

# screen abstract
screen_abstracts(data %>% as.bibliography()) # manually open file above
# manually move saved file from main dir to lit_data/02_title_abstract_screening as 02_lit_abstract_screened.csv
data2 <- read.csv("lit_data/02_title_abstract_screening/02_lit_abstract_screened.csv")

# retain selection from above, remove excluded
data2 <- data2 %>% filter(screened_abstracts != "excluded") 
table(data2$screened_abstracts)
table(data2$source_type)
table(data2$year)

plot(names(table(data2$year)), as.numeric(table(data2$year)), type="l")

#' Go through each article and import into in reference manager for reading
write.csv(data2, file="lit_data/02_title_abstract_screening/03_final_lit_screened.csv")

