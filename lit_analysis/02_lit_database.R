library(tidyverse)

lit <- revtools::read_bibliography("lit_data/lit_review.ris")

table(lit$type); table(lit$journal); table(lit$year)
plot(names(table(lit$year)), as.numeric(table(lit$year)), type="s")

glimpse(lit)
# clean data for manual collection of paper info
data <- arrange(lit, year) %>% 
  transmute(ID = paste0("L_",1:nrow(lit)),
            year, label, title, journal, author, doi,
            
            research_type = NA, # applied(quantitative,empirical), theoretical(qualitative), methodological
            data_type = NA, # database, field, literature, simulation
            data_source = NA, # link to access
             
            response_level = NA, # species, population, community, ecosystem, multi, functional
            response_var = NA, # specific diversity index, population metric etc. 
            response_kingdom = NA, # plantea, fungi, animalia, bacteria etc.
            response_phylum = NA, # chordata, arthropoda, mollusca, annelida, cnidaria, porifera, platyhelmynths
            response_class = NA, # mammalia, aves, reptilia, amphibia, osteichthyes, agnatha, chondrichtyes etc.
            response_family = NA, # carnivora, primates etc..
            response_flex = NA, # generalist, specialist, both
            
            landscape_group = NA, # terrestrial, aquatic 
            landscape_type = NA, # study landscape https://ipbes.net/glossary/units-analysis
            
            change_category = NA, # habitat loss/fragmentation/invasion/overused # habitat gain/restoration # multi
            change_magnitude = NA,
            change_regime = NA, # frequency of changes across temporal scale = not discussed, one change, multiple changes numbers
            change_pace = NA, # pace of change across temporal scale = not discussed, fast, slow, immediate
            
            study_spatial_scale = NA, # local, regional, continental, global
            study_spatial_area = NA, # area size examined, if specified
            study_temporal_scale = NA, # timeframe examined
            
            method = NA, # past vs present etc.
            delay_direction = NA, # debt, credit, both
            delay_type = NA, # specific term
            delay_magnitude = NA, # percentage of response to be lost/gained
            relaxation_time = NA, # timeframe to equilibrium/delay repayment
            
            
            
            )

write.csv(data, "lit_data/lit_database_raw_v0.csv", row.names = F)
