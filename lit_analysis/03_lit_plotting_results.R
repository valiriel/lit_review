library(tidyverse); library(ggthemes); options(tibble.print_max = 50); library(viridis); library(scico); library(knitr); library(kableExtra); library(flextable)
theme <- theme_clean() + 
  theme(axis.title=element_text(size=12), axis.text=element_text(size=9),
        legend.text=element_text(size=9), legend.title=element_text(size=11), 
        plot.background = element_rect(color = "white"), 
        panel.border= element_blank(), plot.title=element_text(size=14),
        panel.grid.major.y = element_line(size=0.1, linetype = "dotted", colour = "grey50"),  
        panel.grid.major.x = element_line(size=0.1, linetype = "dotted", colour = "grey50"),
        legend.background = element_rect(color = NA),
        strip.background.y = element_rect(color = "grey30"))

data <- read.csv("lit_data/lit_database_last_4_years_v2.csv")
#all_data <- read.csv("lit_data/lit_database_base.csv")

# remove variables with no data, or that became too hard to collect
data <- as_tibble(data) %>% select(-landscape_var, - change_regime, - change_magnitude, 
                                   -change_pace, -study_spatial_area_km2, -study_temporal_scale_year,
                                   -delay_magnitude, -relaxation_time_years)

#'--------------
#' *Collected variables table for supplementary*

table <- flextable(tibble(category = names(data), description = ""))
table <- flextable::theme_vanilla(table)
save_as_docx(table, path="lit_output/tables/var_table.docx")

#'--------------
#' *All data table for supplementary*

table <- data %>% select(year, label, title, journal, author, doi, research_type, continent, country,
                         data_type, response_level, response_var, response_kingdom, response_phylum,
                         response_flex, landscape_group, landscape_type, change_category, study_spatial_scale, 
                         method, delay_type, delay_quantified, what_quantified, delay_direction)

table <- flextable(tibble(table, description = ""))
table <- flextable::theme_vanilla(table)
save_as_docx(table, path="lit_output/tables/data_table.docx")

#'--------------
#' *years*

table <- data %>% group_by(year) %>% summarise(n = n(), ratio = round(n()/nrow(data),2)); names(table)[1] <- "year"; table
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/year_table.pdf", bs_theme = "flatly")

sd(table$n)

# plot including all years data, split on last 4 years

year_data <- readxl::read_xlsx("lit_data/lit_database_all_v1.xlsx") %>% select(year)
table <- year_data %>% group_by(year) %>% summarise(n = n(), ratio = round(n()/nrow(data),2)); table <- na.omit(table)
ggplot(table, aes(x=year, y=n)) + theme +
  scale_y_continuous(limits = c(0, 28)) + labs(y="Publications per year", x="Year") + 
  scale_x_continuous(limits = c(1994, 2021), breaks = c(seq(1994, 2021, 3))) + 
  geom_line(size=0.5) +
  geom_line(data=table%>%filter(year>2017),size=1) + 
  geom_vline(xintercept=2018)
ggsave(file = "lit_output/figures/year.svg", units = "cm", dpi = "retina", width =20, height = 10)

#'--------------
#' *journal*

journal <- as_tibble(unlist(stringr::str_split(data$journal, "; ")))

table <- journal %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(journal),2)) %>% arrange(n)
table; journal$value <- factor(journal$value, levels = table$value); names(table)[1] <- "journal"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/journal_table.pdf", bs_theme = "flatly")

ggplot(journal, aes(x=value, fill=value)) + labs(x="journal", fill= "journal") + theme +
  geom_bar() + theme_minimal() + theme(axis.text.x=element_blank()) + labs(x="Journals on which publications appeared", y = "Count", fill="")
ggsave(file = "lit_output/figures/journal.svg", units = "cm", dpi = "retina", width =27, height = 21)
ggsave(file = "lit_output/figures/journal.png", units = "cm", dpi = "retina", width =27, height = 21)

#'--------------
#' *research_type*

research_type <- as_tibble(unlist(stringr::str_split(data$research_type, "; ")))

table <- research_type %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(research_type),2)) %>% arrange(n)
table; research_type$value <- factor(research_type$value, levels = table$value); names(table)[1] <- "research_type"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/research_type_table.pdf", bs_theme = "flatly")

ggplot(research_type, aes(x=value, fill=value)) + labs(x="research_type", fill= "research_type") + 
  geom_bar() + theme_minimal() + scale_fill_scico_d(palette = 'buda')
ggsave(file = "lit_output/figures/research_type.svg", units = "cm", dpi = "retina", width =20, height = 10)

#'--------------
#' *continent*

continent <- as_tibble(unlist(stringr::str_split(data$continent, "; "))) %>% filter(value != "NA")

table <- continent %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(continent),2)) %>% arrange(n)
table; continent$value <- factor(continent$value, levels = table$value); names(table)[1] <- "continent"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/continent_table.pdf", bs_theme = "flatly")

ggplot(continent, aes(x=value, fill=value)) + labs(x="continent", fill= "continent") + 
  geom_bar() + theme + scale_fill_solarized() + theme(axis.text.x=element_blank()) + 
  labs(fill="Continents", x="", y="Count")
  
ggsave(file = "lit_output/figures/continent.svg", units = "cm", dpi = "retina", width =20, height = 10)

#'--------------
#' *country*

country <- as_tibble(unlist(stringr::str_split(data$country, "; "))) %>% filter(value != "NA")

table <- country %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(country),2)) %>% arrange(n)
table; country$value <- factor(country$value, levels = table$value); names(table)[1] <- "country"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/country_table.pdf", bs_theme = "flatly")

ggplot(country, aes(x=value, fill=value)) + labs(x="country", fill= "country") +  
  geom_bar() + theme_minimal() + scale_fill_scico_d(palette = 'roma') + theme(axis.text.x=element_blank())
ggsave(file = "lit_output/figures/country.svg", units = "cm", dpi = "retina", width =20, height = 10)

library(sf); countries <- read_sf("lit_output/figures/countries_shp/simple_country_shp.shp")
names(table)[1] <- names(countries)[5]; table$NAME[table$NAME=="USA"] <- "United States"

countries <- left_join(countries, table, by=names(countries)[5])
countries$n[is.na(countries$n)] <- 0
countries$NAME

ggplot(countries, aes(fill=n)) + geom_sf() + theme + theme(legend.position="bottom") + scale_fill_viridis() + labs(fill="No. of studies per country")
ggsave(file = "lit_output/figures/country_map.svg", units = "cm", dpi = "retina", width =40, height = 20)
ggsave(file = "lit_output/figures/country_map.png", units = "cm", dpi = "retina", width =40, height = 20)

#'--------------
#' *data_type*

data_type <- as_tibble(unlist(stringr::str_split(data$data_type, "; "))) %>% filter(value != "NA")

table <- data_type %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(data_type),2)) %>% arrange(n)
table; data_type$value <- factor(data_type$value, levels = table$value); names(table)[1] <- "data_type"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/data_type_table.pdf", bs_theme = "flatly")

ggplot(data_type, aes(x=value, fill=value)) + labs(x="data_type", fill= "data_type") +  
  geom_bar() + theme_minimal() + scale_fill_scico_d(palette = 'bamako')
ggsave(file = "lit_output/figures/data_type.svg", units = "cm", dpi = "retina", width =20, height = 10)

# how many use combo of databases and filed measurements
sum(data$data_type[-is.na(data$data_type)] == "database; field", na.omit=T)

#'--------------
#' *response_level*

response_level <- as_tibble(unlist(stringr::str_split(data$response_level, "; "))) %>% filter(value != "NA")

table <- response_level %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(response_level),2)) %>% arrange(n)
table; response_level$value <- factor(response_level$value, levels = table$value); names(table)[1] <- "response_level"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/response_level_table.pdf", bs_theme = "flatly")

ggplot(response_level, aes(x=value, fill=value)) + labs(x="response_level", fill= "response_level") +  
  geom_bar() + theme_minimal() + scale_fill_scico_d(palette = 'berlin')
ggsave(file = "lit_output/figures/response_level.svg", units = "cm", dpi = "retina", width =20, height = 10)

#'--------------
#' *response_var*

response_var <- as_tibble(unlist(stringr::str_split(data$response_var, "; "))) %>% filter(value != "NA")

table <- response_var %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(response_var),2)) %>% arrange(n)
table; response_var$value <- factor(response_var$value, levels = table$value); names(table)[1] <- "response_var"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/response_level_table.pdf", bs_theme = "flatly")

ggplot(response_var, aes(x=value, fill=value)) + labs(x="response_var", fill= "response_var") +  
  geom_bar() + theme_minimal() + scale_fill_scico_d(palette = 'berlin')
ggsave(file = "lit_output/figures/response_var.svg", units = "cm", dpi = "retina", width =40, height = 20)

#'--------------
#' *response_kingdom*

response_kingdom <- as_tibble(unlist(stringr::str_split(data$response_kingdom, "; "))) %>% filter(value != "NA")

table <- response_kingdom %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(response_kingdom),2)) %>% arrange(n)
table; response_kingdom$value <- factor(response_kingdom$value, levels = table$value); names(table)[1] <- "response_kingdom"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/response_kingdom_table.pdf", bs_theme = "flatly")

ggplot(response_kingdom, aes(x=value, fill=value)) + labs(x="response_kingdom", fill= "response_kingdom") +  
  geom_bar() + theme + scale_fill_scico_d(palette = 'cork') + theme(legend.position="none") + 
  labs(y="Count", x="Taxonomic kingdom")
ggsave(file = "lit_output/figures/response_kingdom.svg", units = "cm", dpi = "retina", width =20, height = 10)
ggsave(file = "lit_output/figures/response_kingdom.png", units = "cm", dpi = "retina", width =20, height = 10)

#'--------------
#' *response_phylum*

response_phylum <- as_tibble(unlist(stringr::str_split(data$response_phylum, "; "))) %>% filter(value != "NA")

table <- response_phylum %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(response_phylum),2)) %>% arrange(n)
table; response_phylum$value <- factor(response_phylum$value, levels = table$value); names(table)[1] <- "response_phylum"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/response_phylum_table.pdf", bs_theme = "flatly")

ggplot(response_phylum, aes(x=value, fill=value)) + labs(x="response_phylum", fill= "response_phylum") +  
  geom_bar() + theme + scale_fill_viridis_d(option="D") + theme(axis.text.x=element_blank()) + 
  labs(y="Count", fill="Broader animal groups", x="")
ggsave(file = "lit_output/figures/response_phylum.svg", units = "cm", dpi = "retina", width =20, height = 10)
ggsave(file = "lit_output/figures/response_phylum.png", units = "cm", dpi = "retina", width =20, height = 10)

  #'--------------
#' *response_flex*

response_flex <- as_tibble(unlist(stringr::str_split(data$response_flex, "; "))) %>% filter(value != "NA")

table <- response_flex %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(response_flex),2)) %>% arrange(n)
table; response_flex$value <- factor(response_flex$value, levels = table$value); names(table)[1] <- "response_flex"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/response_flex_table.pdf", bs_theme = "flatly")

ggplot(response_flex, aes(x=value, fill=value)) + labs(x="response_flex", fill= "response_flex") + 
  geom_bar() + theme_minimal() + scale_fill_tableau()
ggsave(file = "lit_output/figures/response_flex.svg", units = "cm", dpi = "retina", width =20, height = 10)

#'--------------
#' *landscape_group*

landscape_group <- as_tibble(unlist(stringr::str_split(data$landscape_group, "; "))) %>% filter(value != "NA")

table <- landscape_group %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(landscape_group),2)) %>% arrange(n)
table; landscape_group$value <- factor(landscape_group$value, levels = table$value); names(table)[1] <- "landscape_group"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/landscape_group_table.pdf", bs_theme = "flatly")

ggplot(landscape_group, aes(x=value, fill=value)) + labs(x="landscape_group", fill= "landscape_group") +  
  geom_bar() + theme_minimal() + scale_fill_ptol()
ggsave(file = "lit_output/figures/landscape_group.svg", units = "cm", dpi = "retina", width =20, height = 10)

#'--------------
#' *landscape_type*

landscape_type <- as_tibble(unlist(stringr::str_split(data$landscape_type, "; "))) %>% filter(value != "NA")

table <- landscape_type %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(landscape_type),2)) %>% arrange(n)
table; landscape_type$value <- factor(landscape_type$value, levels = table$value); names(table)[1] <- "landscape_type"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/landscape_type_table.pdf", bs_theme = "flatly")

ggplot(landscape_type, aes(x=value, fill=value)) + labs(x="landscape_type", fill= "landscape_type") + 
  geom_bar() + theme + scale_fill_viridis_d(option = "D")  + theme(axis.text.x=element_blank()) + 
  labs(x="", y="Count", fill="Landscape categories")
ggsave(file = "lit_output/figures/landscape_type.svg", units = "cm", dpi = "retina", width =20, height = 10)
ggsave(file = "lit_output/figures/landscape_type.png", units = "cm", dpi = "retina", width =20, height = 10)

#'--------------
#' *change_category*

change_category <- as_tibble(unlist(stringr::str_split(data$change_category, "; "))) %>% filter(value != "NA")

table <- change_category %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(change_category),2)) %>% arrange(n)
table; change_category$value <- factor(change_category$value, levels = table$value); names(table)[1] <- "change type"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/change_category_table.pdf", bs_theme = "flatly")

ggplot(change_category, aes(x=value, fill=value)) + labs(x="change type") +
  geom_bar() + theme_minimal() + scale_fill_viridis_d(option = "A") + theme(axis.text.x=element_blank())
ggsave(file = "lit_output/figures/change_category.svg", units = "cm", dpi = "retina", width =20, height = 10)

#'--------------
#' *study_spatial_scale*

study_spatial_scale <- as_tibble(unlist(stringr::str_split(data$study_spatial_scale, "; "))) %>% filter(value != "NA")

table <- study_spatial_scale %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(study_spatial_scale),2)) %>% arrange(n)
table; study_spatial_scale$value <- factor(study_spatial_scale$value, levels = table$value); names(table)[1] <- "spatial scale"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/study_spatial_scale_table.pdf", bs_theme = "flatly")

ggplot(study_spatial_scale, aes(x=value, fill=value)) + labs(x="spatial scale", fill="spatial scale") +
  geom_bar() + theme_minimal() + scale_fill_viridis_d(option = "E") + theme(axis.text.x=element_blank())
ggsave(file = "lit_output/figures/study_spatial_scale.svg", units = "cm", dpi = "retina", width =20, height = 10)

#'--------------
#' *method*

method <- as_tibble(unlist(stringr::str_split(data$method, "; "))) %>% filter(value != "NA")

table <- method %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(method),2)) %>% arrange(n)
table; method$value <- factor(method$value, levels = table$value); names(table)[1] <- "method"
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/method_table.pdf", bs_theme = "flatly")

ggplot(method, aes(x=value, fill=value)) + labs(x="method", fill= "method") +
  geom_bar() + theme_minimal() + theme(axis.text.x=element_blank())
ggsave(file = "lit_output/figures/method.svg", units = "cm", dpi = "retina", width =40, height = 20)

#'--------------
#' *delay_type*

delay_type <- as_tibble(unlist(stringr::str_split(data$delay_type, "; "))) %>% filter(value != "NA")

table <- delay_type %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(delay_type),2)) %>% arrange(n)
table; delay_type$value <- factor(delay_type$value, levels = table$value)
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/delay_type_table.pdf", bs_theme = "flatly")

ggplot(delay_type, aes(x=value, fill=value)) + labs(x="delay type", fill= "delay type") +
  geom_bar() + theme_minimal() + scale_fill_pander() + theme(axis.text.x=element_blank())
ggsave(file = "lit_output/figures/delay_type.svg", units = "cm", dpi = "retina", width =15, height = 10)

#'--------------
#' *delay_quantified*

delay_quantified <- as_tibble(unlist(stringr::str_split(data$delay_quantified, "; "))) %>% filter(value != "NA")

table <- delay_quantified %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(delay_quantified),2)) %>% arrange(n)
table; delay_quantified$value <- factor(delay_quantified$value, levels = table$value)
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/delay_quantified_table.pdf", bs_theme = "flatly")

ggplot(delay_quantified, aes(x=value, fill=value)) + 
  geom_bar() + theme_minimal() + scale_fill_excel_new() + theme(legend.position = "none") + labs(x="delay_quantified")
ggsave(file = "lit_output/figures/delay_quantified.svg", units = "cm", dpi = "retina", width =15, height = 10)

#'--------------
#' *delay_direction*

delay_direction <- as_tibble(unlist(stringr::str_split(data$delay_direction, "; "))) %>% filter(value != "NA")

table <- delay_direction %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(delay_direction),2)) %>% arrange(n); table
delay_direction$value <- factor(delay_direction$value, levels = table$value)
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/delay_direction_table.pdf", bs_theme = "flatly")

ggplot(delay_direction, aes(x=value, fill=value)) + 
  geom_bar() + theme_minimal()  + scale_fill_canva() + theme(legend.position = "none")  + labs(x="delay_directionality")
ggsave(file = "lit_output/figures/delay_direction.svg", units = "cm", dpi = "retina", width =15, height = 10)

#'--------------
#' *marking*

mark <- as_tibble(unlist(stringr::str_split(data$mark_0_to_3, "; "))) %>% filter(value != "NA")

table <- mark %>% group_by(value) %>% summarise(n = n(), ratio = round(n()/nrow(mark),2)) %>% arrange(n)
kable(table, "html") %>% kable_styling("striped", font_size = 30, full_width = F, htmltable_class = 'lightable-classic-2') %>% 
  save_kable(file="lit_output/figures/mark_table.pdf", bs_theme = "flatly")

ggplot(mark, aes(x=value, fill=value)) + 
  geom_bar() + theme_minimal() + scale_fill_wsj() + theme(legend.position = "none") + labs(x="mark")
ggsave(file = "lit_output/figures/mark.svg", units = "cm", dpi = "retina", width =15, height = 10)

#'--------------
#' *marking*

quantified <- data %>% filter(delay_quantified == "yes")
