
## domestic production (tonnes live weight)
prod<-read.csv('data/fao_global_salmon_production.csv') %>% 
  janitor::clean_names() %>% 
  select(-starts_with('s')) %>% 
  pivot_longer(-c(country_name:unit), names_to = 'year', values_to = 'tonnes') %>% 
  mutate(country_name = recode(country_name,
                               'United Kingdom' = 'Scotland', 
                               'United States of America' = 'USA'),
         year = as.numeric(str_replace_all(year, 'x_', '')),
         country=ifelse(country_name %in% facs, country_name, 'Other')) %>% 
  group_by(country_name, country, asfis_species_name, unit, year) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  filter(country=='Scotland')

## exports (tonnes) https://www.statista.com/statistics/316401/salmon-export-volume-in-the-united-kingdom-uk/

## imports (tonnes) https://www.statista.com/statistics/316413/salmon-import-volume-in-the-united-kingdom-uk/

## apparent consumption

# edible portion?