source('scripts/00_source.R')
library(tidyverse)
library(readxl)

## domestic production (tonnes live weight)
prod<-read.csv('data/fao_global_salmon_production.csv') %>% 
  janitor::clean_names() %>% 
  select(-starts_with('s')) %>% 
  pivot_longer(-c(country_name:unit), names_to = 'year', values_to = 'tonnes') %>% 
  mutate(country_name = recode(country_name,
                               'United Kingdom' = 'Scotland', 
                               'United States of America' = 'USA'),
         year = as.numeric(str_replace_all(year, 'x_', ''))) %>% 
  group_by(country_name, country, asfis_species_name, unit, year) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  filter(country=='Scotland')

## exports (thousand metric tonnes) https://www.statista.com/statistics/316401/salmon-export-volume-in-the-united-kingdom-uk/
ex<-read_excel('data/statista/statistic_id316401_salmon-export-volume-in-the-united-kingdom--uk--2003-2022.xlsx', sheet=3)

## imports (thousand metric tonnes) https://www.statista.com/statistics/316413/salmon-import-volume-in-the-united-kingdom-uk/
im<-read_excel('data/statista/statistic_id316413_salmon-import-volume-in-the-united-kingdom--uk--2006-2022.xlsx', sheet=3)

## match imp and ex
prod$export<-ex$tonnes[match(prod$year,ex$year)] * 1000
prod$import<-im$tonnes[match(prod$year,im$year)] * 1000

# edible portion of produced
prod$edible_prod<-prod$tonnes * 0.88

# dom supply
prod<-prod %>% mutate(domestic = edible_prod - export + import,
                      year = as.numeric(year)) %>% 
              ungroup() %>% 
              filter(!is.na(domestic))

prodl<-prod %>% select(year, edible_prod, export, import) %>% 
                pivot_longer(-c(year, export), names_to = 'source', values_to = 'tonnes')

ga<-ggplot(prod, aes(year, domestic)) + 
    geom_line() + 
    labs(x = '', y = 'UK supply, t') +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks=seq(2000, 2022, by = 3))

ga

ggplot(prodl, aes(year, tonnes, fill=source)) + 
    geom_area() + 
    geom_line(aes(year, export)) +
    labs(x = '', y = 'UK supply, t') +
    scale_y_continuous(labels = scales::comma, expand=c(0,0)) +
    scale_x_continuous(breaks=seq(2000, 2022, by = 3), expand=c(0,0))

