source('scripts/00_source.R')
# FAO. 2023. Fishery and Aquaculture Statistics. Global production by production source 1950-2021 (FishStatJ). In: FAO Fisheries and Aquaculture Division [online]. Rome. Updated 2023. www.fao.org/fishery/statistics/software/fishstatj/en

facs<-c('Scotland', 'Chile', 'Norway')

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
    summarise(tonnes = sum(tonnes))

# estimate growth since 2000
lab<-prod %>% filter(year %in% c(2000,2021) & country != 'Other') %>% 
    pivot_wider(names_from = 'year', values_from = 'tonnes', names_prefix = 'y_') %>% 
    mutate(chg = paste0('+', round((y_2021-y_2000) / y_2000 * 100,0),'%'))

lab<-prod %>% filter(year %in% c(2021)) %>% ungroup() %>% 
    mutate(tot = sum(tonnes), prop = paste0(round(tonnes / tot * 100, 1), '%')) %>% 
    filter(country != 'Other')

g_prod<-ggplot(prod, aes(year ,tonnes, col=country, group=country_name)) + geom_line() +
    geom_text(data = lab, x = 2021.5, aes(y=tonnes, label = country_name), size=3, hjust=0, vjust=0) +
    geom_text(data = lab, x = 2021.5, aes(y=tonnes, label = prop), size=3, hjust=0, vjust=2) +
    # geom_text(data = lab, x = 2021.5, aes(y=y_2021, label = chg), size=3, hjust=0, vjust=2) +
    # annotate('text', x = 2021.5, y = lab$y_2021[2]-1e5, label = '(2000-2020)', size=2.5, hjust=0, vjust=2, col = 'grey40') +
    annotate('text', x = 2021.5, y = lab$tonnes[2]-1e5, label = 'of total supply\nin 2021', size=2.5, hjust=0, vjust=2, col = 'grey40') +
    guides(colour='none') +
    scale_colour_manual(values=cols) +
    labs(x = '', y = 'Farmed salmon production, t') +
    scale_x_continuous(expand=c(0,0), limits=c(1980, 2026)) +
    scale_y_continuous(labels=scales::comma)


## add farm sites in Scotland
# http://aquaculture.scotland.gov.uk/data/site_details.aspx
site<-read.csv('data/ms_site_details.csv') %>% 
    clean_names() %>% 
    mutate(date = as.numeric(str_split_fixed(date_registered, '-', 3)[,3])) %>%
    filter(water_type == 'Seawater' & str_detect(species, 'Salmon')) %>% 
    mutate(operator_fac = fct_lump_lowfreq(operator))

## add yield per smolt and farm size
# https://data.marine.gov.scot/dataset/scottish-fish-farm-production-survey-data
yield<-read.csv('data/marine_scot/Scottish Fish Farm Production Survey T28 - Atlantic salmon - Survival & production in smolt year classes 1984-_0.csv') %>% 
    clean_names() %>% 
    filter(year_of_smolt_input >= 1990)

size<-read.csv('data/marine_scot/Scottish Fish Farm Production Survey T32 - Atlantic salmon - Prod. methods, capacity, tonnage and stocking densities 1984-_0.csv') %>% 
    clean_names() %>% 
    filter(year >= 1990)


gy<-ggplot(yield, aes(year_of_smolt_input, yield_per_smolt_kg)) + geom_line() +
    labs(x = '', y = 'kg') +
    scale_x_continuous(breaks=seq(1990, 2020, by= 5)) +
    annotate('text', 1990, 4.3, label='Yield per smolt',hjust=0)

gs<-ggplot(size, aes(year, seawater_cages_total_capacity_cubic_metres/1000)) + geom_line() +
    labs(x = '', y = '1,000 m3') + 
    scale_x_continuous(breaks=seq(1990, 2020, by= 5)) +
    annotate('text', 1990, 25000, label='Seawater cage capacity',hjust=0)

