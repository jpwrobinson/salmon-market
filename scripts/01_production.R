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

g1<-ggplot(site, aes(date, fill=producing_in_last_3_years)) + geom_bar() +
    labs(x = '', y ='Number of sites registered') +
    theme(legend.position = c(0.8, 0.8))
g2<-ggplot(site, aes(operator_fac, fill=producing_in_last_3_years)) + geom_bar() +
    labs(x = '', y ='Number of sites registered') +
    theme(legend.position = 'none', legend.title = element_blank())
plot_grid(g1, g2)

cumu<-site %>% filter(producing_in_last_3_years == 'Yes') %>% 
    group_by(date) %>% 
    summarise(n = n_distinct(marine_scotland_site_id)) %>% 
    ungroup() %>% 
    mutate(n_cum = cumsum(n))

g_salm_sites<-ggplot(cumu, aes(date, n_cum)) + 
    geom_line() +
    labs(x = '', y ='Number of active farms') +
    theme(legend.position = 'none', legend.title = element_blank())