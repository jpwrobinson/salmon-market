source('scripts/00_source.R')

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
