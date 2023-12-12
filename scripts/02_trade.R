source('scripts/00_source.R')


trade<-read.csv('data/Seafish_trade_data.csv') %>% 
    clean_names() %>% 
    group_by(species, flow, species_group, year, destination, origin) %>% 
    summarise(value = sum(value), weight_t = sum(weight_tonnes))

trade_top5_byvalue<-trade %>% group_by(species) %>% summarise(value = sum(value)) %>% 
    mutate(species2 = fct_lump_n(species, n = 5, w = value)) %>% 
    filter(species2 != 'Other') %>% distinct(species)

trade_top5_byweight<-trade %>% group_by(species) %>% summarise(weight_t = sum(weight_t)) %>% 
    mutate(species2 = fct_lump_n(species, n = 5, w = weight_t)) %>% 
    filter(species2 != 'Other') %>% distinct(species)

trade_s<-trade %>% mutate(species2 = ifelse(species %in% trade_top5_byvalue$species, species, 'Other')) %>% 
    group_by(species2, flow, year) %>% 
    summarise(value = sum(value), weight_t = sum(weight_t))

trade_s_t<-trade %>% mutate(species2 = ifelse(species %in% trade_top5_byweight$species, species, 'Other')) %>% 
    group_by(species2, flow, year) %>% 
    summarise(value = sum(value), weight_t = sum(weight_t))

g_val<-ggplot(trade_s %>% filter(species2 != 'Other'), aes(year, value/1e6, col=species2)) +
    geom_line() +
    geom_point() +
    labs(x = '', y = 'Exports, £M') +
    geom_text(data = trade_s %>% filter(year == 2022 & species2 !='Other'),
              aes(x = 2022.25, y = value/1e6, label = species2), hjust=0, size=3.5) +
    guides(colour='none') +
    scale_x_continuous(limits=c(2010, 2024), breaks=seq(2010, 2022, by = 2), expand=c(0,0))  +
    scale_colour_manual(values = sp_cols)

g_ton<-ggplot(trade_s_t %>% filter(species2 != 'Other'), aes(year, weight_t, col=species2)) +
    geom_line() +
    geom_point() +
    labs(x = '', y = 'Exports, t') +
    geom_text(data = trade_s_t %>% filter(year == 2022 & species2 !='Other'),
              aes(x = 2022.25, y = weight_t, label = species2), hjust=0, size=3.5) +
    guides(colour='none') +
    scale_x_continuous(limits=c(2010, 2024), breaks=seq(2010, 2022, by = 2), expand=c(0,0)) +
    scale_y_continuous(labels=scales::comma) +
    scale_colour_manual(values = sp_cols)


## summmary stats
trade_s %>% group_by(flow, year) %>% summarise(value = sum(value)/1e6) %>% filter(year == 2022) # £1718 million
trade_s %>% filter(year == 2022 & species2=='Salmon') %>% summarise(value / 1e6) # £701 million

701/1718 # salmon was 41% of seafood exported by value in 2023
