source('scripts/00_source.R')

# Household consumption data - UK gov
# https://www.gov.uk/government/statistical-data-sets/family-food-datasets
drops<-c('White fish, dried, salted or smoked', 'Blue fish, dried or salted or smoked', "Herrings and other blue fish, fresh, chilled or frozen",
         'Salmon, tinned', 'Ready meals and other fish products - frozen or not frozen', 'Shellfish',
         'Takeaway fish', 'White fish, fresh, chilled or frozen')
fish<-c('White fish, dried, salted or smoked','White fish, fresh, chilled or frozen', 'Blue fish, dried or salted or smoked', "Herrings and other blue fish, fresh, chilled or frozen",
        'Salmon, fresh, chilled or frozen','Takeaway fish', 'Salmon, tinned', 'Ready meals and other fish products - frozen or not frozen',
        'Other tinned or bottled fish', 'Shellfish')

con<-read_excel('data/defra_household_cons/defra_avg_weekly_perperson_household_consumption_g.xlsx') %>% 
    clean_names() %>% 
    pivot_longer(-c(product, unit), names_to = 'year', values_to = 'grams') %>% 
    mutate(year = as.numeric(str_replace_all(year, 'x', '')),
           cat = ifelse(product %in% fish, 'Fish', 'Not fish')) %>% 
    mutate(name = case_match(product, 
                         'White fish, dried, salted or smoked' ~ 'White fish (processed)',
                         'Blue fish, dried or salted or smoked' ~ 'Blue fish (processed)',
                         'Herrings and other blue fish, fresh, chilled or frozen' ~ 'Blue fish (fresh/frozen)',
                         'White fish, fresh, chilled or frozen' ~ 'White fish (fresh/frozen)',
                         'Salmon, fresh, chilled or frozen' ~ 'Farmed salmon',
                         'Ready meals and other fish products - frozen or not frozen' ~ 'Ready meals',
                         'Salmon, tinned' ~ 'Salmon (tinned)',
                         'Beef and veal' ~ 'Beef',
                         'Other tinned or bottled fish' ~ 'Other tinned fish',
                         "Chicken, uncooked - whole chicken or chicken pieces" ~ 'Chicken',
                         .default = product),
           name=str_wrap(name, width=21),
           facet = ifelse(!product %in% c(fish), 'a_meat', 'fish'))

g_cons_meat<-ggplot(con %>% filter(!product %in% drops), aes(year, grams, col=product)) + geom_line() +
    geom_text(data = con %>% filter(year == 2022 & !product %in% drops), aes(label = name), size=2.5, hjust=0, nudge_x = 0.25) +
    theme(legend.position = 'none') +
    scale_x_continuous(limits=c(1974, 2028), expand=c(0,0)) +
    labs(x='', y = 'weekly consumption per person, g')

g_cons_fish<-ggplot(con %>% filter(cat =='Fish' & product!='Total fish'), aes(year, grams, col=product)) + geom_line() +
    geom_text(data = con %>% filter(year == 2022 & cat=='Fish' & product!='Total fish'), aes(label = name), size=3, hjust=0, nudge_x = 0.25) +
    theme(legend.position = 'none') +
    scale_x_continuous(limits=c(1974, 2028), expand=c(0,0)) +
    labs(x='', y = 'weekly consumption per person, g')

## duplicate salmon with different facet
plotter<-rbind(con, con %>% filter(product == 'Salmon, fresh, chilled or frozen') %>% mutate(facet = 'a_meat'))

g_cons_facet<-ggplot(plotter, aes(year, grams, col=product)) + geom_line() +
    geom_text(data = plotter %>% filter(year == 2022), aes(label = name), size=2.5, hjust=0, nudge_x = 0.25) +
    theme(legend.position = 'none', strip.text.y = element_blank()) +
    facet_grid(facet~., scales='free_y') +
    scale_x_continuous(limits=c(1974, 2030), breaks=seq(1975, 2020, by = 5), expand=c(0,0)) +
    labs(x='', y = 'weekly consumption per person, g')

## option to do this by income decile, which shows salmon consumption in wealthier households is double that of poorest households
cond<-read_excel('data/defra_household_cons/defra_avg_weekly_perperson_household_consumption_g_deciles.xlsx') %>% 
    clean_names() %>% 
    pivot_longer(-c(food_group), names_to = 'decile', values_to = 'grams') %>% 
    mutate(decile = as.numeric(str_replace_all(decile, 'decile_', ''))) %>% 
    filter(!food_group %in% drops)

g_cond_income<-ggplot(cond %>% filter(!food_group=='Total fish'), aes(decile, grams, fill=food_group)) + 
    geom_area()  +
    labs(x='Wealth decile', y = 'weekly consumption per person, g') +
    scale_x_continuous(expand=c(0.05,0.05),breaks=1:10, limits=c(1, 10))

