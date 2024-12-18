source('scripts/00_source.R')

# Household consumption data - UK gov
# https:// www.gov.uk/government/statistical-data-sets/family-food-datasets
drops<-c('White fish, dried, salted or smoked', 'Blue fish, dried or salted or smoked', "Herrings and other blue fish, fresh, chilled or frozen",
         'Salmon, tinned', 'Ready meals and other fish products - frozen or not frozen', 'Shellfish',
         'Takeaway fish', 'White fish, fresh, chilled or frozen')
fish<-c('White fish, dried, salted or smoked','White fish, fresh, chilled or frozen', 'Blue fish, dried or salted or smoked', "Herrings and other blue fish, fresh, chilled or frozen",
        'Salmon, fresh, chilled or frozen','Takeaway fish', 'Salmon, tinned', 'Ready meals and other fish products - frozen or not frozen',
        'Other tinned or bottled fish', 'Shellfish')

con<-read_excel('data/defra_household_cons/defra_avg_weekly_perperson_household_consumption_g.xlsx') %>% 
    clean_names() %>% 
    pivot_longer(-c(product, unit), names_to = 'year', values_to = 'grams') %>% 
    mutate(year = str_replace_all(year, 'x', ''),
           year = as.numeric(str_split_fixed(year, '_', 2)[,1]),
           cat = ifelse(product %in% fish, 'Fish', 'Not fish')) %>% 
    mutate(name = case_match(product, 
                         'White fish, dried, salted or smoked' ~ 'White fish',
                         'Blue fish, dried or salted or smoked' ~ 'Blue fish',
                         'Herrings and other blue fish, fresh, chilled or frozen' ~ 'Blue fish',
                         'White fish, fresh, chilled or frozen' ~ 'White fish',
                         'Salmon, fresh, chilled or frozen' ~ 'Salmon',
                         'Ready meals and other fish products - frozen or not frozen' ~ 'Ready meals',
                         'Salmon, tinned' ~ 'Tinned fish',
                         'Beef and veal' ~ 'Beef',
                         'Mutton and lamb' ~ 'Lamb',
                         'Other tinned or bottled fish' ~ 'Tinned fish',
                         "Chicken, uncooked - whole chicken or chicken pieces" ~ 'Chicken',
                         .default = product),
           name=str_wrap(name, width=21),
           facet = ifelse(!product %in% c(fish), 'a_meat', 'fish')) %>% 
    group_by(name, facet, year, cat) %>% 
    summarise(grams = sum(grams))

g_cons_meat<-ggplot(con %>% filter(cat != 'Fish'), aes(year, grams, col=name)) + geom_line() +
    geom_line(data = con %>% filter(name == 'Salmon')) + 
    geom_text(data = con %>% filter(year == 2022  & name == 'Salmon'), aes(label=name), size=2.5, hjust=0, nudge_x =0.25) + 
    geom_text(data = con %>% filter(year == 2022 & cat != 'Fish'), aes(label = name), size=2.5, hjust=0, nudge_x = 0.25) +
    theme(legend.position = 'none') +
    scale_x_continuous(limits=c(1974, 2028), expand=c(0,0)) +
    scale_colour_manual(values = col_foods) +
    labs(x='', y = 'weekly consumption per person, g')

g_cons_fish<-ggplot(con %>% filter(cat =='Fish' & name!='Total fish'), aes(year, grams, col=name)) + geom_line() +
    geom_text(data = con %>% filter(year == 2022 & cat=='Fish' & name!='Total fish'), aes(label = name), 
              size=2.5, hjust=0, nudge_x = 0.25) +
    theme(legend.position = 'none') +
    scale_color_manual(values = col_foods) +
    scale_x_continuous(limits=c(1974, 2028), expand=c(0,0)) +
    labs(x='', y = 'weekly consumption per person, g')

## option to do this by income decile, which shows salmon consumption in wealthier households is double that of poorest households
cond<-read_excel('data/defra_household_cons/defra_avg_weekly_perperson_household_consumption_g_deciles.xlsx') %>% 
    clean_names() %>% 
    pivot_longer(-c(food_group), names_to = 'decile', values_to = 'grams') %>% 
    mutate(decile = as.numeric(str_replace_all(decile, 'decile_', ''))) %>% 
    mutate(name = case_match(food_group, 
                             'White fish, dried, salted or smoked' ~ 'White fish',
                             'Blue fish, dried or salted or smoked' ~ 'Blue fish',
                             'Herrings and other blue fish, fresh, chilled or frozen' ~ 'Blue fish',
                             'White fish, fresh, chilled or frozen' ~ 'White fish',
                             'Salmon, fresh, chilled or frozen' ~ 'Salmon',
                             'Ready meals and other fish products - frozen or not frozen' ~ 'Ready meals',
                             'Takeaway fish meals and fish products' ~ 'Takeaway fish',
                             'Salmon, tinned' ~ 'Tinned fish',
                             'Beef and veal' ~ 'Beef',
                             'Other tinned or bottled fish' ~ 'Tinned fish',
                             "Chicken, uncooked - whole chicken or chicken pieces" ~ 'Chicken',
                             .default = food_group)) %>% 
    group_by(decile, name) %>% 
    summarise(grams = sum(grams))
    # filter(!food_group %in% drops)

cond %>% filter(name!='Total fish') %>% group_by(decile) %>% slice_max(grams, n=3) %>% data.frame

g_cond_income<-ggplot(cond %>% filter(!name=='Total fish'), aes(decile, grams, fill=name)) + 
    geom_area()  +
    labs(x='Wealth decile', y = 'weekly consumption per person, g') +
    scale_fill_manual(values = col_foods) +
    scale_x_continuous(expand=c(0,0),breaks=1:10, limits=c(1, 10)) +
    theme(legend.title = element_blank())

cond %>% filter(decile %in% c(1,10))
