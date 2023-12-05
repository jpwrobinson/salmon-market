source('scripts/00_source.R')

# Household consumption data - UK gov
# https://www.gov.uk/government/statistical-data-sets/family-food-datasets
drops<-c('White fish, dried, salted or smoked', 'Blue fish, dried or salted or smoked', "Herrings and other blue fish, fresh, chilled or frozen")
con<-read_excel('data/defra_avg_weekly_perperson_household_consumption_g.xlsx') %>% 
    clean_names() %>% 
    pivot_longer(-c(product, unit), names_to = 'year', values_to = 'grams') %>% 
    mutate(year = as.numeric(str_replace_all(year, 'x', ''))) %>% 
    filter(!product %in% drops)


ggplot(con, aes(year, grams, col=product)) + geom_line() +
    geom_text(data = con %>% filter(year == 2022), aes(label = product), size=3, hjust=0, nudge_x = 0.25) +
    theme(legend.position = 'none') +
    scale_x_continuous(limits=c(1974, 2028), expand=c(0,0)) +
    labs(x='', y = 'weekly consumption per person, g')

## option to do this by income decile, which shows salmon consumption in wealthier households is double that of poorest households
cond<-read_excel('data/defra_avg_weekly_perperson_household_consumption_g_deciles.xlsx') %>% 
    clean_names() %>% 
    pivot_longer(-c(food_group), names_to = 'decile', values_to = 'grams') %>% 
    mutate(decile = as.numeric(str_replace_all(decile, 'decile_', ''))) %>% 
    filter(!food_group %in% drops)

ggplot(cond %>% filter(!food_group=='Total fish'), aes(decile, grams, fill=food_group)) + 
    geom_area()  +
    labs(x='Wealth decile', y = 'weekly consumption per person, g') +
    scale_x_continuous(expand=c(0.05,0.05),breaks=1:10, limits=c(1, 10))

