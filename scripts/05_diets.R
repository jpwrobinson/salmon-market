

trans <- read.csv("data/ndns_foodlevel/CommercialSpecies_translations.csv", sep=",") %>% clean_names() %>% 
    mutate(food_name = unique_product, species = revised_mcs) %>% select(-unique_product, -data_supplier, -revised_mcs)

# for food code inspection
# read_delim('data/ndns_foodlevel/ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.tab') %>% 
    # distinct(MainFoodGroupDesc, MainFoodGroupCode) %>% 
        # write.csv('data/ndns_foodlevel/ndns_food_codes.csv')

### Commented out for run time! 
# Dataset = Study Number 6533 - National Diet and Nutrition Survey Years 1-11, 2008-2019
## read each food level, subset and bind
# files<-list.files('data/ndns_foodlevel/')
# files<-files[files %>% str_detect('foodleveldietarydat')]
# dd<-numeric()
# for(i in 1:length(files)){
#     # read and filter to seafood commodities (33, 34, 35 food codes = oily, white and shellfish)
#     d<-read_delim(paste0('data/ndns_foodlevel/', files[i])) %>% clean_names() %>% 
#         filter(main_food_group_code %in% c(33:35)) %>% 
#         rename_with(~'age', starts_with('age')) %>% 
#         select(seriali, survey_year, age, sex, country, food_name, white_fishg, oily_fishg, canned_tunag, shellfishg)
#     dd<-rbind(dd, d)
# }
# 
# # bring in species names, identify canned salmon (wild Pacific sp.)
# # Calculate daily consumption bc survey is recall on 4 days
# reds<-c('SALMON RED CANNED IN BRINE FISH ONLY', 'SALMON, PINK, CANNED IN BRINE FISH ONLY', 'SALMON UNSPECIFIED CANNED IN BRINE FISH ONLY', 'SALMON RED CANNED IN BRINE FISH AND BONES',
#         'SALMON UNSPECIFIED CANNED IN BRINE WITH BONES', 'SALMON, RED, CANNED IN BRINE, SKINLESS AND BONELESS', 'SALMON, RED, CANNED IN BRINE, NOT SKINLESS AND BONELESS')
# 
# df<-dd %>% left_join(trans) %>% 
#     mutate(pop = ifelse(age < 19, "Child", "Adult"),
#            tot_seafood = white_fishg + oily_fishg + canned_tunag + shellfishg,
#            grams_day = tot_seafood/ 4,
#            grams_week = grams_day * 7,
#            species2 = ifelse(food_name %in% reds, 'Pacific salmon', species),
#            year_n = as.numeric(str_replace_all(survey_year, 'NDNS Year\\ ', '')) + 2007,
#            year_c = paste(year_n, '-', year_n+1))
# 
# write.csv(df, file = 'data/ndns_clean.csv')

df<-read.csv('data/ndns_clean.csv')

# write food names to identify salmon products
# df %>% distinct(food_name, species) %>% write.csv('data/ndns_foodlevel/ndns_seafood_species.csv')


## annual values by seafood
seafood_ann<-df %>% 
    group_by(year_n, pop, sex, species2, seriali) %>% 
    summarise(grams_day = sum(grams_day)) %>% 
    group_by(seriali) %>% 
    mutate(tot_seafood = sum(grams_day)) %>% ungroup() %>% 
    mutate(prop = grams_day / tot_seafood)

freq<-df %>% 
    group_by(year_n, pop, sex, species2, seriali) %>% 
    summarise(grams_day = sum(grams_day)) %>% 
    group_by(year_n, pop, sex, species2) %>% 
    summarise(n = n_distinct(seriali)) %>% 
    group_by(year_n, pop, sex) %>% 
    mutate(tot_n = sum(n)) %>% ungroup() %>% 
    mutate(prop = n / tot_n)

se<-function (x) {sd(x)/sqrt(length(x))}
freq_mean<-freq %>% group_by(year_n, pop, species2) %>% 
    summarise(mean = mean(prop), lo = mean - 2*se(prop), hi = mean + 2*se(prop))
    

ggplot(seafood_ann %>% filter(pop=='Adult' & species2=='Salmon'), aes(as.factor(year_n), grams_day)) + 
    geom_boxplot() 

ggplot(seafood_ann %>% filter(pop=='Adult' & species2=='Salmon'), aes(as.factor(year_n), prop)) + 
    geom_boxplot() 

ggplot(freq %>% filter(pop=='Adult' & species2=='Salmon'), aes(as.factor(year_n), prop)) + 
    geom_boxplot() +
    labs(y = 'Proportion of salmon consumption', x = '') +
    scale_y_continuous(labels = scales::percent_format())

## take species with at least 5% of population consumption
tops<-freq_mean %>% group_by(species2) %>% summarise(maxer =max(mean)) %>% 
    filter(maxer >= 0.05) %>% pull(species2)


ggplot(freq_mean %>% filter(pop=='Adult' & species2 %in% tops), aes(as.factor(year_n), mean, col=species2)) + 
    geom_pointrange(aes(ymin = lo, ymax = hi)) +
    labs(y = 'Proportion of diets', x = '') +
    scale_y_continuous(labels = scales::percent_format())

# sort levels
freq_mean$species2<-factor(freq_mean$species2, levels=rev(c('Tuna', 'Salmon', 'Cod', 'Prawn', 'Haddock', 'Macekerel', 'Fish fingers', 'Fishcake')))

# labber
labber<-freq_mean %>% filter(pop=='Adult' & species2 %in% tops & year_n == 2009) %>% 
   arrange(desc(species2)) %>% 
   mutate(pos =cumsum(mean) - 0.5*mean)

g_diet_prop<-ggplot(freq_mean %>% filter(pop=='Adult'& species2 %in% tops), aes((year_n), mean, fill=species2, group=species2)) + 
    geom_area() +
    # geom_hline(yintercept=0.5, linetype=5, col='grey90') +
    geom_text(data = labber,
              aes(2009, pos, label = species2), col='white',size=3) +
    labs(y = 'Proportion of diets', x = '') +
    theme(legend.position = 'none', axis.title.y = element_blank(),
          axis.title.y.right = element_text()) + 
    scale_y_continuous(labels = scales::percent_format(), expand=c(0,0), sec.axis = dup_axis()) +
    scale_x_continuous(expand=c(0,0), breaks=seq(2008, 2020, by = 2))




    