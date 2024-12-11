## National diet survey data
# Citation: University of Cambridge, MRC Epidemiology Unit, NatCen Social Research. (2023). National Diet and Nutrition Survey Years 1-11, 2008-2019. [data collection]. 19th Edition. UK Data Service. SN: 6533, DOI: http://doi.org/10.5255/UKDA-SN-6533-19
# https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6533

trans <- read.csv("data/ndns_foodlevel/CommercialSpecies_translations.csv", sep=",") %>% 
  clean_names() %>% 
    mutate(food_name = unique_product, species = revised_mcs) %>% 
  select(-unique_product, -data_supplier, -revised_mcs)

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
#     # read and add total n by adult/child, then filter to seafood commodities (33, 34, 35 food codes = oily, white and shellfish)
#     d<-read_delim(paste0('data/ndns_foodlevel/', files[i])) %>% clean_names() %>%
#         rename_with(~'age', starts_with('age')) %>%
#         mutate(pop = ifelse(age < 19, "Child", "Adult")) %>%
#         group_by(pop, survey_year) %>%
#         mutate(total_surveys = n_distinct(seriali)) %>%
#         filter(main_food_group_code %in% c(33:35)) %>%
#         select(seriali, survey_year, age, sex, pop, total_surveys, country, food_name, white_fishg, oily_fishg, canned_tunag, shellfishg)
#     dd<-rbind(dd, d %>% as.data.frame)
# }
# 
# # bring in species names, identify canned salmon (wild Pacific sp.)
# # Calculate daily consumption bc survey is recall on 4 days
# reds<-c('SALMON RED CANNED IN BRINE FISH ONLY', 'SALMON, PINK, CANNED IN BRINE FISH ONLY', 'SALMON UNSPECIFIED CANNED IN BRINE FISH ONLY', 'SALMON RED CANNED IN BRINE FISH AND BONES',
#         'SALMON UNSPECIFIED CANNED IN BRINE WITH BONES', 'SALMON, RED, CANNED IN BRINE, SKINLESS AND BONELESS', 'SALMON, RED, CANNED IN BRINE, NOT SKINLESS AND BONELESS')
# 
# df<-dd %>% left_join(trans) %>%
#     mutate(
#            tot_seafood = white_fishg + oily_fishg + canned_tunag + shellfishg,
#            grams_day = tot_seafood/ 4,
#            grams_week = grams_day * 7,
#            species2 = ifelse(food_name %in% reds, 'Pacific salmon', species),
#            year_n = as.numeric(str_replace_all(survey_year, 'NDNS Year\\ ', '')) + 2007,
#            year_c = paste(year_n, '-', year_n+1))
# 
# write.csv(df, file = 'data/ndns_clean.csv')

df<-read.csv('data/ndns_clean.csv') %>% 
    filter(pop =='Adult')

# write food names to identify salmon products
# df %>% distinct(food_name, species) %>% write.csv('data/ndns_foodlevel/ndns_seafood_species.csv')

# relative frequency of food names by species
df %>% filter(species2 %in% tops) %>% 
    group_by(species2, food_name) %>% 
    summarise(tot_sp = sum(grams_day)) %>% 
    group_by(species2) %>% 
    mutate(prop = tot_sp / sum(tot_sp)) %>% 
    ggplot(aes(food_name, prop)) +
    geom_col() + 
    facet_wrap(~species2, scales='free') + 
    coord_flip()

## annual portion sizes by seafood
seafood_ann<-df %>% 
    group_by(year_n, pop, sex, species2, seriali) %>% 
    summarise(grams_day = sum(grams_day)) %>% 
    group_by(seriali) %>% 
    mutate(tot_seafood = sum(grams_day)*7) %>% ungroup() %>% 
    mutate(prop = grams_day / tot_seafood)

## annual frequency of seafood consumption, relative to all surveys
freq_of_fish<-df %>% 
    group_by(year_n, pop, seriali) %>% 
    mutate(grams_day = sum(grams_day)) %>% 
    group_by(year_n, pop, total_surveys) %>% 
    summarise(n = n_distinct(seriali), grams_day = median(grams_day)) %>% 
    ungroup() %>% rowwise() %>% 
    mutate(prop = n / total_surveys)

freq_of_all<-df %>% 
    group_by(year_n, pop, species2, seriali) %>% 
    mutate(grams_day = sum(grams_day)) %>% 
    group_by(year_n, pop, total_surveys, species2) %>% 
    summarise(n = n_distinct(seriali), grams_day = median(grams_day)) %>% 
    ungroup() %>% rowwise() %>% 
    mutate(prop = n / total_surveys)

div<-df %>% 
    group_by(year_n, pop, seriali) %>% 
    mutate(div = n_distinct(species2)) %>% 
    group_by(year_n, pop) %>% 
    summarise(div = mean(div))

## take species with at least 5% of population consumption (the big 5!)
tops<-freq_of_all %>% group_by(species2) %>% summarise(maxer =max(prop)) %>% 
    filter(maxer >= 0.05) %>% pull(species2)

tops_1pc<-freq_of_all %>% group_by(species2) %>% summarise(maxer =max(prop)) %>% 
    filter(maxer >= 0.01) %>% pull(species2)

freq_of_all$sp_gr<-ifelse(freq_of_all$species2 %in% tops_1pc, freq_of_all$species2, 'Other')

## fewer people are eating fish (63% to 60%), but portion sizes are not changing
ggplot(freq_of_fish, aes(year_n, prop)) + geom_line() 
ggplot(freq_of_fish, aes(year_n, grams_day*7)) + geom_line() 
ggplot(freq_of_all %>% filter(species2%in%tops), aes(grams_day, prop)) + geom_point() + facet_grid(~species2)

# proportion of species in sefaood diets by weight
ggplot(seafood_ann %>% filter(species2 %in% tops) %>% 
           group_by(year_n,species2) %>% 
           summarise(grams_day=median(grams_day)), 
       aes(as.factor(year_n), grams_day)) + 
    geom_bar(stat='identity') +
    facet_wrap(~species2)
    
# proportion of salmon in seafood diets, by weight
ggplot(seafood_ann %>% filter(species2=='Salmon'), aes(as.factor(year_n), prop)) + 
    geom_boxplot() 

## proportion of salmon in all diets, frequency. more seafood eaters are eating salmon
ggplot(freq_of_all %>% filter(species2=='Salmon'), aes(as.factor(year_n), prop)) + 
    geom_bar(stat='identity') +
    labs(y = 'Proportion of salmon consumption', x = '') +
    scale_y_continuous(labels = scales::percent_format())

## don't stack areas because individuals can eat multiple species
## doesn't scan - how is salmon eaters in total population = 20% AND salmon eaters in seafood population = 20%??

## proportion of fish in diets, frequency
ggplot(freq_of_all %>% filter(species2 %in% tops), aes((year_n), prop, col=species2)) + 
    geom_line() + 
    labs(y = 'Proportion of diets', x = '') +
    scale_y_continuous(labels = scales::percent_format())

# diversity of seafoods
ggplot(div, aes(year_n, div)) + geom_line()


## estimate the average seafood diet
## annual frequency of seafood consumption, within seafood eaters only
freq_of_pisci<-df %>% 
    group_by(year_n, pop, species2, seriali) %>% 
    summarise(grams_day = sum(grams_day)) %>% 
    group_by(year_n, pop, species2) %>% 
    summarise(n = n_distinct(seriali), grams_day = mean(grams_day)) %>% 
    group_by(year_n, pop) %>% 
    mutate(tot_n = sum(n)) %>% ungroup() %>% 
    mutate(prop = n / tot_n,
           prop_weight = prop * grams_day) %>% 
    group_by(year_n) %>% 
    mutate(total_prop_weight = sum(prop_weight),
           scaler = prop_weight / total_prop_weight) %>% 
    ungroup()

topper<-freq_of_pisci %>% filter(scaler >= 0.05) %>% distinct(species2) %>% pull(species2)

plotter<-freq_of_pisci %>% mutate(cat = ifelse(species2 %in% topper, species2, 'Other')) %>% 
    group_by(year_n, cat) %>% 
    summarise(scaler = sum(scaler)) %>% 
    mutate(lab = paste0(cat,' ', round(scaler*100, 1), '%'))

# sort levels
# plotter$species2<-factor(plotter$species2, levels=rev(c('Tuna', 'Salmon', 'Cod', 'Prawn', 'Haddock', 'Mackerel', 'Fish fingers', 'Fishcake')))

# labber
labber<-plotter %>% filter(year_n == 2009) %>% 
   arrange(desc(cat)) %>% 
   mutate(pos =cumsum(scaler) - 0.5*scaler)

g_diet_prop<-ggplot(plotter, aes((year_n), scaler, fill=cat, group=cat)) + 
    geom_area() +
    # geom_hline(yintercept=0.5, linetype=5, col='grey90') +
    geom_text(data = labber,
              aes(2009, pos, label = cat), col='white',size=3) +
    labs(y = 'Weighted proportion of diets', x = '') +
    theme(legend.position = 'none',
          axis.title.y.right = element_blank()) + 
    scale_fill_manual(values = sp_cols) +
    scale_y_continuous(labels = scales::percent_format(), expand=c(0,0), sec.axis = dup_axis()) +
    scale_x_continuous(expand=c(0,0), breaks=seq(2008, 2020, by = 2))

g_diet_side<-ggplot(plotter %>% filter(year_n %in% c(2008, 2018)), aes(year_n, scaler, fill=cat)) +
    geom_line(aes(group=cat), col='grey') +
    geom_point(col='black', pch=21, size=3) +
    scale_fill_manual(values = sp_cols) +
    scale_y_continuous(labels = plotter %>% filter(year_n==2008) %>% pull(lab),
                       breaks = plotter %>% filter(year_n==2008) %>% pull(scaler),
                       expand=c(0,0), 
                       sec.axis = dup_axis(
                           labels = plotter %>% filter(year_n==2018) %>% pull(lab),
                           breaks = plotter %>% filter(year_n==2018) %>% pull(scaler)
                       )) +
    scale_x_continuous(expand=c(0.05,0.05), breaks=c(2008, 2018)) +
    labs(x= '', y ='') +
    theme(legend.position = 'none',
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line=element_blank(),
          # axis.text.y = element_blank(),
          axis.ticks = element_blank()
    ) + coord_cartesian(clip='off')

g_sp_freq<-ggplot(freq_of_all %>% filter(species2 %in% tops), aes((year_n), prop, col=species2)) + 
    geom_line() + 
    labs(y = 'Proportion of diets', x = '') +
    geom_text(data = freq_of_all %>% filter(species2 %in% tops & year_n == 2018),
              aes(2018.5, prop, label = species2, col=species2),size=2.5) +
    scale_colour_manual(values = sp_cols) + 
    theme(legend.position = 'none' 
          # axis.title.y = element_blank(),
          # axis.title.y.right = element_text()
          ) + 
    scale_y_continuous(labels = scales::percent_format(), expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0), breaks=seq(2008, 2018, by = 2), limits=c(2008, 2019))


