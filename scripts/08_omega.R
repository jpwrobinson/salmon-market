library(tidyverse)
library(readxl)
library(janitor)

om<-read_excel('data/mccance_widdowsons_selected.xlsx', sheet=3) %>% 
    clean_names() %>% 
    select(food_code, food_name, c22_6_100g_food_g, c20_5_100g_food_g) %>% 
    mutate(across(c22_6_100g_food_g:c20_5_100g_food_g, as.numeric)) %>% 
    mutate(across(c22_6_100g_food_g:c20_5_100g_food_g, ~ replace_na(., 0))) %>% 
    mutate(omega_3_epadha_g = c22_6_100g_food_g + c20_5_100g_food_g) %>% 
    select(food_code, food_name, omega_3_epadha_g)

# seafood
af<-om %>% filter(str_detect(food_code, '16\\-'))

ggplot(af, aes(omega_3_epadha_g)) + geom_histogram() +
  geom_text(data = af %>% filter(omega_3_epadha_g>1),size=4, hjust=0,
            aes(omega_3_epadha_g, 10, label=food_name), angle=90)

write.csv(af, file = 'data/mccance_aquatic_foods.csv', row.names=FALSE)


uk_nut_vals<-read.csv(file = 'data/mccance_aquatic_foods_labelled.csv') %>% 
  mutate(food_label2 = ifelse(food_label == 'Farmed salmon', 'Farmed salmon', 'Other wild fish'),
         food_label = ifelse(food_label == '', 'Other', food_label)) %>% 
  filter(food_label != 'Other') %>% 
  group_by(food_label) %>% 
  mutate(o3 = round(mean(omega_3_epadha_g), 1), lab = paste0(o3, ' g'))

gOmega<-ggplot(uk_nut_vals, aes(fct_reorder(food_label, omega_3_epadha_g), 
                                omega_3_epadha_g)) + 
  geom_hline(yintercept=1.1, linetype=5, col='grey') +
  geom_point(aes(fill = con), size=4, pch=21) +
  coord_flip(clip='off') +
  geom_text(data = uk_nut_vals %>% distinct(food_label, o3, lab),
            aes(food_label, label = lab, y = 3.7), hjust = -.25,size=3) +
  scale_y_continuous(expand=c(0.01,0), sec.axis = sec_axis(~., breaks=1.1, labels = 'NRV')) +
  labs(x = '', y = 'EPA and DHA, g per 100 g') +
  theme(legend.position = c(0.7, 0.4), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        plot.margin=unit(c(.1,1,.1,.1), 'cm'))
