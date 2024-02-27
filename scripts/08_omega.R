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

## filter foods of interest (from master excel)
## uk items
focs<-c(#'16-175',	## Herring, flesh only, raw
        '16-372',   ## Cod, flesh only, raw
        '16-375',	## Haddock, flesh only, raw
        '16-387',	## Prawns, king, raw
        '16-356',	## Salmon, farmed, flesh only, raw
        '16-393',	## Mackerel, flesh only, raw
        '16-399'	## Tuna, flesh only, raw
)

focs_lab<-data.frame(food_code = focs, 
                     food_lab = c('Cod', 'Haddock', 'Prawn', 'Salmon', 'Mackerel', 'Tuna'))

uk_nut_vals<-om %>% filter(food_code %in% focs) %>% 
    left_join(focs_lab)

ggplot(uk_nut_vals, aes(food_lab, ))