library(tidyverse)
library(janitor)
library(cowplot)
library(magrittr)
# theme_set(funk::theme_sleek())

# dat
chi<-read.csv('data/ons/fresh_chicken_per_kgseries-070723.csv', skip=7) %>% 
      set_colnames(c("year", 'gbp_kg')) %>% 
    filter(!str_length(year) > 4) %>% 
    mutate(food = 'chicken')
beef<-read.csv('data/ons/beefmince_kg_series-070723.csv', skip=7) %>% 
      set_colnames(c("year", 'gbp_kg')) %>% 
    filter(!str_length(year) > 4) %>%
    mutate(food = 'beef')
salm<-read.csv('data/ons/salmon_fillets_kg_series-070723.csv', skip=7) %>% 
      set_colnames(c("year", 'gbp_kg')) %>% 
    filter(!str_length(year) > 4) %>% 
    filter(year > 2001) %>% 
    mutate(food = 'salmon')
white<-read.csv('data/ons/whitefish_fillets_series-070723.csv', skip=7) %>% 
    set_colnames(c("year", 'gbp_kg')) %>% 
    filter(!str_length(year) > 4) %>% 
    mutate(food = 'whitefish')

names(white)
price<-rbind(chi, beef, salm, white) %>% 
    mutate(year = as.Date(year, '%Y'))

ggplot(price, aes(year, gbp_kg/1000, col=food)) + 
    geom_line(aes(group=food)) +
    labs(x = '', y = '£ per kg') +
    scale_x_date(limits=as.Date(c('2000-01-01', '2023-12-12')), expand=c(0,0))


## ONS food basket data: 2018-2024 
# https://www.ons.gov.uk/economy/inflationandpriceindices/articles/shoppingpricescomparisontool/2023-05-03

ids<-c(211101, 211105, 211106, 211207, 211211, 210406, 210506, 210703, 210910, 211014) 

code<-read.csv('data/price/food_codes_ONS.csv') %>% 
    clean_names() %>% 
    filter(item_id %in% ids)

price<-read.csv('data/price/avg_price_all_foods_ONS.csv') %>% 
    clean_names() %>% 
    filter(item_id %in% ids) %>% 
    pivot_longer(-item_id, names_to = 'date', values_to = 'price') %>% 
    mutate(date = as.Date(str_replace_all(str_replace_all(date, 'x', ''), '_', '-'))) %>% 
    left_join(code %>% select(-(item_start:coicop3), -category1))

# ggplot(price, aes(date, price, col=item_desc)) + geom_line()

price_mean<-price %>% group_by(item_id, item_desc, category2, coicop5) %>% 
    summarise(price = mean(price)) %>% 
    mutate(price = case_when(item_id == 211207 ~ price * 1000 / 165, ## correct tuna assuming price is for midpoint weight (165g)
                             item_id == 211211 ~ price * 1000 / 475, ## correct Frozen breaded/battered white fish assuming price is for midpoint weight (475g)
                             .default = price)) 

gprice<-ggplot(price_mean, aes(fct_reorder(item_desc, price), price, fill=item_desc)) + 
    geom_col() + 
    coord_flip() +
    scale_y_continuous(expand=c(0,0)) +
    labs(x = '', y = '£ per kg (average 2018-2024)') +
    scale_fill_manual(values=col_foods) +
    guides(fill='none')
