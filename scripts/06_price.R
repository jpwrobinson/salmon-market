library(tidyverse)
library(cowplot)
library(magrittr)
theme_set(funk::theme_sleek())

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
