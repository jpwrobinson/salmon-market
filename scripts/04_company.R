source('scripts/00_source.R')

# Marine Scotland. 2021. Scottish Fish Farm Production Survey Data. doi: 10.7489/1918-1
# https://data.marine.gov.scot/dataset/scottish-fish-farm-production-survey-data
comp<-read.csv('data/marine_scot/Scottish Fish Farm Production Survey T34 Old - Atlantic salmon - No. of companies, production (t) staff & productivity 1993-_0.csv') %>% 
    clean_names()

comp2<-comp %>% group_by(year) %>% summarise(n = sum(number_of_companies), t = sum(total_tonnes_produced), s = sum(total_number_of_staff))
comp3<-comp %>% group_by(year, production_category_tonnes) %>% summarise(n = sum(number_of_companies), t = sum(total_tonnes_produced))

# ggplot(comp, aes(year, number_of_companies, col=production_category_tonnes)) + geom_line()
# 
# ggplot(comp2, aes(year, t)) + geom_line()
# ggplot(comp2, aes(year, s)) + geom_line()
# 
# ggplot(comp3, aes(year, n, fill=production_category_tonnes)) + geom_area()
# ggplot(comp3, aes(year, t, col=production_category_tonnes)) + geom_line()


g_company<-ggplot(comp2, aes(year, n)) + geom_line() +
            labs(x = '', y = 'N companies') +
            scale_x_continuous(breaks=seq(1990, 2020, by= 5))

## company names (note these are gutted production weight, so different to comp dataset from Scot gov (whole fish weight))
mowi<-data.frame(comp = c('Mowi', 'Scottish Sea Farms', 'Cooke', 'Bakkafrost', 'Other'),
                 prod = c(48000, 36000, 28000, 24000, 12500))

mowi<-mowi %>% 
    mutate(prop = prod / 148500) %>% 
    arrange(prod) %>%
    mutate(label_ypos=1-(cumsum(prop) - 0.5*prop))

mowi$comp<-factor(mowi$comp, levels=mowi$comp)

g_mowi<-ggplot(mowi, aes(x=2, prop)) + 
    geom_bar(stat = 'identity', aes(fill=comp), col='transparent', width=0.5) +
    ggrepel::geom_text_repel(aes(y = label_ypos, label = comp, col=comp), size=2) +
    scale_fill_manual(values=comp_cols) +
    scale_colour_manual(values=comp_cols) +
    xlim(1, 3) +
    coord_polar(theta='y', start=0, direction = 1, clip='off') +
    theme_void(base_size=0) +
    theme(legend.position = 'none') 

