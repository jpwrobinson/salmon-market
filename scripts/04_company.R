source('scripts/00_source.R')


comp<-read.csv('data/marine_scot/Scottish Fish Farm Production Survey T34 Old - Atlantic salmon - No. of companies, production (t) staff & productivity 1993-_0.csv') %>% 
    clean_names()

comp2<-comp %>% group_by(year) %>% summarise(n = sum(number_of_companies), t = sum(total_tonnes_produced), s = sum(total_number_of_staff))
comp3<-comp %>% group_by(year, production_category_tonnes) %>% summarise(n = sum(number_of_companies), t = sum(total_tonnes_produced))

ggplot(comp, aes(year, number_of_companies, col=production_category_tonnes)) + geom_line()

ggplot(comp2, aes(year, t)) + geom_line()
ggplot(comp2, aes(year, s)) + geom_line()

ggplot(comp3, aes(year, n, fill=production_category_tonnes)) + geom_area()
ggplot(comp3, aes(year, t, col=production_category_tonnes)) + geom_line()


g_company<-ggplot(comp2, aes(year, n)) + geom_line() +
            labs(x = '', y = 'Number of companies') +
            scale_x_continuous(breaks=seq(1990, 2020, by= 5))
