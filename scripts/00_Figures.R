

source('scripts/01_production.R')
source('scripts/02_trade.R')
source('scripts/03_consumption.R')
source('scripts/04_company.R')
source('scripts/05_diets.R')
source('scripts/06_price.R')

## Fig 1 = timeline of Scottish industry
# trade
# rhs<-plot_grid(g_salm_sites, g_val, g_ton, nrow=3, labels=c('b','c', 'd'), align='hv')
bot<-plot_grid(g_val, g_ton, nrow=1, labels=c('b', 'c'), align='hv')
fig1<-plot_grid(g_prod, bot, nrow=2, labels=c('a'))

pdf(file = 'fig/Figure1.pdf', height=6, width=8)
print(fig1)
dev.off()

## number of sites in 2021
site %>% filter(producing_in_last_3_years == "Yes") %>% summarise(n_distinct(marine_scotland_site_id)) # 203
# 53 ASC certified
53 / 203 * 100

## Fig 2 =  Influence of retail and industry on farmed salmon market-making
# companies
axis.drop<-theme(axis.text.x = element_blank(),
                 plot.margin = unit(c(.1, .1, .1, .1),'cm'))
grids<-plot_grid(g_company + 
                    annotate('text', 1990, 105, label='Consolidation',hjust=0) +
                    theme_half_open(12) + axis.drop,
                gy + theme_half_open(12) + axis.drop, 
                 gs + theme_half_open(12) + theme(plot.margin = unit(c(.1, .1, .1, .1),'cm')),
                 nrow =3 , align= 'v')

fig2<-ggdraw(grids) +
    draw_plot(g_mowi, .66, .75, .3, .3) 

pdf(file = 'fig/Figure2.pdf', height=6, width=7)
print(fig2)
dev.off()

## Figure 3 | Consumption and dietary contributions of farmed salmon in the UK
house<-plot_grid(g_cons_meat, g_cons_fish, nrow=1, labels=c('a', 'b'))

bot<-plot_grid(g_diet_prop, g_diet_side, nrow=1, rel_widths=c(1, 0.6), labels=c('c', 'd'))
# top<-plot_grid(g_cons_meat, g_sp_freq, nrow=1, labels=c('a', 'b'))
top<-house

fig3<-plot_grid(top, bot, nrow=2)

pdf(file = 'fig/Figure3.pdf', height=6, width=10)
print(fig3)
dev.off()

pdf(file = 'fig/FigureS1.pdf', height=5, width=10)
print(g_cond_income)
dev.off()

## Figure 4 | Food profiles of animal-source vs salmon
pdf(file = 'fig/Figure4.pdf', height=3, width=10)
plot_grid(gprice, gOmega, labels =c('a',  'b'), nrow=1)
dev.off()

## summary stats
con %>% filter(cat =='Fish' & name!='Total fish' & year == 2022)
con %>% filter(cat =='Fish' & name!='Total fish' & year == 2022) %>% ungroup() %>% summarise(sum(grams))
52.3 / 132 * 100 # ready meal proportion of diet in 2022
(3.4 + 18.2) / 132 * 100 # white fish + takeaway prop diet in 202

freq_of_fish %>% ungroup() %>% summarise(mean(grams_day), mean(prop)*100)
