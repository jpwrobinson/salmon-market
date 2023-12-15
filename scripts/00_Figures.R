

source('scripts/01_production.R')
source('scripts/02_trade.R')
source('scripts/03_consumption.R')
source('scripts/04_company.R')
source('scripts/05_diets.R')

## Fig 1 = timeline of Scottish industry
# trade
rhs<-plot_grid(g_salm_sites, g_val, g_ton, nrow=3, labels=c('b','c', 'd'), align='v')

fig1<-plot_grid(g_prod, rhs, nrow=1, labels=c('a'))

## Fig 2 =  Influence of retail and industry on farmed salmon market-making
# companies
cons<-ggdraw(g_company + theme_half_open(12)) +
    draw_plot(g_mowi, .56, .45, .6, .6) 


## Figure 3 | Consumption and dietary contributions of farmed salmon in the UK
# house<-plot_grid(g_cons_meat, g_cons_fish, nrow=2, labels=c('a', 'b'))

fig3<-plot_grid(g_cons_facet, g_diet_prop, nrow=1, labels=c('a', 'b'))

## PDFS
pdf(file = 'fig/Figure1.pdf', height=5, width=12)
print(fig1)
dev.off()

pdf(file = 'fig/Figure3.pdf', height=5, width=12)
print(fig3)
dev.off()

