

source('scripts/01_production.R')
source('scripts/02_trade.R')
source('scripts/03_consumption.R')
source('scripts/04_company.R')

## Fig 1 = timeline of Scottish industry

# companies
cons<-ggdraw(g_company + theme_half_open(12)) +
    draw_plot(g_mowi, .56, .45, .6, .6) 

# trade
trade<-plot_grid(g_val, g_ton, nrow=2, labels=c('c', 'd'), align='v')

fig1<-plot_grid(g_prod,
          plot_grid(cons, trade, nrow=2, labels=c('b'), rel_heights=c(1, 1.5)), nrow=1, labels=c('a'))

## Fig 2 = food related myths?



## PDFS
pdf(file = 'fig/Figure1.pdf', height=5, width=12)
print(fig1)
dev.off()

