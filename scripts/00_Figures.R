

source('scripts/01_production.R')
source('scripts/03_consumption.R')
source('scripts/04_company.R')

## Fig 1 = timeline of Scottish industry
ggdraw(g_company + theme_half_open(12)) +
    draw_plot(g_mowi, .56, .45, .6, .6) 

## Fig 2 = food related myths?
