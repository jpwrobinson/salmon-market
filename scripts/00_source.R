pacman::p_load(tidyverse, janitor, cowplot, scales, readxl, ggrepel, vegan)
theme_set(theme_bw())
se<-function (x) {sd(x)/sqrt(length(x))}

## colours
sp_cols<-c('Salmon' = '#d6604d',
        'Mackerel' = '#386cb0',
        'Herring' = '#a6cee3',
        "Other marine" = 'grey50',
        'Cod' = '#1b9e77',
        'Lobster, Norway' = '#c2a5cf',
        'Crab' = '#a6d96a',
        'Scallop' = '#de77ae',
        'Tuna' = '#54278f',
        'Prawn' = '#de77ae',
        'Haddock' = '#006837'
)

cols<-c('Scotland' = '#c51b7d', 'Chile' = '#D39200', 'Norway' = '#00B9E3', 'Other' = 'grey')

comp_cols<-c('#ffffcc', '#a1dab4','#41b6c4','#2c7fb8', '#253494')