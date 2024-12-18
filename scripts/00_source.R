pacman::p_load(tidyverse, janitor, cowplot, scales, readxl, ggrepel, vegan)
theme_set(theme_classic())
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

col_foods<-c('Total fish' = 'black',
             'Farmed salmon' = '#d6604d',
             'Salmon' = '#d6604d',
             'Fresh salmon fillets' = '#d6604d',
             'Fresh white fish fillets' = '#1b9e77',
             'Frozen breaded/battered white fish' = '#1b9e77',
             'Frozen prawns' = '#de77ae',
             'Shellfish' = '#de77ae',
             'Beef' = '#084081',
             'Mutton and lamb' = '#4eb3d3',
             'Pork' = '#a8ddb5',
             'Chicken' = '#fd8d3c',
             'Other tinned fish' = '',
             'Blue fish' = '#4eb3d3',
             'Ready meals' = '#CDCD00',
             'White fish' = '#1b9e77',
             'Takeaway fish' = 'grey',
             'Canned tuna' = '#54278f',
             'Tinned fish' = '#54278f')

