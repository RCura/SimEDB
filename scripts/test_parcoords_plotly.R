library(tidyverse)
library(plotly)

load("data/sim_data_4_4_A-C.Rdata")

nonUniqueParams <- sim_parameters %>%
  gather(key = "Var", value = "Value") %>%
  group_by(Var, Value) %>%
  mutate(Freq = n()) %>%
  ungroup() %>%
  filter(Freq != nrow(sim_parameters)) %>%
  distinct(Var) %>%
  pull(Var)

sim_parameters %>% dplyr::select(!!nonUniqueParams)
df <- read.csv("https://raw.githubusercontent.com/bcdunbar/datasets/master/parcoords_data.csv")

p <- df %>%
  plot_ly(width = 1000, height = 600) %>%
  add_trace(type = 'parcoords',
            line = list(color = ~colorVal,
                        colorscale = 'Jet',
                        showscale = TRUE,
                        reversescale = TRUE,
                        cmin = -4000,
                        cmax = -100),
            dimensions = list(
              list(range = c(~min(blockHeight),~max(blockHeight)),
                   constraintrange = c(100000,150000),
                   label = 'Block Height', values = ~blockHeight),
              list(range = c(~min(blockWidth),~max(blockWidth)),
                   label = 'Block Width', values = ~blockWidth),
              list(tickvals = c(0,0.5,1,2,3),
                   ticktext = c('A','AB','B','Y','Z'),
                   label = 'Cyclinder Material', values = ~cycMaterial),
              list(range = c(-1,4),
                   tickvals = c(0,1,2,3),
                   label = 'Block Material', values = ~blockMaterial),
              list(range = c(~min(totalWeight),~max(totalWeight)),
                   visible = TRUE,
                   label = 'Total Weight', values = ~totalWeight),
              list(range = c(~min(assemblyPW),~max(assemblyPW)),
                   label = 'Assembly\nPenalty\nWeight', values = ~assemblyPW),
              list(range = c(~min(HstW),~max(HstW)),
                   label = 'Height st Width', values = ~HstW),
              list(range = c(~min(minHW),~max(minHW)),
                   label = 'Min Height Width', values = ~minHW),
              list(range = c(~min(minWD),~max(minWD)),
                   label = 'Min Width Diameter', values = ~minWD),
              list(range = c(~min(rfBlock),~max(rfBlock)),
                   label = 'RF Block', values = ~rfBlock)
            )
  )
p
