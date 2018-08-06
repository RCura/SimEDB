library(tidyverse)
library(echarts4r)

  tmp_parameters <- read_csv("~/Dropbox/params_4_5_mapd.csv")
  nonUniqueParams <- tmp_parameters %>%
    gather(key = "Var", value = "Value") %>%
    group_by(Var, Value) %>%
    mutate(Freq = n()) %>%
    ungroup() %>%
    filter(Freq != nrow(tmp_parameters)) %>%
    distinct(Var) %>%
    pull(Var)
  
  blob <- tmp_parameters %>%
    dplyr::select(!!nonUniqueParams) %>%
    mutate(sim_name = sim_name) %>%
    as.data.frame()

blob %>%
  e_charts() %>%
  e_parallel(!!nonUniqueParams, x.index = 1) %>%
  e_x_axis(index = 1, axisLabel = list()) %>%
  e_title("test")
#%>%
  
