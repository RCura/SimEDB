library(tidyverse)
library(echarts4r)

load("data/sim_data_4_4_A-C.Rdata")

  tmp_parameters <- sim_parameters
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
  e_parallel(!!nonUniqueParams, 
             ... = ) %>%
  e_title("test") %>%

df <- data.frame(
  price = rnorm(5, 10),
  amount = rnorm(5, 15),
  sim_name = blob$sim_name[1:5]
) %>%
  arrange(desc(sim_name))

df %>%
  e_charts() %>%
  e_parallel(price, amount, sim_name)
