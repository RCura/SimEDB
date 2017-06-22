library(tidyverse)
simName <- "4_4_A"
simDataPath <- "../GAMA/transition8/outputs/"


sim_parameters <- read_csv(paste0(simDataPath, simName, "_parameters.csv"))
sim_results <- read_csv(paste0(simDataPath, simName, "_results_global.csv"))

# On ne garde que les simulations complètes

goodSeeds <- sim_parameters %>%
  left_join(sim_results %>%
              filter(Annee == 1160) %>%
              select(seed, Annee),
            by = "seed") %>%
  filter(Annee == 1160) %>%
  select(seed)

sim_parameters <- sim_parameters %>%
  semi_join(goodSeeds, by = "seed")

sim_results <- sim_results %>%
  semi_join(goodSeeds, by = "seed")


sim_seigneurs <-  read_csv(paste0(simDataPath, simName, "_results_seigneurs.csv")) %>%
  semi_join(goodSeeds, by = "seed")

sim_agregats <- read_delim(paste0(simDataPath, simName, "_results_agregats.csv"),
                           delim = ",", quote = "'") %>%
  semi_join(goodSeeds, by = "seed")

sim_poles <- read_delim(paste0(simDataPath, simName, "_results_poles.csv"),
                        delim = ",", quote = "'") %>%
  semi_join(goodSeeds, by = "seed")

sim_FP <- read_delim(paste0(simDataPath, simName, "_results_FP.csv"),
                     delim = ",", quote = "'") %>%
  semi_join(goodSeeds, by = "seed")

sim_paroisses <- read_delim(paste0(simDataPath, simName, "_results_paroisses.csv"),
                            delim = ",", quote = "'") %>%
  semi_join(goodSeeds, by = "seed")


sim_results <- sim_results %>%
  inner_join({
    sim_agregats %>%
      group_by(seed, Annee) %>%
      summarise(NbAgregats = n())
  },
  by=c("seed", "Annee")
  ) %>%
  inner_join({
    filter(., Annee == 840) %>%
      mutate(CFinit = charge_fiscale) %>%
      select(seed, CFinit)
  }, by="seed") %>%
  mutate(RatioChargeFiscale = charge_fiscale / CFinit) %>%
  select(-CFinit)


save(
  list = c(
    "sim_agregats",
    "sim_FP",
    "sim_parameters",
    "sim_paroisses",
    "sim_poles",
    "sim_results",
    "sim_seigneurs",
    "goodSeeds"
  ),
  file = "data/sim_data.Rdata"
)



# 
# 
# JIAP_parameters %>% group_by(name) %>% summarise(nbRep = n())
# 
# 
# 
# foo <- sim_FP %>%
#   gather(key = Type, value = Satisfaction, sMat:Satis) %>%
#   mutate(Type = ifelse(Type == "Satis", "Globale", Type)) %>%
#   mutate(Type = ifelse(Type == "sMat", "Matérielle", Type)) %>%
#   mutate(Type = ifelse(Type == "sProt", "Protection", Type)) %>%
#   mutate(Type = ifelse(Type == "sRel", "Religieuse", Type))
# 
# ggplot(foo, aes(Annee, Satisfaction, col = Type, fill = Type)) +
#   geom_violin(aes(group = factor(Annee))) +
#   facet_wrap(~ Type) +
#   geom_smooth() +
#   theme(legend.position = "bottom")
