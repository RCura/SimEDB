options( java.parameters = c("-Xss2560k", "-Xmx7g") ) # Needed fix for rJava (JDBC) + ggplot2
source("packages.R")
source("src_plots/themes.R")

omnisci_driver <- JDBC("com.omnisci.jdbc.OmniSciDriver",
                       "/data/user/c/rcura/omnisci-jdbc-4.7.1.jar",
                       identifier.quote="'")
gc()
conMapD <- dbConnect(omnisci_driver, "jdbc:omnisci:mapdi.cura.info:6274:omnisci", "admin", "HyperInteractive")

parameters <- tbl(conMapD, "parameters_6_4")

all_sim_names <- parameters %>%
  select(sim_name) %>%
  distinct() %>%
  arrange(sim_name) %>%
  collect() %>%
  pull()

dbDisconnect(conMapD)
rm(conMapD)

enableBookmarking(store = "url")

source("src_plots/plotDownloadRate_module.R")


########## SENSITIVITY #########
filtered_data <- readRDS("data/sensib/sensib_6.6.Rds")
sensib_data_scaled <- filtered_data %>%
  mutate(
    agregats_sc = (nb_agregats - 200) / 10.45,
    gds_chateaux_sc = (nb_grands_chateaux - 10) / 2.87,
    eglises_par_sc = (nb_eglises_paroissiales - 300) / 12.96,
    dist_eglises_par_sc = (distance_eglises_paroissiales - 3000) / 97,
    prop_isoles_sc = (prop_fp_isoles - 0.2) / 0.08,
    augm_charge_fisc_sc = (ratio_charge_fiscale - 3) / 0.03
  ) %>%
  select_at(vars(ends_with("_sc"), starts_with("sensibility"), "type")) %>%
  rename_at(vars(ends_with("_sc")), ~str_replace_all(.,"_sc", "")) %>%
  rename(param = sensibility_parameter)

sensib_data_gathered <- sensib_data_scaled %>%
  select(-sensibility_value) %>%
  gather(Indicateur, Valeur_norm, -param, -type) 

global_sensib <- sensib_data_gathered %>%
  select(-type) %>%
  group_by(param) %>%
  summarise(sensibilite = mean(abs(Valeur_norm), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(sensibilite)) %>%
  select(param, sensibilite)

summary_sensib <- sensib_data_gathered %>%
  group_by(param, type, Indicateur) %>%
  summarise(sensibilite = mean(abs(Valeur_norm), na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key = Indicateur, value = sensibilite) %>%
  left_join(global_sensib, by = "param") %>%
  select(param, type, sensibilite,
         agregats, gds_chateaux, eglises_par,
         dist_eglises_par, prop_isoles, augm_charge_fisc)
rm(sensib_data_scaled, sensib_data_gathered, global_sensib)

## global.R ##


