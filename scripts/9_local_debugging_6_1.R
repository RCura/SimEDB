source("packages.R")

options( java.parameters = c("-Xss2560k", "-Xmx7g") ) # Needed fix for rJava (JDBC) + ggplot2

drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/data/user/c/rcura/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")
conMapD <- dbConnect(drv, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")

seeds <- tbl(conMapD, "seeds_6_1")
agregats <- tbl(conMapD, "agregats_6_1")
fp <- tbl(conMapD, "fp_6_1")
parameters <- tbl(conMapD, "parameters_6_1")
paroisses <- tbl(conMapD, "paroisses_6_1")
poles <- tbl(conMapD, "poles_6_1")
results <- tbl(conMapD, "global_6_1")
seigneurs <- tbl(conMapD, "seigneurs_6_1")
chateaux <- tbl(conMapD, "chateaux_6_1")

##############################################################
##############################################################
##############################################################

sample_sim_name <- "Exp_6_1_Scenario_Base"

FP_data <- fp %>% filter(sim_name %in% !!sample_sim_name)
results_data <- results %>% filter(sim_name %in% !!sample_sim_name)
agregats_data <- agregats %>% filter(sim_name %in% !!sample_sim_name)
poles_data <- poles %>% filter(sim_name %in% !!sample_sim_name)
paroisses_data <- paroisses %>% filter(sim_name %in% !!sample_sim_name)
seigneurs_data <- seigneurs %>% filter(sim_name %in% !!sample_sim_name)
parameters_data <- parameters %>%  filter(sim_name %in% !!sample_sim_name)
chateaux_data <- chateaux %>% filter(sim_name %in% !!sample_sim_name)

##############################################################
##############################################################
##############################################################



##############################################################
##############################################################
##############################################################

dbDisconnect(conn = conMapD)
