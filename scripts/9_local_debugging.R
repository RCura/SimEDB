source("packages.R")

options( java.parameters = c("-Xss2560k", "-Xmx7g") ) # Needed fix for rJava (JDBC) + ggplot2

drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/data/user/c/rcura/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")
conMapD <- dbConnect(drv, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")

seeds <- tbl(conMapD, "seeds")
agregats <- tbl(conMapD, "agregats")
fp <- tbl(conMapD, "fp")
parameters <- tbl(conMapD, "parameters")
paroisses <- tbl(conMapD, "paroisses")
poles <- tbl(conMapD, "poles")
results <- tbl(conMapD, "results")
seigneurs <- tbl(conMapD, "seigneurs")

##############################################################
##############################################################
##############################################################

FP_data <- fp %>% filter(sim_name %in% c("5_0"))
results_data <- results %>% filter(sim_name %in% c("5_0"))
agregats_data <- agregats %>% filter(sim_name %in% c("5_0"))
poles_data <- poles %>% filter(sim_name %in% c("5_0"))
paroisses_data <- paroisses %>% filter(sim_name %in% c("5_0"))
seigneurs_data <- seigneurs %>% filter(sim_name %in% c("5_0"))

##############################################################
##############################################################
##############################################################



##############################################################
##############################################################
##############################################################

dbDisconnect(conn = conMapD)
