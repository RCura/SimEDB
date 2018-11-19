source("packages.R")

options( java.parameters = c("-Xss2560k", "-Xmx7g") ) # Needed fix for rJava (JDBC) + ggplot2

drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/data/user/c/rcura/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")
conMapD <- dbConnect(drv, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")
parameters <- tbl(conMapD, "parameters")

all_sim_names <- parameters %>%
  select(sim_name) %>%
  distinct() %>%
  arrange(sim_name) %>%
  collect() %>%
  pull()

dbDisconnect(conMapD)

source("src_plots/plotDownloadRate_module.R")


########## SENSITIVITY #########

experiment_plan <- readRDS("data/sensib/experiment_plan.Rds")
filtered_data <- readRDS("data/sensib/filtered_data.Rds")
sensibility_summary_table <- readRDS("data/sensib/sensibility_summary_table.Rds")

library(tictoc)