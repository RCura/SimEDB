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

source("src_plots/plotDownloadRate_module.R")


########## SENSITIVITY #########

experiment_plan <- readRDS("data/sensib/experiment_plan.Rds")
filtered_data <- readRDS("data/sensib/filtered_data.Rds")
sensibility_summary_table <- readRDS("data/sensib/sensibility_summary_table.Rds")

## global.R ##


