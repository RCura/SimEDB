options( java.parameters = c("-Xss2560k", "-Xmx7g") ) # Needed fix for rJava (JDBC) + ggplot2
source("packages.R")

omnisci_driver <- JDBC("com.omnisci.jdbc.OmniSciDriver",
                       "/data/user/c/rcura/omnisci-jdbc-4.7.1.jar",
                       identifier.quote="'")
gc()
conMapD <- dbConnect(omnisci_driver, "jdbc:omnisci:mapdi.cura.info:6274:omnisci", "admin", "HyperInteractive")
#parameters <- tbl(conMapD, "parameters_6_1")
#parameters <- tbl(conMapD, "parameters_6_3")
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

theme_simedb <- function(){
  ggplot2::theme_light() +
    theme(legend.position = "bottom") +
    theme(plot.caption = element_text(size = 6, hjust = 0))
}

theme_simedb_no_x <- function(){
  theme_simedb() +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())
}

theme_simedb_no_y <- function(){
  theme_simedb() +
    theme(axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank())
}

theme_simedb_rotate_x <- function(){
  theme_simedb() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

theme_simedb_seigneurs <- function(x){
  theme_simedb_rotate_x() +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
}
