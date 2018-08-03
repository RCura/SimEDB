library(DBI)
library(MonetDBLite)

conMonetDB <- dbConnect(MonetDBLite::MonetDBLite(), "data/db_Transition8")

seeds <- tbl(conMonetDB, "seeds")
agregats <- tbl(conMonetDB, "agregats")
fp <- tbl(conMonetDB, "fp")
parameters <- tbl(conMonetDB, "parameters")
paroisses <- tbl(conMonetDB, "paroisses")
poles <- tbl(conMonetDB, "poles")
results <- tbl(conMonetDB, "results")
seigneurs <- tbl(conMonetDB, "seigneurs")

sim <- list()
simNames <- "4_4_A"

sim$seeds <- seeds %>% filter(sim_name %in% simNames)
sim$agregats <- agregats %>% filter(sim_name %in% simNames)
sim$FP <- fp %>% filter(sim_name %in% simNames)
sim$parameters <- parameters %>% filter(sim_name %in% simNames)
sim$paroisses <- paroisses %>% filter(sim_name %in% simNames)
sim$poles <- poles %>% filter(sim_name %in% simNames)
sim$results <- results %>% filter(sim_name %in% simNames)
sim$seigneurs <- seigneurs %>% filter(sim_name %in% simNames)

agregats_data <- sim$agregats
FP_data <- sim$fp
poles_data <- sim$poles
seigneurs_data <- sim$seigneurs
paroisses_data <- sim$paroisses

dbDisconnect(conMonetDB)
MonetDBLite::monetdblite_shutdown()
