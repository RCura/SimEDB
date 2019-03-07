library(tidyverse)
library(DBI)
library(RJDBC)

suffixe_tables <- "_2"


options( java.parameters = c("-Xss2560k", "-Xmx8g") ) # Needed fix for rJava (JDBC) + ggplot2

conMapD <- NULL
connectToMapD <- function(){
  conMapD <<- dbConnect(drv = JDBC("com.mapd.jdbc.MapDDriver",
                                   "/data/user/c/rcura/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
                                   identifier.quote="'"),
                        "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")
}

#####################################
############# ATTENTION #############
#####################################

# Utilisé pour créer les tables au départ
# Ne pas décommenter sans raison

# Creation et vidage des tables #

# connectToMapD()
# 
# thisTable <- "agregats"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                  sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                          thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "fp"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "parameters"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "paroisses"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "poles"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "results"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "seeds"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "seigneurs"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# dbDisconnect(conMapD)