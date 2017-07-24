library(tidyverse)
library(dplyr)
library(dbplyr)
library(DBI)
library(MonetDBLite)

tmpDir <- "data/testMonetDB.db"
my_db_monetdb <- MonetDBLite::src_monetdblite(tmpDir)

# flights <- nycflights13::flights
# flights$time_hour <- as.numeric( flights$time_hour )
# 
# flights_monetdb <<- copy_to(my_db_monetdb, flights, temporary = FALSE, indexes = list(
#   c("year", "month", "day"), "carrier", "tailnum"))
 
flightsDb <- tbl(my_db_monetdb, from = "flights")

colnames(flightsDb)

system.time({
  blob <- flightsDb %>%
    select(dep_delay, month) %>%
    mutate(B = if_else(month <= 3, "<3",
                      if_else(month <= 6, "<6", "Autre")))
    
  
  blob %>%
    group_by(B) %>%
    count() %>%
    print()
})

MonetDBLite::monetdblite_shutdown()
