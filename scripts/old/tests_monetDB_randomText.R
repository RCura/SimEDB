library(tidyverse)
library(dbplyr)
library(MonetDBLite)


conMonetDB <- MonetDBLite::src_monetdblite("~/outputs_TR8/testMonetDB")


fpMonet <- tbl(src = conMonetDB, "fp") %>%
  filter(sim_name == "4_4_D") %>%
  filter(annee == 1160) %>%
  show_query()


MonetDBLite::monetdblite_shutdown()

library(MonetDBLite)
library(dplyr)

dbdir <- file.path(tempdir(), "dplyrdir")
my_db_monetdb <- MonetDBLite::src_monetdblite(dbdir)
flights <- nycflights13::flights
flights$time_hour <- as.numeric( flights$time_hour )
flights_monetdb <- copy_to(my_db_monetdb, flights,overwrite = TRUE, temporary = FALSE)

filter(filter(flights_monetdb, month == 1), day == 1) %>% explain()
  filter(month == 1) %>%
  filter(day == 1) %>%
  filter(origin == "EWR") %>%
  filter(air_time <= 100) %>%
  show_query()

flights_monetdb %>%
  filter(month == 1, day == 1, origin == "EWR", air_time <= 100) %>%
  show_query()

MonetDBLite::monetdblite_shutdown()