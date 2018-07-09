library(tidyverse)
library(nycflights13)
library(dbplyr)
library(DBI)
library(RSQLite)
library(MonetDBLite)

dbdir <- file.path(tempdir(), "dplyrdir")
my_db_sqlite <- FALSE
my_db_monetdb <- FALSE
flights_sqlite <- FALSE
flights_monetdb <- FALSE

  my_db_sqlite <<- src_sqlite(tempfile(), create = T)
  my_db_monetdb <- MonetDBLite::src_monetdblite(dbdir)

flights <- nycflights13::flights
flights$time_hour <- as.numeric( flights$time_hour )

  
flights_sqlite <- copy_to(my_db_sqlite, flights, temporary = FALSE, indexes = list(
  c("year", "month", "day"), "carrier", "tailnum"), )

flights_monetdb <- copy_to(my_db_monetdb, flights, temporary = FALSE, indexes = list(
  c("year", "month", "day"), "carrier", "tailnum"))

tbl(my_db_monetdb, "flights")


SQLite1 <- flights_sqlite %>% mutate(speed = air_time / distance) %>% show_query()
MonetDB1 <- flights_monetdb %>% mutate(speed = air_time / distance) %>% show_query()
MonetDB1_1 <- flights_monetdb %>% mutate(speed = air_time * 10L) %>% collect()

SQLite2 <- data.frame(summarise(flights_sqlite, delay = mean(dep_time)))
MonetDB2 <- data.frame(summarise(flights_monetdb, delay = mean(dep_time)))

SQLite3 <- collect(mutate(c2_sqlite, speed = distance / air_time * 60))
MonetDB3 <- collect(mutate(c2_monetdb, speed = distance / air_time * 60))


DBI::dbDisconnect(my_db_sqlite)
DBI::dbDisconnect(my_db_monetdb)

