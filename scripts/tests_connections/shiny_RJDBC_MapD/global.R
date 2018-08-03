library(shiny)
library(tidyverse)
library(dbplyr)
library(RJDBC)

options(java.parameters = "-Xss2560k")
options(java.parameters = "-Xmx8000m")

drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/home/robin/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")

