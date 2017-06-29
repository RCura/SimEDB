library(tidyverse)


x <- "cyl"
test <- list(
  quo(!!x) <= 4 ~ "fizz",
  quo(!!x) <= 6 ~ "buzz",
  quo(!!x) <= 8 ~ "fizz buzz"
)


mtcars %>%
  mutate(ABC = case_when(!!!test))


library(dplyr)
patterns <- list(
  x <= 2 ~ "<=2",
  x <= 4 ~ "2<->4",
  x > 4 ~ ">4"
)
x <- 1:10
case_when(!!!patterns)


library(dplyr)
library(rlang)
patterns_lazy <- list(
  UQ(!!x) <= 2 ~ "<=2",
  UQ(!!x) <= 4 ~ "2<->4",
  UQ(!!x) > 4 ~ ">4"
)
x <- "cyl"
mtcars %>%
  select(cyl) %>%
  mutate(ABC = case_when(!!!patterns_lazy)) %>%
  head()



patterns_lazy <- exprs(
  .data[[x]] <= 2 ~ "<=2",
  .data[[x]] <= 4 ~ "2<->4",
  .data[[x]] > 4 ~ ">4"
)

x <- "cyl"
pull(mutate(mtcars, case_when(!!!patterns_lazy)))

patterns_lazy <- exprs(
  .data[[x]] <= 2 ~ "<=2",
  .data[[x]] <= 4 ~ "2<->4",
  .data[[x]] > 4 ~ ">4"
)

case_cut <- function(var, patterns){
  x <- var
  case_when(!!!patterns)
}

mtcars %>% mutate(ABC = case_cut(cyl,patterns_lazy))


