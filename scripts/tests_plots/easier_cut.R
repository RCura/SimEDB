x <- 1:50
case_when(
  x < 3 ~ "<3",
  x < 5 ~ "3<x<5",
  x <= 10 ~ "5<x<=10",
  x < 2 ~ "<2",
  TRUE ~ ">10"
)

tibble(A = 1:15) %>%
  mutate(B = case_when(
    A < 3 ~ "<3",
    A < 5 ~ "3<x<5",
    A <= 10 ~ "5<x<=10",
    A < 2 ~ "<2",
    TRUE ~ ">10"
  ))
