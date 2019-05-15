create_dims <- function(df, index){
  label <- colnames(df)[index]
  values <- df %>% select(index) %>% pull()
  # if (all(as.character(as.numeric(values)) == values)){
  #   values <- as.numeric(values)
  # }
  returnList <- list(label = label, values = values, visible = TRUE, multiselect = FALSE)
  if (is.numeric(values)){
    returnList[["range"]] <- c(min(values), max(values))
    if (n_distinct(values) < 10){
      returnList[["tickvals"]] <- values %>% sort() %>% unique() %>% signif(., digits = 2)
    }
  } else {
   # if (!is.null(values) && all(as.character(as.numeric(values)) == values)){
   #  MyFactors <- values %>% as.numeric() %>% sort() %>% as.character() %>% unique() %>% as_factor()
   # } else {
      MyFactors <- values %>% as.character() %>% unique() %>% sort() %>% as_factor()
    # }
    returnList[["values"]] <- values %>% factor(levels = levels(MyFactors)) %>% as.numeric()
    returnList[["tickvals"]] <- MyFactors %>% as.numeric() %>% unique()
    returnList[["ticktext"]] <-  levels(MyFactors)
  }
  if (label == "seed"){
    returnList[["visible"]] <- FALSE
  }
  return(returnList)
}

char_to_num <- function(x){
  MyFactors <- as.character(x) %>% unique() %>% sort() %>% as.factor()
  values <- x %>% factor(levels = levels(MyFactors)) %>% as.numeric()
  return(values)
}

expression_from_input <- function(inputJSON){
  if (is.null(inputJSON)){
    return()
  } else if (!jsonlite::validate(inputJSON)){
    return()
  } else {
    filterDF <- jsonlite::fromJSON(txt = inputJSON,
                                   simplifyMatrix = FALSE,
                                   simplifyDataFrame = FALSE) %>%
      purrr::compact(.x = ., "constraintrange") %>%
      tibble(listcol = .) %>%
      mutate(var = map_chr(listcol, "label")) %>%
      mutate(range = map(listcol, "constraintrange")) %>%
      select(-listcol) %>%
      mutate(min = map_dbl(range, 1),
             max = map_dbl(range, 2)) %>%
      select(-range) %>%
      mutate(var = paste0(var, "_char"))
    
    expressions <- with(filterDF, pmap(list(var, min, max),
                                       function(var, min, max){
                                         expr(between(!!sym(var), !!min, !!max))}))
    
    return(expressions)
  }
}

parametres_from_input <- function(inputJSON){
  if (is.null(inputJSON)){
    return()
  } else if (!jsonlite::validate(inputJSON)){
    return()
  } else {
    filterColumns <- jsonlite::fromJSON(txt = inputJSON,
                                        simplifyMatrix = FALSE,
                                        simplifyDataFrame = FALSE) %>%
      purrr::compact(.x = ., "constraintrange") %>%
      tibble(listcol = .) %>%
      mutate(var = map_chr(listcol, "label")) %>%
      pull(var)
    
    return(filterColumns)
  }
}