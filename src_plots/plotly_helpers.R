create_dims <- function(df, index, constraints = NULL){
  label <- colnames(df)[index]
  values <- df %>% select(index) %>% pull()
  
  constraintrange <- ""
  if (!is.null(constraints)){
    constraintPosition <- match(label, map_chr(constraints, "label"))
    if (!is.na(constraintPosition)){
      constraintrange <- ifelse(!is.na(constraintPosition), yes = list(constraints[[constraintPosition]]$constraintrange), no = "")
      constraintrange <- unlist(constraintrange)
    }
  }
  
  returnList <- list(label = label, values = values, constraintrange = constraintrange, visible = TRUE, multiselect = FALSE)
  if (is.numeric(values)){
    returnList[["range"]] <- c(min(values), max(values))
  } else {
    MyFactors <- values %>% as.character() %>% unique() %>% sort() %>% as_factor()
    returnList[["values"]] <- values %>% factor(levels = levels(MyFactors)) %>% as.numeric()
    returnList[["tickvals"]] <- MyFactors %>% as.numeric() %>% sort()
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