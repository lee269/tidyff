pacman::p_load(tidyverse, tidyxl, unpivotr)

# Extract expenditure data
get_expenditure_table <- function(cells, lastcol){
  
  cells %>% 
    dplyr::filter(row>=7 & row <= 369 & col <= lastcol) %>% 
    dplyr::filter(data_type != "blank") %>% 
    unpivotr::behead("N", year) %>% 
    unpivotr::behead("W", code) %>% 
    unpivotr::behead_if(character == toupper(character), direction = "WNW", name = "type") %>% 
    unpivotr::behead("W", item1) %>% 
    unpivotr::behead("W", item2) %>% 
    unpivotr::behead("W", item3) %>% 
    unpivotr::behead("W", item4) %>% 
    unpivotr::behead("W", unit) %>%
    mutate(description = coalesce(item1, item2, item3, item4),
           level = case_when(!is.na(item1) ~ 1,
                             !is.na(item2) ~ 2,
                             !is.na(item3) ~ 3,
                             !is.na(item4) ~ 4),
           value = numeric) %>% 
    select(code, level, type, description, year, unit, value)
}
