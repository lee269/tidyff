pacman::p_load(tidyverse, tidyxl, unpivotr)

# Extract nutrient data
get_nutrient_table <- function(cells, indent, lastcol) {
  cells %>% 
    dplyr::filter(row>=7 & row <= 72 & col <= lastcol) %>% 
    unpivotr::behead("N", year) %>% 
    unpivotr::behead_if(str_starts(character, "Average"), direction = "WNW", name = "type") %>% 
    unpivotr::behead("W", code) %>% 
    unpivotr::behead_if(indent[local_format_id] == 0, direction = "WNW", name = "nutrient1") %>% 
    unpivotr::behead("W", "nutrient2") %>% 
    unpivotr::behead("W", "unit") %>% 
    mutate(value = numeric) %>% 
    select(code, type,  nutrient1, nutrient2, unit, year, value)
}
