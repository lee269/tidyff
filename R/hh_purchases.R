pacman::p_load(tidyverse, tidyxl, unpivotr)


# Extract purchase data
get_purchase_table <- function(cells, lastcol){

  cells %>% 
    dplyr::filter(row >= 8 & row <=345 & col <= lastcol) %>% 
    unpivotr::behead("N", year) %>% 
    unpivotr::behead("W", code) %>%
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
    select(code, level, description, year, unit, value)
}




# Regions
sheets <- xlsx_sheet_names(here::here("data", "ConsGORHH-26apr18.xlsx"))
tabsheets <- sheets[2:length(sheets)]
cells <- tidyxl::xlsx_cells(here::here("data", "ConsGORHH-26apr18.xlsx"), sheets = tabsheets, include_blank_cells = FALSE)

# Regional purchases
table <- 
  cells %>%
  group_by(sheet) %>% 
  nest() %>%
  mutate(data = map2(data, 23, get_purchase_table)) %>% 
  unnest() %>% 
  rename(region = sheet)




cells <- tidyxl::xlsx_cells(here::here("data", "UKExp-26apr18.xlsx"), sheets = "expenditure")


