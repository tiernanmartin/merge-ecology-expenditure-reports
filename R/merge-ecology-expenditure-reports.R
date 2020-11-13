
# SETUP -------------------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(googlesheets4)
library(lubridate)
library(glue)

not_all_na <- function(x) {!all(is.na(x))}

# GREENLINK PORT ANGELES --------------------------------------------------

gl_all <- list.files(here("data/greenlink-port-angeles/"),
                 full.names = TRUE) %>% 
  map_dfr(read_xlsx) %>% 
  clean_names("screaming_snake") %>% 
  select_if(not_all_na)

gl_contract_id <- "DD3CCF9C"

gl_budget_url <- "https://docs.google.com/spreadsheets/d/1onIQUHVF4PUXHPRp1exrZFOx7wQJtuVgLtBtgq65Hpo/"

gl_tasks <- googlesheets4::read_sheet(ss = gs4_get(gl_budget_url),
                          sheet = "BUDGET BY TASK") %>%  
  mutate(TASK_TITLE = str_remove(TASK,"^.{5}")) %>% 
  select(TASK, TASK_TITLE)

gl_ready <- gl_all %>% 
  filter(APPROVED_DENIED_MODIFIED %in% c("Approved", "Modified")) %>%  
  mutate(DATE_INCURRED_START = as.Date(word(DATE_INCURRED_START),"%m/%d/%Y"),
         DATE_INCURRED_END = as.Date(word(DATE_INCURRED_END),"%m/%d/%Y"),
         YEAR_QUARTER = glue("{year(DATE_INCURRED_END)} Q{quarter(DATE_INCURRED_END)}")) %>% 
  mutate(REVENUE_TYPE = case_when(
    ITEM_CATEGORY %in% "Contracts" ~ "02 - pass-through revenue",
    TRUE ~ "01 - retained revenue"
  )) %>% 
  mutate(CONTRACT_ID = gl_contract_id) %>% 
  left_join(gl_tasks, by = "TASK_TITLE") %>% 
  relocate(YEAR_QUARTER, .before = DATE_INCURRED_START) %>% 
  relocate(REVENUE_TYPE, .after = ITEM_CATEGORY) %>% 
  relocate(TASK, .after = TASK_TITLE)

gl_url <- "https://docs.google.com/spreadsheets/d/1lf3pZapfGeGCbdMD_L6h3QHcbthtebk0fOgJPzL99zk/edit?usp=sharing" 


# run this this first time *only*
# sheet_write(gl_ready,
#             ss = gs4_get(gl_url),
#             sheet = "PAYMENTS")

range_write(ss = gs4_get(gl_url),
            data = gl_ready,
            sheet = "PAYMENTS",
            range = "A1")  # this means the overwrite will start at the top-left corner


# ALGONA PPG --------------------------------------------------------------

algona_all <- list.files(here("data/algona-ppg/"),
                 full.names = TRUE) %>% 
  map_dfr(read_xlsx) %>% 
  clean_names("screaming_snake") %>% 
  select_if(not_all_na)

algona_ready <- algona_all %>% 
  filter(APPROVED_DENIED_MODIFIED %in% c("Approved", "Modified")) %>%  
  mutate(DATE_INCURRED_START = as.Date(word(DATE_INCURRED_START),"%m/%d/%Y"),
         DATE_INCURRED_END = as.Date(word(DATE_INCURRED_END),"%m/%d/%Y"),
         YEAR_QUARTER = glue("{year(DATE_INCURRED_END)} Q{quarter(DATE_INCURRED_END)}")) %>% 
  mutate(REVENUE_TYPE = case_when(
    ITEM_CATEGORY %in% "Contracts" ~ "02 - pass-through revenue",
    TRUE ~ "01 - retained revenue"
  )) %>% 
  relocate(YEAR_QUARTER, .before = DATE_INCURRED_START) %>% 
  relocate(REVENUE_TYPE, .after = ITEM_CATEGORY)



algona_url <- "https://docs.google.com/spreadsheets/d/1kjZQZBLGANNzt9l4akPUZBDA98YuzdrwGzcxKZ-_bkE/edit?usp=sharing" 


# run this this first time *only*
# sheet_write(algona_ready,
#             ss = gs4_get(algona_url),
#             sheet = "[REF]  PAYMENTS")

range_write(ss = gs4_get(algona_url),
            data = algona_ready,
            sheet = "[REF]  PAYMENTS",
            range = "A1")  # this means the overwrite will start at the top-left corner

