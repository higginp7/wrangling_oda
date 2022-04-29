library(countrycode)
library(tidyverse)
library(janitor)

oda %<>% 
  clean_names() %>% 
  filter(country_name != "World") %>% 
  filter(!grepl("region", country_name)) %>% 
  select(country_code, fiscal_year, contains("_name"), oda_amount) %>% 
  filter(transaction_type_name == "Disbursements") %>% 
  mutate(year = parse_number(fiscal_year),
         country_name = ifelse(grepl("Serbia", country_name), "Serbia", country_name),
         cow_code = countrycode(country_name, "country.name", "cown"), 
         cow_code = ifelse(country_name == "Serbia", 345,
                           ifelse(country_name == "China (Hong Kong, S.A.R., P.R.C.)", 997,
                                  ifelse(country_name == "West Bank and Gaza", 6666, cow_code)))) %>% 
  filter(!is.na(cow_code))

oda %>% 
  count(category_name) %>% 
  arrange(desc(n))


oda %>% 
  group_by(cow_code, year) %>% 
  summarise(annual_oda = sum(oda_amount, na.rm = TRUE)) %>% 
  ungroup() -> oda_sum
