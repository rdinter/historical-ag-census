# Robert Dinterman
# KX values at the county level
# Utilizes data downloaded from ICPSR project 35206
# https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35206#

# ---- start --------------------------------------------------------------

library("stringr")
library("tidyverse")

# Create a directory for the data
local_dir    <- "0-data/ICPSR"
data_source <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)

# ---- map-solution -------------------------------------------------------

wide_vars <- read_csv("0-data/varlists/KX-vars - Sheet1.csv") %>% 
  filter(year > 1840)

wide_vars_list <- list(x = paste0(data_source, "/", wide_vars$file),
                       y = wide_vars$year)
# wide_vars_list$x[20]->x
# wide_vars_list$y[20]->y

j5 <- pmap(wide_vars_list, function(x, y) {
  print(x)
  temp        <- read_tsv(x, col_types = cols(.default = "c"))
  names(temp) <- tolower(names(temp))
  temp$year   <- y
  
  vars_here <- wide_vars %>%
    filter(year == y) %>%
    select_if(~sum(!is.na(.)) > 0) %>%
    select(-file, -year) %>% 
    slice(1) %>%
    unlist()
  
  temp <- temp %>%
    rename_at(vars(all_of(vars_here)), ~names(vars_here)) %>% 
    select(year, names(vars_here)) %>% 
    group_by(name) %>% 
    mutate_if(is_character, parse_number)
  
  return(temp)
})

census <- j5 %>% 
  bind_rows() %>% 
  mutate(farms = if_else(year == 1860, farm39 + farm1019 + farm2049 +
                           farm5099 + farm100 + farm500 + farm1000, farms),
         acres = case_when(year < 1870 ~ acres_improved + acres_unimproved,
                           year == 1870 ~ acres_improved + acres_unimproved +
                             acres_wooded,
                           T ~ acres)) %>% 
  select(-farm39, -farm1019, -farm2049, -farm5099, -farm100, -farm500,
         -farm1000, -acres_improved, -acres_unimproved, -acres_wooded)

# Fill in those missing FIPS codes from 1954 and 1959
census <- census %>% 
  filter(!is.na(farms)) %>% 
  arrange(year) %>% 
  group_by(state, county, name) %>% 
  fill(fips)

census <- census %>% 
  mutate_at(vars(contains("val")), function(x) x*1000) %>% 
  mutate(farmland_val = farmland_val/1000,
         farmland_val_per_farm = farmland_val_per_farm/1000,
         farmland_val_per_acre = farmland_val_per_acre/1000,
         stfips = floor(fips / 1000),
         ctyfips = fips - 1000*stfips)

census <- census %>% 
  mutate(farmland_val_per_farm = if_else(is.na(farmland_val_per_farm),
                                         farmland_val / farms / 1000,
                                         farmland_val_per_farm),
         farmland_val_per_acre = if_else(is.na(farmland_val_per_acre),
                                         farmland_val / acres / 1000,
                                         farmland_val_per_acre),
         farmland_val = if_else(is.na(farmland_val),
                                (farmland_val_per_acre*acres +
                                   farmland_val_per_farm*farms)/2,
                                farmland_val),
         acres = if_else(is.na(acres),
                         acres_full_owned + acres_part + acres_tenant_rented +
                           acres_managers,
                         acres),
         avg_age = if_else(year %in% c(1978, 1982, 1987),
                           avg_age / 10,
                           avg_age),
         avg_age = if_else(avg_age > 1000,
                           avg_age / 100,
                           avg_age))

# United States with missing fips for a few years
us_census <- census %>% 
  filter(county == 0,
         !grepl("U\\.S\\.", name, ignore.case = T),
         !grepl("UNITED STATE", name, ignore.case = T)) %>% 
  group_by(year) %>% 
  summarise(state = 99, county = 0, name = "US",
            fips = 99000, stfips = 99, ctyfips = 0,
            avg_age = weighted.mean(avg_age, farms, na.rm = T),
            farms = sum(farms, na.rm = T),
            acres = sum(acres, na.rm = T),
            farmland_val = sum(farmland_val, na.rm = T),
            farmland_val_per_farm = farmland_val / farms,
            farmland_val_per_acre = farmland_val / acres) %>% 
  mutate_all(~replace(., is.nan(.), NA))

# Add in the summaries of the US
census_all <-
  census %>% 
  filter(state != 100) %>% 
  bind_rows(us_census) %>% 
  arrange(year, fips)

census_all %>% 
  ungroup() %>% 
  filter(stfips %in% c(39, 99), ctyfips != 998) %>% 
  arrange(fips, year) %>% 
  select(year, stfips, ctyfips, fips, name, avg_age, farms, acres) %>%
  write_csv(paste0(local_dir, "/kx_counties_1850.csv"))
