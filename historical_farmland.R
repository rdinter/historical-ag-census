# Robert Dinterman
# Farmland values at the county level
# Utilizes data downloaded from ICPSR project 35206
# https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35206#

# ---- start --------------------------------------------------------------

library("stringr")
library("tidyverse")

wide_vars <- read_csv("vars-wide.csv") %>% 
  filter(year > 1840)
j5 <- read_tsv("raw/data/35206-0001-Data.tsv")

names(j5) <- tolower(names(j5))

j6 <- j5 %>% 
  select(faval850:fips) %>% 
  group_by(fips) %>% 
  gather(year, farmland_val_per_acre, -fips) %>% 
  mutate(year = parse_number(str_replace(year, "faval", "1")))

# write_csv(j6, "county_farmland_to_1959.csv")

# ---- map-solution -------------------------------------------------------

wide_vars_list <- list(x = paste0("raw/data/", wide_vars$file),
                       y = wide_vars$year)


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
    rename_at(vars(vars_here), ~names(vars_here)) %>% 
    select(year, names(vars_here)) %>% 
    group_by(name) %>% 
    mutate_all(parse_number)
  
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
  mutate(farmland_val_per_farm = farmland_val_per_farm/1000,
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
                                1000*(farmland_val_per_acre*acres +
                                        farmland_val_per_farm*farms)/2,
                                farmland_val),
         acres = if_else(is.na(acres),
                         acres_full_owned + acres_part + acres_tenant_rented +
                           acres_managers,
                         acres))

census %>% 
  filter(ctyfips != 0, !is.na(farms)) %>% 
  arrange(fips, year) %>% 
  select(year, stfips, ctyfips, fips, everything()) %>% 
  write_csv("icpsr_counties_1850.csv")

# ---- 1954 ---------------------------------------------------------------

j5 <- read_tsv("raw/35206-0025-Data.tsv", col_types = cols(.default = "c"))
names(j5) <- tolower(names(j5))
j5$year <- 1954

vars54 <- wide_vars %>% 
  filter(year == 1954) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% 
  select(-file, -year) %>% 
  slice(1) %>% 
  unlist()

j5 <- j5 %>%
  rename_at(vars(vars54), ~names(vars54)) %>% 
  select(year, state, county, name, names(vars54)) %>% 
  group_by(name) %>% 
  mutate_all(parse_number)

j6 <- bind_rows(j5, j6)

# ---- 1959 ---------------------------------------------------------------

j5 <- read_tsv("raw/35206-0027-Data.tsv", col_types = cols(.default = "c"))
names(j5) <- tolower(names(j5))
j5$year <- 1959

vars59 <- wide_vars %>% 
  filter(year == 1959) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% 
  select(-file, -year) %>% 
  slice(1) %>% 
  unlist()

j5 <- j5 %>%
  rename_at(vars(vars59), ~names(vars59)) %>% 
  select(year, state, county, name, names(vars59)) %>% 
  group_by(name) %>% 
  mutate_all(parse_number)

j6 <- bind_rows(j5, j6)


# ---- 1964 ---------------------------------------------------------------

j5 <- read_tsv("raw/35206-0030-Data.tsv", col_types = cols(.default = "c"))
names(j5) <- tolower(names(j5))
j5$year <- 1964

vars64 <- c("var1" = "farms", "var4" = "acres",
            "var6" = "farmland_val_per_farm",
            "var7" = "farmland_val_per_acre",
            "var425" = "income_hh_reporting",
            "var426" = "income_val",
            "var433" = "income_rent_farms_reporting",
            "var434" = "income_rent_val",
            "var151" = "acres_full_owned",
            "var152" = "acres_part",
            "var153" = "acres_managers",
            "var154" = "acres_tenant_rented")

j5 <- j5 %>%
  rename_at(vars(names(vars64)), ~vars64) %>% 
  select(year, fips, vars64) %>% 
  mutate_all(parse_number)

j6 <- bind_rows(j5, j6)

# ---- 1969 ---------------------------------------------------------------

j5 <- read_tsv("raw/35206-0033-Data.tsv", col_types = cols(.default = "c"))
names(j5) <- tolower(names(j5))
j5$year <- 1969

vars69 <- c("item01001" = "farms",
            "item01002" = "acres",
            "item01006" = "farmland_val",
            "item01007" = "farmland_val_per_farm",
            "item01008" = "farmland_val_per_acre",
            "item04010" = "income_rent_farms_reporting",
            "item04011" = "income_rent_val",
            "item02005" = "acres_full_owned",
            "item02008" = "acres_part",
            "item02011" = "acres_tenant_rented")

j5 <- j5 %>%
  rename_at(vars(names(vars69)), ~vars69) %>% 
  select(year, fips, vars69) %>% 
  mutate_all(parse_number) %>% 
  mutate_at(vars(farmland_val, income_rent_val), function(x) x*1000)

j6 <- bind_rows(j5, j6)


# ---- 1974 ---------------------------------------------------------------

j5 <- read_tsv("raw/35206-0036-Data.tsv", col_types = cols(.default = "c"))
names(j5) <- tolower(names(j5))
j5$year <- 1974

vars74 <- c("item01001" = "farms",
            "item01002" = "acres",
            "item01006" = "farmland_val",
            "item01007" = "farmland_val_per_farm",
            "item01008" = "farmland_val_per_acre",
            "item04010" = "income_rent_farms_reporting",
            "item04011" = "income_rent_val",
            "item02005" = "acres_full_owned",
            "item02008" = "acres_part",
            "item02011" = "acres_tenant_rented")

j5 <- j5 %>%
  rename_at(vars(names(vars74)), ~vars74) %>% 
  select(year, fips, vars74) %>% 
  mutate_all(parse_number) %>% 
  mutate_at(vars(farmland_val, income_rent_val), function(x) x*1000)

j6 <- bind_rows(j5, j6)

# ---- 1978 ---------------------------------------------------------------

j5 <- read_tsv("raw/35206-0039-Data.tsv", col_types = cols(.default = "c"))
names(j5) <- tolower(names(j5))
j5$year <- 1978

vars78 <- c("item01001" = "farms",
            "item01002" = "acres",
            "item01004" = "farmland_val_per_farm",
            "item01005" = "farmland_val_per_acre",
            "item03036" = "interest_farms",
            "item03037" = "interest_val",
            "item03038" = "interest_re_farms",
            "item03039" = "interest_re_val",
            "item03042" = "cash_rent_farms",
            "item03043" = "cash_rent_val",
            "item03044" = "property_tax_farms",
            "item03045" = "property_tax_val",
            "item04001" = "net_cash_farms",
            "item04002" = "net_cash_val",
            "item04022" = "income_rent_farms_reporting",
            "item04023" = "income_rent_val",
            "item10006" = "acres_full_owned",
            "item10010" = "acres_part",
            "item10011" = "acres_part_owned",
            "item10012" = "acres_part_rented",
            "item10016" = "acres_tenant_rented")

j5 <- j5 %>%
  rename_at(vars(names(vars78)), ~vars78) %>% 
  select(year, fips, vars78) %>% 
  mutate_all(parse_number) %>% 
  mutate_at(vars(interest_val, interest_re_val, cash_rent_val,
                 property_tax_val,net_cash_val, income_rent_val),
            function(x) x*1000)

j6 <- bind_rows(j5, j6)

# ---- 1982 ---------------------------------------------------------------

j5 <- read_tsv("raw/35206-0040-Data.tsv", col_types = cols(.default = "c"))
names(j5) <- tolower(names(j5))
j5$year <- 1982

vars82 <- c("item01001" = "farms",
            "item01002" = "acres",
            "item01004" = "farmland_val_per_farm",
            "item01005" = "farmland_val_per_acre",
            "item03036" = "interest_farms",
            "item03037" = "interest_val",
            "item03038" = "interest_re_farms",
            "item03039" = "interest_re_val",
            "item03042" = "cash_rent_farms",
            "item03043" = "cash_rent_val",
            "item03044" = "property_tax_farms",
            "item03045" = "property_tax_val",
            "item04001" = "net_cash_farms",
            "item04002" = "net_cash_val",
            "item04022" = "income_rent_farms_reporting",
            "item04023" = "income_rent_val",
            "item10006" = "acres_full_owned",
            "item10010" = "acres_part",
            "item10011" = "acres_part_owned",
            "item10012" = "acres_part_rented",
            "item10016" = "acres_tenant_rented")

j5 <- j5 %>%
  rename_at(vars(names(vars82)), ~vars82) %>% 
  select(year, fips, vars82) %>% 
  mutate_all(parse_number) %>% 
  mutate_at(vars(interest_val, interest_re_val, cash_rent_val,
                 property_tax_val,net_cash_val, income_rent_val),
            function(x) x*1000)

j6 <- bind_rows(j5, j6)

# ---- 1987 ---------------------------------------------------------------

j5 <- read_tsv("raw/35206-0041-Data.tsv", col_types = cols(.default = "c"))
names(j5) <- tolower(names(j5))
j5$year <- 1987

vars87 <- c("item01001" = "farms",
            "item01002" = "acres",
            "item01004" = "farmland_val_per_farm",
            "item01005" = "farmland_val_per_acre",
            "item03036" = "interest_farms",
            "item03037" = "interest_val",
            "item03038" = "interest_re_farms",
            "item03039" = "interest_re_val",
            "item03042" = "cash_rent_farms",
            "item03043" = "cash_rent_val",
            "item03044" = "property_tax_farms",
            "item03045" = "property_tax_val",
            "item04001" = "net_cash_farms",
            "item04002" = "net_cash_val",
            "item04022" = "income_rent_farms_reporting",
            "item04023" = "income_rent_val",
            "item10006" = "acres_full_owned",
            "item10010" = "acres_part",
            "item10011" = "acres_part_owned",
            "item10012" = "acres_part_rented",
            "item10016" = "acres_tenant_rented")

j5 <- j5 %>%
  rename_at(vars(names(vars87)), ~vars87) %>% 
  select(year, fips, vars87) %>% 
  mutate_all(parse_number) %>% 
  mutate_at(vars(interest_val, interest_re_val, cash_rent_val,
                 property_tax_val,net_cash_val, income_rent_val),
            function(x) x*1000)

j6 <- bind_rows(j5, j6)

# ---- 1992 ---------------------------------------------------------------

j5 <- read_tsv("raw/35206-0042-Data.tsv", col_types = cols(.default = "c"))
names(j5) <- tolower(names(j5))
j5$year <- 1992

vars92 <- c("item010001" = "farms",
            "item010002" = "acres",
            "item010004" = "farmland_val_per_farm",
            "item010005" = "farmland_val_per_acre",
            "item030036" = "interest_farms",
            "item030037" = "interest_val",
            "item030038" = "interest_re_farms",
            "item030039" = "interest_re_val",
            "item030042" = "cash_rent_farms",
            "item030043" = "cash_rent_val",
            "item030044" = "property_tax_farms",
            "item030045" = "property_tax_val",
            "item040001" = "net_cash_farms",
            "item040002" = "net_cash_val",
            "item040021" = "income_rent_farms_reporting",
            "item040022" = "income_rent_val",
            "item110006" = "acres_full_owned",
            "item110010" = "acres_part",
            "item110011" = "acres_part_owned",
            "item110012" = "acres_part_rented",
            "item110016" = "acres_tenant_rented")

j5 <- j5 %>%
  rename_at(vars(names(vars92)), ~vars92) %>% 
  select(year, fips, vars92) %>% 
  mutate_all(parse_number) %>% 
  mutate_at(vars(interest_val, interest_re_val, cash_rent_val,
                 property_tax_val,net_cash_val, income_rent_val),
            function(x) x*1000)

j6 <- bind_rows(j5, j6)


# ---- 1997 ---------------------------------------------------------------

j5 <- read_tsv("raw/35206-0043-Data.tsv", col_types = cols(.default = "c"))
names(j5) <- tolower(names(j5))
j5$year <- 1997

vars97 <- c("item01001" = "farms",
            "item01002" = "acres",
            "item01005" = "farmland_val_per_farm",
            "item01006" = "farmland_val_per_acre",
            "item03036" = "interest_farms",
            "item03037" = "interest_val",
            "item03038" = "interest_re_farms",
            "item03039" = "interest_re_val",
            "item03042" = "cash_rent_farms",
            "item03043" = "cash_rent_val",
            "item03044" = "property_tax_farms",
            "item03045" = "property_tax_val",
            "item04001" = "net_cash_farms",
            "item04002" = "net_cash_val",
            "item04022" = "income_rent_farms_reporting",
            "item04023" = "income_rent_val",
            "item11006" = "acres_full_owned",
            "item11010" = "acres_part",
            "item11011" = "acres_part_owned",
            "item11012" = "acres_part_rented",
            "item11016" = "acres_tenant_rented")

j5 <- j5 %>%
  rename_at(vars(names(vars97)), ~vars97) %>% 
  select(year, fips, vars97) %>% 
  mutate_all(parse_number) %>% 
  filter(!is.na(farms)) %>% 
  mutate_at(vars(interest_val, interest_re_val, cash_rent_val,
                 property_tax_val,net_cash_val, income_rent_val),
            function(x) x*1000)

j6 <- bind_rows(j5, j6)

j6 %>% 
  mutate(stfips = floor(fips / 1000),
         ctyfips = fips - 1000*stfips,
         farmland_val = if_else(is.na(farmland_val),
                                farmland_val_per_farm*farms,
                                farmland_val)) %>% 
  filter(ctyfips != 0) %>% 
  arrange(fips, year) %>% 
  select(year, stfips, ctyfips, fips, everything()) %>% 
  write_csv("icpsr_county_historical.csv")
