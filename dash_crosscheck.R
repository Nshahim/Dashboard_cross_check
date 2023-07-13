library(tidyverse)
library(readxl)
`%notin%` <- Negate(`%in%`)

# FI/NFI Price  --------------------------------------------------
FI_analysis <- read_excel("input/analysis/FI-NFI/FI price comparison_2023-07-13.xlsx")
NFI_analysis <- read_excel("input/analysis/FI-NFI/NFI price comparison_2023-07-13.xlsx")

#FI Price
FI_analysis <- FI_analysis %>% 
  rename(is_equal_atr_integ=is_equal) %>% 
  mutate(items = case_when(
    items == "Locally produced wheat flour" ~ "Local wheat flour",
    items == "low-quality rice" ~ "Low quality rice",
    items == "Beef (with bone)" ~ "Beef with bone",
    items == "Clean chicken (farm)" ~ "Clean chicken",
    items == "Eggs" ~ "Egg",
    items == "Green tea (loose leaf)" ~ "Green tea",
    items == "high-quality rice" ~ "High quality rice",
    items == "Milk (fresh)" ~ "Fresh Milk",
    items == "Nan (bread)" ~ "Nan",
    items == "Red Beans (dried)" ~ "Dried Red Beans",
    TRUE ~ items
  ), 
  month = str_sub(month, 1,3),
  year = as.character(year)) %>% filter(stats == "mean") %>% select(-stats)

#NFI
NFI_analysis <- NFI_analysis %>% 
  rename(is_equal_atr_integ=is_equal) %>% 
  mutate(items = case_when(
    items == "Children’s shoes (commonly used/low quality)" ~ "Children's shoes",
    items == "Fabric for boys (child)" ~ "Fabric for boys",
    items == "Fabric for girls (child)" ~ "Fabric for girls",
    items == "Men’s shoes (commonly used, low quality)" ~ "Men's shoes",
    items == "Notebooks for students (40 pages, ruled/commonly used/regular quality notebook)" ~ "Notebook",
    items == "Pens (commonly used/regular quality)" ~ "Pen",
    items == "Soap (bar)" ~ "Soap",
    items == "Women’s shoes (commonly used, low quality)" ~ "Women's shoes",
    items == "Shared taxi/van/risckshaw driver" ~ "Shared transport",
    items == "Shared taxi/van/rickshaw" ~ "Shared transport",
    items == "Men’s haircut" ~ "Men's haircut",
    items == "One man’s perahan wa tonban" ~ "Tailor services (One man's clothing)",
    items == "One girl’s dress" ~ "Tailor services (One child's (girl) clothing)",
    items == "One child’s (boy) perahan wa tonban" ~ "Tailor services (One child's (boy) clothing)",
    items == "One women’s perahan" ~ "Tailor services (One woman's clothing)",
    items ==  "Doctor Consultation Fee" ~ "Doctor visit",
    items == "4 Bedroom house" ~ "Accommodation Rent (4 bedroom)",
    items == "3 Bedroom house" ~ "Accommodation Rent (3 bedroom)",
    items == "national calls within your company network" ~ "Phone call within the same network",
    items == "national calls to other networks" ~ "Phone call to other networks",
    TRUE ~ items
  ), 
  month = str_sub(month, 1,3),
  year = as.character(year)) %>% filter(stats == "mean") %>% select(-stats)

###Dashboard Data ------------------------------------------------------------------------
FI_dashboard <- read_excel("input/FI/price/Mean Price (AFN) by Reporting Month and Food Items.xlsx")
NFI_dashboard <- read_excel("input/NFI/price/Mean Price (AFN) by Reporting Month and Non-Food Items.xlsx")

FI_dashboard <- FI_dashboard %>% 
  rename(year = `Year > Reporting Month`,
         month = Month) %>% 
  filter(month %notin% "Total" & year %in% c("2021", "2022", "2023")) %>%
  pivot_longer(-c(year,month), names_to = "items", values_to = "dash_val") %>% 
  mutate(items = str_extract(items, "^[^\\(]+") %>% str_squish(),
         items = case_when(
           items == "Bananas: 1 dozen" ~ "Bananas",
           TRUE ~ items
         ),
         dash_val = round(dash_val, 2))

NFI_dashboard <- NFI_dashboard %>% 
  rename(year = `Year > Reporting Month`,
         month = Month) %>% 
  filter(month %notin% "Total" & year %in% c("2021", "2022", "2023")) %>%
  pivot_longer(-c(year,month), names_to = "items", values_to = "dash_val") %>% 
  mutate(items = case_when(
    grepl("Tailor|Rent", items) ~ items,
    TRUE ~ str_extract(items, "^[^\\(]+") %>% str_squish()
  ),
  dash_val = round(dash_val, 2))

#all dash items in analysis
unique(FI_dashboard$items)[unique(FI_dashboard$items) %notin% unique(FI_analysis$items)]
which(unique(NFI_dashboard$items) %notin% unique(NFI_analysis$items))

#Join
FI_analysis_joined <- FI_analysis %>%
  left_join(FI_dashboard, by=c("year", "month", "items")) %>% 
  mutate(is_equal_atr_dash = near(atr_values, dash_val),
         is_equal_dash_integ = near(integrity_values, dash_val))

NFI_analysis_joined <- NFI_analysis %>%
  left_join(NFI_dashboard, by=c("year", "month", "items")) %>% 
  mutate(is_equal_atr_dash = near(atr_values, dash_val),
         is_equal_dash_integ = near(integrity_values, dash_val))

Price_list <- list(
  FI_analysis=FI_analysis_joined,
  NFI_analysis=NFI_analysis_joined
)

writexl::write_xlsx(Price_list, paste0("output/Price_Crosscheck_", lubridate::today(),".xlsx"))


# FI/NFI Availability  -------------------------------------------------------------------
FI_availability <- readxl::read_excel("input/analysis/FI-NFI/FI availability comparison_2023-07-13.xlsx")
NFI_availability <- readxl::read_excel("input/analysis/FI-NFI/NFI availability comparison_2023-07-13.xlsx")

#FI
FI_availability <- FI_availability %>% 
  rename(is_equal_atr_integ=is_equal) %>% 
  select(-c(atr_percent, integrity_percent)) %>% 
  mutate(availability = case_when(
    availability == "Regularly available" ~ "Regular availability",
    TRUE ~ availability
  ), 
  items = case_when(
    items == "Locally produced wheat flour" ~ "Local wheat flour",
    items == "low-quality rice" ~ "Low quality rice",
    items == "high-quality rice" ~ "High quality rice",
    TRUE ~ items
  ), year=as.character(year), month = str_sub(month, 1,3))

#NFI
NFI_availability <- NFI_availability %>% 
  rename(is_equal_atr_integ=is_equal) %>% 
  select(-c(atr_percent, integrity_percent)) %>% 
  mutate(items = case_when(
    items == "Children’s shoes (commonly used/low quality)" ~ "Children's shoes",
    items == "Fabric for boys (child)" ~ "Fabric for boys",
    items == "Fabric for girls (child)" ~ "Fabric for girls",
    items == "Men’s shoes (commonly used, low quality)" ~ "Men's shoes",
    items == "Notebooks for students (40 pages, ruled/commonly used/regular quality notebook)" ~ "Notebook",
    items == "Pens (commonly used/regular quality)" ~ "Pen",
    items == "Soap (bar)" ~ "Soap",
    items == "Women’s shoes (commonly used, low quality)" ~ "Women's shoes",
    TRUE ~ items
  ),
  availability = case_when(
    availability == "Regularly available" ~ "Regular availability",
    TRUE ~ availability
  ), year=as.character(year), month = str_sub(month, 1,3))

#Dashbaord
path <- "input/FI/availability/"
files <- list.files(path)
FI_avail_dash <- data.frame()
NFI_avail_dash <- data.frame()

for(file in files){
  nfi_path <- "input/NFI/availability/"
  
  # FI
  fi_data <- read_excel(paste0(path,file))
  year=str_extract(names(fi_data)[1], "(?<=\\(1\\) )(.*?)(?=\\s\\(Year\\))")
  month=str_extract(names(fi_data)[1], "(?<=\\(Quarter\\) \\+ )(.*?)(?=\\s\\(Month\\))")
  fi_data <- fi_data %>% 
    janitor::row_to_names(2) %>% 
    rename(items=Item,
           availability=Availability,
           dash_val= `Value (%)`) %>% 
    mutate(year=year, month=month, dash_val=as.numeric(dash_val))

  
  # NFI
  nfi_data <- read_excel(paste0(nfi_path,file))
  year=str_extract(names(nfi_data)[1], "(?<=\\(1\\) )(.*?)(?=\\s\\(Year\\))")
  month=str_extract(names(nfi_data)[1], "(?<=\\(Quarter\\) \\+ )(.*?)(?=\\s\\(Month\\))")
  nfi_data <- nfi_data %>% 
    janitor::row_to_names(2) %>% 
    rename(items=Item,
           availability=Availability,
           dash_val= `Value (%)`) %>% 
    mutate(year=year, month=month, dash_val=as.numeric(dash_val))


  # Merging
  FI_avail_dash <- rbind(FI_avail_dash, fi_data)
  NFI_avail_dash <- rbind(NFI_avail_dash, nfi_data)
}
rm("fi_data", "nfi_data", "path", "files", "nfi_path", "nfi_file", "file_num")

#all dash items in analysis
which(unique(FI_avail_dash$items) %notin% unique(FI_availability$items))
which(unique(NFI_avail_dash$items) %notin% unique(NFI_availability$items))

#join
FI_availability_joined <- FI_availability %>%
  left_join(FI_avail_dash, by=c("year", "month", "items", "availability")) %>% 
  mutate(is_equal_atr_dash = near(atr_freq, dash_val),
         is_equal_dash_integ = near(integrity_freq, dash_val))

NFI_availability_joined <- NFI_availability %>%
  left_join(NFI_avail_dash, by=c("year", "month", "items", "availability")) %>% 
  mutate(is_equal_atr_dash = near(atr_freq, dash_val),
         is_equal_dash_integ = near(integrity_freq, dash_val))

Availability_list <- list(
  FI_availability=FI_availability_joined,
  NFI_availability=NFI_availability_joined
)

writexl::write_xlsx(Availability_list, paste0("output/Availability_Crosscheck_", lubridate::today(),".xlsx"))


