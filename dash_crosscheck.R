library(tidyverse)
library(readxl)
`%notin%` <- Negate(`%in%`)

# FI/NFI Price  --------------------------------------------------
FI_analysis <- read_excel("input/analysis/FI prices_2022-07-17.xlsx")
NFI_analysis <- read_excel("input/analysis/NFI price-2022-07-17.xlsx")

#FI Price
FI_analysis <- FI_analysis %>% 
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
  ), week = as.numeric(week)) %>% filter(stats == "mean") %>% select(-stats)

#NFI
NFI_analysis <- NFI_analysis %>% 
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
  ), week = as.numeric(week)) %>% filter(stats == "mean") %>% select(-stats)

###Dashboard Data
FI_dashboard <- read_excel("input/FI/price/Mean Price (AFN) by Reporting Week and Food Items.xlsx")
NFI_dashboard <- read_excel("input/NFI/price/Mean Price (AFN) by Reporting Week and Non-Food Items.xlsx")

FI_dashboard <- FI_dashboard %>% 
  rename(year = `Year > Reporting Week`,
         week = `Week of the Year - Name`) %>% 
  filter(week %notin% "Total" & year %in% c("2021", "2022")) %>%
  pivot_longer(-c(year,week), names_to = "items", values_to = "dash_val") %>% 
  mutate(items = str_extract(items, "^[^\\(]+") %>% str_squish(),
         week = as.numeric(str_remove(week, "Wk ")), 
         dash_val = round(dash_val, 2),
         year=NULL)

NFI_dashboard <- NFI_dashboard %>% 
  rename(year = `Year > Reporting Week`,
         week = `Week of the Year - Name`) %>% 
  filter(week %notin% "Total" & year %in% c("2021", "2022")) %>%
  pivot_longer(-c(year,week), names_to = "items", values_to = "dash_val") %>% 
  mutate(items = case_when(
    grepl("Tailor|Rent", items) ~ items,
    TRUE ~ str_extract(items, "^[^\\(]+") %>% str_squish()
  ),
  week = as.numeric(str_remove(week, "Wk ")), 
  dash_val = round(dash_val, 2),
  year=NULL)

#all dash items in analysis
which(unique(FI_dashboard$items) %notin% unique(FI_analysis$items))
which(unique(NFI_dashboard$items) %notin% unique(NFI_analysis$items))

#Join
FI_analysis <- FI_analysis %>%
  left_join(FI_dashboard, by=c("week", "items")) %>% 
  mutate(is_equal = near(atr_values, dash_val))

NFI_analysis <- NFI_analysis %>%
  left_join(NFI_dashboard, by=c("week", "items")) %>% 
  mutate(is_equal = near(atr_values, dash_val))

Price_list <- list(
  FI_analysis=FI_analysis,
  NFI_analysis=NFI_analysis
)

writexl::write_xlsx(Price_list, paste0("output/Price_Crosscheck_", lubridate::today(),".xlsx"))


# FI/NFI Availability  --------------------------------------------------
FI_availability <- readxl::read_excel("input/analysis/FI availability_2022-07-17.xlsx")
NFI_availability <- readxl::read_excel("input/analysis/NFI availability_2022-07-17.xlsx")

#FI
FI_availability <- FI_availability %>% 
  mutate(availability = case_when(
    availability == "Regularly available" ~ "Regular availability",
    TRUE ~ availability
  ), 
  items = case_when(
    items == "Locally produced wheat flour" ~ "Local wheat flour",
    items == "low-quality rice" ~ "Low quality rice",
    items == "high-quality rice" ~ "High quality rice",
    TRUE ~ items
  ), week = as.numeric(week)) %>% rename(atr_freq = n)

#NFI
NFI_availability <- NFI_availability %>% 
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
  ), week = as.numeric(week)) %>% rename(atr_freq = n)

#Dashbaord
path <- "input/FI/availability/"
files <- list.files(path)
#**add the extra week each time
file_numbers <- c(NA,1:31)
#we dnt have week18
week_numbers = c(46:52,1:17,19:26)

FI_avail_dash <- data.frame()
NFI_avail_dash <- data.frame()
for(file in files){
  file_num <- str_extract(file, "[0-9]+")
  week_num <- week_numbers[which(file_numbers %in% file_num)]
  nfi_path <- "input/NFI/availability/"
  # nfi_file <- str_replace(file, ".xlsx", ".csv")
  
  fi_data <- read_excel(paste0(path,file), skip = 2) %>% 
    mutate(week = week_num) %>% 
    rename(items=Item,
           availability=Availability,
           dash_val= `Value (%)`)
  
  nfi_data <- read_excel(paste0(nfi_path,file), skip = 2) %>%
    mutate(week = week_num) %>%
    rename(items=Item,
           availability=Availability,
           dash_val= `Value (%)`)

  
  FI_avail_dash <- rbind(FI_avail_dash, fi_data)
  NFI_avail_dash <- rbind(NFI_avail_dash, nfi_data)
}
rm("fi_data", "nfi_data", "path", "files", "nfi_path", "nfi_file", "file_num", "week_num")

# #temporary
# NFI_avail_dash <- read_excel("input/NFI/availability/Availability by Item.xlsx", skip = 2) %>%
#   mutate(week = 16) %>% 
#   rename(items=Item,
#          availability=Availability,
#          dash_val= `Value (%)`)


#all dash items in analysis
which(unique(FI_avail_dash$items) %notin% unique(FI_availability$items))
which(unique(NFI_avail_dash$items) %notin% unique(NFI_availability$items))

#join
FI_availability <- FI_availability %>%
  left_join(FI_avail_dash, by=c("week", "items", "availability")) %>% 
  mutate(is_equal = near(atr_freq, dash_val))

NFI_availability <- NFI_availability %>%
  left_join(NFI_avail_dash, by=c("week", "items", "availability")) %>% 
  mutate(is_equal = near(atr_freq, dash_val))

Availability_list <- list(
  FI_availability=FI_availability,
  NFI_availability=NFI_availability
)

writexl::write_xlsx(Availability_list, paste0("output/Availability_Crosscheck_", lubridate::today(),".xlsx"))


