library(tidyverse)
library(readxl)
`%notin%` <- Negate(`%in%`)

# Hawala  --------------------------------------------------------------------------------
hawala_path <- "input/analysis/Hawala/Hawala Fee comparison_2023-05-14.xlsx"
#Domestic
hawala_domest_analysis <- read_excel(hawala_path, sheet="domestic_fee_by_month")
hawala_prov_domest <- read_excel(hawala_path, sheet = "domestic_fee_by_month_provinc")

hawala_prov_domest <- rbind(
  hawala_prov_domest,
  hawala_domest_analysis %>% mutate(HAWALA_ORIGIN="Total")
) %>% 
  mutate(Month = str_sub(Month, 1,3), is_equal=NULL, Year = as.character(Year)) %>% 
  rename(mean_ESM_data=mean_integ) %>% 
  arrange(Month)

#International
hawala_internat_analysis <- read_excel(hawala_path, sheet="international_fee_by_month")
hawala_prov_international <- read_excel(hawala_path, sheet = "international_fee_by_month_pr")

hawala_prov_international <- rbind(
  hawala_prov_international,
  hawala_internat_analysis %>% mutate(HAWALA_ORIGIN="Total")
) %>% 
  mutate(Month = str_sub(Month, 1,3), is_equal=NULL, Year = as.character(Year)) %>% 
  rename(mean_ESM_data=mean_integ) %>% 
  arrange(Month)

###Dashboard Data ------------------------------------------------------------------------
path <- "input/Hawala/"
files <- list.files(path)

hawala_domestic_dash <- data.frame()
hawala_international_dash <- data.frame()
# file="Average International Transfer Fee (AFN) (12).xlsx"
for(file in files){
  #Get the type of Transfer Fee
  type = case_when(
    grepl("Domestic", file) ~ "Domestic",
    grepl("International", file) ~ "International"
  )
  #Read
  data <- read_excel(paste0(path,file), skip = 1)
  #Rename
  names(data) <- c("HAWALA_ORIGIN", "10k", "50k", "100k", "500k")
  #Extract month
  year = str_extract(data$HAWALA_ORIGIN[nrow(data)], "(?<=\\(1\\) )(.*?)(?=\\s\\(Year\\))")
  month = str_extract(data$HAWALA_ORIGIN[nrow(data)], "(?<=\\(Quarter\\) \\+ )(.*?)(?=\\s\\(Month\\))")
  #Pivot Longer
  data <- data %>% 
    mutate(Year=year, Month=month) %>% 
    slice(1:which(HAWALA_ORIGIN %in% "Total")) %>% 
    pivot_longer(-c(Year,Month,HAWALA_ORIGIN), names_to = "Range_num_k", values_to = "mean_dash") 
  #rbind
  if(type %in% "Domestic"){
    hawala_domestic_dash <- rbind(hawala_domestic_dash, data)
  } else if(type %in% "International"){
    hawala_international_dash <- rbind(hawala_international_dash, data)
  } else{
    print("Data Type in None!")
  }
}

hawala_domestic_dash <- hawala_domestic_dash %>% 
  mutate(mean_dash=round(mean_dash))
hawala_international_dash <- hawala_international_dash %>% 
  mutate(mean_dash=round(mean_dash))

rm("data", "file", "files", "hawala_domest_analysis", "hawala_internat_analysis", 
   "hawala_path", "month", "path", "type")

#join
hawala_prov_domest_joined <- hawala_prov_domest %>%
  left_join(hawala_domestic_dash) %>% 
  mutate(is_equal_ESM_dash = near(mean_ESM_data, mean_dash),
         is_equal_atr_dash = near(mean_atr, mean_dash))

hawala_prov_international_joined <- hawala_prov_international %>%
  left_join(hawala_international_dash) %>% 
  mutate(is_equal_ESM_dash = near(mean_ESM_data, mean_dash),
         is_equal_atr_dash = near(mean_atr, mean_dash))

hawala_fee_list <- list(
  domestic_fee_by_month=hawala_prov_domest_joined,
  international_fee_by_month=hawala_prov_international_joined
)

writexl::write_xlsx(hawala_fee_list, paste0("output/Hawala_Fee_cross_check_", lubridate::today(),".xlsx"))


