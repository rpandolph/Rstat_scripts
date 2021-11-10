library(janitor)
library(tidyverse)
library(writexl)

#function adapted from mgsub to minimize input errors
recode_country_names <- function(x, original, replace) {
  gsub(original, replace, x, fixed = TRUE)
}


# rename first row as column names, combine into single dataframe
green_roast_data <-row_to_names(green_roast_data, 1)
soluble_data <-row_to_names(soluble_data, 1)
all_exports <- rbind(green_roast_data, soluble_data)

im_green_roast<-row_to_names(im_green_roast,1)
im_soluble<-row_to_names(im_soluble,1)
all_imports <-rbind(im_green_roast, im_soluble)


#remove rows with total, rename columns and add trade flow to later split
all_exports<- all_exports %>% 
  filter(!grepl("total",CTY_NAME, ignore.case = TRUE)) %>% 
  rename(COMMODITY_LDESC = E_COMMODITY_LDESC, COMMODITY = E_COMMODITY) %>% 
    mutate(tradeFlow = "export")

all_imports <-all_imports %>% 
  filter(!grepl("total", CTY_NAME, ignore.case = TRUE)) %>% 
  rename(COMMODITY_LDESC = I_COMMODITY_LDESC, QTY_1_MO=GEN_QY1_MO, 
         ALL_VAL_MO=GEN_VAL_MO,QTY_1_MO_FLAG =GEN_QY1_MO_FLAG, COMMODITY = I_COMMODITY ) %>% 
          mutate(tradeFlow = "import")

#hs codes to be included
us_hs_codes <- c(
  "0901110000",
  "0901110015",
  "0901110025",
  "0901110045",
  "0901110055",
  "0901120000",
  "0901120015",
  "0901120025",
  "0901210035",
  "0901210000",
  "0901210045",
  "0901210055",
  "0901210065",
  "0901220000",
  "0901220035",
  "0901220045",
  "0901220060",
  "2101112126",
  "2101112129",
  "2101112131",
  "2101112139",
  "2101112941",
  "2101112949",
  "2101123200",
  "2101123200",
  "2101123800",
  "2101124800",
  "2101125400",
  "2101125800",
  "2101129000"
)


#combine exports and imports since this processing is identical 
all_coffee <-bind_rows(all_exports, all_imports) %>% 
    filter(COMMODITY %in% us_hs_codes) %>% 
    separate(time, into = c("year", "month"), sep = "-") %>% 
    mutate(value = NA, country_code = NA) %>% 
    filter(QTY_1_MO >= 0.05) %>% 
    rename(quantity = QTY_1_MO, unit_wt = UNIT_QY1, valueUSD = ALL_VAL_MO, hsCode = COMMODITY)
  

stopifnot(dim(filter(all_coffee, valueUSD <= 0))[1]==0)


all_coffee <- all_coffee %>% 
  select(month, year, country_code, CTY_NAME, quantity, unit_wt, value, valueUSD, hsCode, tradeFlow)


original_names <-c("CZECH REPUBLIC", 
                   "WEST BANK ADMINISTERED BY ISRAEL",
                   "ST KITTS AND NEVIS")
replacement_names <- c("Czechia",
                       "State of Palestine",
                       "Saint Kitts and Nevis")

recode_country_names(all_coffee, original_names, replacement_names) 

usa_imports <- all_coffee %>% 
  filter(tradeFlow == "import") %>% 
  select(-tradeFlow)

usa_reexports <-all_coffee %>% 
  filter(tradeFlow == "export") %>% 
  select(-tradeFlow)


write_xlsx(x = usa_imports,  path = ".\\usa_imports.xlsx", col_names = TRUE)
write_xlsx(x = usa_reexports ,  path = ".\\usa_reexports.xlsx", col_names = TRUE)
