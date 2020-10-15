#This script takes a eurostat csv file for EU tradde and reformats for upload to ICO's statisical database
library(tidyverse)
library(readxl)
library(writexl)


#function taken from mgsub
recode_values <- function(x, original, replace) {
  result <- x
  for (i in 1:length(original)) {
    result <- gsub(original[i], replace[i], result, fixed = TRUE)
  }
  result
}

#function to export list of df to excel
exp_df_list_excel <- function(x, path, endname = ".xlsx") {
  filename <- list()
  (for (i in 1:length(x)) {
    filename[i] <- paste0(path, names(x[i]), endname, collapse = "")
  })
  (for (i in 1:length(x)) {
    write_xlsx(x = x[[i]],
               path = filename[[i]],
               col_names = TRUE)
  })
  print("Done")
}


#import data file from eurostat bulk download and excel file with ICO's ISO labels
workbook_location <-
  "D:/documents/R/eurostat/k16105648/data-16105648.csv"
EU_data <- read.csv(workbook_location, stringsAsFactors = FALSE) %>%
  select(-REPORTER,-FLOW,-PERIOD_LAB,-INDICATORS_LAB)

ico_Country <-
  read_excel("D:/documents/R/raw_data/ico_Country_Codes_Names.xlsx")

#remove aggregates from partners (eg. exports to EU-19)
eu_df <- EU_data %>%
  filter(nchar(PARTNER) <= 2)

#pivot wider to put volume/value in same row and separate period into month/year
eu_df <- eu_df %>%
  pivot_wider(names_from = INDICATORS, values_from = INDICATOR_VALUE) %>%
  filter(!is.na(QUANTITY_IN_100KG)) %>%
  separate(PERIOD, into = c("year", "month"),  sep = 4)


#This code chunk checks for differences between ICO ISO codes and EU ones
#create df of unique EU codes and merge with ICO excel
code_list <- eu_df %>%
  distinct(PARTNER, PARTNER_LAB)
code_no_match <-
  anti_join(code_list,
            ico_Country,
            by = c("PARTNER" = "isoCode"),
            keep = TRUE)

iso_to_replace <- code_no_match$PARTNER
iso_replacement <- c("RS",
                     "QQ",
                     "AN",
                     "QU",
                     "QU",
                     "QQ",
                     "XF",
                     "QU",
                     "QU")

#replace eu codes with ico codes (typically these are the unreserved codes from ISO)
eu_df$PARTNER <-
  recode_values(eu_df$PARTNER, iso_to_replace, iso_replacement)

#change reporter names to shorter version for file names
original_names <- eu_df %>%
  select(REPORTER_LAB) %>%
  filter(nchar(REPORTER_LAB) > 20) %>%
  unique() %>%
  pull()

replacement_names <- c("Belgium",
                       "Germany",
                       "Spain",
                       "France",
                       "Italy")

eu_df$REPORTER_LAB <-
  recode_values(eu_df$REPORTER_LAB, original_names, replacement_names)

#sum up new totals-if any- created by recoding and remove records with no volume
eu_df <-  eu_df %>%
  group_by(REPORTER_LAB,
           PARTNER,
           PRODUCT,
           PRODUCT_LAB,
           FLOW_LAB,
           year,
           month) %>%
  mutate(volume = sum(QUANTITY_IN_100KG),
         value = sum(VALUE_IN_EUROS)) %>%
  ungroup() %>%
  select(-QUANTITY_IN_100KG,-VALUE_IN_EUROS) %>%
  filter(volume > 0)

#change PRODUCT to appropriate hs codes as characters
eu_df$PRODUCT <- as.character(eu_df$PRODUCT)
original_hscodes <- c("90111",
                      "90112",
                      "90121",
                      "90122",
                      "210112")

replacement_hs <- c("090111",
                    "090112",
                    "090121",
                    "090122",
                    "21011292")
eu_df$PRODUCT <-
  recode_values(eu_df$PRODUCT, original_hscodes, replacement_names)

#add in columns needed for auto-import into ICO stats db
eu_df["wt_unit"] <- "100kg"
eu_df["usd_val"] <-NA #won't be needed after migration to new DB since can save format for aut upload

#sort columns in the right order for auto-import into ICO stats db and remove UK
eu_df <- eu_df %>%
  select(
    month,
    year,
    PARTNER,
    PARTNER_LAB,
    volume,
    wt_unit,
    value,
    usd_val,
    PRODUCT,
    REPORTER_LAB,
    FLOW_LAB
  ) %>%
  filter(REPORTER_LAB != "United Kingdom")

#create new dfs of just re-exports and imports (these have to be uploaded separately by country)
eu_reExports <- eu_df %>%
  filter(FLOW_LAB == "EXPORT") %>%
  select(-FLOW_LAB)

eu_imports <- eu_df %>%
  filter(FLOW_LAB == "IMPORT") %>%
  select(-FLOW_LAB)

#Split file by importing country
ex_split <- split(eu_reExports, eu_reExports$REPORTER_LAB) %>%
  lapply(function(y) {
    y$reporter_name <- NULL
    y
  })

im_split <- split(eu_imports, eu_imports$REPORTER_LAB) %>%
  lapply(function(y) {
    y$reporter_name <- NULL
    y
  })

#Export files as excel to folder. Each country will have its own file for imports and exports

exp_df_list_excel(ex_split,
                  path = "D:/documents/R/output/",
                  endname = "re-exports.xlsx")


exp_df_list_excel(im_split,
                  path = "D:/documents/R//output/",
                  endname = "imports.xlsx")


