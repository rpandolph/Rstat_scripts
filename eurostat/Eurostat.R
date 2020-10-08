#test with one file, uploading first sheet from all data
library(tidyverse)
library(readxl) 
library(writexl)

#excel original cleaned to rid file of the table of contents
#also to delete rows with data downloaded and link back to the TOC. All else same
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = FALSE, skip = 1))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

#function to minimize input for recoding ISO codes in gsub
recode_ptCode <- function(x, original, replace) {
  gsub(original, replace, x, fixed = TRUE)
}

#funcion export list of df to excel
exp_df_list_excel <- function(x, path, endname = ".xlsx"){
  filename <- list()
  (for (i in 1:length(x)){
    filename[i] <- paste0( path, names(x[i]), endname, collapse = "")
  })
  (for (i in 1:length(x)){ write_xlsx(x = x[[i]],
                                      path = filename[[i]],
                                      col_names = TRUE) })
}



workbook_location <- "C:/Users/TRAVEL/Documents/R/raw_data/k15805264_toc_removed.xlsx"

#import data workbook and excel file with ISO labels
EU_data <- read_excel_allsheets(workbook_location)
ico_Country <- read_excel("R/raw_data/ico_Country_Codes_Names.xlsx")

#pulls out info from header section of each work sheet (partner name, hscode, flow type and unit)
#then removes the header rows
EU_data_cleaned <- lapply(EU_data, function(x) transform(x, flow = x[1,2])) %>% 
  lapply(function(x) transform(x, unit = x[2,2])) %>% 
    lapply(function(x) transform(x, period = x[3,2])) %>% 
      lapply(function(x) transform(x, hsCode = x[4,2])) %>% 
        lapply(function(x) subset(x[-c(1:5), ]))


#get new column names to replace all
x<- as.character(EU_data_cleaned$`k15805264.xlsx 1`[1, c(1:228)])
y<- c("flow", "unit", "period", "hsCode")
columns <- c(x, y)

#set names using columns vector then remove first row
EU_named<- lapply(EU_data_cleaned, setNames, columns) %>% 
#remove first row with column names
lapply( function(x) subset(x[-1, ]))

#pivot data frames so monthly data moved from wide to long, then bind rows for one data frame
EU_tidied<- lapply(EU_named, function(x) pivot_longer(x, cols = c(2:228), names_to = "partner"))


eu_df <- bind_rows(EU_tidied)

#pivot wider to put volume/value in same row
eu_df2 <- eu_df %>% 
  pivot_wider(names_from = unit, values_from = value) %>% 
  rename(reporter =`REPORTER/PARTNER`, value = `VALUE_IN_EUROS - VALUE_IN_EUROS`, 
              volume =`QUANTITY_IN_100KG - QUANTITY_IN_100KG`, 
              secondVol = `SUPPLEMENTARY_QUANTITY - SUPPLEMENTARY_QUANTITY`) %>% 
  filter(!is.na(volume)) %>% 
  separate(reporter, into = c("reporter_code", "reporter_name"),  extra = "merge")

#remove EU totals
eu_totals <- c("EA19",
               "EU",
               "EU27", 
               "EU28",
               "EUROZONE")

`%notin%` <- Negate(`%in%`)

 eu_df3 <- eu_df2 %>% 
   filter(reporter_code %notin% eu_totals) %>% 
   separate(period, into = c("date_num", "date"), sep = "-") %>% 
   separate(date_num, into = c("year", "month"), sep =  4) %>% 
   select(-date) %>% 
   separate(hsCode, into = c("code", "description"), sep = 8) %>% 
   select(-description) %>% 
   separate(partner, into = c("pt_code", "pt_name"), sep= "-", extra = "merge") 
 
 eu_df4 <- eu_df3 %>% 
   filter(volume > 0) %>% 
   select(-secondVol)
 
 #first clean up the pt codes can use previous mappings, but also EU?
 #drop second vol, what do we do when no volume but value and vice versa 
 #does 21011298 need to be recoded to 292?
 
 #clean white spaces from codes in order to recode for ICO

 eu_df4$pt_code <- gsub("[[:space:]]", "",  eu_df4$pt_code)
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "QY", "XF")
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "QZ", "QU")
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "SX", "AN")
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "AQ", "QU")
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "BL", "QU")
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "BV", "QU")
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "GS", "QU")
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "HM", "QU")
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "IO", "QU")
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "TF", "QU")
 eu_df4$pt_code <- recode_ptCode(eu_df4$pt_code, "QR", "QQ")
 
 
#change reporter names
eu_df4$reporter_name <- gsub("BELGIUM (and LUXBG -> 1998)", "BELGIUM",eu_df4$reporter_name, fixed = TRUE)
eu_df4$reporter_name <- gsub("CZECHIA (CS->1992)", "CZECHIA", eu_df4$reporter_name, fixed = TRUE)
eu_df4$reporter_name <- gsub("GERMANY (incl DD from 1991)", "GERMANY", eu_df4$reporter_name, fixed = TRUE)

 #change from chr to numeric
 eu_df4$value <- as.numeric(eu_df4$value)
 eu_df4$volume <- as.numeric(eu_df4$volume)
 
 #sum up new totals created by recoding
 eu_df4 <-  eu_df4 %>% 
   group_by(reporter_code, flow, year, month, code, pt_code) %>% 
   mutate(tot_vol = sum(volume), tot_val = sum(value)) %>% 
   ungroup() %>% 
   select(-volume, -value)
 
 #This code chunk checks to see if there are extra EU only iso codes not already converted. 
 #need_code Should be 0 obs or just EU aggregates(10ish)
 #section to find which iso codes in EU but not ICO before recoding
 #create df of unique EU codes and merge with ICO excel
 code_list <-eu_df4 %>% 
   distinct(pt_code, pt_name) %>% 
   rename(isoCode = pt_code)
 code_list$isoCode <- gsub("[[:space:]]", "", code_list$isoCode)
 ico_Country$isoCode <- gsub("[[:space:]]", "", ico_Country$isoCode)
 code_match <- merge(code_list, ico_Country, by = "isoCode", all = TRUE)
 need_code <- code_match %>% 
   filter(is.na(Code))

 
 #check for missing values
 #no_val should be 0 obs, otherwise need to estimate missing values using average unit val for other shipments
 no_val <- eu_df4 %>% 
   filter(tot_val < 1)
 
 #add in columns needed for auto upload
 
 eu_df4["wt_unit"] <- "100kg"
 eu_df4["usd_val"] <- NA
 
 #remove eu aggregates if included (3 because space is ra)
 eu_df5 <- eu_df4 %>% 
   filter(nchar(pt_code) <= 2) %>% 
   select(month, 
          year, 
          pt_code,
          pt_name,
          tot_vol,
          wt_unit,
          tot_val,
          usd_val,
          code,
          reporter_name,
          flow)
 
 #create new dfs of just re-exports and imports
 eu_reExports <- eu_df5 %>%
   filter(flow == "2 - EXPORT") %>% 
   select(-flow)
 
 eu_imports <- eu_df5 %>%
   filter(flow == "1 - IMPORT") %>% 
   select(-flow)
 
 

#Split file by importing country
ex_split <- split(eu_reExports, eu_reExports$reporter_name) %>%
  lapply(function(y){
    y$reporter_name <- NULL
    y
  })

im_split <- split(eu_imports, eu_imports$reporter_name) %>% 
  lapply(function(y){
    y$reporter_name<- NULL
    y
  }
    )



exp_df_list_excel(ex_split,
                  path = "C:/Users/TRAVEL/Documents/R/output/", 
                  endname = "re-exports.xlsx")


exp_df_list_excel(im_split,
                  path = "C:/Users/TRAVEL/Documents/R/output/", 
                  endname = "imports.xlsx")


 