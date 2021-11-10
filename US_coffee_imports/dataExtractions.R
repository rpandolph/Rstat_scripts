library(jsonlite)
library(httr)

#Main source: US census bureau https://www.census.gov/data/developers/data-sets/international-trade.html
#shared by imports/export call, and where time period is set
time_param <-"time=from+2020-01+to+2020-08&COMM_LVL=HS10&"

#import only paramaters
base_url_imports <- "https://api.census.gov/data/timeseries/intltrade/imports/hs?"
shared_im_parameters <-"get=CTY_CODE,CTY_NAME,I_COMMODITY_LDESC,GEN_QY1_MO,GEN_VAL_MO,UNIT_QY1,GEN_QY1_MO_FLAG&SUMMARY_LVL=DET&"
icomm_09 <-"I_COMMODITY=0901*" 
icomm_21 <-"I_COMMODITY=2101*"

#export only parameters
base_url_exports <- "https://api.census.gov/data/timeseries/intltrade/exports/hs?"
shared_exp_parameters <-"get=CTY_CODE,CTY_NAME,E_COMMODITY_LDESC,QTY_1_MO,ALL_VAL_MO,UNIT_QY1,QTY_1_MO_FLAG&SUMMARY_LVL=DET&"
ecomm_09 <-"E_COMMODITY=0901*" 
ecomm_21 <-"E_COMMODITY=2101*"

#create full urls for GET request
export_url_09 <-paste0(base_url_exports, shared_exp_parameters, time_param, ecomm_09)
export_url_21 <-paste0(base_url_exports, shared_exp_parameters, time_param, ecomm_21)

import_url_09 <-paste0(base_url_imports, shared_im_parameters, time_param, icomm_09)
import_url_21 <-paste0(base_url_imports, shared_im_parameters, time_param, icomm_21)

#gets response from API, this will get everything at ten digit level for the chapters and subchapter
res_ch09=GET(export_url_09)
green_roast_data = as.data.frame(fromJSON(rawToChar(res_ch09$content)), stringsAsFactors = FALSE)
res_ch21=GET(export_url_21)
soluble_data=as.data.frame(fromJSON(rawToChar(res_ch21$content)), stringsAsFactors = FALSE)

res_im_09=GET(import_url_09)
im_green_roast <- as.data.frame(fromJSON(rawToChar(res_im_09$content)), stringsAsFactors = FALSE)
res_im_21=GET(import_url_21)
im_soluble<-as.data.frame(fromJSON(rawToChar(res_im_21$content)), stringsAsFactors = FALSE)
