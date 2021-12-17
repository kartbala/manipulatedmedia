## * Load pacakges
library(dplyr)
library(data.table)
library(googlesheets4)
library(googledrive)
library(tidyverse)
options(digits=4)

## * Load data

google_workbook_name = "https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587"
sheet_names = sheet_names(google_workbook_name)
number_of_sheets = sheet_names %>% length

#exclude Dictionary worksheet (last sheet)
number_of_sheets = number_of_sheets - 1

Data_table_maker = function(sheetnum){
assign(sheet_names[sheetnum],data.table(read_sheet(google_workbook_name,sheet=sheet_names[sheetnum])),envir = .GlobalEnv)
return(sheet_names[sheetnum])}

sapply(1:number_of_sheets,Data_table_maker)

Combined = list(daily.tweets,covid.daily.tweets,noncovid.daily.tweets,english.daily.tweets,no.farsi.daily.tweets,covid,oil,daily.events,protest.events,repression.events,forex,weather) %>% reduce(left_join, by = "Date")

write.csv(Combined,"InCombined.csv")
