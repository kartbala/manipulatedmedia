## * Load pacakges
library(dplyr)
library(data.table)
library(googlesheets4)
library(googledrive)
library(tidyverse)
options(digits=4)
setwd("~/Google\ Drive/")

## * Load data

google_workbook_name = "https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587"
sheet_names = sheet_names(google_workbook_name)
number_of_sheets = sheet_names %>% length

# Exclude Dictionary worksheet (last sheet)
number_of_sheets = number_of_sheets - 1

# Make a data table for every sheet other than Dictionary
Data_table_maker = function(sheetnum){
assign(sheet_names[sheetnum],data.table(read_sheet(google_workbook_name,sheet=sheet_names[sheetnum])),envir = .GlobalEnv)
return(sheet_names[sheetnum])}
sapply(1:number_of_sheets,Data_table_maker)

# Make a list of data tables
data_table_list = NULL

Data_table_lister = function(sheetnum){
data_table_list <<- list(data_table_list,get(sheet_names[sheetnum]))
return()}
sapply(1:number_of_sheets,Data_table_lister)

# Merge all of the data tables
l.dt <- lapply(sheet_names[1:number_of_sheets], function(x) get(x))
Combined = l.dt %>% reduce(left_join, by = "Date")

# Write file to Google Drive
write.csv(Combined,file="InCombined.csv")
