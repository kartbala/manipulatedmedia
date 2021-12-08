## * Load pacakges
library(dplyr)
library(data.table)
library(lubridate)
library(googlesheets4)
library("googledrive")
options(digits=4)
 
## * Load data

google_workbook_name = "https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587"

Daily_tweets <- read_sheet(google_workbook_name,sheet="daily.tweets") %>% data.table
Covid <- read_sheet(google_workbook_name,sheet="WHO-COVID-19-global-data") %>% data.table 
Oil <- read_sheet(google_workbook_name,sheet="Brent Oil Prices") %>% data.table
Forex <- read_sheet(google_workbook_name,sheet="Forex") %>% data.table
Weather <- read_sheet(google_workbook_name,sheet="weather") %>% data.table
Search_term <- read_sheet(google_workbook_name,sheet="iran search term") %>% data.table
Daily_events <- read_sheet(google_workbook_name,sheet="daily.events") %>% data.table
Google_trends <- read_sheet(google_workbook_name,sheet="gtrendsweekly") %>% data.table

Combined = Daily_tweets %>% merge(Covid[Country_code=="IR",],by="Date") %>% merge(Oil,by="Date") %>% merge(Forex,by="Date") %>% merge(Search_term,by="Date") %>% merge(Daily_events,by="Date") %>% merge(Google_trends,by="Date") %>% merge(Weather[NAME=="TEHRAN MEHRABAD, IR",],by="Date")

Combined[,Country:=NULL]
Combined[,Country_code:=NULL]
Combined[,WHO_region:=NULL]
Combined[,Open:=NULL]
Combined[,High:=NULL]
Combined[,Low:=NULL]
Combined[,STATION:=NULL]
Combined[,NAME:=NULL]
Combined[,LATITUDE:=NULL]
Combined[,LONGITUDE:=NULL]
Combined[,ELEVATION:=NULL]
Combined[,Buy:=NULL]
Combined[,TMIN:=NULL]
Combined[,TMAX:=NULL]
Combined[,covidtrend:=NULL]
Combined[,irantrend:=NULL]

setnames(Combined,"TweetCount","Tweet_count")
setnames(Combined,"IranFraction New_cases","Iran_fraction_of_new_cases")
setnames(Combined,"IranFraction Cumulative_cases","Iran_fraction_of_cumulative_cases")
setnames(Combined,"IranFraction_New_deaths","Iran_fraction_of_new_deaths")
setnames(Combined,"IranFraction_Cumulative_deaths","Iran_fraction_of_cumulative_deaths")
setnames(Combined,"Close/Last","Oil_price")
setnames(Combined,"Volume","Oil_trading_volume")
setnames(Combined,"Sell","Exchange_rate")
setnames(Combined,"iran: (United States)","Google_searches_of_term_iran_in_US")
setnames(Combined,"IranEventCount","Event_count_Iran")
setnames(Combined,"AllEventCount","Event_count_all")
setnames(Combined,"Irant%","Iran_percent_of_all_events")
setnames(Combined,"PRCP","Precipitation")
setnames(Combined,"SNWD","Snow_depth")
setnames(Combined,"TAVG","Average_temperature")

write.csv(Combined,"InCombined.csv")
