## * Load pacakges
library(dplyr)
library(data.table)
library(lubridate)
library(googlesheets4)
library("googledrive")
options(digits=4)
 
## * Load data

Daily_tweets <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="daily.tweets") %>% data.table
Covid <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="WHO-COVID-19-global-data") %>% data.table 
Oil <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="Brent Oil Prices") %>% data.table
Search_term <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="iran search term") %>% data.table
Daily_events <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="daily.events") %>% data.table
Google_trends <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="gtrendsweekly") %>% data.table
Forex <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="Forex") %>% data.table
Weather <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="weather") %>% data.table

Combined = Daily_tweets %>% merge(Covid[Country_code=="IR",],by="Date") %>% merge(Oil,by="Date") %>% merge(Forex,by="Date")  %>% merge(Daily_events,by="Date")

write.csv(Combined,"InCombined.csv")
