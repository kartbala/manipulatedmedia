## * Load pacakges
library(dplyr)
library(data.table)
library(lubridate)
library(googlesheets4)
library("googledrive")
options(digits=4)
 
## * Load data

Daily_tweets <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="daily.tweets")
Covid <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="WHO-COVID-19-global-data")
Oil <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="Brent Oil Prices")
Search_term <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="iran search term")
Daily_events <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="daily.events")
Google_trends <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="gtrendsweekly")
Forex <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="Forex")
Weather <- read_sheet("https://docs.google.com/spreadsheets/d/1Nmn1_sS8a-JG_XBeNvlUwIv2vP_pV7IxplEapdOdsps/edit#gid=2129984587",sheet="weather")

Combined = Covid

write.csv(Combined,"InCombined.csv")
