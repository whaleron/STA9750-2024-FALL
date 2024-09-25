if(!require("tidyverse")) install.packages("tidyverse")

# Let's start with Fare Revenue
library(tidyverse)
if(!file.exists("2022_fare_revenue.xlsx")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "2022_fare_revenue.xlsx" in your project
  # directory.
  download.file("http://www.transit.dot.gov/sites/fta.dot.gov/files/2024-04/2022%20Fare%20Revenue.xlsx", 
                destfile="2022_fare_revenue.xlsx", 
                quiet=FALSE, 
                method="wget")
}
FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
  select(-`State/Parent NTD ID`, 
         -`Reporter Type`,
         -`Reporting Module`,
         -`TOS`,
         -`Passenger Paid Fares`,
         -`Organization Paid Fares`) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`)

# Next, expenses
if(!file.exists("2022_expenses.csv")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "2022_expenses.csv" in your project
  # directory.
  download.file("https://data.transportation.gov/api/views/dkxx-zjd6/rows.csv?date=20231102&accessType=DOWNLOAD&bom=true&format=true", 
                destfile="2022_expenses.csv", 
                quiet=FALSE, 
                method="wget")
}
EXPENSES <- readr::read_csv("2022_expenses.csv") |>
  select(`NTD ID`, 
         `Agency`,
         `Total`, 
         `Mode`) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses)) |>
  ungroup()

FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))

# Monthly Transit Numbers
library(tidyverse)
if(!file.exists("ridership.xlsx")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "ridership.xlsx" in your project
  # directory.
  download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-09/July%202024%20Complete%20Monthly%20Ridership%20%28with%20adjustments%20and%20estimates%29_240903.xlsx", 
                destfile="ridership.xlsx", 
                quiet=FALSE, 
                method="wget")
}
TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet="UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  rename(metro_area = `UZA Name`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`, metro_area), 
               names_to="month", 
               values_to="UPT") |>
  drop_na() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs

MILES <- readxl::read_xlsx("ridership.xlsx", sheet="VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="VRM") |>
  drop_na() |>
  rename(metro_area =`UZA Name`) |> #task1
  group_by(`NTD ID`, `Agency`, metro_area, 
           `Mode`, `3 Mode`, month) |>
  summarize(VRM = sum(VRM)) |>
  ungroup() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs

USAGE <- inner_join(TRIPS, MILES) |>
  mutate(`NTD ID` = as.integer(`NTD ID`))
if(!require("DT")) install.packages("DT")
library(DT)

sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable()

#task2
USAGE <- USAGE |>
  mutate(Mode=case_when(
    Mode == "AR" ~ "Alaska Railroad", 
    Mode == "CB" ~ "Commuter Bus",
    Mode == "CC" ~ "Cable Car",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "LR" ~ "Light Rail",
    Mode == "MB" ~ "Bus",
    Mode == "MG" ~ "Monorail/Automated Guideway",
    Mode == "PB" ~ "Publico",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "SR" ~ "Streercar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "TR" ~ "Aerial Tramway",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Hybrid Rail",
    TRUE ~ "Unknown"
))
distinct(USAGE, Mode)
if(!require("DT")) install.packages("DT")
library(DT)

sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable()

#task3 
#1 What transit agency had the most total VRM in this sample?
most_vrm_agency <- USAGE |>
  group_by(Agency) |>
  summarize(total_VRM = sum(VRM, na.rm = TRUE)) |>
  arrange(desc(total_VRM)) |>
  slice(1) #top result
most_vrm_agency

#2 What transit mode had the most total VRM in this sample?
most_vrm_mode <- USAGE |>
  group_by(Mode) |>
  summarize(total_VRM = sum(VRM, na.rm = TRUE)) |>
  arrange(desc(total_VRM)) |>
  slice(1) #top result
most_vrm_mode

#3 How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?
nyc_subway_may2024 <- USAGE |>
  filter(Agency == "MTA New York City Transit", Mode == "Heavy Rail", month == "2024-05-01") |>
  summarize(total_trips = sum(UPT, na.rm = TRUE))
nyc_subway_may2024

#4 What mode of transport had the longest average trip in May 2024? #wrong, we need passenger miles
longest_avg_trip_mode <- USAGE |>
  filter(month =="2024-05-01") |>
  group_by(Mode) |>
  summarize(avg_trip_length = sum(VRM, na.rm = TRUE) / sum(UPT, na.rm = TRUE)) |>
  arrange(desc(avg_trip_length)) |>
  slice(1) #top result
longest_avg_trip_mode

#5 How much did NYC subway ridership fall between April 2019 and April 2020?
nyc_subway_april_ridership <- USAGE |>
  filter(Agency == "MTA New York City Transit", 
         Mode == "Heavy Rail", 
         month %in% c("2019-04-01", "2020-04-01")) |>
  group_by(month) |>
  summarize(total_trips = sum(UPT, na.rm = TRUE)) #ridership fall

ridership_fall <- nyc_subway_april_ridership |> # % fall 
  summarize(fall = 100* (total_trips[month == "2020-04-01"] - total_trips[month == "2019-04-01"]) / total_trips[month == "2019-04-01"])
ridership_fall

#task4
#1 Which transit agency had the most trips in a single month?
most_trip_single_month <- USAGE |>
  group_by(Agency, month) |>
  summarize(total_trips = sum(UPT,na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total_trips)) |>
  slice(1)
most_trip_single_month

#2 Which mode of transit had the highest trip length in 2024?
longest_avg_trip_2024 <-USAGE |>
  filter(grepl("2024", month)) |>
  group_by(Mode) |>
  summarize(avg_trip_length = sum(VRM, na.rm = TRUE) / sum(UPT, na.rm = TRUE)) |>
  arrange(desc(avg_trip_length)) |>
  slice(1)
longest_avg_trip_2024

#3 Which transit agency had the largest vehicle fleet(VRM) in 2024?
largest_fleet_2024 <- USAGE |>
  filter(grepl("2024", month)) |>
  group_by(Agency) |>
  summarize(total_VRM = sum(VRM, na.rm = TRUE)) |>
  arrange(desc(total_VRM)) |>
  slice(1)
largest_fleet_2024

#Task 5 table summarization
library(dplyr)
library(lubridate) #year function

USAGE_2022_ANNUAL <- USAGE |>
  filter(year(month) == 2022) |>
  group_by(`NTD ID`, Agency, metro_area, Mode) |>
  summarize(
    UPT = sum(UPT, na.rm = TRUE),
    VRM = sum(VRM, na.rm = TRUE),
    .groups = "drop"
  )
USAGE_2022_ANNUAL

FINANCIALS_mode_updated <- FINANCIALS %>%
  mutate(Mode =case_when(
    Mode == "AR" ~ "Alaska Railroad", 
    Mode == "CB" ~ "Commuter Bus",
    Mode == "CC" ~ "Cable Car",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "LR" ~ "Light Rail",
    Mode == "MB" ~ "Bus",
    Mode == "MG" ~ "Monorail/Automated Guideway",
    Mode == "PB" ~ "Publico",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "SR" ~ "Streercar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "TR" ~ "Aerial Tramway",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Hybrid Rail",
    TRUE ~ "Unknown"
  ))
FINANCIALS_mode_updated

USAGE_AND_FINANCIALS <- left_join(USAGE_2022_ANNUAL, 
                                  FINANCIALS_mode_updated, 
                                  join_by(`NTD ID`, Mode)) |>
  drop_na()

#glimpse(USAGE_2022_ANNUAL)
#glimpse(FINANCIALS)
View(USAGE_AND_FINANCIALS)
#Task6 Farebox Recovery Among Major Systems
#1 Which transit system (agency and mode) had the most UPT in 2022?
most_upt_2022 <- USAGE_AND_FINANCIALS |>
  filter(UPT >= 400000) |>
  arrange(desc(UPT)) |>
  slice(1)
most_upt_2022

#2 Which transit system (agency and mode) had the highest farebox recovery, defined as the highest ratio of Total Fares to Expenses?
highest_farebox_recovery <- USAGE_AND_FINANCIALS |>
  filter(UPT >= 400000) |>
  mutate(farebox_recovery = `Total Fares` / Expenses) |>
  arrange(desc(farebox_recovery)) |>
  slice(1)
highest_farebox_recovery

#3 Which transit system (agency and mode) has the lowest expenses per UPT?
lower_expenses_per_upt <- USAGE_AND_FINANCIALS |>
  filter(UPT >= 400000) |>
  mutate(expenses_per_upt = Expenses / UPT) |>
  arrange(expenses_per_upt) |>
  slice(1)
lower_expenses_per_upt

#4 Which transit system (agency and mode) has the highest total fares per UPT?
highest_fares_per_upt <- USAGE_AND_FINANCIALS |>
  filter(UPT >= 400000) |>
  mutate(fares_per_upt = `Total Fares` / UPT) |>
  arrange(desc(fares_per_upt)) |>
  slice(1)

highest_fares_per_upt

#5 Which transit system (agency and mode) has the lowest expenses per VRM?
lowest_expenses_per_vrm <-USAGE_AND_FINANCIALS |>
  filter(UPT >= 400000) |>
  mutate(expenses_per_vrm = Expenses / VRM) |>
  arrange(expenses_per_vrm) |>
  slice(1)

lowest_expenses_per_vrm

#6 Which transit system (agency and mode) has the highest total fares per VRM?
highest_fares_per_vrm <- USAGE_AND_FINANCIALS |>
  filter(UPT >= 400000) |>
  mutate(fares_per_vrm = `Total Fares` / VRM) |>
  arrange(desc(fares_per_vrm)) |>
  slice(1)

highest_fares_per_vrm