##--------------------##
## CorrelAid Project: ##
##    erlassjahr.de   ##
##--------------------##
##    Data Cleaning   ##
##--------------------##

##--------------##
## Prerequisits ##
##--------------##

# Dependencies 
library(readxl)
library(countrycode)
library(tidyverse)
library(magrittr)

# Load data

# IMPORTANT: Trend variable will be read as POSCIX variable due to its pre-defined format in the orginial data source
#            I manually changed this in the excel file to work with the data in R.

sr19_erlassjahr <- read_xlsx("SR19 Überblickstabelle.xlsx")
sr19_erlassjahr <- sr19_erlassjahr[2:127,1:17] # keep only relevant variables and rows that contain data points

##----------------------##
##  Basic Data Cleaning ##
##----------------------##

# Rename variables 
names(sr19_erlassjahr)[8] <- "foreign_debt_exp"
names(sr19_erlassjahr)[10] <- "external_debt_service_exp"

sr19_erlassjahr %<>% 
  filter(!is.na(Land)) %>% 
  rename(country = `Land`,
         public_debt_bip = `Öffentliche Schulden / BIP`,
         trend_pdb = `Trend1`,
         public_debt_state_rev = `Öffentliche Schulden / Staatseinnahmen`,
         trend_pdsr = `Trend12`,
         foreign_debt_bip = `Auslandsschuldenstand / BIP`,
         trend_fdp = `Trend13`,
         trend_fde = `Trend14`,
         trend_edse = `Trend15`,
         risk_excessive_debt = `Risiko der Überschuldung laut IWF am 1.11.2018`,
         exceedance = `Überschreitungen`,
         debt_situation = `Verschuldungs-situation`,
         tendency = `Tendenz`,
         income = `Income`,
         trend = `Trend`)

# Turn character into numeric
sr19_erlassjahr$foreign_debt_bip <- as.numeric(sr19_erlassjahr$foreign_debt_bip)
sr19_erlassjahr$foreign_debt_exp <- as.numeric(sr19_erlassjahr$foreign_debt_exp)
sr19_erlassjahr$external_debt_service_exp <- as.numeric(sr19_erlassjahr$external_debt_service_exp)

# Convert German country names to ISO3c codes
custom_match <- c("Moldawien" = "MDA")
sr19_erlassjahr$country <- countrycode::countrycode(sr19_erlassjahr$country, 
                                                 "country.name.de", "iso3c", custom_match = custom_match)

# Add regions to the dataframe
sr19_erlassjahr$region <- countrycode::countrycode(sr19_erlassjahr$country, "iso3c", "region")

sr19_erlassjahr$region_large <- ifelse(sr19_erlassjahr$region %in% c("Eastern Africa", "Western Africa", "Middle Africa", "Southern Africa"), "Sub-Saharan Africa", NA)
sr19_erlassjahr$region_large <- ifelse(sr19_erlassjahr$region %in% c("Australia and New Zealand", "Micronesia", "Melanesia", "Polynesia"), "Oceania", sr19_erlassjahr$region_large)
sr19_erlassjahr$region_large <- ifelse(sr19_erlassjahr$region %in% c("Central Asia", "Eastern Asia", "South-Eastern Asia", "Southern Asia"), "Asia", sr19_erlassjahr$region_large)
sr19_erlassjahr$region_large <- ifelse(sr19_erlassjahr$region %in% c("Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe") | sr19_erlassjahr$country %in% c("CAN","CYP"), "Europe", sr19_erlassjahr$region_large)
sr19_erlassjahr$region_large <- ifelse(sr19_erlassjahr$region %in% c("Caribbean", "Central America", "South America"), "Latin America", sr19_erlassjahr$region_large)
sr19_erlassjahr$region_large <- ifelse(sr19_erlassjahr$region %in% c("Western Asia", "Northern Africa") | sr19_erlassjahr$country %in% c("AFG","IRN"), "Middle East" , sr19_erlassjahr$region_large)
sr19_erlassjahr$region_large <- ifelse(sr19_erlassjahr$region == "Northern America", "North America", sr19_erlassjahr$region_large)

# remove ambiguous states
sr19_erlassjahr <- sr19_erlassjahr[!is.na(sr19_erlassjahr$country), ]

##--------------------##
##  Rescale Variables ##
##--------------------##

####################
## Debt Situation ##
####################

## Create five categories for debt situation

# Function to recode the five indicators according to the coding scheme in Schuldenreport 2020 (page 17)
filter_recode <- function(var, filter) {
  var <- ifelse(var < filter[1],
                0,
                ifelse(
                  var >= filter[1] &
                    var <= filter[2],
                  1,
                  ifelse(var > filter[2] &
                           var <= filter[3],
                         2,
                         ifelse(var > filter[3],
                                3,
                                NA))
                ))
  return(var)
}

# Define thresholds for each filter category
public_debt_bip_filter <- c(50, 75, 100)
public_debt_state_rev_filter <- c(200, 300, 400)
foreign_debt_bip_filter <- c(40, 60, 80)
foreign_debt_exp_filter <- c(150, 225, 300)
external_debt_service_exp_filter <- c(15, 22.5, 30)

# Apply filter categories and recode function to each of the five category variables
sr19_erlassjahr$public_debt_bip2 <- filter_recode(sr19_erlassjahr$public_debt_bip, public_debt_bip_filter)
sr19_erlassjahr$public_debt_state_rev2 <- filter_recode(sr19_erlassjahr$public_debt_state_rev, public_debt_state_rev_filter)
sr19_erlassjahr$foreign_debt_bip2 <- filter_recode(sr19_erlassjahr$foreign_debt_bip, foreign_debt_bip_filter)
sr19_erlassjahr$foreign_debt_exp2 <- filter_recode(sr19_erlassjahr$foreign_debt_exp, foreign_debt_exp_filter)
sr19_erlassjahr$external_debt_service_exp2 <- filter_recode(sr19_erlassjahr$external_debt_service_exp, external_debt_service_exp_filter)

# Verschuldungssituation variable
sr19_erlassjahr$debt_sit_total <- rowSums(sr19_erlassjahr[, c(20:24)], na.rm = TRUE)
sr19_erlassjahr$debt_sit_cat <-
  ifelse(
    sr19_erlassjahr$debt_sit_total == 0,
    0,
    ifelse(
      sr19_erlassjahr$debt_sit_total > 0 &
        sr19_erlassjahr$debt_sit_total < 5,
      1,
      ifelse(
        sr19_erlassjahr$debt_sit_total >= 5 &
          sr19_erlassjahr$debt_sit_total < 10,
        2,
        ifelse(sr19_erlassjahr$debt_sit_total >= 10, 3, NA)
      )
    )
  )

sr19_erlassjahr$debt_sit_cat <- factor(sr19_erlassjahr$debt_sit_cat)
levels(sr19_erlassjahr$debt_sit_cat) <- c("nicht kritisch", "leicht kritisch", "kritisch", "sehr kritisch")
############
## Trend ##
###########

# Define a function to rescale the + and - signs in the trend variables to numeric outputs
trend_recode <- function(var){
  var <- ifelse(
    is.na(var),
    0,
    ifelse(
      var == "+",
      1,
      ifelse(
        var == "-",
        -1,
        var
      )
    )
  )
  var <- as.numeric(var)
}

# Apply function to all 5 trend variables
sr19_erlassjahr$trend_pdb <- trend_recode(sr19_erlassjahr$trend_pdb)
sr19_erlassjahr$trend_pdsr <- trend_recode(sr19_erlassjahr$trend_pdsr)
sr19_erlassjahr$trend_fdp <- trend_recode(sr19_erlassjahr$trend_fdp)
sr19_erlassjahr$trend_fde <- trend_recode(sr19_erlassjahr$trend_fde)
sr19_erlassjahr$trend_edse <- trend_recode(sr19_erlassjahr$trend_edse)


