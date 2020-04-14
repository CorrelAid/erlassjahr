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
library(zoo)

# Load data

# IMPORTANT: Trend variable will be read as POSCIX variable due to its pre-defined format in the orginial data source
#            I manually changed this in the excel file to work with the data in R.

sr20_erlassjahr <- read_xlsx("SR20 Überblickstabelle-191107.xlsx")
# add Grundgesamtheit
country_all <- read_xlsx("SR20 Überblickstabelle-191107.xlsx", sheet =  7)
sr20_erlassjahr <- sr20_erlassjahr[1:129,1:22] # keep only relevant variables and rows that contain data points

##----------------------##
##  Basic Data Cleaning ##
##----------------------##
# Rename variables 
names(sr20_erlassjahr)[11] <- "foreign_debt_exp"
sr20_erlassjahr$foreign_debt_exp <- as.numeric(sr20_erlassjahr$foreign_debt_exp)
names(sr20_erlassjahr)[14] <- "external_debt_service_exp"

sr20_erlassjahr %<>% 
  filter(!is.na(Land)) %>% 
  dplyr::rename(country = `Land`,
         public_debt_bip = `Öffentliche Schulden / BIP`,
         public_debt_bip2 = `Wert`, #rename to make inline with app.R coding
         trend_pdb = `Trend1`,
         public_debt_state_rev = `Öffentliche Schulden / Staatseinnahmen`,
         public_debt_state_rev2 = `Wert2`, #rename to make inline with app.R coding
         trend_pdsr = `Trend12`,
         foreign_debt_bip = `Auslandsschuldenstand / BIP`,
         foreign_debt_bip2 = `Wert3`, #rename to make inline with app.R coding
         trend_fdp = `Trend3`,
         foreign_debt_exp2 = `Wert4`, #rename to make inline with app.R coding
         trend_fde = `Trend4`,
         external_debt_service_exp2 = `Wert5`, #rename to make inline with app.R coding
         trend_edse = `Trend5`,
         risk_excessive_debt = `Risiko der Überschuldung laut IWF am 31.7.2019`,
         exceedance = `Überschreibtungen`,
         debt_situation = `Verschuldungs-situation`,
         tendency = `Tendenz`,
         income = `Income`,
         trend = `Trend`)

# Turn character into numeric
sr20_erlassjahr$debt_situation <- as.numeric(sr20_erlassjahr$debt_situation)
sr20_erlassjahr$trend <- as.numeric(sr20_erlassjahr$trend)

sr20_erlassjahr$country <- gsub("\\*", "", sr20_erlassjahr$country)
# Convert German country names to ISO3c codes
custom_match <- c("Moldawien" = "MDA")
sr20_erlassjahr$country_A3 <- countrycode::countrycode(sr20_erlassjahr$country, 
                                                 "country.name.de", "iso3c", custom_match = custom_match)

sr20_erlassjahr$region <- NA
sr20_erlassjahr$region <- ifelse(is.na(sr20_erlassjahr$country_A3), sr20_erlassjahr$country, NA)
sr20_erlassjahr$region <- zoo::na.locf(sr20_erlassjahr$region)

# remove ambiguous states
sr20_erlassjahr <- sr20_erlassjahr[!is.na(sr20_erlassjahr$country_A3), ]

# add all country names
country_all <- country_all[1:194,1]
names(country_all)[1] <- "country_A3"
country_all <- rbind(country_all, "Nordkorea")
country_all <- rbind(country_all, "Kuba")

custom_match2 <- c("Eswatini" = "SWZ", "Kosovo" = "RKS", "Micronesia" = "FSM", "S†o Tom_ and PrÍncipe" = "STP", "Nordkorea" = "PRK", "Kuba" = "CUB")
country_all$country_A3 <- countrycode::countrycode(country_all$country_A3, 
                                                       "country.name", "iso3c", custom_match = custom_match2)

sr20_erlassjahr <- merge(sr20_erlassjahr, country_all, by = "country_A3", all.y = TRUE)
# create a variable that is 1 if the country is part of the sample and 0 otherwise

sr20_erlassjahr$sample <- ifelse(is.na(sr20_erlassjahr$country), 0, 1)
sr20_erlassjahr$sample <- ifelse(sr20_erlassjahr$country_A3 == "CUB" | sr20_erlassjahr$country_A3 == "PRK", 1, sr20_erlassjahr$sample)

##--------------------##
##  Rescale Variables ##
##--------------------##

############
## Trend ##
###########

# Define a function to rescale the + and - signs in the trend variables to numeric outputs
trend_recode <- function(var){
  var <- ifelse(
    var == "o",
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
sr20_erlassjahr$trend_pdb <- trend_recode(sr20_erlassjahr$trend_pdb)
sr20_erlassjahr$trend_pdsr <- trend_recode(sr20_erlassjahr$trend_pdsr)
sr20_erlassjahr$trend_fdp <- trend_recode(sr20_erlassjahr$trend_fdp)
sr20_erlassjahr$trend_fde <- trend_recode(sr20_erlassjahr$trend_fde)
sr20_erlassjahr$trend_edse <- trend_recode(sr20_erlassjahr$trend_edse)

###############
## Trend New ##
###############

trend_new_recode <- function(var){
  var <- plyr::revalue(as.character(as.english(var)), c("minus one"="minus_one"))
}

sr20_erlassjahr$trend_new <- trend_new_recode(sr20_erlassjahr$trend)
sr20_erlassjahr$trend_pdb_new <- trend_new_recode(sr20_erlassjahr$trend_pdb)
sr20_erlassjahr$trend_pdsr_new <- trend_new_recode(sr20_erlassjahr$trend_pdsr)
sr20_erlassjahr$trend_fdp_new <- trend_new_recode(sr20_erlassjahr$trend_fdp)
sr20_erlassjahr$trend_fde_new <- trend_new_recode(sr20_erlassjahr$trend_fde)
sr20_erlassjahr$trend_edse_new <- trend_new_recode(sr20_erlassjahr$trend_edse)

##############
## Debt Sit ##
##############

sr20_erlassjahr$debt_sit_cat <-
  ifelse(
    sr20_erlassjahr$exceedance == 0,
    0,
    ifelse(
      sr20_erlassjahr$exceedance > 0 &
        sr20_erlassjahr$exceedance < 5,
      1,
      ifelse(
        sr20_erlassjahr$exceedance >= 5 &
          sr20_erlassjahr$exceedance < 10,
        2,
        ifelse(sr20_erlassjahr$exceedance >= 10, 3, NA)
      )
    )
  )

# sr20_erlassjahr$debt_sit_cat <- factor(sr20_erlassjahr$debt_sit_cat)
# levels(sr20_erlassjahr$debt_sit_cat) <- c("nicht kritisch", "leicht kritisch", "kritisch", "sehr kritisch")

#######################
## Payment Situation ##
#######################

payment_high <- c("ERI", "CUB", "PRK", "ZWE", "SOM", "SDN", "SYR")
payment_med <- c("ARG", "YEM", "GMB", "GRD", "MOZ", "COG", "STP")
payment_low <- c("IRQ", "KHM", "UKR")

sr20_erlassjahr$payment_stop <-
  ifelse(
    sr20_erlassjahr$country_A3 %in% payment_high,
    "feuer_rot",
    ifelse(
       sr20_erlassjahr$country_A3 %in% payment_med,
      "feuer_orange",
      ifelse(sr20_erlassjahr$country_A3 %in% payment_low, "feuer_grau", NA)
    )
  )

# rename country_A3 variable
names(sr20_erlassjahr)[1] <- "ISO3"

##------------------##
## Save the Dataset ##
##------------------##

save(sr20_erlassjahr, file = "sr20_erlassjahr.RData")






##---------------------------------##
## Needed only for sr19_erlassjahr ##
##---------------------------------##



####################
## Debt Situation ##
####################

## Create five categories for debt situation
# 
# # Function to recode the five indicators according to the coding scheme in Schuldenreport 2020 (page 17)
# filter_recode <- function(var, filter) {
#   var <- ifelse(var < filter[1],
#                 0,
#                 ifelse(
#                   var >= filter[1] &
#                     var <= filter[2],
#                   1,
#                   ifelse(var > filter[2] &
#                            var <= filter[3],
#                          2,
#                          ifelse(var > filter[3],
#                                 3,
#                                 NA))
#                 ))
#   return(var)
# }
# 
# # Define thresholds for each filter category
# public_debt_bip_filter <- c(50, 75, 100)
# public_debt_state_rev_filter <- c(200, 300, 400)
# foreign_debt_bip_filter <- c(40, 60, 80)
# foreign_debt_exp_filter <- c(150, 225, 300)
# external_debt_service_exp_filter <- c(15, 22.5, 30)
# 
# # Apply filter categories and recode function to each of the five category variables
# sr20_erlassjahr$public_debt_bip2 <- filter_recode(sr20_erlassjahr$public_debt_bip, public_debt_bip_filter)
# sr20_erlassjahr$public_debt_state_rev2 <- filter_recode(sr20_erlassjahr$public_debt_state_rev, public_debt_state_rev_filter)
# sr20_erlassjahr$foreign_debt_bip2 <- filter_recode(sr20_erlassjahr$foreign_debt_bip, foreign_debt_bip_filter)
# sr20_erlassjahr$foreign_debt_exp2 <- filter_recode(sr20_erlassjahr$foreign_debt_exp, foreign_debt_exp_filter)
# sr20_erlassjahr$external_debt_service_exp2 <- filter_recode(sr20_erlassjahr$external_debt_service_exp, external_debt_service_exp_filter)
