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
library(english)

##-------------##
##  Load Data  ##
##-------------##

##---- Main Data ----##
sr20_erlassjahr <- read_xlsx("SR20 Überblickstabelle-191107.xlsx")
sr20_erlassjahr <- sr20_erlassjahr[1:129,1:22] # keep only relevant variables and rows that contain data points

##---- Total Countries ----##
country_all <- read_xlsx("SR20 Überblickstabelle-191107.xlsx", sheet =  7)
country_all <- country_all[1:194,1]

##---- Risk Overview ----##
risks_overview <- read_xlsx("Risiken-Überblick.xlsx")
risks_overview <- risks_overview[1:129,1:5]

##----------------------##
##  Basic Data Cleaning ##
##----------------------##

##---- Main Data ----##

# Rename variables (not all variables could be renamed using dplyr::rename())
names(sr20_erlassjahr)[11] <- "foreign_debt_exp"
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
sr20_erlassjahr$foreign_debt_exp <- as.numeric(sr20_erlassjahr$foreign_debt_exp)
sr20_erlassjahr$debt_situation <- as.numeric(sr20_erlassjahr$debt_situation)
sr20_erlassjahr$trend <- as.numeric(sr20_erlassjahr$trend)

# Clean country names
sr20_erlassjahr$country <- gsub("\\*", "", sr20_erlassjahr$country)

# Convert country names (from German to ISO3)
custom_match <- c("Moldawien" = "MDA")
sr20_erlassjahr$ISO3 <- countrycode::countrycode(sr20_erlassjahr$country, 
                                                 "country.name.de", "iso3c", custom_match = custom_match)

# Add regions as defined by erlassjahr.de
sr20_erlassjahr$region <- NA
sr20_erlassjahr$region <- ifelse(is.na(sr20_erlassjahr$ISO3), sr20_erlassjahr$country, NA)
sr20_erlassjahr$region <- zoo::na.locf(sr20_erlassjahr$region)

# remove ambiguous states
sr20_erlassjahr <- sr20_erlassjahr[!is.na(sr20_erlassjahr$ISO3), ]

##---- Total Countries ----##

# Rename country name variable & add missing countries
names(country_all)[1] <- "ISO3"
country_all <- rbind(country_all, "Nordkorea")
country_all <- rbind(country_all, "Kuba")

# Convert country names (from English to ISO3)
custom_match2 <- c("Eswatini" = "SWZ", "Kosovo" = "RKS", "Micronesia" = "FSM", "S†o Tom_ and PrÍncipe" = "STP", "Nordkorea" = "PRK", "Kuba" = "CUB")
country_all$ISO3 <- countrycode::countrycode(country_all$ISO3, 
                                                       "country.name", "iso3c", custom_match = custom_match2)

# Merge total countries with main data
sr20_erlassjahr <- merge(sr20_erlassjahr, country_all, by = "ISO3", all.y = TRUE)

# Create a sample variable that is 1 if the country is part of the sample and 0 otherwise
sr20_erlassjahr$sample <- ifelse(is.na(sr20_erlassjahr$country), 0, 1)
# the two countries that were additionally added need to be coded manually to become part of the sample
sr20_erlassjahr$sample <- ifelse(sr20_erlassjahr$ISO3 == "CUB" | sr20_erlassjahr$ISO3 == "PRK", 1, sr20_erlassjahr$sample)

##---- Risk Overview ----##

# Rename variables
risks_overview %<>%
  rename(ISO3 = `Land`,
         extractivism = `Extraktivismus`,
         fragility = `Fragiität`,
         debt_prob = `Problematische Schuldenstruktur`,
         vulnerability = `Vulnerabilität gegenüber Naturkatstrophen`)

# Clean country names
risks_overview$ISO3 <- gsub("\\*", "", risks_overview$ISO3)

# Convert country names (from German to ISO3) -- we can re-use the custom_match from above as the list of countries
# remains the same
risks_overview$ISO3 <- countrycode::countrycode(risks_overview$ISO3, 
                                             "country.name.de", "iso3c", custom_match = custom_match)

# remove ambiguous states
risks_overview <- risks_overview[!is.na(risks_overview$ISO3), ]

# Merge total countries with main data
sr20_erlassjahr <- merge(sr20_erlassjahr, risks_overview, by = "ISO3", all.x = TRUE)

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

#######################
## Payment Situation ##
#######################

payment_high <- c("ERI", "CUB", "PRK", "ZWE", "SOM", "SDN", "SYR")
payment_med <- c("ARG", "YEM", "GMB", "GRD", "MOZ", "COG", "STP")
payment_low <- c("IRQ", "KHM", "UKR")

sr20_erlassjahr$payment_stop <-
  ifelse(
    sr20_erlassjahr$ISO3 %in% payment_high,
    "feuer_rot",
    ifelse(
       sr20_erlassjahr$ISO3 %in% payment_med,
      "feuer_orange",
      ifelse(sr20_erlassjahr$ISO3 %in% payment_low, "feuer_grau", NA)
    )
  )

#####################
## Risk Situations ##
#####################

risk_new <- function(var){
  var <- ifelse(is.na(var), 0, 1)
}

sr20_erlassjahr$extractivism <- risk_new(sr20_erlassjahr$extractivism)
sr20_erlassjahr$fragility <- risk_new(sr20_erlassjahr$fragility)
sr20_erlassjahr$debt_prob <- risk_new(sr20_erlassjahr$debt_prob)
sr20_erlassjahr$vulnerability <- risk_new(sr20_erlassjahr$vulnerability)

##------------------##
## Save the Dataset ##
##------------------##

save(sr20_erlassjahr, file = "sr20_erlassjahr.RData")