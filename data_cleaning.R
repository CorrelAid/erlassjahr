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
country_all <- country_all[1:194,c(1,4,5)]

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
names(country_all)[1:3] <- c("ISO3", "oecd", "no_data")
country_all <- as.data.frame(country_all)

additional_countries <- data.frame(ISO3 = c("Nordkorea", "Kuba"), oecd = c(NA, NA), no_data = c(1,1))
additional_countries$ISO3 <- as.character(additional_countries$ISO3)
country_all <- rbind(country_all, additional_countries)

# Convert country names (from English to ISO3)
custom_match2 <- c("Eswatini" = "SWZ", "Kosovo" = "RKS", "Micronesia" = "FSM", "S†o Tom_ and PrÍncipe" = "STP", "Nordkorea" = "PRK", "Kuba" = "CUB")
country_all$ISO3 <- countrycode::countrycode(country_all$ISO3, 
                                                       "country.name", "iso3c", custom_match = custom_match2)

country_all[is.na(country_all)] <- 0

# Merge total countries with main data
sr20_erlassjahr <- merge(sr20_erlassjahr, country_all, by = "ISO3", all.y = TRUE)

# # Create a sample variable that is 1 if the country is part of the sample and 0 otherwise
# sr20_erlassjahr$sample <- ifelse(sr20_erlassjahr$oecd == 1, 1, 0)
# # the two countries that were additionally added need to be coded manually to become part of the sample
# sr20_erlassjahr$sample <- ifelse(sr20_erlassjahr$ISO3 == "CUB" | sr20_erlassjahr$ISO3 == "PRK", 1, sr20_erlassjahr$sample)

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

sr20_erlassjahr$debt_sit_cat2 <-
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

sr20_erlassjahr$debt_sit_cat2 <- ifelse(sr20_erlassjahr$oecd == 1, -1, sr20_erlassjahr$debt_sit_cat2)

sr20_erlassjahr$debt_sit_cat <- factor(sr20_erlassjahr$debt_sit_cat2)
levels(sr20_erlassjahr$debt_sit_cat) <- c("nicht Teil der Betrachtung", "nicht kritisch", "leicht kritisch", "kritisch", "sehr kritisch")

################
## Indicators ##
################

indicator_recode <- function(var){
  var <- ifelse(sr20_erlassjahr$oecd == 1, 0, var)
}

sr20_erlassjahr$public_debt_bip <- indicator_recode(sr20_erlassjahr$public_debt_bip)
sr20_erlassjahr$public_debt_state_rev <- indicator_recode(sr20_erlassjahr$public_debt_state_rev)
sr20_erlassjahr$foreign_debt_bip <- indicator_recode(sr20_erlassjahr$foreign_debt_bip)
sr20_erlassjahr$foreign_debt_exp <- indicator_recode(sr20_erlassjahr$foreign_debt_exp)
sr20_erlassjahr$external_debt_service_exp <- indicator_recode(sr20_erlassjahr$external_debt_service_exp)

indicator2_recode <- function(var){
  var <- ifelse(sr20_erlassjahr$oecd == 1, -1, var)
}

sr20_erlassjahr$public_debt_bip2 <- indicator2_recode(sr20_erlassjahr$public_debt_bip2)
sr20_erlassjahr$public_debt_state_rev2 <- indicator2_recode(sr20_erlassjahr$public_debt_state_rev2)
sr20_erlassjahr$foreign_debt_bip2 <- indicator2_recode(sr20_erlassjahr$foreign_debt_bip2)
sr20_erlassjahr$foreign_debt_exp2 <- indicator2_recode(sr20_erlassjahr$foreign_debt_exp2)
sr20_erlassjahr$external_debt_service_exp2 <- indicator2_recode(sr20_erlassjahr$external_debt_service_exp2)

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

###################################
## Add Links to Country Profiles ##
###################################

# add link variable for country profiles
sr20_erlassjahr <- sr20_erlassjahr %>% 
  mutate(country_prep = gsub(" und | ", "-", tolower(country))) %>% 
  mutate(country_prep2 = stringi::stri_replace_all_fixed(country_prep, c("ä", "ü", "ö", "ß"), c("ae", "ue", "oe", "ss"), vectorize_all=FALSE))  %>% 
  mutate(link = ifelse(!is.na(region), paste0("https://erlassjahr.de/laenderinfos/", country_prep2, "/"), NA)) %>% 
  select(-country_prep, -country_prep2)

# manually change ambibuous links
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "BIH", "https://erlassjahr.de/laenderinfos/bosnien-herzigowina/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "COD", "https://erlassjahr.de/laenderinfos/kongo-d-r/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "COG", "https://erlassjahr.de/laenderinfos/republik-kongo/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "LAO", "https://erlassjahr.de/laenderinfos/laos/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "LCA", "https://erlassjahr.de/laenderinfos/st-lucia/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "MDA", "https://erlassjahr.de/laenderinfos/moldau/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "VCT", "https://erlassjahr.de/laenderinfos/st-vincent/",sr20_erlassjahr$link)

# manually drop those that don't have a country profile but appear in the sample
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "NRU", NA, sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "OMN", NA, sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "SGP", NA, sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "TKM", NA, sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "TWM", NA, sr20_erlassjahr$link)

# manually add country links for profiles
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "DZA", "https://erlassjahr.de/laenderinfos/algerien/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "GNQ", "https://erlassjahr.de/laenderinfos/aequatorialguinea/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "BWA", "https://erlassjahr.de/laenderinfos/aserbaidschan/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "FJI", "https://erlassjahr.de/laenderinfos/fidschi/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "IRQ", "https://erlassjahr.de/laenderinfos/irak/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "RKS", "https://erlassjahr.de/laenderinfos/kosovo/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "LSO", "https://erlassjahr.de/laenderinfos/lesotho/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "NPL", "https://erlassjahr.de/laenderinfos/nepal/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "PHL", "https://erlassjahr.de/laenderinfos/philippinen/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "SLB", "https://erlassjahr.de/laenderinfos/salomonen/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "WSM", "https://erlassjahr.de/laenderinfos/samoa/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "SOM", "https://erlassjahr.de/laenderinfos/somalia/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "SWZ", "https://erlassjahr.de/laenderinfos/swasiland/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "SYR", "https://erlassjahr.de/laenderinfos/syrien/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "THA", "https://erlassjahr.de/laenderinfos/thailand/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "TLS", "https://erlassjahr.de/laenderinfos/ost-timor/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "TTO", "https://erlassjahr.de/laenderinfos/trinidad-tobago/",sr20_erlassjahr$link)
sr20_erlassjahr$link <- ifelse(sr20_erlassjahr$ISO3 == "UZB", "https://erlassjahr.de/laenderinfos/usbekistan/",sr20_erlassjahr$link)

##------------------##
## Save the Dataset ##
##------------------##

save(sr20_erlassjahr, file = "sr20_erlassjahr.RData")