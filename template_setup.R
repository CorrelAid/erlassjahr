##---------------------##
## CorrelAid Project:  ##
##    erlassjahr.de    ##
##---------------------##
##    Excel Template   ##
##---------------------##

##--------------##
## Prerequisits ##
##--------------##

library(magrittr)
library(countrycode)
library(dplyr)
library(writexl)

## Load the main data that was used for the initial map to build template
load("sr20_erlassjahr.RData")

##------------------##
## Data Preparation ##
##------------------##

# keep only static information (country name, region, oecd member, etc.)
sr_df <- sr20_erlassjahr %>%
  select(ISO3, country, region, oecd, no_data)

# German country names
custom_match <- c("RKS" = "Kosovo")
sr_df$country <-
  countrycode::countrycode(sr_df$ISO3, "iso3c", "country.name.de", custom_match = custom_match)

# rename country and data variable
sr_df %<>%
  rename(land = country,
         keine_daten = no_data)

# add columns for indicators
indicator_names <-
  c(
    "oeffentliche_schulden_bip",
    "oeffentliche_schulden_staatseinnahmen",
    "auslandsschulden_bip",
    "auslandsschuldenstand_exporteinnahmen",
    "auslandsschuldendienst_exporteinnahmen",
    "iwf_einschaetzung",
    "extraktivismus",
    "fragilitaet",
    "problematische_schuldenstruktur",
    "vulnerabilitaet_naturkatastrophen",
    "zahlungssituation"
  )
# add empty columns to add manually
sr_df[, indicator_names] <- NA

##------------------##
## Export Dataframe ##
##------------------##

writexl::write_xlsx(sr_df, path = "schuldenreport_vorlage.xlsx")

