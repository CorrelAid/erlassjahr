##---------------------##
## CorrelAid Project:  ##
##    erlassjahr.de    ##
##---------------------##
##    Excel Template   ##
##---------------------##

##--------------##
## Prerequisits ##
##--------------##

library(magrittr) # Forward-Pipe Operator for R
library(countrycode) # Convert Country Names and Country Codes
library(dplyr) # A Grammar of Data Manipulation
library(writexl) # Export Data Frames to Excel 'xlsx' Format
library(WDI) # World Development Indicators (World Bank) # World Development Indicators (World Bank)

## Load the main data that was used for the initial map to build template
sr_df <-
  get(load(paste0("data/final_data_", year - 1, ".RData"), e <-
             new.env()), e)
rm(list = ls(envir = e), envir = e)

##------------------##
## Data Preparation ##
##------------------##

# keep only static information (country name, region, oecd member, etc.)
sr_df <- sr_df %>%
  dplyr::select(
    ISO3,
    country,
    region,
    oecd,
    not_critical,
    no_data,
    extractivism,
    fragility,
    debt_prob,
    vulnerability,
    payment_stop,
    income,
    link
  )

# German country names
custom_match <- c("RKS" = "Kosovo")
sr_df$country <-
  countrycode::countrycode(sr_df$ISO3, "iso3c", "country.name.de", custom_match = custom_match)

# rename country and data variable
sr_df %<>%
  rename(
    land = country,
    keine_daten = no_data,
    extraktivismus = extractivism,
    fragilitaet = fragility,
    problematische_schuldenstruktur = debt_prob,
    vulnerabilitaet_naturkatastrophen = vulnerability,
    zahlungssituation = payment_stop
  )

# replace feuer_* with numeric indicators

sr_df$zahlungssituation <- ifelse(
  sr_df$zahlungssituation == "feuer_grau",
  1,
  ifelse(
    sr_df$zahlungssituation == "feuer_orange",
    2,
    ifelse(sr_df$zahlungssituation == "feuer_rot", 3, NA)
  )
)

# add columns for indicators
indicator_names <-
  c(
    "oeffentliche_schulden_bip",
    "trend_oe_schulden_bip",
    "oeffentliche_schulden_staatseinnahmen",
    "trend_oe_schulden_staat",
    "auslandsschulden_bip",
    "trend_ausl_bip",
    "auslandsschuldenstand_exporteinnahmen",
    "trend_aus_schuldenstand_export",
    "auslandsschuldendienst_exporteinnahmen",
    "trend_ausl_schuldendienst_export",
    "iwf_einschaetzung"
  )
# add empty columns to add manually
sr_df[, indicator_names] <- NA

##--------------##
## WDI API call ##
##--------------##

# function to extract the three main WDI variables from the API using WDI
WB_input <- function (year) {
  dataSeries <- c("DT.DOD.DECT.GN.ZS",
                  "DT.DOD.DECT.EX.ZS",
                  "DT.TDS.DECT.EX.ZS")
  data <- WDI(
    indicator = dataSeries,
    country = "all",
    start = year,
    end = year
  )
  return(data)
}

# using the function for the year t-2 to get the most recent data available
raw_indicators <- WB_input(year - 2)

# merging the data
raw_indicators$iso2c <-
  countrycode::countrycode(raw_indicators$iso2c,
                           "iso2c", "iso3c")

sr_df <-
  merge(
    sr_df,
    raw_indicators,
    by.x = "ISO3",
    by.y = "iso2c",
    all.x = TRUE
  )
sr_df$oeffentliche_schulden_staatseinnahmen <-
  sr_df$DT.DOD.DECT.GN.ZS
sr_df$auslandsschuldenstand_exporteinnahmen <-
  sr_df$DT.DOD.DECT.EX.ZS
sr_df$auslandsschuldendienst_exporteinnahmen <-
  sr_df$DT.TDS.DECT.EX.ZS
#deleting the helping columns
sr_df <-
  subset(
    sr_df,
    select = -c(
      country,
      year,
      DT.DOD.DECT.GN.ZS,
      DT.DOD.DECT.EX.ZS,
      DT.TDS.DECT.EX.ZS
    )
  )

##------------------##
## Export Dataframe ##
##------------------##

writexl::write_xlsx(sr_df, path = paste0("schuldenreport_vorlage_", year, ".xlsx"))
