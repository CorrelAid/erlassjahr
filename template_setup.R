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
  select(ISO3, country, region, oecd, no_data, extractivism, fragility, debt_prob, vulnerability, payment_stop, income)

# German country names
custom_match <- c("RKS" = "Kosovo")
sr_df$country <-
  countrycode::countrycode(sr_df$ISO3, "iso3c", "country.name.de", custom_match = custom_match)

# rename country and data variable
sr_df %<>%
  rename(land = country,
         keine_daten = no_data,
         extraktivismus = extractivism,
         fragilitaet = fragility,
         problematische_schuldenstruktur = debt_prob,
         vulnerabilitaet_naturkatastrophen = vulnerability,
         zahlungssituation = payment_stop)

# replace feuer_* with numeric indicators

sr_df$zahlungssituation <- ifelse(
  sr_df$zahlungssituation == "feuer_grau", 1,
  ifelse(
    sr_df$zahlungssituation == "feuer_orange", 2,
    ifelse(sr_df$zahlungssituation == "feuer_rot", 3, NA)
  )
)

# add columns for indicators
indicator_names <-
  c(
    "oeffentliche_schulden_bip",
    "trend_oe_schulden_bip",
    "oeffentliche_schulden_staatseinnahmen",
    "oeffentliche_schulden_staatseinnahmen_indicator",
    "trend_oe_schulden_staat",
    "auslandsschulden_bip",
    "auslandsschulden_bip_indicator",
    "trend_ausl_bip",
    "auslandsschuldenstand_exporteinnahmen",
    "auslandsschuldenstand_exporteinnahmen_indicator",
    "trend_aus_schuldenstand_export",
    "auslandsschuldendienst_exporteinnahmen",
    "trend_ausl_schuldendienst_export",
    "iwf_einschaetzung"
  )
# add empty columns to add manually
sr_df[, indicator_names] <- NA

# add link variable for country profiles
sr_df <- sr_df %>% 
  mutate(country_prep = gsub(" und | ", "-", tolower(land))) %>% 
  mutate(country_prep2 = stringi::stri_replace_all_fixed(country_prep, c("ä", "ü", "ö", "ß"), c("ae", "ue", "oe", "ss"), vectorize_all=FALSE))  %>% 
  mutate(link = ifelse(!is.na(region), paste0("https://erlassjahr.de/laenderinfos/", country_prep2, "/"), NA)) %>% 
  select(-country_prep, -country_prep2)

# manually change ambibuous links
sr_df$link <- ifelse(sr_df$ISO3 == "BIH", "https://erlassjahr.de/laenderinfos/bosnien-herzigowina/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "COD", "https://erlassjahr.de/laenderinfos/kongo-d-r/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "COG", "https://erlassjahr.de/laenderinfos/republik-kongo/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "LAO", "https://erlassjahr.de/laenderinfos/laos/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "LCA", "https://erlassjahr.de/laenderinfos/st-lucia/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "MDA", "https://erlassjahr.de/laenderinfos/moldau/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "VCT", "https://erlassjahr.de/laenderinfos/st-vincent/",sr_df$link)

# manually drop those that don't have a country profile but appear in the sample
sr_df$link <- ifelse(sr_df$ISO3 == "NRU", NA, sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "OMN", NA, sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "SGP", NA, sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "TKM", NA, sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "TWM", NA, sr_df$link)

# manually add country links for profiles
sr_df$link <- ifelse(sr_df$ISO3 == "DZA", "https://erlassjahr.de/laenderinfos/algerien/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "GNQ", "https://erlassjahr.de/laenderinfos/aequatorialguinea/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "BWA", "https://erlassjahr.de/laenderinfos/aserbaidschan/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "FJI", "https://erlassjahr.de/laenderinfos/fidschi/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "IRQ", "https://erlassjahr.de/laenderinfos/irak/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "RKS", "https://erlassjahr.de/laenderinfos/kosovo/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "LSO", "https://erlassjahr.de/laenderinfos/lesotho/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "NPL", "https://erlassjahr.de/laenderinfos/nepal/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "PHL", "https://erlassjahr.de/laenderinfos/philippinen/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "SLB", "https://erlassjahr.de/laenderinfos/salomonen/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "WSM", "https://erlassjahr.de/laenderinfos/samoa/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "SOM", "https://erlassjahr.de/laenderinfos/somalia/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "SWZ", "https://erlassjahr.de/laenderinfos/swasiland/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "SYR", "https://erlassjahr.de/laenderinfos/syrien/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "THA", "https://erlassjahr.de/laenderinfos/thailand/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "TLS", "https://erlassjahr.de/laenderinfos/ost-timor/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "TTO", "https://erlassjahr.de/laenderinfos/trinidad-tobago/",sr_df$link)
sr_df$link <- ifelse(sr_df$ISO3 == "UZB", "https://erlassjahr.de/laenderinfos/usbekistan/",sr_df$link)

#######################
##WB Input           ##
#######################

# getter function
WB_input <- function (year, countries){
  library(WDI)
  dataSeries = c("DT.DOD.DECT.GN.ZS", "DT.DOD.DECT.EX.ZS", "DT.TDS.DECT.EX.ZS")
  data = WDI(indicator=dataSeries, country=countries, start=year, end=year)
  return(data);
}
# getting the data
countries <- sr_df %>% 
  filter(oecd == 0 & keine_daten==0) %>% 
  pull(ISO3)
raw_indicators <- WB_input(2018 ,countries)

#merging the data
raw_indicators$iso2c <-  countrycode::countrycode(raw_indicators$iso2c, 
                         "iso2c", "iso3c")
sr_df <- merge(sr_df, raw_indicators, by.x="ISO3", by.y="iso2c")
sr_df$oeffentliche_schulden_staatseinnahmen <- sr_df$DT.DOD.DECT.GN.ZS
sr_df$auslandsschuldenstand_exporteinnahmen <- sr_df$DT.DOD.DECT.EX.ZS
sr_df$auslandsschuldendienst_exporteinnahmen <- sr_df$DT.TDS.DECT.EX.ZS
#deleting the helping colums 
sr_df <- subset(sr_df, select = -c(DT.DOD.DECT.GN.ZS,DT.DOD.DECT.EX.ZS,DT.TDS.DECT.EX.ZS))

###########################
##Coding into Indicators ##
###########################
sr_df$oeffentliche_schulden_staatseinnahmen_indicator <- 
  ifelse(sr_df$oeffentliche_schulden_staatseinnahmen < 200, 0,
  ifelse(sr_df$oeffentliche_schulden_staatseinnahmen < 300, 1,
  ifelse(sr_df$oeffentliche_schulden_staatseinnahmen<=400, 2, 3)))
sr_df$auslandsschuldenstand_exporteinnahmen_indicator <- 
  ifelse(sr_df$auslandsschuldenstand_exporteinnahmen < 150, 0,
  ifelse(sr_df$auslandsschuldenstand_exporteinnahmen <=225, 1,
  ifelse(sr_df$auslandsschuldenstand_exporteinnahmen <=300, 2,3)))
sr_df$auslandsschulden_bip_indicator <- 
  ifelse(sr_df$auslandsschulden_bip < 40, 0,
  ifelse(sr_df$auslandsschulden_bip < 60, 1,
  ifelse(sr_df$auslandsschulden_bip <= 80, 2, 3)))
sr_df$auslandsschuldendienst_exporteinnahmen_indicator <- 
  ifelse(sr_df$auslandsschuldendienst_exporteinnahmen<15, 0,
  ifelse(sr_df$auslandsschuldendienst_exporteinnahmen<22.5, 1,
  ifelse(sr_df$auslandsschuldendienst_exporteinnahmen<=30,2,3)))

##------------------##
## Export Dataframe ##
##------------------##

writexl::write_xlsx(sr_df, path = "schuldenreport_vorlage.xlsx")

