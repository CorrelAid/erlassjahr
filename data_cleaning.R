library(countrycode)
library(tidyverse)
library(magrittr)
sr19_erlassjahr <- read_xlsx("~/Downloads/SR19 Überblickstabelle.xlsx")
sr19_erlassjahr <- sr19_erlassjahr[2:127,1:17]

custom_match <- c("Moldawien" = "MDA")
sr19_erlassjahr$Land <- countrycode::countrycode(sr19_erlassjahr$Land, 
                                                 "country.name.de", "iso3c", custom_match = custom_match)

names(sr19_erlassjahr)[8] <- "foreign_debt_exp"
names(sr19_erlassjahr)[10] <- "external_debt_service_exp"

sr19_erlassjahr %<>% 
  filter(!is.na(Land)) %>% 
  rename(public_debt_bip = `Öffentliche Schulden / BIP`,
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

public_debt_bip_filter <- c(50, 75, 100)
public_debt_state_rev_filter <- c(200, 300, 400)
foreign_debt_bip_filter <- c(40, 60, 80)
foreign_debt_exp_filter <- c(150, 225, 300)
external_debt_service_exp_filter <- c(15, 22.5, 30)

sr19_erlassjahr$public_debt_bip2 <- filter_recode(sr19_erlassjahr$public_debt_bip, public_debt_bip_filter)
sr19_erlassjahr$public_debt_state_rev2 <- filter_recode(sr19_erlassjahr$public_debt_state_rev, public_debt_state_rev_filter)
sr19_erlassjahr$foreign_debt_bip2 <- filter_recode(sr19_erlassjahr$foreign_debt_bip, foreign_debt_bip_filter)
sr19_erlassjahr$foreign_debt_exp2 <- filter_recode(sr19_erlassjahr$foreign_debt_exp, foreign_debt_exp_filter)
sr19_erlassjahr$external_debt_service_exp2 <- filter_recode(sr19_erlassjahr$external_debt_service_exp, external_debt_service_exp_filter)

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

sr19_erlassjahr$trend_pdb <- trend_recode(sr19_erlassjahr$trend_pdb)
sr19_erlassjahr$trend_pdsr <- trend_recode(sr19_erlassjahr$trend_pdsr)
sr19_erlassjahr$trend_fdp <- trend_recode(sr19_erlassjahr$trend_fdp)
sr19_erlassjahr$trend_fde <- trend_recode(sr19_erlassjahr$trend_fde)
sr19_erlassjahr$trend_edse <- trend_recode(sr19_erlassjahr$trend_edse)
