##-----------------------##
##   CorrelAid Project:  ##
##      erlassjahr.de    ##
##-----------------------##
##    Data Preparation   ##
##-----------------------##

##--------------##
## Prerequisits ##
##--------------##

# Dependencies 
library(readxl)
library(magrittr)
library(tidyverse)
library(english)

##-------------##
##  Load Data  ##
##-------------##

sr_df <- read_xlsx("schuldenreport_vorlage.xlsx")

##--------------------##
##  Data Preparation  ##
##--------------------##

# rename German variable names to make them compatible with the app structure
sr_df %<>%
  dplyr::rename(country = land,
                no_data = keine_daten,
                public_debt_bip = oeffentliche_schulden_bip,
                trend_pdb = trend_oe_schulden_bip,
                public_debt_state_rev = oeffentliche_schulden_staatseinnahmen,
                trend_pdsr = trend_oe_schulden_staat,
                foreign_debt_bip = auslandsschulden_bip,
                trend_fdp = trend_ausl_bip,
                foreign_debt_exp = auslandsschuldenstand_exporteinnahmen,
                trend_fde = trend_aus_schuldenstand_export,
                external_debt_service_exp = auslandsschuldendienst_exporteinnahmen,
                trend_edse = trend_ausl_schuldendienst_export,
                risk_excessive_debt = iwf_einschaetzung,
                extractivism = extraktivismus,
                fragility = fragilitaet,
                debt_prob = problematische_schuldenstruktur,
                vulnerability = vulnerabilitaet_naturkatastrophen,
                payment_stop = zahlungssituation
                )

##-------------------##
##  Set-up Variables ##
##-------------------##

###########################
## Debt situation value ##
##########################

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
sr_df$public_debt_bip2 <- filter_recode(sr_df$public_debt_bip, public_debt_bip_filter)
sr_df$public_debt_state_rev2 <- filter_recode(sr_df$public_debt_state_rev, public_debt_state_rev_filter)
sr_df$foreign_debt_bip2 <- filter_recode(sr_df$foreign_debt_bip, foreign_debt_bip_filter)
sr_df$foreign_debt_exp2 <- filter_recode(sr_df$foreign_debt_exp, foreign_debt_exp_filter)
sr_df$external_debt_service_exp2 <- filter_recode(sr_df$external_debt_service_exp, external_debt_service_exp_filter)

###########################
## Total debt situation ##
##########################

# accumulated debt indicators
sr_df$debt_sit_total <- rowSums(sr_df[, c(22:26)], na.rm = TRUE)

# use accumulated debt indicator value to categorize states' debt situation
sr_df$debt_sit_cat2 <-
  ifelse(
    sr_df$debt_sit_total == 0,
    0,
    ifelse(
      sr_df$debt_sit_total > 0 &
        sr_df$debt_sit_total < 5,
      1,
      ifelse(
        sr_df$debt_sit_total >= 5 &
          sr_df$debt_sit_total < 10,
        2,
        ifelse(sr_df$debt_sit_total >= 10, 3, NA)
      )
    )
  )

# set OECD countries' debt situation to -1 to display accurately in map
sr_df$debt_sit_cat2 <- ifelse(sr_df$oecd == 1, -1, sr_df$debt_sit_cat2)

# create a factor variable for the debt situation category
sr_df$debt_sit_cat <- factor(sr_df$debt_sit_cat2)
levels(sr_df$debt_sit_cat) <- c("nicht Teil der Betrachtung", "nicht kritisch", "leicht kritisch", "kritisch", "sehr kritisch")


###############
## Trend New ##
###############

trend_new_recode <- function(var){
  var <- plyr::revalue(as.character(as.english(var)), c("minus one"="minus_one"))
}

#sr_df$trend_new <- trend_new_recode(sr_df$trend)
sr_df$trend_pdb_new <- trend_new_recode(sr_df$trend_pdb)
sr_df$trend_pdsr_new <- trend_new_recode(sr_df$trend_pdsr)
sr_df$trend_fdp_new <- trend_new_recode(sr_df$trend_fdp)
sr_df$trend_fde_new <- trend_new_recode(sr_df$trend_fde)
sr_df$trend_edse_new <- trend_new_recode(sr_df$trend_edse)

################
## Indicators ##
################

# Modify indicator variables for OECD countries so that they can be displayed accurately on the map

indicator_recode <- function(var){
  var <- ifelse(sr_df$oecd == 1, 0, var)
}

sr_df$public_debt_bip <- indicator_recode(sr_df$public_debt_bip)
sr_df$public_debt_state_rev <- indicator_recode(sr_df$public_debt_state_rev)
sr_df$foreign_debt_bip <- indicator_recode(sr_df$foreign_debt_bip)
sr_df$foreign_debt_exp <- indicator_recode(sr_df$foreign_debt_exp)
sr_df$external_debt_service_exp <- indicator_recode(sr_df$external_debt_service_exp)

indicator2_recode <- function(var){
  var <- ifelse(sr_df$oecd == 1, -1, var)
}

sr_df$public_debt_bip2 <- indicator2_recode(sr_df$public_debt_bip2)
sr_df$public_debt_state_rev2 <- indicator2_recode(sr_df$public_debt_state_rev2)
sr_df$foreign_debt_bip2 <- indicator2_recode(sr_df$foreign_debt_bip2)
sr_df$foreign_debt_exp2 <- indicator2_recode(sr_df$foreign_debt_exp2)
sr_df$external_debt_service_exp2 <- indicator2_recode(sr_df$external_debt_service_exp2)

#####################
## Risk Situations ##
#####################

risk_new <- function(var){
  var <- ifelse(is.na(var), 0, 1)
}

sr_df$extractivism <- risk_new(sr_df$extractivism)
sr_df$fragility <- risk_new(sr_df$fragility)
sr_df$debt_prob <- risk_new(sr_df$debt_prob)
sr_df$vulnerability <- risk_new(sr_df$vulnerability)

#######################
## Payment Situation ##
#######################

sr_df$payment_stop <- ifelse(
  sr_df$payment_stop == 1,
  "feuer_grau",
  ifelse(
    sr_df$payment_stop == 2,
    "feuer_orange",
    ifelse(sr_df$payment_stop == 3, "feuer_rot", NA)
  )
)

