##-----------------------##
##   CorrelAid Project:  ##
##      erlassjahr.de    ##
##-----------------------##
##    Helper Functions   ##
##-----------------------##

##--------------##
## Prerequisits ##
##--------------##

# Dependencies
library(readxl)
library(magrittr)
library(tidyverse)
library(english)

##--------------------##
##  Define Functions  ##
##--------------------##

# The following functions are helper functions for the data_preparation.R file

# Recode indicator to treshold category -----------------------------------

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


# Recode trend variable from numeric to character display -----------------

trend_new_recode <- function(var) {
  var <-
    plyr::revalue(as.character(as.english(var)), c("minus one" = "minus_one"))
}


# Recode indicator variable for OECD countries ----------------------------

indicator_recode <- function(var) {
  var <- ifelse(sr_df$oecd == 1, 0, var)
}


# Recode trend variable for OECD countries --------------------------------

indicator2_recode <- function(var) {
  var <- ifelse(sr_df$oecd == 1,-1, var)
}
