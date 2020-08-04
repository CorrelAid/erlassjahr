## --------------------------------------------------##
## CorrelAid Project:                               ##
##    erlassjahr.de                                 ##
## --------------------------------------------------##
##  Legends definitions
## --------------------------------------------------##

debt_legends <- function() {
  legends <- list(
    debt_sit_cat2 = c(
      "very critical",
      "critical",
      "slightly critical",
      "not critical",
      "no information",
      "not in sample",
      " ",
      "Source: erlassjahr.de"
    ),
    public_debt_bip2 = c(
      "> 100 %",
      "75% - 100%",
      "50% - 75%",
      "< 50%",
      "no information",
      "not in sample",
      " ",
      "Source: erlassjahr.de"
    ),
    public_debt_state_rev2 = c(
      "> 400 %",
      "300% - 400%",
      "200% - 300%",
      "< 200%",
      "no information",
      "not in sample",
      " ",
      "Source: erlassjahr.de"
    ),
    foreign_debt_bip2 = c(
      "> 80 %",
      "60% - 80%",
      "40% - 60%",
      "< 40%",
      "no information",
      "not in sample",
      " ",
      "Source: erlassjahr.de"
    ),
    foreign_debt_exp2 = c(
      "> 300 %",
      "225% - 300%",
      "150% - 225%",
      "< 150%",
      "no information",
      "not in sample",
      " ",
      "Source: erlassjahr.de"
    ),
    default = c(
      "> 30 %",
      "22,5% - 30%",
      "15% - 22,5%",
      "< 15%",
      "no information",
      "not in sample",
      " ",
      "Source: erlassjahr.de"
    )
  )
  return(legends)
}
