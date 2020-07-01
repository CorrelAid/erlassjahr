## --------------------------------------------------##
## CorrelAid Project:                               ##
##    erlassjahr.de                                 ##
## --------------------------------------------------##
##  Legends definitions
## --------------------------------------------------##

debt_legends <- function() {
  legends <- list(
    debt_sit_cat2 = c(
      "sehr kritisch",
      "kritisch",
      "leicht kritisch",
      "nicht kritisch",
      "keine Daten vorhanden",
      "nicht Teil der Betrachtung",
      " ",
      "Quelle: erlassjahr.de"
    ),
    public_debt_bip2 = c(
      "> 100 %",
      "75% - 100%",
      "50% - 75%",
      "< 50%",
      "keine Daten vorhanden",
      "nicht Teil der Betrachtung",
      " ",
      "Quelle: erlassjahr.de"
    ),
    public_debt_state_rev2 = c(
      "> 400 %",
      "300% - 400%",
      "200% - 300%",
      "< 200%",
      "keine Daten vorhanden",
      "nicht Teil der Betrachtung",
      " ",
      "Quelle: erlassjahr.de"
    ),
    foreign_debt_bip2 = c(
      "> 80 %",
      "60% - 80%",
      "40% - 60%",
      "< 40%",
      "keine Daten vorhanden",
      "nicht Teil der Betrachtung",
      " ",
      "Quelle: erlassjahr.de"
    ),
    foreign_debt_exp2 = c(
      "> 300 %",
      "225% - 300%",
      "150% - 225%",
      "< 150%",
      "keine Daten vorhanden",
      "nicht Teil der Betrachtung",
      " ",
      "Quelle: erlassjahr.de"
    ),
    default = c(
      "> 30 %",
      "22,5% - 30%",
      "15% - 22,5%",
      "< 15%",
      "keine Daten vorhanden",
      "nicht Teil der Betrachtung",
      " ",
      "Quelle: erlassjahr.de"
    )
  )
  return(legends)
}
