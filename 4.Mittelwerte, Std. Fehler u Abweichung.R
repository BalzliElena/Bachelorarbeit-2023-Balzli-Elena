############### MITTELWERTE, STANDARDABWEICHUNGEN UND STANDARDFEHLER ############### 
library(stargazer)
library(ggplot2)

Daten1 <- read.csv("/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Daten_bereinigt.csv")

######## Mittelwert, Standardabweichung und -fehler #########
Daten1$VierTageWoche <- as.numeric(Daten1$VierTageWoche)
VIERn <- length(Daten1$VierTageWoche)
VIERmean <- mean(Daten1$VierTageWoche)
VIERstd <- sd(Daten1$VierTageWoche)
VIERsderror <- sd(Daten1$VierTageWoche) / sqrt(length(Daten1$VierTageWoche))

VIERn
VIERmean
VIERstd
VIERsderror


Daten1$Jahresarbeitszeit <- as.numeric(Daten1$Jahresarbeitszeit)
JAHRn <- length(Daten1$Jahresarbeitszeit)
JAHRmean <- mean(Daten1$Jahresarbeitszeit)
JAHRstd <- sd(Daten1$Jahresarbeitszeit)
JAHRsderror <- sd(Daten1$Jahresarbeitszeit) / sqrt(length(Daten1$Jahresarbeitszeit))

JAHRn
JAHRmean
JAHRstd
JAHRsderror


Daten1$Vertrauensarbeitszeit <- as.numeric(Daten1$Vertrauensarbeitszeit)
VERTn <- length(Daten1$Vertrauensarbeitszeit)
VERTmean <- mean(Daten1$Vertrauensarbeitszeit)
VERTstd <- sd(Daten1$Vertrauensarbeitszeit)
VERTsderror <- sd(Daten1$Vertrauensarbeitszeit) / sqrt(length(Daten1$Vertrauensarbeitszeit))

VERTn
VERTmean
VERTstd
VERTsderror


Tabelle1 <- data.frame(
  flexiblesArbeitszeitmodell = c("Vier-Tage-Woche", "Jahresarbeitszeit", "Vertrauensarbeitszeit"),
  N = c(VIERn, JAHRn, VERTn),
  Mittelwert = c(VIERmean, JAHRmean, VERTmean),
  Standardabweichung = c(VIERstd, JAHRstd, VERTstd),
  Standardfehler = c(VIERsderror, JAHRsderror, VERTsderror)
)

colnames(Tabelle1) <- c("Flexibles Arbeitszeitmodell", "N", "Mittelwert", "Standardabweichung", "Standardfehler")


stargazer(Tabelle1, 
          title = "Flexible Arbeitszeitmodelle",
          align = TRUE,
          summary = FALSE,
          out = "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Tabellen/Table 1.html")

    