############### DATENBEREINIGUNG ############### 
library(readxl)

###### Datenimport - Daten VORHER ###### 
Daten <- read_excel("/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Rohdaten.xlsx")

View(Daten)

nrow(Daten)  # -> 128 Beobachtungen


###### Wahl für flexibles Arbeitszeitmodell = 1, Wahl Lohnerhöhung = 0  ###### 
Daten$VierTageWoche <- ifelse(Daten$VierTageWoche == "Vier-Tage-Woche", 1, 0)

Daten$Jahresarbeitszeit <- ifelse(Daten$Jahresarbeitszeit == "Jahresarbeitszeit", 1, 0)

Daten$Vertrauensarbeitszeit <- ifelse(Daten$Vertrauensarbeitszeit == "Vertrauensarbeitszeit", 1, 0)

###### Geschlecht: weiblich, männlich  ###### 
#bleibt bestehen

###### Alterskategorie ###### 
#jünger als 20 Jahre und älter als 65 Jahre werden aus der Stichprobe genommen
#Es gibt nur sehr wenige Beobachtungen in dieser Alterkategorien, was zu instabilen Schätzungen führen kann.

Daten <- Daten[Daten$Alterskategorie !="jünger als 20 Jahre", ]
Daten <- Daten[Daten$Alterskategorie !="älter als 65 Jahre", ]


##### Beziehungsstatus #####
#getrennt, verwitwet oder geschieden werden ebenfalls aus der Stichprobe genommen
#Es gibt nur sehr wenige Beobachtungen für diese Beziehungsstatus, was zu instabilen Schätzungen führen kann.
# verwitwet ist bereits bei der Bereinigung der Alerskategorie "älter als 65 Jahre" rausgefallen
Daten[Daten$Beziehungsstatus == "geschieden",]$Beziehungsstatus <- "ledig / getrennt / geschieden"
Daten[Daten$Beziehungsstatus == "getrennt",]$Beziehungsstatus <- "ledig / getrennt / geschieden"
Daten[Daten$Beziehungsstatus == "ledig",]$Beziehungsstatus <- "ledig / getrennt / geschieden"


##### Familienverantwortung #####
#bleibt bestehen

##### Haushaltseinkommen pro Monat #####
# "weniger als 2'000 CHF" und "2'001 bis 6'000 CHF" werden zusammengefasst als "weniger als 6'000 CHF"
xtabs(~ VierTageWoche + HaushaltseinkommenProMonat, data = Daten)
xtabs(~ Jahresarbeitszeit + HaushaltseinkommenProMonat, data = Daten)
xtabs(~ Vertrauensarbeitszeit + HaushaltseinkommenProMonat, data = Daten)

Daten[Daten$HaushaltseinkommenProMonat == "weniger als 2'000 CHF",]$HaushaltseinkommenProMonat <- "weniger als 6'000 CHF"
Daten[Daten$HaushaltseinkommenProMonat == "2'001 bis 6'000 CHF",]$HaushaltseinkommenProMonat <- "weniger als 6'000 CHF"

Daten[Daten$HaushaltseinkommenProMonat == "14'001 bis 18'000 CHF",]$HaushaltseinkommenProMonat <- "mehr als 14'000 CHF"
Daten[Daten$HaushaltseinkommenProMonat == "mehr als 18'000 CHF",]$HaushaltseinkommenProMonat <- "mehr als 14'000 CHF"

##### Branchen #####
# wie bereits beim Balkendiagramm, werden einige Branchen durch übrige Branchen ersetzt
xtabs(~ Vertrauensarbeitszeit + Branche, data = Daten)
xtabs(~ VierTageWoche + Branche, data = Daten)
xtabs(~ Jahresarbeitszeit + Branche, data = Daten)

Branchen <- data.frame(Branche = Daten$Branche)

einzelneBranchen <- c("Gesundheits- und Sozialwesen", "Baubranche", "Finanz- und Versicherungsdienstleistung")
einzelneBranchen

Branchen_neu <- Branchen %>%
  mutate(Branche_neu = ifelse(Branche %in% einzelneBranchen, Branche, "Übrige Branchen")) %>%
  group_by(Branche_neu)

Daten$Branche <- Branchen_neu$Branche_neu

xtabs(~ Branche, data = Daten)

##### bereinigte Daten in neue csv.Datei für die Analyse ##### 
Daten_bereinigt <- data.frame(Daten)

View(Daten_bereinigt)

write.csv(Daten_bereinigt, file = "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Daten_bereinigt.csv", row.names = FALSE)


