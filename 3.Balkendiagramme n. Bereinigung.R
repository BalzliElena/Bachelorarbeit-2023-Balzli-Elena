############### BALKENDIAGRAMME N. BEREINIGUNG ############### 
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(png)
library(readxl)
library(dplyr)
library(cowplot)
loadfonts()


###### Datenimport ###### 
Daten <- read.csv("/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Daten_bereinigt.csv")

nrow(Daten)  # -> 121 Beobachtungen

###### Flexible Arbeitszeitmodelle vs. Lohnerhöhung ###### 
xtabs(~ VierTageWoche, data = Daten)
xtabs_VierTageWoche <- xtabs(~ VierTageWoche, data = Daten)
VierTageWocheVS.Lohnerhöhung <- prop.table(xtabs_VierTageWoche) * 100

xtabs(~ Jahresarbeitszeit, data = Daten)
xtabs_Jahresarbeitszeit <- xtabs(~ Jahresarbeitszeit, data = Daten)
JahresarbeitszeitVS.Lohnerhöhung <- prop.table(xtabs_Jahresarbeitszeit) * 100

xtabs(~ Vertrauensarbeitszeit, data = Daten)
xtabs_Vertrauensarbeitszeit <- xtabs(~ Vertrauensarbeitszeit, data = Daten)
VertrauensarbeitszeitVS.Lohnerhöhung <- prop.table(xtabs_Vertrauensarbeitszeit) * 100

dataframe_Total <- data.frame(
  Kategorie = c("Vier-Tage-Woche", "Jahresarbeitszeit", "Vertrauensarbeitszeit"),
  flexiblesArbeitszeitmodell = c(VierTageWocheVS.Lohnerhöhung[2], JahresarbeitszeitVS.Lohnerhöhung[2], VertrauensarbeitszeitVS.Lohnerhöhung[2]),
  Anteil_Lohnerhoehung = c(VierTageWocheVS.Lohnerhöhung[1], JahresarbeitszeitVS.Lohnerhöhung[1], VertrauensarbeitszeitVS.Lohnerhöhung[1]))


data_long <- tidyr::gather(data = dataframe_Total, key = "Anteil", value = "Prozent", -Kategorie)
data_long$Kategorie <- factor(data_long$Kategorie, levels = c("Vier-Tage-Woche", "Jahresarbeitszeit", "Vertrauensarbeitszeit"))


FlexibleArbeitszeitvs.Lohnerhöhung2 <- ggplot(data_long, aes(x = Kategorie, y = Prozent, fill = Anteil)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = paste0(round(Prozent, 1), "%")), position = position_stack(vjust = 0.5), size = 3.75) +
  ggtitle("Flexible Arbeitszeitmodelle vs. eine Lohnerhöhung, Überblick Ergebnisse") +
  scale_fill_manual(
    values = c("#CCCC99", "#99CCFF"),
    labels = c("Lohnerhöhung", "Flexibles Arbeitszeitmodell")
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x.bottom = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    title = element_text(size = 8)
  ) +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )


speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Balkendiagramme/FlexibleArbeitszeitvs.Lohnerhöhung.png"
ggsave(speicherort, plot = FlexibleArbeitszeitvs.Lohnerhöhung2, width = 4.5, height = 4.5, units = "in")


###### Geschlechterverteilung: Prozentualer Anteil von Frauen und Männern in der Stichprobe ###### 
xtabs(~ Geschlecht, data = Daten)
Geschlecht <- xtabs(~ Geschlecht, data = Daten)
Geschlecht <- prop.table(Geschlecht) * 100
Geschlecht <- data.frame(Geschlecht)

Geschlechterverteilung2 <- ggplot(Geschlecht, aes(x = factor(1), y = Freq, fill = Geschlecht)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) +
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), position = position_stack(vjust = 0.5), size = 3.75) +
  scale_fill_manual(values = c("männlich" = "#99CCFF", "weiblich" = "#CCCC99")) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )+
  ggtitle("Geschlechterverteilung") +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )


speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Balkendiagramme/Geschlechterverteilung.png"
ggsave(speicherort, plot = Geschlechterverteilung2, width = 4.5, height = 4.5 , units = "in")


###### Altersverteilung: Anteil der Stichprobe in unterschiedlichen Alterskategorien ###### 
Alterskategorie <- xtabs(~ Alterskategorie, data = Daten)
Alterskategorie <- prop.table(Alterskategorie) * 100
Alterskategorie <- data.frame(Alterskategorie)

Alterskategorie$Alterskategorie <- factor(Alterskategorie$Alterskategorie, 
                                          levels = c("jünger als 20 Jahre", "20 - 29 Jahre", 
                                                     "30 - 39 Jahre", "40 - 49 Jahre", "50 - 65 Jahre", "älter als 65 Jahre"))

Altersverteilung2 <- ggplot(Alterskategorie, aes(x = factor(1), y = Freq, fill = Alterskategorie)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) + 
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), position = position_stack(vjust = 0.5), size = 3.75) +
  scale_fill_manual(values = c("jünger als 20 Jahre" = "#CCCC99", "20 - 29 Jahre" = "#999966", "30 - 39 Jahre" = "#666633", 
                               "40 - 49 Jahre" = "#999933", "50 - 65 Jahre" = "#CCCC66", "älter als 65 Jahre" = "#FFFF99")) +
  guides(fill = guide_legend(ncol = 2)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )+
  ggtitle("Verteilung der Alterskategorien") +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )


speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Balkendiagramme/Altersverteilung.png"
ggsave(speicherort, plot = Altersverteilung2, width = 4.5, height = 4.5 , units = "in")


###### Verteilung nach Beziehungsstatus ###### 
Beziehungsstatus <- xtabs(~ Beziehungsstatus, data = Daten)
Beziehungsstatus <- prop.table(Beziehungsstatus) * 100
Beziehungsstatus
Beziehungsstatus <- data.frame(Beziehungsstatus)

Beziehungsstatus$Beziehungsstatus <- factor(Beziehungsstatus$Beziehungsstatus, 
                                            levels = c("ledig / getrennt / geschieden", "in einer Beziehung", 
                                                       "verheiratet"))


BeziehungsstatusVerteilung2 <- ggplot(Beziehungsstatus, aes(x = factor(1), y = Freq, fill = Beziehungsstatus)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) + 
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), position = position_stack(vjust = 0.5), size = 3.35) +
  scale_fill_manual(values = c("ledig / getrennt / geschieden" = "#CCCC99", "in einer Beziehung" = "#999966", "verheiratet" = "#666633")) +
  guides(fill = guide_legend(ncol = 2)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )+
  ggtitle("Verteilung der Beziehungsstatus") +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Balkendiagramme/Beziehungsstatus.png"
ggsave(speicherort, plot = BeziehungsstatusVerteilung2, width = 4.5, height = 4.5 , units = "in")


###### Verteilung von Familienverantwortung ###### 
Familienverantwortung <- xtabs(~ Familienverantwortung, data = Daten)
Familienverantwortung <- prop.table(Familienverantwortung) * 100
Familienverantwortung <- data.frame(Familienverantwortung)
Familienverantwortung

Familienverantwortung$Familienverantwortung <- factor(
  Familienverantwortung$Familienverantwortung,
  levels = c(levels(Familienverantwortung$Familienverantwortung), "mit Familienverantwortung")
)

Familienverantwortung$Familienverantwortung <- factor(
  Familienverantwortung$Familienverantwortung,
  levels = c(levels(Familienverantwortung$Familienverantwortung), "ohne Familienverantwortung")
)

# Ersetze "Ja" durch "mit" und "Nein" durch "ohne"
Familienverantwortung$Familienverantwortung[Familienverantwortung$Familienverantwortung == "Ja"] <- "mit Familienverantwortung"
Familienverantwortung$Familienverantwortung[Familienverantwortung$Familienverantwortung == "Nein"] <- "ohne Familienverantwortung"


FamilienverantwortungVerteilung2 <- ggplot(Familienverantwortung, aes(x = factor(1), y = Freq, fill = Familienverantwortung)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) + 
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), position = position_stack(vjust = 0.5), size = 3.75) +
  scale_fill_manual(values = c("mit Familienverantwortung" = "#99CCFF", "ohne Familienverantwortung" = "#CCCC99")) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )+
  ggtitle("Verteilung von Familienverantwortung") +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Balkendiagramme/Familienverantwortung.png"
ggsave(speicherort, plot = FamilienverantwortungVerteilung2, width = 4.5, height = 4.5 , units = "in")


###### Verteilung von Haushaltseinkommen ###### 
Haushaltseinkommen <- xtabs(~ HaushaltseinkommenProMonat, data = Daten)
Haushaltseinkommen <- prop.table(Haushaltseinkommen) * 100
Haushaltseinkommen <- data.frame(Haushaltseinkommen)

Haushaltseinkommen$HaushaltseinkommenProMonat <- factor(Haushaltseinkommen$HaushaltseinkommenProMonat, 
                                                        levels = c("weniger als 6'000 CHF", "6'001 bis 10'000 CHF", 
                                                                   "10'001 bis 14'000 CHF", "mehr als 14'000 CHF"))


HaushaltseinkommenVerteilung2 <- ggplot(Haushaltseinkommen, aes(x = factor(1), y = Freq, fill = HaushaltseinkommenProMonat)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) + 
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3.75) +
  scale_fill_manual(values = c("weniger als 6'000 CHF" = "#999966", "6'001 bis 10'000 CHF" = "#666633", "10'001 bis 14'000 CHF" = "#999933", 
                               "mehr als 14'000 CHF" = "#CCCC66")) +
  guides(fill = guide_legend(ncol = 2)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  ggtitle("Verteilung von Haushaltseinkommen pro Monat") +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Balkendiagramme/Haushaltseinkommen.png"
ggsave(speicherort, plot = HaushaltseinkommenVerteilung2, width = 4.5, height = 4.5 , units = "in")
















###### Branchenverteilung ###### 
Branchen <- xtabs(~ Branche, data = Daten)
Branchen <- prop.table(Branchen) * 100
Branchen <- data.frame(Branchen)
View(Branchen)

Branchen$Branche <- factor(Branchen$Branche, 
                                   levels = c("Baubranche", "Finanz- und Versicherungsdienstleistung", 
                                              "Gesundheits- und Sozialwesen", "Übrige Branchen"))

Branchenverteilung <- ggplot(Branchen, aes(x = factor(1), y = Freq, fill = Branche)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) + 
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), position = position_stack(vjust = 0.5), size = 3.35) +
  scale_fill_manual(values = c("Baubranche" = "#CCCC99", "Finanz- und Versicherungsdienstleistung" = "#666633", 
                               "Gesundheits- und Sozialwesen" = "#999933", "Übrige Branchen" = "#CCCC66")) +
  guides(fill = guide_legend(ncol = 2)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggtitle("Branchenverteilung") +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Balkendiagramme/Branchenverteilung.png"
ggsave(speicherort, plot = Branchenverteilung, width = 5.2, height = 5.2 , units = "in")


###### Erfahrungsverteilung ###### 

# Erfahrung mit der Vier-Tage-Woche
xtabs(~ ErfahrungVier, data = Daten)
xtabs_ErfahrungVier <- xtabs(~ ErfahrungVier, data = Daten)
ErfahrungVierTageWoche <- prop.table(xtabs_ErfahrungVier) * 100

ErfahrungVierTageWoche

# Erfahrung mit der Jahresarbeitszeit
xtabs(~ ErfahrungJahr, data = Daten)
xtabs_ErfahrungJahr <- xtabs(~ ErfahrungJahr, data = Daten)
ErfahrungJahresarbeitszeit <- prop.table(xtabs_ErfahrungJahr) * 100

ErfahrungJahresarbeitszeit

# Erfahrung mit der Vertrauensarbeitszeit
xtabs(~ ErfahrungVertrauen, data = Daten)
xtabs_ErfahrungVertrauen <- xtabs(~ ErfahrungVertrauen, data = Daten)
ErfahrungVertrauensarbeitszeit <- prop.table(xtabs_ErfahrungVertrauen) * 100

ErfahrungVertrauensarbeitszeit

# Zusammenfassung in einen Dataframe nur % Anteil von Erfahrungen
ErfahrungFlexiblesArbeitszeitmodell <- data.frame(
  Kategorie = c("Vier-Tage-Woche", "Jahresarbeitszeit", "Vertrauensarbeitszeit"),
  Prozent = c(ErfahrungVierTageWoche[2], ErfahrungJahresarbeitszeit[1], 
              ErfahrungVertrauensarbeitszeit[2]))

print(ErfahrungFlexiblesArbeitszeitmodell)

# Prozentsatz der Teilnehmenden mit vorheriger Erfahrung in den verschiedenen Modellen

ErfahrungFlexiblesArbeitszeitmodell$Kategorie <- factor(ErfahrungFlexiblesArbeitszeitmodell$Kategorie, levels = c("Vier-Tage-Woche", "Jahresarbeitszeit", "Vertrauensarbeitszeit"))

Erfahrung <- ggplot(ErfahrungFlexiblesArbeitszeitmodell, aes(x = Kategorie, y = Prozent, fill = Kategorie)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) + 
  geom_bar(stat = "identity", position = "stack", width = 0.7, fill = "#99CCFF") +
  geom_text(aes(label = paste0(round(Prozent, 1), "%")), position = position_stack(vjust = 0.5), size = 3.75) +
  ggtitle("Erfahrungsbreite: Flexible Arbeitszeitmodelle") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x.bottom = element_blank(),
    legend.position = "none"
  ) +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Balkendiagramme/Erfahrung.png"
ggsave(speicherort, plot = Erfahrung, width = 4.5, height = 4.5, units = "in")

###### Präferenz Lohnerhöhung trotz vorheriger Erfahrung ###### 

xtabs(~ PräferenzLohnerhöhung, data = Daten)
xtabs_PräferenzLohnerhöhung <- xtabs(~ PräferenzLohnerhöhung, data = Daten)
PräferenzLohnerhöhung <- prop.table(xtabs_PräferenzLohnerhöhung) * 100
PräferenzLohnerhöhung <- data.frame(PräferenzLohnerhöhung)


PräferenzLohnerhöhung$PräferenzLohnerhöhung <- factor(
  PräferenzLohnerhöhung$PräferenzLohnerhöhung,
  levels = c(levels(PräferenzLohnerhöhung$PräferenzLohnerhöhung), "Lohnerhöhung")
)

PräferenzLohnerhöhung$PräferenzLohnerhöhung <- factor(
  PräferenzLohnerhöhung$PräferenzLohnerhöhung,
  levels = c(levels(PräferenzLohnerhöhung$PräferenzLohnerhöhung), "flexibles Arbeitszeitmodell")
)

# Ersetze "Ja" durch "Lohnerhöhung" und "Nein" durch "flexibles Arbeitszeitmodell"
PräferenzLohnerhöhung$PräferenzLohnerhöhung[PräferenzLohnerhöhung$PräferenzLohnerhöhung == "Ja"] <- "Lohnerhöhung"
PräferenzLohnerhöhung$PräferenzLohnerhöhung[PräferenzLohnerhöhung$PräferenzLohnerhöhung == "Nein"] <- "flexibles Arbeitszeitmodell"


PräferenzLohnerhöhung1 <- ggplot(PräferenzLohnerhöhung, aes(x = factor(1), y = Freq, fill = PräferenzLohnerhöhung)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) +
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), position = position_stack(vjust = 0.5), size = 3.75) +
  scale_fill_manual(values = c("Lohnerhöhung" = "#CCCC99", "flexibles Arbeitszeitmodell" = "#99CCFF")) +
  ggtitle("Vorangegangene Erfahrung:", subtitle ="Treue zur flexiblen Arbeitszeit oder Präferenz zur Lohnerhöhung") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  ) +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Balkendiagramme/PräferenzLohnerhöhung.png"
ggsave(speicherort, plot = PräferenzLohnerhöhung1, width = 4.5, height = 4.5 , units = "in")



