############### BALKENDIAGRAMME ############### 

library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)
install.packages("png")
library(png)
library(readxl)
library(dplyr)
install.packages("extrafont")
library(extrafont)
font_import(pattern = "Times")
loadfonts()



###### Datenimport ###### 
Daten <- read_excel("/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Rohdaten.xlsx")

View(Daten)

nrow(Daten)  # -> 128 Beobachtungen

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

rownames(dataframe_Total)[2] <- "Jahresarbeitszeit"

print(dataframe_Total)

data_long <- tidyr::gather(data = dataframe_Total, key = "Anteil", value = "Prozent", -Kategorie)
data_long$Kategorie <- factor(data_long$Kategorie, levels = c("Vier-Tage-Woche", "Jahresarbeitszeit", "Vertrauensarbeitszeit"))


FlexibleArbeitszeitvs.Lohnerhöhung1 <- ggplot(data_long, aes(x = Kategorie, y = Prozent, fill = Anteil)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = paste0(round(Prozent, 1), "%")), position = position_stack(vjust = 0.5), size = 3.75) +
  ggtitle("Flexible Arbeitszeitmodelle vs. eine Lohnerhöhung, Überblick Ergebnisse", ) +
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
    title = element_text(size = 8),
    text = element_text(family = "Times New Roman", size = 12))


speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Balkendiagramme/FlexibleArbeitszeitvs.Lohnerhöhung.png"
ggsave(speicherort, plot = FlexibleArbeitszeitvs.Lohnerhöhung1, width = 4.5, height = 4.5, units = "in")



###### Geschlechterverteilung: Prozentualer Anteil von Frauen und Männern in der Stichprobe ###### 
xtabs(~ Geschlecht, data = Daten)
Geschlecht <- xtabs(~ Geschlecht, data = Daten)
Geschlecht <- prop.table(Geschlecht) * 100
Geschlecht <- data.frame(Geschlecht)

Geschlechterverteilung1 <- ggplot(Geschlecht, aes(x = factor(1), y = Freq, fill = Geschlecht)) +
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
    axis.title.x = element_blank(),
    text = element_text(family = "Times New Roman", size = 12)
  )+
  ggtitle("Geschlechterverteilung")

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Balkendiagramme/Geschlechterverteilung.png"
ggsave(speicherort, plot = Geschlechterverteilung1, width = 4.5, height = 4.5 , units = "in")



###### Altersverteilung: Anteil der Stichprobe in unterschiedlichen Alterskategorien ###### 
Alterskategorie <- xtabs(~ Alterskategorie, data = Daten)
Alterskategorie <- prop.table(Alterskategorie) * 100
Alterskategorie <- data.frame(Alterskategorie)

Alterskategorie$Alterskategorie <- factor(Alterskategorie$Alterskategorie, 
                                          levels = c("jünger als 20 Jahre", "20 - 29 Jahre", 
                                                     "30 - 39 Jahre", "40 - 49 Jahre", "50 - 65 Jahre", "älter als 65 Jahre"))

Altersverteilung1 <- ggplot(Alterskategorie, aes(x = factor(1), y = Freq, fill = Alterskategorie)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) + 
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), position = position_stack(vjust = 0.75), size = 3.75) +
  scale_fill_manual(values = c("jünger als 20 Jahre" = "#CCCC99", "20 - 29 Jahre" = "#999966", "30 - 39 Jahre" = "#666633", 
                               "40 - 49 Jahre" = "#999933", "50 - 65 Jahre" = "#CCCC66", "älter als 65 Jahre" = "#FFFF99")) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
    ) +
      ggtitle("Verteilung der Alterskategorien") +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )
  

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Balkendiagramme/Altersverteilung.png"
ggsave(speicherort, plot = Altersverteilung1, width = 4.5, height = 4.5 , units = "in")


###### Verteilung nach Beziehungsstatus ###### 
Beziehungsstatus <- xtabs(~ Beziehungsstatus, data = Daten)
Beziehungsstatus <- prop.table(Beziehungsstatus) * 100
Beziehungsstatus
Beziehungsstatus <- data.frame(Beziehungsstatus)

Beziehungsstatus$Beziehungsstatus <- factor(Beziehungsstatus$Beziehungsstatus, 
                                          levels = c("ledig", "in einer Beziehung", 
                                                     "verheiratet", "getrennt", "geschieden", "verwitwet"))


BeziehungsstatusVerteilung1 <- ggplot(Beziehungsstatus, aes(x = factor(1), y = Freq, fill = Beziehungsstatus)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) + 
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(data = subset(Beziehungsstatus, Beziehungsstatus %in% c("ledig")),
            aes(x = factor(1), label = paste0(round(Freq, 1), "%")),
            position = position_stack(vjust = 5.9), size = 3.35) +
  geom_text(data = subset(Beziehungsstatus, Beziehungsstatus %in% c("in einer Beziehung")),
            aes(x = factor(1), label = paste0(round(Freq, 1), "%")),
            position = position_stack(vjust = 1.9), size = 3.35) +
  geom_text(data = subset(Beziehungsstatus, Beziehungsstatus %in% c("verheiratet")),
            aes(x = factor(1), label = paste0(round(Freq, 1), "%")),
            position = position_stack(vjust = 0.65), size = 3.35) +
  geom_text(data = subset(Beziehungsstatus, Beziehungsstatus %in% c("getrennt")),
             aes(x = factor(1), label = paste0(round(Freq, 1), "%")),
             position = position_stack(vjust = 3), hjust = 1.5, size = 3.35) +
  geom_text(data = subset(Beziehungsstatus, Beziehungsstatus %in% c("geschieden")),
            aes(x = factor(1), label = paste0(round(Freq, 1), "%")),
            position = position_stack(vjust = 0.75), hjust = 0.5, size = 3.35) +
  geom_text(data = subset(Beziehungsstatus, Beziehungsstatus %in% c("verwitwet")),
            aes(x = factor(1), label = paste0(round(Freq, 1), "%")),
            position = position_stack(vjust = 1.75), hjust = -0.75, size = 3.35) +
  scale_fill_manual(values = c("ledig" = "#CCCC99", "in einer Beziehung" = "#999966", "verheiratet" = "#666633", 
                               "getrennt" = "#999933", "geschieden" = "#CCCC66", "verwitwet" = "#FFFF99")) +
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


speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Balkendiagramme/Beziehungsstatus.png"
ggsave(speicherort, plot = BeziehungsstatusVerteilung1, width = 4.5, height = 4.5 , units = "in")


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


FamilienverantwortungVerteilung1 <- ggplot(Familienverantwortung, aes(x = factor(1), y = Freq, fill = Familienverantwortung)) +
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
  ggtitle("Verteilung von Familienverantwortung")+
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Balkendiagramme/Familienverantwortung.png"
ggsave(speicherort, plot = FamilienverantwortungVerteilung1, width = 4.5, height = 4.5 , units = "in")


###### Verteilung von Haushaltseinkommen ###### 
Haushaltseinkommen <- xtabs(~ HaushaltseinkommenProMonat, data = Daten)
Haushaltseinkommen <- prop.table(Haushaltseinkommen) * 100
Haushaltseinkommen <- data.frame(Haushaltseinkommen)

Haushaltseinkommen$HaushaltseinkommenProMonat <- factor(Haushaltseinkommen$HaushaltseinkommenProMonat, 
                                            levels = c("weniger als 2'000 CHF", "2'001 bis 6'000 CHF", 
                                                       "6'001 bis 10'000 CHF", "10'001 bis 14'000 CHF", "14'001 bis 18'000 CHF", "mehr als 18'000 CHF"))

HaushaltseinkommenVerteilung1 <- ggplot(Haushaltseinkommen, aes(x = factor(1), y = Freq, fill = HaushaltseinkommenProMonat)) +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) +
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), position = position_stack(vjust = 0.5), size = 3.75) +
  scale_fill_manual(values = c("weniger als 2'000 CHF" = "#CCCC99", "2'001 bis 6'000 CHF" = "#999966", "6'001 bis 10'000 CHF" = "#666633", 
                               "10'001 bis 14'000 CHF" = "#999933", "14'001 bis 18'000 CHF" = "#CCCC66", "mehr als 18'000 CHF" = "#FFFF99")) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())+
  ggtitle("Verteilung von Haushaltseinkommen pro Monat")+
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Balkendiagramme/Haushaltseinkommen.png"
ggsave(speicherort, plot = HaushaltseinkommenVerteilung1, width = 5.3, height = 5.3 , units = "in")

