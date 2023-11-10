############### REGRESSIONSANALYSE ############### 
library(arm)
library(ggplot2)
library(stargazer)
library(tidyr)
library(broom)
library(dplyr)
library(data.table)
loadfonts()

#Datenimport
Daten1 <- read.csv("/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/Daten_bereinigt.csv")


############# Variablen: numerisch oder kategorial #############
Daten1$VierTageWoche <- as.numeric(Daten1$VierTageWoche)
Daten1$Jahresarbeitszeit <- as.numeric(Daten1$Jahresarbeitszeit)
Daten1$Vertrauensarbeitszeit <- as.numeric(Daten1$Vertrauensarbeitszeit)
Daten1$Geschlecht <- as.factor(Daten1$Geschlecht)
Daten1$Alterskategorie <- as.factor(Daten1$Alterskategorie)
Daten1$Beziehungsstatus <- as.factor(Daten1$Beziehungsstatus)
Daten1$Familienverantwortung <- as.factor(Daten1$Familienverantwortung)
Daten1$HaushaltseinkommenProMonat <- as.factor(Daten1$HaushaltseinkommenProMonat)
Daten1$Branche <- as.factor(Daten1$Branche)
Daten1$ErfahrungVier <- as.factor(Daten1$ErfahrungVier)
Daten1$ErfahrungJahr <- as.factor(Daten1$ErfahrungJahr)
Daten1$ErfahrungVertrauen <- as.factor(Daten1$ErfahrungVertrauen)

############# Modell Vier-Tage-Woche #############
modelVier <- glm(VierTageWoche ~ Geschlecht + Alterskategorie + Beziehungsstatus + 
                   Familienverantwortung + HaushaltseinkommenProMonat + Branche, 
                 data = Daten1, family = binomial())
summary(modelVier)

############# Tabelle Vier-Tage-Woche ############# 
exp_coefs_VIER <- exp(coef(modelVier))


Tabelle3 <- tidy(modelVier)

colnames(Tabelle3) <- c("Variable", "Schätzer (B)", "Standardfehler", "z-Wert", "p-Wert")
Tabelle3[Tabelle3$Variable == "(Intercept)",]$Variable <- "Konstante"

Tabelle_exp_coefs <- data.frame(exp_coefs_VIER)
Tabelle3$exp_coefs_VIER <- Tabelle_exp_coefs$exp_coefs_VIER

Tabelle3 <- Tabelle3 %>%
  mutate(across(c(`Schätzer (B)`, exp_coefs_VIER, Standardfehler, `z-Wert`, `p-Wert`), ~round(., 3)))

Tabelle3 <- dplyr::select(Tabelle3, Variable, `Schätzer (B)`, exp_coefs_VIER, Standardfehler, `z-Wert`, `p-Wert`)

stargazer(Tabelle3, 
          title = "Regression 1",
          align = TRUE,
          summary = FALSE,
          out = "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Tabellen/Table 3.html")

stargazer(modelVier, 
          title = "Regression 1",
          align = TRUE,
          summary = FALSE,
          out = "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Tabellen/Table 3.1.html")


############# Plot  Vier-Tage-Woche ############# 
# Vorhersagen
predicted_probs_Vier <- modelVier$fitted.values
predicted_probs_Vier


# Daten Plot
VIERscatter_data <- data.frame(
  Observed = Daten1$VierTageWoche,
  Predicted = predicted_probs_Vier
)


# Plot erstellen
PlotVier <- ggplot(VIERscatter_data, aes(x = Predicted, y = Observed)) +
  theme_light()+
  geom_point(alpha =2, shape = 20, stroke = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  labs(x = "Vorhergesagte Wahrscheinlichkeit: Präferenz für Vier-Tage-Woche", y = "Beobachtete Präferenzen") +
  ggtitle("Regressionsmodell: Vier-Tage-Woche")  +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/PlotVierTageWoche.png"
ggsave(speicherort, plot = PlotVier, width = 6, height = 6 , units = "in")


############# Modell Jahresarbeitszeit #############

modelJahr <- glm(Jahresarbeitszeit ~ Geschlecht + Alterskategorie + Beziehungsstatus + 
                   Familienverantwortung + HaushaltseinkommenProMonat + Branche, 
                 data = Daten1, family = binomial())
summary(modelJahr)




############# Tabelle  Jahresarbeitszeit ############# 
exp_coefs_JAHR <- exp(coef(modelJahr))


Tabelle4 <- tidy(modelJahr)

colnames(Tabelle4) <- c("Variable", "Schätzer (B)", "Standardfehler", "z-Wert", "p-Wert")
Tabelle4[Tabelle4$Variable == "(Intercept)",]$Variable <- "Konstante"

Tabelle_exp_coefs_Jahr <- data.frame(exp_coefs_JAHR)
Tabelle4$exp_coefs <- Tabelle_exp_coefs_Jahr$exp_coefs_JAHR

Tabelle4 <- dplyr::select(Tabelle4, Variable, `Schätzer (B)`, exp_coefs, Standardfehler, `z-Wert`, `p-Wert`)

Tabelle4 <- Tabelle4 %>%
  mutate(across(c(`Schätzer (B)`, exp_coefs, Standardfehler, `z-Wert`, `p-Wert`), ~round(., 3)))

stargazer(Tabelle4, 
          title = "Regression 2",
          align = TRUE,
          summary = FALSE,
          out = "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Tabellen/Table 4.html")




############# Plot Jahresarbeitszeit ############# 
# Vorhersagen
predicted_probs_Jahr <- modelJahr$fitted.values
predicted_probs_Jahr

# Daten für den Plot 
JAHRscatter_data <- data.frame(
  Observed = Daten1$Jahresarbeitszeit,
  Predicted = predicted_probs_Jahr
)


# Plot erstellen
PlotJahr <- ggplot(JAHRscatter_data, aes(x = Predicted, y = Observed)) +
  theme_light()+
  geom_point(alpha =2, shape = 20, stroke = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  labs(x = "Vorhergesagte Wahrscheinlichkeit: Präferenz für Jahresarbeitszeit", y = "Beobachtete Präferenzen") +
  ggtitle("Regressionsmodell: Jahresarbeitszeit")  +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )


speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/PlotJahresarbeitszeit.png"
ggsave(speicherort, plot = PlotJahr, width = 6, height = 6 , units = "in")









############# Modell Vertrauensarbeitszeit #############
modelVertrauen <- glm(Vertrauensarbeitszeit ~ Geschlecht + Alterskategorie + Beziehungsstatus + 
                        Familienverantwortung + HaushaltseinkommenProMonat + 
                        Branche, data = Daten1, family = binomial())
summary(modelVertrauen)

############# Tabelle  Vertrauensarbeitszeit ############# 
exp_coefs_Ver <- exp(coef(modelVertrauen))


Tabelle5 <- tidy(modelVertrauen)

colnames(Tabelle5) <- c("Variable", "Schätzer (B)", "Standardfehler", "z-Wert", "p-Wert")
Tabelle5[Tabelle5$Variable == "(Intercept)",]$Variable <- "Konstante"

Tabelle_exp_coefs_Ver <- data.frame(exp_coefs_Ver)
Tabelle5$exp_coefs <- Tabelle_exp_coefs_Ver$exp_coefs_Ver

Tabelle5 <- dplyr::select(Tabelle5, Variable, `Schätzer (B)`, exp_coefs, Standardfehler, `z-Wert`, `p-Wert`)

Tabelle5 <- Tabelle5 %>%
  mutate(across(c(`Schätzer (B)`, exp_coefs, Standardfehler, `z-Wert`, `p-Wert`), ~round(., 3)))

stargazer(Tabelle5, 
          title = "Regression 3",
          align = TRUE,
          summary = FALSE,
          out = "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Tabellen/Table 5.html")

stargazer(modelVertrauen, 
          title = "Regression 3",
          align = TRUE,
          summary = FALSE,
          out = "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Tabellen/Table 5.1.html")



############# Plot Vertrauensarbeitszeit ############# 
# Vorhersagen
predicted_probs_Vertrauen <- modelVertrauen$fitted.values
predicted_probs_Vertrauen

# Daten für den Plot
VERTRAUEN_data <- data.frame(
  Observed = Daten1$Vertrauensarbeitszeit,
  Predicted = predicted_probs_Vertrauen
)

# Plot erstellen
library(ggplot2)

PlotVertrauen <- ggplot(VERTRAUEN_data, aes(x = predicted_probs_Vertrauen, y = Observed)) +
  theme_light() +
  geom_point(alpha =1, shape = 20, stroke = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  labs(x = "Vorhergesagte Wahrscheinlichkeit: Präferenz für Vertrauensarbeitszeit", y = "Beobachtete Präferenzen") +
  ggtitle("Regressionsmodell: Vertrauensarbeitszeit") +
  theme(
    text = element_text(family = "Times New Roman", size = 12))

speicherort <- "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/PlotVertrauensarbeitszeit.png"
ggsave(speicherort, plot = PlotVertrauen, width = 6, height = 6 , units = "in")


############# Omnibus-Tests ############# 
# Vier-Tage-Woche
modelchiVIER <- modelVier$null.deviance - modelVier$deviance
modelchiVIER

chidfVIER <- modelVier$df.null - modelVier$df.residual
chidfVIER

chisqpVIER <- 1-pchisq(modelchiVIER, chidfVIER)
chisqpVIER

#Jahresarbeitszeit 
modelchiJAHR <- modelJahr$null.deviance - modelJahr$deviance
modelchiJAHR

chidfJAHR <- modelJahr$df.null - modelJahr$df.residual
chidfJAHR

chisqpJAHR <- 1-pchisq(modelchiJAHR, chidfJAHR)
chisqpJAHR

#Vertrauensarbeitszeit
modelchiVERT <- modelVertrauen$null.deviance - modelVertrauen$deviance
modelchiVERT

chidfVERT <- modelVertrauen$df.null - modelVertrauen$df.residual
chidfVERT

chisqpVERT <- 1-pchisq(modelchiVERT, chidfVERT)
chisqpVERT


# Tabelle Chi-Quadrat Test der Modell
Tabelle2 <- data.frame(
  Modell = c("Vier-Tage-Woche", "Jahresarbeitszeit", "Vertrauensarbeitszeit"),
  "Chi-Quadrat" = c(modelchiVIER, modelchiJAHR, modelchiVERT),
  "Freiheitsgrade" = c(chidfVIER, chidfJAHR, chidfVERT),
  "p-Wert" = c(chisqpVIER, chisqpJAHR, chisqpVERT)
)

stargazer(Tabelle2, 
          title = "Chi-Quadrat Test der Modelle",
          align = TRUE,
          summary = FALSE,
          out = "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Tabellen/Table 2.html")


############# Modell 1 Vier-Tage-Woche und Modell 3 Vertrauensarbeitszeit #############
stargazer(modelVier, modelVertrauen, title="Regressionsergebnisse", align=TRUE,
          out = "/Users/elenabalzli/Documents/!!! Bachelorarbeit 22:23/Datenanalyse/nach Bereinigung/Tabellen/Table 6.html")



xtabs(~ VierTageWoche + Branche, data = Daten1)
xtabs(~ Vertrauensarbeitszeit + Branche, data = Daten1)

xtabs(~ VierTageWoche + Beziehungsstatus, data = Daten1)
xtabs(~ Vertrauensarbeitszeit + Beziehungsstatus, data = Daten1)

xtabs(~ VierTageWoche + Alterskategorie, data = Daten1)
xtabs(~ Vertrauensarbeitszeit + Alterskategorie, data = Daten1)

xtabs(~ VierTageWoche + HaushaltseinkommenProMonat, data = Daten1)
xtabs(~ Vertrauensarbeitszeit + HaushaltseinkommenProMonat, data = Daten1)

xtabs(~ VierTageWoche + Familienverantwortung, data = Daten1)
xtabs(~ Vertrauensarbeitszeit + Familienverantwortung, data = Daten1)
