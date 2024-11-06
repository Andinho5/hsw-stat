## ---------------  Hauptkomponentenanalyse ---------------------
##


# benoetigte Pakete instalieren
install.packages("REdaS")  # KMO, MSA und Barlett-Test
install.packages("psych")  # Hauptkomponentenanalyse
install.packages("readr")  # Einlesen der Daten

# Daten einlesen
library(readr)
smarkt <- read.delim("smarktdat.sec", sep=" ") # alternative, wenn read nich klappt
# smarkt <- read_table2("R/Daten Hatzinger/smarkt.dat", header=TRUE)
View(smarkt)

# NAs loeschen
smd<-na.omit(smarkt[, 6:25])  # smd enthaelt die bereinigten Daten der Spalten 6-25
summary(smd)

# Spalten sinnvoll umbenennen

itemnam<-c(
  "Nichtlebensmittel",
  "offene Kassen",
  "Expresskassen",
  "Babyeinrichtungen",
  "Tankstelle",
  "Restaurant",
  "Stammkundenrabatt",
  "Parkplatz",
  "Standort",
  "Kundenservice",
  "Sonderangebote",
  "Freundlichkeit",
  "Atmosphaere",
  "Einpackhilfe",
  "Schlangen Kassen",
  "Preise",
  "Qual.Frischprod.",
  "Qual.verp.Prod.",
  "Qual.Wagen",
  "Zustellung"
)
colnames(smd)<-itemnam
View(smd)

###
#
# Prüfen, ob die Voraussetzungen erfüllt sind
#
###


# Korrelationsmatrix anzeigen
cor(smd)
# -> man kann alle Korrelationen berechnen, Tabelle ist aber sehr unuebersichtlich
# -> es existieren Korrelationen

# Barlett-Test durchfuehren
library("REdaS")
bart_spher(smd)
# Großes x2 und kleiner p-value => Hypothese, dass keine Korrelationen existieren
#                                 muss verworfen werden


# KMO angeben
kmosmd <- KMOS(smd)
print(kmosmd, stats = "KMO") # Ausgabe des KMO
print(kmosmd, stats = "MSA") # Ausgabe des MSA
print(kmosmd, stats = "MSA", sort = TRUE) # Ausgabe des MSA, sortiert
print(kmosmd, stats = "MSA", sort = TRUE, digits = 3) # Ausgabe des MSA, sortiert, gerundet
print(kmosmd) # alles wird ausgegeben


# den KMO gibt es auch in der library "psych"
library("psych")
?KMO
# ist das Kaiser-Meyer-Olkrin-Kirterium erfüllt?
KMO(smd)
# alle Werte sind ueber 0.5 -> Über alles: MSA bei 0.85
KMO(smd)$MSA

?VSS.scree
?principal



VSS.scree(smd)

# einfache Berechnung (keine Rotation)
pca.smd<-principal(smd,5,rotate="none")
pca.smd

# optimierte Berechnung (Methode VARIMAX)
pca.smdr<-principal(smd, 5)
pca.smdr$criteria <- NULL # werden diese Werte entfernt, so ist die Ausgabe etwas kürzer
pca.smdr

# bessere Ansicht
# cut=0.5 => kleine Betraege werden nicht ausgegeben
print(pca.smdr, cut=0.5, sort=TRUE, digits=2)


op<-par(mai=c(0,0,0,0))
?fa.diagram
fa.diagram(pca.smdr, cut=0.5, cex=0.8, rsize=0.5, main="") # cex <- Schriftgroeße; rsize = Rechteckgroesse
fa.diagram(pca.smdr, cut=0.5, cex=0.7, rsize=0.8, main="")
par(op)

# wie sieht es bei zwei Komponenten aus?
pca.smd2<-principal(smd, 2)
pca.smd2$criteria <- NULL
print(pca.smd2, cut=0.5, sort=TRUE, digits=2)

###
#
# Es zeigt sich, dass die Verwendung von 5 Komponenten am besten zu interpretieren ist
#
###

###
#
# Wie kann man die Daten verwenden?
#
# Kläre die Frage, ob der Qualitätsaspekt für Frauen und Männer gleich ist
#
###

pca.smd<-principal(smd, 5, scores = TRUE)
head(pca.smd$scores) # Zeigt den Komponentenwert der Person an


# lege eine neue Tabelle mit den Komponentenwerten an
smd.scores<-data.frame(pca.smd$scores)
names(smd.scores)<-c("Qual","Serv","Preis-Leis","Auto",
                     "Bequem")
View(smd.scores)
# in der ersten Spalte sind noch die originalen Nummern enthalten


# benutzes den Vektor Geschlecht aus der Ausgangstabelle
# lege einen Vektor an
SEX<-smarkt$"sex"
idx<-as.numeric(rownames(smd.scores)) # der index wird zur richtigen Zuordnung benötigt
SEX<-SEX[idx]  # nur die in der PCA betrachteten Personen nutzen
View(SEX)

smd.scores<-data.frame(smd.scores,SEX) # Zusammensetzten zu einer Tabelle
head(smd.scores)

boxplot(Qual~SEX, data=smd.scores) # schnelle optische Übersicht

describeBy(smd.scores$Qual, SEX, skew=FALSE) # numerische Darstellung der Box

#  Descriptive statistics by group
# group: männlich
# vars   n  mean   sd   min  max range  se
# X1    1 120 -0.19 1.06 -3.61 1.75  5.37 0.1
# ------------------------------------------------------------------
#   group: weiblich
# vars   n mean   sd   min  max range   se
# X1    1 371 0.06 0.97 -3.73 2.08  5.81 0.05

wilcox.test(Qual~SEX, data=smd.scores) # testet, ob sich zwei Werte unterscheiden. Hypothese: die Werte sind gleich

# Hypothese muss verworfen werden, da p<0.05


detach(package:psych)
detach(package:REdaS)
