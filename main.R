library("haven")
library("stats")

za_main <- read_sav("ZA7716_main_v1-0-0.sav")
za_main_sub <- za_main[, c("t6", "t45", "t40", "t145", "t16", "t5", "t15b", "t15a", "t15g", "t15k", "t15e", "t15l", "t15c", "t15m")]
colnames(za_main_sub) <- c("DemSatis", "FreqPolTalk", "IntUsagePol", "WirtLageReg", "SatisRegierung", "PolInterest", "ScholzZuf", "MerzZuf", "HabeckZuf", "BaerbockZuf", "LindnerZuf", "WeidelZuf", "SoederZuf", "WagenknechtZuf")
za_main_sub[] <- lapply(za_main_sub, as.integer)
invalid_values <- c(-99, -98, -97, -72, -71, -83, -84, -93, -99)

za_main_sub[sapply(za_main_sub, is.numeric)] <- lapply(za_main_sub[sapply(za_main_sub, is.numeric)], function(x) {
  x[x %in% invalid_values] <- NA
  return(x)
})
za_main_sub <- na.omit(za_main_sub)
za_main_scaled <- scale(za_main_sub)

library("REdaS")
print(bart_spher(za_main_scaled))
print(KMOS(za_main_scaled))

# optimierte Berechnung (Methode VARIMAX)
pca.smdr<-principal(za_main_scaled, 4)
pca.smdr$criteria <- NULL # werden diese Werte entfernt, so ist die Ausgabe etwas kürzer
print(pca.smdr, cut=0.5, sort=TRUE, digits=2)

library("psych")
op<-par(mai=c(0,0,0,0))
fa.diagram(pca.smdr, cut=0.5, cex=0.8, rsize=0.5, main="", simple = TRUE) # cex <- Schriftgroeße; rsize = Rechteckgroesse
fa.diagram(pca.smdr, cut=0.5, cex=0.7, rsize=0.8, main="")
par(op)

library(factoextra)
fviz_pca_var(pca_result,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

screeplot(pca_result, type = "lines", main = "Scree Plot")
biplot(pca_result, xlim = c(-0.025, 0.025), ylim = c(-0.025, 0.025))


print(pca.smdr$scores)
pca.scores <- data.frame(pca.smdr$scores)
names(pca.scores) <- c("ZufWirtPol", "KonLibPol", "PolEng", "PopPol")
head(pca.scores)
print(pca.scores)

# boxplot(ZufWirtPol~KonLibPol, data = pca.scores)
# describeBy(pca.scores$ZufWirtPol, pca.scores$KonLibPol, skew = FALSE)

sex <- za_main$t1
pca.scores <- data.frame(pca.scores, sex)

boxplot()

plot(pca.smdr$x[, 1:2],
     xlab = "Hauptkomponente 1",
     ylab = "Hauptkomponente 2",
     main = "Scatter Plot der PCA",
     pch = 19,
     #xlim = c(-2, 2), ylim = c(-1, 1),
     col = "blue")



# View(za_main)