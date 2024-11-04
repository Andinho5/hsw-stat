library("haven")
library("stats")

za_main <- read_sav("ZA7716_main_v1-0-0.sav")
za_main_sub <- za_main[, c("t6", "t2", "t45", "t80", "t40", "t70", "t50")]
colnames(za_main_sub) <- c("DemSatis", "Alter", "FreqPolTalk", "IntUsage", "IntUsagePol", "haushaltSal", "haushaltSize")
za_main_sub$Alter <- gsub("1959 und frueher", "1959", za_main_sub$Alter)
za_main_sub$Alter <- as.integer(format(Sys.Date(), "%Y")) - as.integer(za_main_sub$Alter)
za_main_sub[] <- lapply(za_main_sub, as.integer)
#print(cov(za_main_sub))
#print(eigen(cov(za_main_sub)))
#View(za_main_sub)
za_main_sub <- na.omit(za_main_sub)
za_main_scaled <- scale(za_main_sub)

pca_result <- prcomp(za_main_scaled, center = TRUE, scale. = TRUE)
print(summary(pca_result))
print(pca_result$rotation)

screeplot(pca_result, type = "lines", main = "Scree Plot")
biplot(pca_result, xlim = c(-0.025, 0.025), ylim = c(-0.025, 0.025))

plot(pca_result$x[, 1:2],
     xlab = "Hauptkomponente 1",
     ylab = "Hauptkomponente 2",
     main = "Scatter Plot der PCA",
     pch = 19,
     #xlim = c(-2, 2), ylim = c(-1, 1),
     col = "blue")

# View(za_main)