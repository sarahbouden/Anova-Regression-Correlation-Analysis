#phase 1:Analyse statistique et graphique d'un série temporelle:
install.packages("readxl")
library(readxl)
install.packages("tseries")
library(tseries)
library(strucchange)
library(forecast)

#importation des données
data<- read_excel(file.choose(), skip = 10)
names(data)
#Conversion des dates en format Date
data$observation_date <- as.Date(data$observation_date)
View(data)
# Afficher les premières lignes
head(data)
tail(data)
summary(data)
str(data)

# Créer une série temporelle avec fréquence mensuelle
data_ts<- ts(data$IPB53122N,start = c(2009, 1),  end = c(2024, 3), frequency = 12)
#vérifier qu'elle s'agit bien d'une série temporelle 
class(data_ts)
# Tracer la série
plot(data_ts, main = "Production industrielle - électronique", ylab = "Production", xlab = "Année") #Pour explorer rapidement toutes les variables
#interprétation
#la série temporelle de production industrielle électronique présente :

#Une tendance croissante forte :  une évolution générale à long terme(hausse continue depuis 2009),

#absence composante siasonniere :fluctuations régulières liées aux cycles mensuels,

#Une composante aléatoire ou aléatoire :variations imprévisibles autour de la tendance.
acf(data_ts)
#interprétation
#******La série admet une tendance(décroissance lent)
#*****la série ne présente pas une composante saisonniére.
#  Analyse PACF 
Pacf(data_ts, lag.max = 36)



#Moyenne
mean(data_ts)
#Écart-type
sd(data_ts)
#Variance
var(data_ts)
#Les résultats indiquent une volatilité notable et suggèrent que la série pourrait ne pas être stationnaire

decomposed <- decompose(data_ts)
plot(decomposed)


######modèle additif
# Décomposition avec le modèle additif
model_additive <- decompose(data_ts, type = "additive")

# Affichage de la décomposition (tendance, saisonnalité, résidus)
plot(model_additive)

# Extraire la composante aléatoire (résidus)
aleatoire_additive <- model_additive$random

# Vérification de l'autocorrélation des résidus aléatoires
acf(aleatoire_additive, na.action = na.pass)
Box.test(aleatoire_additive, type = "Ljung-Box")


######modèle multiplicatif
# Décomposition avec le modèle multiplicatif
model_multiplicative <- decompose(data_ts, type = "multiplicative")

# Affichage de la décomposition (tendance, saisonnalité, résidus)
plot(model_multiplicative)

# Extraire la composante aléatoire (résidus)
aleatoire_multiplicative <- model_multiplicative$random

# Vérification de l'autocorrélation des résidus aléatoires
acf(aleatoire_multiplicative, na.action = na.pass)

Box.test(aleatoire_multiplicative, type = "Ljung-Box")  # Si p-value > 0.05, validation

#donc on choisi de travailler avec le modèle multiplicatif car :
  
  #****Résidus mieux comportés (ACF plus proche de l'aléatoire pur).

  #***Adapté aux séries croissantes avec saisonnalité variable
acf(data_ts,plot = F)
#alors ACF pointe vers une série non stationnaire avec tendance


lag.plot(data_ts,lags=12)
lag.plot(data_ts,lags=24)
lag.plot(data_ts,lags=36)


# Test de stationnarité avec Augmented Dickey-Fuller Test (ADF)
adf.test(data_ts)

#Le test ADF est utilisé pour vérifier si une série est stationnaire. Il se base sur les hypothèses suivantes :
  
  #H₀ (hypothèse nulle) : La série a une racine unitaire → elle n'est pas stationnaire.

  #H₁ (hypothèse alternative) : La série est stationnaire.

#Ici, p-value > 0.05, donc on ne rejette pas l’hypothèse nulle(la série est non sationnaire)



#data2 :Job Openings Construction:

#importation des données
data2Job<- read_excel(file.choose(), skip = 10)
names(data2Job)
#Conversion des dates en format Date
data2Job$observation_date <- as.Date(data2Job$observation_date)
View(data2Job)
# Afficher les premières lignes
head(data2Job)
tail(data2Job)
summary(data2Job)
str(data2Job)


# Créer une série temporelle avec fréquence mensuelle
data2Job_ts<- ts(data2Job$JTU2300JOL,start = c(2004, 1),  end = c(2024, 3), frequency = 12)
#vérifier qu'elle s'agit bien d'une série temporelle 
class(data2Job_ts)
# Tracer la série
plot(data2Job_ts, main = "Job Openings Construction", ylab = "Construction", xlab = "Année") #Pour explorer rapidement toutes les variables
#interprétation
#la série temporelle de production industrielle électronique présente :

#Une tendance croissante forte :  une évolution générale à long terme(hausse continue depuis 2009),

#absence d'une composante siasonniere,

#Une composante aléatoire ou aléatoire :variations imprévisibles autour de la tendance.
acf(data2Job_ts)
#interprétation
#******La série admet une tendance(décroissance lent)
#*****la série ne présente pas une composante saisonniére.
#  Analyse PACF 
Pacf(data2Job_ts, lag.max = 36)



#Moyenne
mean(data2Job_ts)
#Écart-type
sd(data2Job_ts)
#Variance
var(data2Job_ts)
#Les résultats indiquent une volatilité notable et suggèrent que la série pourrait ne pas être stationnaire

decomposedJob <- decompose(data2Job_ts)
plot(decomposedJob)



acf(data2Job_ts,plot = F)
#alors ACF pointe vers une série non stationnaire avec tendance


lag.plot(data2Job_ts,lags=12)
lag.plot(data2Job_ts,lags=24)
lag.plot(data2Job_ts,lags=36)

######modèle additif
# Décomposition avec le modèle additif
model_additive2 <- decompose(data2Job_ts, type = "additive")

# Affichage de la décomposition (tendance, saisonnalité, résidus)
plot(model_additive2)

# Extraire la composante aléatoire (résidus)
aleatoire_additive2 <- model_additive2$random

# Vérification de l'autocorrélation des résidus aléatoires
acf(aleatoire_additive2, na.action = na.pass)
Box.test(aleatoire_additive2, type = "Ljung-Box")


######modèle multiplicatif
# Décomposition avec le modèle multiplicatif
model_multiplicative2 <- decompose(data2Job_ts, type = "multiplicative")

# Affichage de la décomposition (tendance, saisonnalité, résidus)
plot(model_multiplicative2)

# Extraire la composante aléatoire (résidus)
aleatoire_multiplicative2 <- model_multiplicative2$random

# Vérification de l'autocorrélation des résidus aléatoires
acf(aleatoire_multiplicative2, na.action = na.pass)

Box.test(aleatoire_multiplicative2, type = "Ljung-Box") # Si p-value > 0.05, validation

plot(na.omit(aleatoire_additive2), type = "l", col = "blue", ylab = "Résidus")  
lines(na.omit(aleatoire_multiplicative2), col = "red")  
legend("topright", legend = c("Additif", "Multiplicatif"), col = c("blue", "red"), lty = 1) 
#✅ Le modèle multiplicatif est optimal pour cette série temporelle car :

#Capture mieux la dynamique des données :

#Saisonnalité proportionnelle à la tendance (typique des séries industrielles).

#Résidus validés statistiquement et graphiquement :

#p-value > 0.05 + ACF propre + variance stable.

#Supériorité visuelle :

#Résidus multiplicatifs (rouge) nettement plus aléatoires que les résidus additifs (bleu).



#donc on choisi de travailler avec le modèle multiplicatif car :

#****Résidus mieux comportés (ACF plus proche de l'aléatoire pur).

#***Adapté aux séries croissantes avec saisonnalité variable


# Test de stationnarité avec Augmented Dickey-Fuller Test (ADF)
adf.test(data2Job_ts)

#Le test ADF est utilisé pour vérifier si une série est stationnaire. Il se base sur les hypothèses suivantes :

#H₀ (hypothèse nulle) : La série a une racine unitaire → elle n'est pas stationnaire.

#H₁ (hypothèse alternative) : La série est stationnaire.

#Ici, p-value > 0.05, donc on ne rejette pas l’hypothèse nulle(la série est non sationnaire)


###auto-correlation croiser 
# Calcul de l'autocorrélation croisée
ccf(data2Job_ts, data_ts, lag.max = 48, main = "Autocorrélation Croisée entre Serie1 et Serie2")

ccf(data2Job_ts, data_ts, lag.max = 48, plot=F)



#phase2 :Ajustement de la série temporelle 
#Data 1:
#Modèles pour l'Ajustement de la Tendance
#Calcul des moyennes mobiles avec différents ordres (k)
m4<-filter(data_ts,rep(1/4,4),method = c("convolution") ,sides = 2)
plot(data_ts)
lines(m4,col="blue")

m5<-filter(data_ts,rep(1/5,5),method = c("convolution") ,sides = 2)
plot(data_ts)
lines(m5,col="blue")

m7<-filter(data_ts,rep(1/12,12),method = c("convolution") ,sides = 2)
plot(data_ts)
lines(m7,col="blue")

#a) Régression Linéaire Simple
# 1. Créer un dataframe aligné
df <- data.frame(
  Y = as.numeric(data_ts),  # Extraction des valeurs
  t = time(data_ts)         # Extraction des dates
)

model_lin <- lm(data_ts ~ t, data = df)
model_lin
summary(model_lin)
resid(model_lin)
plot(resid(model_lin))
acf(resid(model_lin))

# Version simplifiée avec plot() de base
plot(df$t, df$Y, 
     type = "l",  # 'l' pour lignes
     main = "Régression Linéaire Simplifiée",
     xlab = "Temps",
     ylab = "Valeurs",
     col = "black")

# Ajout de la droite de régression
abline(model_lin, col = "red", lwd = 2)

# Légende
legend("topleft", 
       legend = c("Données", "Tendance linéaire"),
       col = c("black", "red"),
       lty = 1,
       lwd = c(1, 2))

#b) Régression Polynomiale
model_quad <- lm(data_ts ~ t + I(t^2), data = df)
model_quad
summary(model_quad)
resid(model_quad)
plot(resid(model_quad))
acf(resid(model_quad))

#comparaison 
AIC(model_lin)
AIC(model_quad)
#===> alors le modele quadratique est le meilleur modele car il posséde le AIC le plus faible.
#2. Modèles pour la Composante Saisonnière 
#a) Harmoniques (Séries de Fourier)
#Modèle sinusoïdal 
# 1. Préparation des données
t <- 1:length(data_ts)  # Vecteur temps numérique
p <- 12  # Période saisonnière (12 pour du mensuel)

# 2. Création des matrices cos/sin (6 harmoniques comme dans votre code)
MC <- matrix(0, nrow = length(data_ts), ncol = 6)
MS <- matrix(0, nrow = length(data_ts), ncol = 6)

for (i in 1:6) {
  MC[, i] <- cos(2 * pi * i * t / p)  # Termes cosinus
  MS[, i] <- sin(2 * pi * i * t / p)  # Termes sinus
}
#  Ajustement du modèle 
model <- lm(data_ts~ t + MC +MS)

summary(model)

# 1. Réajuster le modèle en excluant les termes non significatifs
model_optimise <- lm(data_ts ~ t 
                     + MC4  # Seul terme MC significatif
                     + MS1  # Seul terme MS significatif
)


donnees <- data.frame(
  Y = data_ts,        # Votre série temporelle
  t = 1:length(data_ts),  # Variable temps
  MC1 = MC[,1], MC2 = MC[,2], MC3 = MC[,3], 
  MC4 = MC[,4], MC5 = MC[,5], MC6 = MC[,6],
  MS1 = MS[,1], MS2 = MS[,2], MS3 = MS[,3],
  MS4 = MS[,4], MS5 = MS[,5], MS6 = MS[,6]
)


model_optimise <- lm(Y ~ t + MC4 + MS1, data = donnees)

# 3. Vérification
summary(model_optimise)

#3. Modèles pour les Résidus
# Appliquer une différenciation d'ordre 1
production_industrielle_diff <- diff(data_ts)
# Afficher la série différenciée
plot(production_industrielle_diff, main = "Série différenciée d'ordre 1")
adf.test(diff_data)
adf.test(production_industrielle_diff)
#Le p-value est maintenant de 0.01 < 0.05
#alors la série différenciée est maintenant stationnaire
# Ajustement AR(1) sur la série DIFFÉRENCIÉE (équivalent à ARIMA(1,1,0) sur l'originale)

arima_model_diff<- auto.arima(production_industrielle_diff, seasonal = TRUE)
summary(arima_model_diff)

# Prévision sur 12 mois
forecast_prod_arima <- forecast(arima_model_diff, h = 12)
plot(forecast_prod_arima)
forecast_prod_arima

accuracy(arima_model_diff)


#Data 2:


#Modèles pour l'Ajustement de la Tendance
#Calcul des moyennes mobiles avec différents ordres (k)
m4J<-filter(data2Job_ts,rep(1/4,4),method = c("convolution") ,sides = 2)
plot(data2Job_ts)
lines(m4J,col="blue")

m5J<-filter(data2Job_ts,rep(1/5,5),method = c("convolution") ,sides = 2)
plot(data2Job_ts)
lines(m5J,col="blue")

m7J<-filter(data2Job_ts,rep(1/12,12),method = c("convolution") ,sides = 2)
plot(data2Job_ts)
lines(m7J,col="blue")

#a) Régression Linéaire Simple
# 1. Créer un dataframe aligné
df2 <- data.frame(
  Y = as.numeric(data2Job_ts),  # Extraction des valeurs
  t = time(data2Job_ts)         # Extraction des dates
)

model_lin2 <- lm(data2Job_ts ~ t, data = df2)
model_lin2
summary(model_lin2)
resid(model_lin2)
plot(resid(model_lin2))
acf(resid(model_lin2))

# Version simplifiée avec plot() de base
plot(df2$t, df2$Y, 
     type = "l",  # 'l' pour lignes
     main = "Régression Linéaire Simplifiée",
     xlab = "Temps",
     ylab = "Valeurs",
     col = "black")

# Ajout de la droite de régression
abline(model_lin2, col = "red", lwd = 2)

# Légende
legend("topleft", 
       legend = c("Données", "Tendance linéaire"),
       col = c("black", "red"),
       lty = 1,
       lwd = c(1, 2))

#b) Régression Polynomiale
model_quad2 <- lm(data2Job_ts ~ t + I(t^2), data = df2)
model_quad2
summary(model_quad2)
resid(model_quad2)
plot(resid(model_quad2))
acf(resid(model_quad2))

#comparaison 
AIC(model_lin2)
AIC(model_quad2)
#===> alors le modele quadratique est le meilleur modele car il posséde le AIC le plus faible.
#2. Modèles pour la Composante Saisonnière 
#a) Harmoniques (Séries de Fourier)
#Modèle sinusoïdal 
# 1. Préparation des données
t <- 1:length(data2Job_ts)  # Vecteur temps numérique
p <- 12  # Période saisonnière (12 pour du mensuel)

# 2. Création des matrices cos/sin (6 harmoniques comme dans votre code)
MCc <- matrix(0, nrow = length(data2Job_ts), ncol = 6)
MSc <- matrix(0, nrow = length(data2Job_ts), ncol = 6)

for (i in 1:6) {
  MCc[, i] <- cos(2 * pi * i * t / p)  # Termes cosinus
  MSc[, i] <- sin(2 * pi * i * t / p)  # Termes sinus
}
#  Ajustement du modèle 
model2 <- lm(data2Job_ts~ t + MCc +MSc)
summary(model2)

donnees2 <- data.frame(
  Y = data2Job_ts,        # Votre série temporelle
  t = 1:length(data2Job_ts),  # Variable temps
  MCc1 = MCc[,1], MCc2 = MCc[,2], MCc3 = MCc[,3], 
  MCc4 = MCc[,4], MCc5 = MCc[,5], MCc6 = MCc[,6],
  MSc1 = MSc[,1], MSc2 = MSc[,2], MSc3 = MSc[,3],
  MSc4 = MSc[,4], MSc5 = MSc[,5], MSc6 = MSc[,6]
)


model2_optimise <- lm(data2Job_ts ~ t + MCc1,  # t et MCc1 sont significatifs (p < 0.05)
                      data = donnees2)  

# Vérification
summary(model2_optimise)

#3. Modèles pour les Résidus
# Appliquer une différenciation d'ordre 1
constructionJOB <- diff(data2Job_ts)
# Afficher la série différenciée
plot(constructionJOB, main = "Série différenciée d'ordre 1")
adf.test(constructionJOB)
#Le p-value est maintenant de 0.01 < 0.05
#alors la série différenciée est maintenant stationnaire
# Ajustement AR(1) sur la série DIFFÉRENCIÉE (équivalent à ARIMA(1,1,0) sur l'originale)
manual_model <- Arima(constructionJOB,
                      order = c(1, 1, 1),
                      seasonal = list(order = c(0, 0, 1), period = 12),
                      include.constant = FALSE)
summary(manual_model)

# Prévision sur 12 mois
forecast_prod_arima2 <- forecast(manual_model, h = 12)
plot(forecast_prod_arima2)
forecast_prod_arima2

accuracy(arima_model_diff2)


#phase3: Analyse et modélisation des résidus issus de l’ajustement
#Data1:


### 1. Analyse des résidus du modèle harmonique optimisé
residus_harmoniques <- residuals(model_optimise)
# 2. Test de Ljung-Box pour l'autocorrélation
Box.test(residus_harmoniques, lag = 20, type = "Ljung-Box")  # Test à 20 décalages

# Interprétation :
# - p-value > 0.05 : les résidus sont non corrélés (bruit blanc)
# - p-value < 0.05 : autocorrélation détectée
# Histogramme des résidus
hist(residus_harmoniques, main = "Histogramme des Résidus ", xlab = "Résidus", breaks = 20)
# Test de normalité des résidus
shapiro.test(residus_harmoniques)
#interpretation:
#La p-value très faible (0.00019) indique que les résidus ne suivent pas une distribution normale

#lineaire
residu_lin <- residuals(model_lin)
Box.test(residu_lin, lag = 20, type = "Ljung-Box")
shapiro.test(residu_lin)
hist(residu_lin, main = "Histogramme des Résidus ", xlab = "Résidus", breaks = 20)
#Quadratique
residu_quad <- residuals(model_quad)
Box.test(residu_quad, lag = 20, type = "Ljung-Box")
shapiro.test(model_quad2)
hist(residu_quad, main = "Histogramme des Résidus ", xlab = "Résidus", breaks = 20)


# Analyse des résidus du modèle ARIMA 
residus_ARIMA <- residuals(arima_model_diff)
Box.test(residus_ARIMA, lag = 20, type = "Ljung-Box") 
#Histogramme des résidus
hist(residus_ARIMA, main = "Histogramme des Résidus ARIMA ", xlab = "Résidus", breaks = 20)
# Test de normalité des résidus
shapiro.test(residus_ARIMA)
#La p-value très faible indique que les résidus ne suivent pas une distribution normale



#Data2:


### 1. Analyse des résidus du modèle harmonique optimisé
residus_harmoniques2 <- residuals(model2_optimise)
# 2. Test de Ljung-Box pour l'autocorrélation
Box.test(residus_harmoniques2, lag = 20, type = "Ljung-Box")  # Test à 20 décalages

# Interprétation :
# - p-value > 0.05 : les résidus sont non corrélés (bruit blanc)
# - p-value < 0.05 : autocorrélation détectée
# Histogramme des résidus
hist(residus_harmoniques2, main = "Histogramme des Résidus ", xlab = "Résidus", breaks = 20)
# Test de normalité des résidus
shapiro.test(residus_harmoniques2)
#interpretation:
#La p-value très faible (6.728e-09) indique que les résidus ne suivent pas une distribution normale



#lineaire
residu_lin2 <- residuals(model_lin2)
Box.test(residu_lin2, lag = 20, type = "Ljung-Box")
shapiro.test(residu_lin)
hist(residu_lin2, main = "Histogramme des Résidus ", xlab = "Résidus", breaks = 20)
#Quadratique
residu_quad2 <- residuals(model_quad)
Box.test(residu_quad2, lag = 20, type = "Ljung-Box")
shapiro.test(residu_quad2)
hist(residu_quad2, main = "Histogramme des Résidus ", xlab = "Résidus", breaks = 20)
# Analyse des résidus du modèle ARIMA 
residus_ARIMA2 <- residuals(manual_model)
Box.test(residus_ARIMA2, lag = 20, type = "Ljung-Box") 
#Histogramme des résidus
hist(residus_ARIMA2, main = "Histogramme des Résidus ARIMA ", xlab = "Résidus", breaks = 20)
# Test de normalité des résidus
shapiro.test(residus_ARIMA)
#La p-value très faible indique que les résidus ne suivent pas une distribution normale

acf(residus_ARIMA2)     # pour identifier q (composante MA)
pacf(residus_ARIMA2)



# 6. Autocorrélation des résidus (ACF/PACF)
par(mfrow = c(1,2))
acf(residus_harmoniques2, main = "ACF des Résidus")
pacf(residus_harmoniques2, main = "PACF des Résidus")
par(mfrow = c(1,1))
