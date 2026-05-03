library(readxl)

setwd("C:/Users/jolas/Desktop")
dane=read_excel("temat4.xlsx")
head(dane)

# rozpoznaję zmienne jakościowe: płeć i kierunek studiów
dane$Płeć= as.factor(dane$Płeć)
dane$Kierunek_studiów= as.factor(dane$Kierunek_studiów)
summary(dane)

sum(is.na(dane$Numer_indeksu))

sum(is.na(dane$Płeć))

sum(is.na(dane$Kierunek_studiów))

sum(is.na(dane$Wiek))

sum(is.na(dane$Czas_nauki))

sum(is.na(dane$Średnia_ocen))

dane <- dane[!is.na(dane$Numer_indeksu), ]
sredni_wiek <- round(mean(dane$Wiek, na.rm = TRUE))
dane$Wiek[is.na(dane$Wiek)] <- sredni_wiek
library(Hmisc)

dane$Czas_nauki <- impute(dane$Czas_nauki, mean)
dane$Średnia_ocen <- impute(dane$Średnia_ocen, mean)
summary(dane)

head(dane)

sum(is.na(dane))

# teraz zmienne jakościowe zastępuję modą
najczestsza_plec <- names(which.max(table(dane$Płeć)))
najczestszy_kierunek <- names(which.max(table(dane$Kierunek_studiów)))
dane$Płeć[is.na(dane$Płeć)] <- najczestsza_plec
dane$Kierunek_studiów[is.na(dane$Kierunek_studiów)] <- najczestszy_kierunek
sum(is.na(dane))

ramka_danych = dane[, c("Numer_indeksu", "Średnia_ocen")]
summary(ramka_danych)

oblicz_stypendium = function(ramka_danych) {
  # tworzenie pustego wektora na kwoty stypendiów 
  stypendia <- numeric(nrow(ramka_danych)) #wektor wypełniony zerami dla każdego studenta
  
  # kwoty stypendium 
  stypendia[ramka_danych$Średnia_ocen >= 4.9] <- 950
  stypendia[ramka_danych$Średnia_ocen >= 4.7 & ramka_danych$Średnia_ocen < 4.9] <- 850
  stypendia[ramka_danych$Średnia_ocen >= 4.5 & ramka_danych$Średnia_ocen < 4.7] <- 750
  
  stypendysci <- ramka_danych[stypendia > 0, ]  # Wybieramy tylko osoby, które otrzymały stypendium
  stypendysci$Kwota <- stypendia[stypendia > 0]  # Dodajemy kolumnę "Kwota" z odpowiednimi kwotami 
  stypendysci$Średnia_ocen <- NULL  # Usuwamy kolumnę "Średnia_ocen" (nie ma w poleceniu zadania)
  # Liczba przyznanych stypendiów
  liczba_stypendiow <- nrow(stypendysci) 
  
  # Całkowity koszt stypendiów
  koszt_stypendiow <- sum(stypendysci$Kwota)
  
  # Zwrócenie listy wyników
  wynik <- list(
    liczba_stypendiow = liczba_stypendiow,
    stypendysci = stypendysci,
    koszt_stypendiow = koszt_stypendiow
  )
  
  return(wynik)
}

wynik <- oblicz_stypendium(ramka_danych)
print(wynik)

# Nikt ze studentów nie miał średniej powyżej 4,50 i dlatego nikt nie otrzymał stypendium,
# a całkowity koszt stypendiów to 0

summary(ramka_danych$Średnia_ocen) #rzeczywiście - najwyzsza srednia: 4,1

# dodajmy jednak sztucznego studenta, któremu należy się stypendium
# ramka_danych <- rbind(ramka_danych, data.frame(Numer_indeksu = 99999, Średnia_ocen = 4.8))
# summary(ramka_danych$Średnia_ocen) # teraz najwyzsza srednia: 4,8

# wynik <- oblicz_stypendium(ramka_danych)
# print(wynik) #i teraz juz jest wypłacone jedno stypendium

library(ggplot2)

ggplot(dane, aes(x = Kierunek_studiów, fill = Płeć)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Kierunek studiów a płeć",
    x = "Kierunek studiów",
    y = "Liczba studentów"
  )+scale_fill_manual(values = c("hotpink","deepskyblue"))+theme(
    plot.title = element_text(color="brown"),
    panel.background = element_rect(fill = 'floralwhite')
        )

library(RVAideMemoire) #potrzebuję do mody

hist(dane$Średnia_ocen, breaks = 15, main="Rozkład średniej ocen", ylab="gęstość", xlab="ocena",
     col = "floralwhite",  border = "navy")
abline(v=mean(dane$Średnia_ocen), col="red",lty = 3)
abline(v=median(dane$Średnia_ocen),col="blue",lty = 2)
lines(density(dane$Średnia_ocen), col="orangered",lty = 1)
abline(v =mod(dane$Średnia_ocen), col = "green", lty = 2)   

legend(
  "topright",  
  legend = c("Średnia", "Mediana", "Moda", "Estymator gęstości"),
  col = c("red", "blue", "green", "orangered"),
  lty = c(3, 2, 2, 1),  # styl linii (przerywana dla statystyk; ciągła dla gęstości)
  bg = "white"
)

#akurat średnia pokrywa się z medianą

ggplot(dane, aes(y = Czas_nauki, fill = Kierunek_studiów)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +  
  labs(
    title = "Czas nauki w zależności od kierunku studiów",
    x = "Kierunek studiów",
    y = "Czas nauki (w godzinach)"
  ) + scale_fill_manual(values = c("pink", "khaki", "gray", "plum"))

summary(dane$Kierunek_studiów)

#jak widać, najliczniejszy kierunek to Psychologia
#ale sprawdźmy to formalnie
table(dane$Kierunek_studiów)

najliczniejszy = names(which.max(table(dane$Kierunek_studiów)))
najliczniejszy

#jak widać, najliczniejszy kierunek to Psychologia
psychologia = najliczniejszy

dane_psychologia = subset(dane, Kierunek_studiów == psychologia)
czas_nauki = dane_psychologia$Czas_nauki

#install.packages("fitdistrplus")
#install.packages("ggplot2")
library(fitdistrplus)

library(ggplot2)

summary(czas_nauki)

value1 <- as.numeric(as.character(czas_nauki)) #dane w kolumnie czas_nauki będą traktowane jako liczby, bez tego nie działa
value1 <- value1[is.finite(value1)] # dane koniecznie muszą być skończone

# Funkcja descdist() z pakietu fitdistrplus: do dopasowania odpowiedniego rozkładu do danych. 
# generuje wykres i wypisuje informacje, które pomagają w ocenie jak dobrze dane empiryczne pasują do wybranych rozkładów teoretycznych
descdist(value1, discrete = FALSE)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalized_value1 = normalize(value1)
normalized_value1[normalized_value1==0]=0.00001
normalized_value1[normalized_value1==1]=0.99999
range(normalized_value1) #wszystko w porządku

fit_norm = fitdist(normalized_value1, "norm")
plot(fit_norm)

fit_norm$estimate

fit_gamma = fitdist(normalized_value1, "gamma")
plot(fit_gamma)

fit_gamma$estimate

fit_beta = fitdist(normalized_value1, "beta")
plot(fit_beta)

fit_beta$estimate

fit_unif = fitdist(normalized_value1, "unif")
plot(fit_unif)

fit_unif$estimate

#Ocena dopasowania
gof_results = gofstat(list(fit_gamma,fit_beta, fit_unif,fit_norm), fitnames = c("Gamma", "Beta",  "Jedostajny", "Normalny"))
print(gof_results)

# AIC (Akaike Information Criterion) – im niższa wartość, tym lepsze dopasowanie;
cat("Najlepszy rozkład na podstawie AIC to:", names(which.min(gof_results$aic)), "\n")

# BIC (Bayesian Information Criterion) – podobnie jak AIC, ale mocno karze złożone modele;
# Mniejsza wartość BIC oznacza lepsze dopasowanie;
cat("Najlepszy rozkład na podstawie BIC to:", names(which.min(gof_results$bic)), "\n")

# Kolmogorov-Smirnov Statistic (KS) – statystyka ta mierzy różnicę między empiryczną funkcją rozkładu a funkcją rozkładu teoretycznego; ponownie - im mniejsza wartość, tym lepsze dopasowanie
cat("Najlepszy rozkład na podstawie statystyki Kolmogorova-Smirnova to:", names(which.min(gof_results$ks)), "\n")

# Cramer-von Mises Statistic – podobnie jak Kolmogorov-Smirnov, mierzy różnice między funkcjami rozkładów;
cat("Najlepszy rozkład na podstawie statystyki Cramera-von Misesa to:", names(which.min(gof_results$cvm)), "\n")

# Anderson-Darling Statistic – bardziej wrażliwa na wartości odstające;
cat("Najlepszy rozkład na podstawie statystyki Andersona-Darlinga to:", names(which.min(gof_results$ad)), "\n")

# tutaj przy histogramach pomógł chat gpt, chciałam sprawdzić czy rzeczywiście histogram najlepiej przybliżony jest poprzez rozkład normalny
hist(normalized_value1, breaks = 28, prob = TRUE, main = "Dopasowanie rozkładów", xlab = "Znormalizowany czas nauki", col = "lightblue")
curve(dgamma(x, shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]), add = TRUE, col = "orange", lwd = 2)
curve(dbeta(x, shape1 = fit_beta$estimate["shape1"], shape2 = fit_beta$estimate["shape2"]), add = TRUE, col = "navy", lwd = 2)
curve(dunif(x, min = 0, max = 1), add = TRUE, col = "green", lwd = 2)
curve(dnorm(x, mean = fit_norm$estimate["mean"], sd = fit_norm$estimate["sd"]), add = TRUE, col = "plum", lwd = 2)
legend("topright", legend = c("Gamma", "Beta", "Jednostajny", "Normalny"),
       col = c("orange", "navy", "green", "plum"), lwd = 2)

# teraz mniej breaks

hist(normalized_value1, breaks = 14, prob = TRUE, main = "Dopasowanie rozkładów", xlab = "Znormalizowany czas nauki", col = "lightblue")
curve(dgamma(x, shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]), add = TRUE, col = "orange", lwd = 2)
curve(dbeta(x, shape1 = fit_beta$estimate["shape1"], shape2 = fit_beta$estimate["shape2"]), add = TRUE, col = "navy", lwd = 2)
curve(dunif(x, min = 0, max = 1), add = TRUE, col = "green", lwd = 2)
curve(dnorm(x, mean = fit_norm$estimate["mean"], sd = fit_norm$estimate["sd"]), add = TRUE, col = "plum", lwd = 2)
legend("topright", legend = c("Gamma", "Beta", "Jednostajny", "Normalny"),
       col = c("orange", "navy", "green", "plum"), lwd = 2)

kierunek_biologia <- dane[dane$Kierunek_studiów == "Biologia", ]
plot(x=kierunek_biologia$Średnia_ocen,y=kierunek_biologia$Czas_nauki, type="p",pch=19, col="salmon",main="Zależność średniej ocen od czasu nauki dla kierunku Biologia",xlab="Średnia ocen",ylab="Czas nauki")
legend("topleft",legend="Biologia",col="salmon",pch=19,cex=1.2)

korelacja=cor(kierunek_biologia$Średnia_ocen,kierunek_biologia$Czas_nauki)
korelacja #obliczony współczynnik

#założenia regresji liniowej i diagnostyka modelu
model_liniowy_biologia <- lm(kierunek_biologia$Średnia_ocen~kierunek_biologia$Czas_nauki)
shapiro.test(residuals(model_liniowy_biologia))

# W = 0.99184: to wysoka wartość; reszty są bliskie normalnemu rozkładowi
# p-value = 0.7798: większa niż standardowy poziom istotności ; brak podstaw do odrzucenia
# czyli test Shapiro-Wilka wskazuje, że założenie o normalności reszt jest spełnione.


# diagnostyka modelu
par(mfrow = c(2, 2))  # Wykresy diagnostyczne w układzie 2x2
plot(model_liniowy_biologia) # cztery ważne wykresy

#wyestymowane współczynniki
model_liniowy_biologia$coefficients

# dodanie linii regresji 
par(mfrow = c(1, 1))
plot(x=kierunek_biologia$Czas_nauki,y=kierunek_biologia$Średnia_ocen, type="p",pch=19, col="salmon",main="Zależność średniej ocen od czasu nauki dla kierunku Biologia",xlab="Czas nauki",ylab="Średnia ocen")

#dodanie wyestymowanych współczynników na wykres
a = model_liniowy_biologia$coefficients[2]  # Czas_nauki
b = model_liniowy_biologia$coefficients[1]  # Intercept
x = kierunek_biologia$Czas_nauki
y_regression = a * x + b

# rysowanie linii regresji
lines(x, y_regression, type = "l", col = "red", lwd = 2)
legend("topleft", legend = "Regresja liniowa", col = "red", lwd = 2)