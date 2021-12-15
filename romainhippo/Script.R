### Devoir Wilhelmus - Visualisations ###

# On se place de le bon environnement de travail #

source("functions.R")

# Ouverture du fichier
data = as.data.frame(read.csv(file="FrequenceTop20.csv", 
                              sep = ",", 
                              header = TRUE, 
                              row.names=1, 
                              quote = '\"'))


Wilhelmus = as.data.frame(read.csv(file = "FeaturesWilhelmus.csv",
                               sep = ",", 
                               header = TRUE, 
                               row.names = 1,
                               quote = '\"'))


# Préparation des données

## Puis on sépare le tableau en trois en fonction du
## type de feature à analyser

Corpus_pos = data[1:50,]   ### Part of speech
Corpus_tri = data[51:99,]   ### Trigrammes de caractères
Corpus_lem = data[100:148,]   ### Lemmes

names = c('Aegidius.Haeffacker', 'Boudewijn.Jansen.Wellens', 
          'C..J..Wits', 'Christianus.de.Placker', 
          'Frans.vander.Straten.de.jongere', 'Guillaume.Abrahams.Ooijevaer', 
          'Haerlem.Soetendal', 'Hendrick.Jansz..Prins', 
          'Hermanus.van.den.Burg', 'Jacobus.Viverius', 
          'Jan.Jansz..Deutel', 'Jan.Zoet', 
          'Joan.Ysermans', 'Justus.de.Harduwijn', 
          'Karel.van.Mander', 'Leenaert.Clock', 
          'Michiel.Vlack', 'Pieter.Brughman', 
          'Pieter.de.Bisschop', 'Zacharias.Heyns')


# Moyenne des fréquences relatives pour chaque auteur

## POS
Corpus_pos_mean = as.data.frame(matrix(nrow = 50))

for (i in names) {
  Row_Sum = as.data.frame(rowMeans(Corpus_pos[,grepl(i, colnames(Corpus_pos))], na.rm = TRUE))
  Corpus_pos_mean = as.data.frame(cbind(Corpus_pos_mean, Row_Sum))
}

Corpus_pos_mean = Corpus_pos_mean[,2:ncol(Corpus_pos_mean)]
Corpus_pos_mean = cbind(Corpus_pos_mean, Wilhelmus[1:50,])
colnames(Corpus_pos_mean) = c(names, 'Wilhelmus')
View(Corpus_pos_mean)

## Trigrammes
Corpus_tri_mean = as.data.frame(matrix(nrow = 49))

for (i in names) {
  Row_Sum = as.data.frame(rowMeans(Corpus_tri[,grepl(i, colnames(Corpus_tri))], na.rm = TRUE))
  Corpus_tri_mean = as.data.frame(cbind(Corpus_tri_mean, Row_Sum))
}

Corpus_tri_mean = Corpus_tri_mean[,2:ncol(Corpus_tri_mean)]
Corpus_tri_mean = cbind(Corpus_tri_mean, Wilhelmus[51:99,])
colnames(Corpus_tri_mean) = c(names, 'Wilhelmus')
View(Corpus_tri_mean)

## Lemmes
Corpus_lem_mean = as.data.frame(matrix(nrow = 49))

for (i in names) {
  Row_Sum = as.data.frame(rowMeans(Corpus_lem[,grepl(i, colnames(Corpus_lem))], na.rm = TRUE))
  Corpus_lem_mean = as.data.frame(cbind(Corpus_lem_mean, Row_Sum))
}

Corpus_lem_mean = Corpus_lem_mean[,2:ncol(Corpus_lem_mean)]
Corpus_lem_mean = cbind(Corpus_lem_mean, Wilhelmus[100:148,])
colnames(Corpus_lem_mean) = c(names, 'Wilhelmus')
View(Corpus_lem_mean)


# ACP

library('FactoMineR')
monACP_pos = PCA(t(Corpus_pos_mean))
monACP_tri = PCA(t(Corpus_tri_mean))
monACP_lem = PCA(t(Corpus_lem_mean))



# CAH

myCAH_pos = cluster::agnes(t(Corpus_pos_mean), metric = "manhattan", method="ward")
cahPlot(myCAH_pos, k = 6)

myCAH_tri = cluster::agnes(t(Corpus_tri_mean), metric = "manhattan", method="ward")
cahPlot(myCAH_tri, k = 6)

myCAH_lem = cluster::agnes(t(Corpus_lem_mean), metric = "manhattan", method="ward")
cahPlot(myCAH_lem, k = 5)
