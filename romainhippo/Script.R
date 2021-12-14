### Devoir Wilhelmus - Visualisations ###

# On se place de le bon environnement de travail #

source("functions.R")

# Ouverture du fichier
data = as.data.frame(read.csv(file="FrequenceTop20.csv", 
                              sep = ",", 
                              header = TRUE, 
                              row.names=1, 
                              quote = '\"'))

View(data)

# Préparation des données

## Pour que les fonctions de visualisation marchent, il faut 
## transformer les valeurs inexistantes en 0

for(x in 1:nrow((data))){
  for(y in 1:ncol(data)){
    if (is.na(data[x,y])) data[x,y] = 0
  }
}

View(data)

## Puis on sépare le tableau en trois en fonction du
## type de feature à analyser

Corpus_pos = data[1:50,]
Corpus_tri = data[51:99,]
Corpus_lem = data[100:148,]

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
  Row_Sum = as.data.frame(rowSums(Corpus_pos[,grepl(i, colnames(Corpus_pos))])/ncol(Corpus_pos[,grepl(i, colnames(Corpus_pos))]))
  Corpus_pos_mean = as.data.frame(cbind(Corpus_pos_mean, Row_Sum))
}

Corpus_pos_mean = Corpus_pos_mean[,2:ncol(Corpus_pos_mean)]
colnames(Corpus_pos_mean) = names
View(Corpus_pos_mean)


## Trigrammes
Corpus_tri_mean = as.data.frame(matrix(nrow = 49))

for (i in names) {
  Row_Sum = as.data.frame(rowSums(Corpus_tri[,grepl(i, colnames(Corpus_tri))])/ncol(Corpus_tri[,grepl(i, colnames(Corpus_tri))]))
  Corpus_tri_mean = as.data.frame(cbind(Corpus_tri_mean, Row_Sum))
}

Corpus_tri_mean = Corpus_tri_mean[,2:ncol(Corpus_tri_mean)]
colnames(Corpus_tri_mean) = names
View(Corpus_tri_mean)


## Lemmes
Corpus_lem_mean = as.data.frame(matrix(nrow = 49))

for (i in names) {
  Row_Sum = as.data.frame(rowSums(Corpus_lem[,grepl(i, colnames(Corpus_lem))])/ncol(Corpus_lem[,grepl(i, colnames(Corpus_lem))]))
  Corpus_lem_mean = as.data.frame(cbind(Corpus_lem_mean, Row_Sum))
}

Corpus_lem_mean = Corpus_lem_mean[,2:ncol(Corpus_lem_mean)]
colnames(Corpus_lem_mean) = names
View(Corpus_lem_mean)


###ACP

library('FactoMineR')
monACP_pos = PCA(t(Corpus_pos_mean))
monACP_tri = PCA(t(Corpus_tri_mean))
monACP_lem = PCA(t(Corpus_lem_mean))
