# Wilhelmus project

Projet collectif du cours de philologie computationnelle Ecole nationale des chartes - Université PSL - année universitaire 2021-2022
Florian Cafiero & Jean-Baptiste Camps

## Description des dossiers

train_txt_langs: fichiers .xml convertis en .txt, organisés en dossiers selon la langue détectée par fasttext.

## Description des fichiers

**authors_corrections.csv:** 
liste des correction faites aux métadonnées "auteur". Ces corrections sont parfois simplement faites pour harmoniser les dénominations. Les autres corrections concernent des champs "auteurs" qui avaient été remplis de manière erronée, le nom de l'éditeur du recueil, de l'ethnographe ayant collecté le chant etc., ayant été saisi. Les textes associés ont été temporairement considérés comme "Anonymous". Mais il se peut que le véritable auteur soit connu. Il est donc nécessaire de le vérifier.

**lang_certs.csv**: 
résultat d'un algorithme de détection automatique de la langue. Sont indiqués la langue et le degré de certitude de l'algorithme quant à la langue détectée. Plus ce degré est loin de 1, plus il est nécessaire de vérifier le texte.

**langcert_revised.csv**: 
même résultat, déjà partiellement révisé par nos soins. Ceci donne un exemple de la structure à adopter: on garde l'information sur ce que l'algorithme avait initialement détecté, mais on ajoute une colonne pour la langue finalement repérée.





## Source

These data have originally been produced for the 2019 
[Wilhelmus Challenge](https://staticweb.hum.uu.nl/dh2019/dh2019.adho.org/wilhelmus-challenge/index.html)

The initial release can be found on the dedicated repository, under MIT License:

[https://github.com/fbkarsdorp/meertens-song-collection/releases/tag/DH2019](https://github.com/fbkarsdorp/meertens-song-collection/releases/tag/DH2019).

## Références

Bibliothèque numérique des lettres néerlandaises: https://www.dbnl.org/

"Did a Poet with Donkey Ears Write the Oldest Anthem in the World? Ideological Implications of the Computational Attribution of the Dutch National Anthem
to Petrus Dathenus": https://dh2017.adho.org/abstracts/079/079.pdf


