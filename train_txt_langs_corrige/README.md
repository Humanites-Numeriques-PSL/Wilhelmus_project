_Pour rappel : correspondance des codes ISO 639-1_

_af : Afrikaans [dérivé du dialecte hollandais]_

_de : Allemand_

_en : Anglais_

_fr : Français_

_fy : frison occidental [langue régionale néerlandaise, https://fr.wikipedia.org/wiki/Frison_occidental - promu par le poète Gysbert japiks, 1580]_

_it : Italien_

_la : Latin_

_li : Limbourgeois [langue régionale néerlandaise, https://fr.wikipedia.org/wiki/Limbourgeois]_

_nl : Néerlandais_


## Vérification sur l'ensemble des .txt dont le score est inférieur à 0.5420 (choix arbitraire permettant d'avoir les 150 premiers textes détectés en néerlandais) ##

En cas de doublon, le fichier a été placé dans le dossier de la langue autre que le néerlandais.  

Remarque : Le manuscrit Handschrift Berlijn mgo 185 compose une grande partie des textes ayant un score de détection du néerlandais inférieur à 0.5; il est néanmoins écrit en néerlandais. 

Sur les 212 textes repris, 150 étaient catégorisés en néerlandais. Sur ces 150 textes catégorisés en néerlandais, on compte 8 erreurs : 
	- 7 textes écrits en langue frisonne (dialecte dérivé du néerlandais) catégorisés en néerlandais, qui avaient un score très faible (de 0.11 à 0.33)
	- Le texte 115.xml, écrit en néerlandais, avec un vers répété en français (score de 0.4571; sur 60 vers dans ce chant, 13 vers en français).

## La vérification manuelle pour les scores supérieurs à 0.50 s'impose-t-elle ? ##

On rappelle que les textes sont classés par ordre croissant suivant les indices de détections de langue. 
Sur les 78 textes suivant le texte 115.xml, qui ont un indice de détection du néerlandais croissant, de 0.4571 à 0.5420, les revérifications manuelles nous permettent de constater qu'aucune erreur de détection de langue n'est à déplorer. Les erreurs précédentes, par ailleurs, étaient des erreurs minimes d'attribution de langue : 7 erreurs sur un dialecte issu du néerlandais, et 1 erreur de manque d'information d'un texte écrit en néerlandais, avec une citation répétée à chaque vers en français. 

Peut-on en conclure que la marge d'erreur est suffisament faible pour nous éviter de vérifier les 5 557 textes manuellement ? 

On utilise une formule simple d"échantillonnage pour observer à quel moment l'échantillon de textes étudié sera représentatif. 
On utilise les résultats des 150 premières occurences pour déterminer la proportion p estimée de la population qui présente la caractéristique recherchée (à savoir "le texte est bien en néerlandais): 142/150 = 0.94
On a n = 1.96^2 * 0.94 (0.06) / (0,05)^2 
Donc l'échantillon serait représentatif à 95%, avec une marge d'erreur de 5%, si n = 87. 

On peut donc raisonnablement avoir confiance dans les langues détectées par fasttext pour les scores supérieurs à 0.50, d'autant que notre calcul supposait la proportion p constante alors qu'elle est croissante (les indices de détection étant croissants).

Pour vérifier néanmoins nos suppositions on va tirer 100 résultats au sort parmi les 5345 textes que nous n'avons pas revérifiés manuellement. 
la ligne python "random.sample(range(212, 5556), 100)" nous a donné les résultats suivants, que nous avons revérifié manuellement : 

[3192, 4132, 1617, 2805, 2665, 2254, 220, 960, 607, 3738, 368, 2119, 1655, 1558, 3243, 4717, 583, 1198, 4354, 5471, 4481, 5063, 3026, 1967, 3830, 5175, 3368, 3788, 3701, 761, 790, 5543, 1429, 4682, 2070, 1313, 734, 4148, 1750, 3784, 1150, 2567, 4734, 3219, 4551, 4180, 4663, 464, 3681, 3568, 1433, 4716, 679, 1854, 4808, 2124, 3116, 1908, 3039, 1220, 2542, 2055, 3666, 4745, 5389, 4331, 873, 5482, 3332, 3966, 2953, 5143, 281, 2027, 2436, 5169, 3322, 4223, 3269, 5180, 275, 4168, 1080, 4027, 4560, 3538, 3011, 1746, 2101, 2301, 3916, 4694, 4330, 987, 4943, 3751, 1844, 2587, 4603, 3473]

Sans surprise, nous n'avons, sur ces 100 textes tirés au hasard, trouvé que des textes néerlandais. Le choix de faire confiance dans les langues détectées par fasttext lorsque l'indice est supérieur à 0.50 paraît donc raisonnablement satisfaisant. 

