# Wilhelmus project

Collective project of the computational philology course at Ecole nationale des chartes - PSL Univeristy - academic years 2021-2022 and 2022-2023
Florian Cafiero & Jean-Baptiste Camps

## Description des dossiers

train_txt_langs: fichiers .xml convertis en .txt, organisés en dossiers selon la langue détectée par fasttext.

## Description des fichiers

**authors_corrections.csv:** 
list of corrections made to the "author" metadata. These corrections are sometimes simply made to standardize the names. Other corrections concern "author" fields that had been filled in erroneously, the name of the editor of the collection, the ethnographer who collected the song, etc., having been mistakenly entered. The associated texts have been temporarily considered as "Anonymous". But it is possible that the real author is known. It is therefore necessary to verify this.

**lang_certs.csv**: 
result of an automatic language detection algorithm. The language and the degree of certainty of the algorithm about the detected language are indicated. The further away from 1 the degree of certainty is, the more necessary it is to check the text.

**langcert_revised.csv**: 
same result, already partially revised by us. This gives an example of the structure to adopt: we keep the information on what the algorithm had initially detected, but we add a column for the language finally detected.





## Source

These data have originally been produced for the 2019 
[Wilhelmus Challenge](https://staticweb.hum.uu.nl/dh2019/dh2019.adho.org/wilhelmus-challenge/index.html)

The initial release can be found on the dedicated repository, under MIT License:

[https://github.com/fbkarsdorp/meertens-song-collection/releases/tag/DH2019](https://github.com/fbkarsdorp/meertens-song-collection/releases/tag/DH2019).

## References

Digitale Bibliotheek voor de Nederlandse Letteren: https://www.dbnl.org/

"Did a Poet with Donkey Ears Write the Oldest Anthem in the World? Ideological Implications of the Computational Attribution of the Dutch National Anthem
to Petrus Dathenus": https://dh2017.adho.org/abstracts/079/079.pdf


