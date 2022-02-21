# Analiza podatkov s programom R - 2021/22

Avtor: Ian Lampič

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2021/22

## Tematika

Za projektno nalogo bom analiziral zdravstevno stanje v različnih državah in v EU. Primerjal bom 
podatke o analizi povprečne starosti, dokler so še zmeraj zdravi, glede na spol (podatki so predstavljeni v letih). Nato bom te podatke primerjal s številom pojstel glede na državo in pa poiskusil poiskati še korelacijo z številom nesreč v službi, kjer so podatki dani kot število ljudi, ki zaradi nesreče ne pridejo v službo 4 ali več dni ali pa se smrtno ponesrečijo.
Na koncu bom poskusil še poiskati korelacijo z zadnjimi podatki in sicer razlogi za smrt, kot so okužbe, bolezni dihal, živčevja, mišic, skeleta, dihal pa vse do duševnih bolezni. S korelacijo teh podatkov bom tako lahko ocenil zdravstveno stanje v dani državi, 
s tem pa tudi lahko sklepal o sami razvitosti le-te.

## Viri

Za analizo bom uporabil podatke iz [Eurostat](https://ec.europa.eu). 

Vsi viri imajo podatke v obliki CSV, razen število prebivalstva, ima obliko html in število postelj, ki ima oblika TSV.

## Tabele

* Smrt v državah glede na vzrok (atributi po stolpcih so leto(integer), država(character), enota(, vzrok(character), rezidenti(character), spol(character), število smrti(integer))
* Povprečna starost zdravega življenja v državah (atributi po stolpcih so leto(integer), država(character), enota, spol(character), povprečno leto starosti(double))
* Število postelj v bolnicah (atributi so stolpcih država(character) in leta(integer))
* Število prebivalstva (atributi so država(character), leto(integer) in število prebivalstca(integer))
* Nesreče v službah (atributi so država(character) in leta (država))


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r` 

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
