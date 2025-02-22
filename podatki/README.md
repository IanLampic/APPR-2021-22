# Analiza podatkov s programom R - 2021/22

Avtor: Ian Lampič

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2021/22

## Tematika

Za projektno nalogo bom analiziral zdravstevno stanje v različnih državah in v EU. Primerjal bom 
podatke o potrošnji za bolnice, umobolnice, zobozdravstvene ordinacije in lekarne (podatki so izraženi v milijon evrih) v različnih državah. 
Nato bom te podatke primerjal z analizo povprečne starosti ljudi, dokler so še zmeraj zdravi, glede na spol (podatki so predstavljeni v letih).
Na koncu bom poskusil še poiskati povezavo z razlogi za smrt, kot so okužbe, bolezni dihal, živčevja, mišic, skeleta, dihal pa vse do duševnih bolezni in zračunal procent smrtnosti glede na število prebivalstva v posamezni državi. S korelacijo teh podatkov bom tako lahko ocenil zdravstveno stanje v dani državi, s tem pa tudi lahko sklepal o sami razvitosti le-te.

## Viri  

Za analizo bom uporabil podatke iz [Eurostat](https://ec.europa.eu/eurostat). 

Vsi viri imajo podatke v obliki CSV, razen število prebivalstva, ki pa ima obliko TSV.

## Tabele

* Smrt v državah glede na vzrok (atributi po stolpcih so leto(integer), država(character), enota(, vzrok(character), rezidenti(character), spol(character), število smrti(integer))
* Povprečna starost zdravega življenja v državah (atributi po stolpcih so leto(integer), država(character), enota, spol(character), povprečno leto starosti(double))
* Državna potrošnja za zdravstvene institucije (atributi po stolpcih so leto(integer), država(character), enota, vrsta institucije(character), vrednost potrošnje(double))
* Število prebivalstva (atributi so država(character) in leto(integer))

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
