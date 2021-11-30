# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza zdravstvenega stanja

# Ideja

Za projektno nalogo bom analiziral zdravstevno stanje v različnih državah. Med državami bom primerjal 
podatke o potrošnji za bolnice, umobolnice, zobozdravstvene ordinacije in lekarne (podatki so izraženi v milijon evrih). 
Nato bom te podatke primerjal z analizo povprečne starosti po državah glede na spol (podatki so predstavljeni v letih).
Na koncu bom poiskusil še poiskati korelacijo z zadnjimi podatki in sicer razlogi za smrt, kot so okužbe, bolezni dihal, živčevja, mišic, skeleta, dihal pa vse do duševnih bolezni. S korelacijo teh podatkov bom tako lahko ocenil zdravstveno stanje v dani državi, 
s tem pa tudi lahko sklepal o sami razvitosti le-te.

# Viri

Za analizo bom uporabil podatke iz [Eurostat](https://ec.europa.eu). 

Viri imajo podatke v obliki CSV.

# Tabele

*Smrt v državah glede na vzrok
*Povprečna starost v državah
*Državna potrošnja za zdravstvene institucije

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
