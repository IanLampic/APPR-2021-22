# 2. faza: Uvoz podatkov
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

Vzroki <- read_csv("podatki/Vzroki/podatki1.csv", na=",", locale=locale(encoding="Windows-1250"))
Zdrava <- read_csv("podatki/Zdravaleta/podatki2.csv", na=",", locale=locale(encoding="Windows-1250"))
Potrosnja <- read_csv("podatki/Potrošnja/podatki3.csv", na=",", locale=locale(encoding="Windows-1250"))
Prebivalstvo <- read_tsv("podatki/število_prabivalstva.tsv", na=" ", locale=locale(encoding="Windows-1250"))

#Ugotovit, katere države so pri prebivalstvu, končnice