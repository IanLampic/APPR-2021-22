# 2. faza: Uvoz podatkov
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

Vzroki <- read_csv("podatki/Vzroki/podatki1.csv", na=",", locale=locale(encoding="Windows-1250"), 
  col_types = cols(.default = col_guess(), 
  UNIT = col_skip(), 
  AGE = col_skip(), 
  RESID = col_skip(),
  FlagandFootnotes = col_skip()
))

Vzroki <- Vzroki %>% relocate(območje = GEO, leto = TIME, vzrok = ICD10, spol = SEX, število.prebivalcev = Value)

Vzroki$območje[Vzroki$območje == "European Union - 28 countries (2013-2020)"] <- "Evropska unija"
Vzroki$območje[Vzroki$območje == "Belgium"] <- "Belgija"
Vzroki$območje[Vzroki$območje == "Bulgaria"] <- "Bulgarija"
Vzroki$območje[Vzroki$območje == "Czechia"] <- "Češka"
Vzroki$območje[Vzroki$območje == "Denmark"] <- "Danska"
Vzroki$območje[Vzroki$območje == "Germany (until 1990 former territory of the FRG)"] <- "Nemčija"
Vzroki$območje[Vzroki$območje == "Estonia"] <- "Estonija"
Vzroki$območje[Vzroki$območje == "Ireland"] <- "Irska"
Vzroki$območje[Vzroki$območje == "Greece"] <- "Grčija"
Vzroki$območje[Vzroki$območje == "Spain"] <- "Španija"
Vzroki$območje[Vzroki$območje == "France"] <- "Francija" #izbriši vrstice 529-576
Vzroki$območje[Vzroki$območje == "Croatia"] <- "Hrvaška"
Vzroki$območje[Vzroki$območje == "Italy"] <- "Italija"
Vzroki$območje[Vzroki$območje == "Cyprus"] <- "Ciper"
Vzroki$območje[Vzroki$območje == "Latvia"] <- "Latvija"
Vzroki$območje[Vzroki$območje == "Lithuania"] <- "Litva"
Vzroki$območje[Vzroki$območje == "Luxembourg"] <- "Luksemburg"
Vzroki$območje[Vzroki$območje == "Hungary"] <- "Madžarska"
Vzroki$območje[Vzroki$območje == "Netherlands"] <- "Nizozemska"
Vzroki$območje[Vzroki$območje == "Austria"] <- "Avstrija"
Vzroki$območje[Vzroki$območje == "Poland"] <- "Poljska"
Vzroki$območje[Vzroki$območje == "Portugal"] <- "Portugalska"
Vzroki$območje[Vzroki$območje == "Romania"] <- "Romunija"
Vzroki$območje[Vzroki$območje == "Slovenia"] <- "Slovenija"
Vzroki$območje[Vzroki$območje == "Slovakia"] <- "Slovaška"
Vzroki$območje[Vzroki$območje == "Finland"] <- "Finska"
Vzroki$območje[Vzroki$območje == "Sweden"] <- "Švedska"
Vzroki$območje[Vzroki$območje == "Iceland"] <- "Islandija"
Vzroki$območje[Vzroki$območje == "Liechtenstein"] <- "Lihtenštajn"
Vzroki$območje[Vzroki$območje == "Norway"] <- "Norveška"
Vzroki$območje[Vzroki$območje == "Switzerland"] <- "Švica"
Vzroki$območje[Vzroki$območje == "United Kingdom"] <- "Združeno kraljestvo"
Vzroki$območje[Vzroki$območje == "Serbia"] <- "Serbija"
Vzroki$območje[Vzroki$območje == "Turkey"] <- "Turčija"

Vzroki$spol[Vzroki$spol == "Total"] <- "Skupaj"
Vzroki$spol[Vzroki$spol == "Males"] <- "Moški"
Vzroki$spol[Vzroki$spol == "Females"] <- "Ženske"

Vzroki$vzrok[Vzroki$vzrok == "Certain infectious and parasitic diseases (A00-B99)"] <- "nekatere nalezljive in parazitske bolezni"
Vzroki$vzrok[Vzroki$vzrok == "Malignant neoplasms (C00-C97)"] <- "Maligne novotvorbe"
Vzroki$vzrok[Vzroki$vzrok == "Non-malignant neoplasms (benign and uncertain)"] <- "Nemaligne novotvorbe"
Vzroki$vzrok[Vzroki$vzrok == "Endocrine, nutritional and metabolic diseases (E00-E90)"] <- "Endokrine, prehranske in presnovne bolezni"
Vzroki$vzrok[Vzroki$vzrok == "Mental and behavioural disorders (F00-F99)"] <- "Duševne in vedenjske motnje"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the nervous system and the sense organs (G00-H95)"] <- "Bolezni živčnega sistema in čutnih organov"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the circulatory system (I00-I99)"] <- "Bolezni cirkulacijskega sistema"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the respiratory system (J00-J99)"] <- "Bolezni dihalnega sistema"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the digestive system (K00-K93)"] <- "Bolezni prebavnega sistema"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the skin and subcutaneous tissue (L00-L99)"] <- "Bolezni kože in podkožja"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the musculoskeletal system and connective tissue (M00-M99)"] <- "Bolezni mišično-skeletnega sistema in vezivnega tkiva"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the genitourinary system (N00-N99)"] <- "Bolezni genitourinarnega sistema"
Vzroki$vzrok[Vzroki$vzrok == "Pregnancy, childbirth and the puerperium (O00-O99)"] <- "Nosečnost, porod in poporodno obdobje"
Vzroki$vzrok[Vzroki$vzrok == "Certain conditions originating in the perinatal period (P00-P96)"] <- "Nekatera stanja, ki izvirajo iz perinatalnega obdobja"
Vzroki$vzrok[Vzroki$vzrok == "Congenital malformations, deformations and chromosomal abnormalities (Q00-Q99)"] <- "Prirojene malformacije, deformacije in kromosomske nepravilnosti"
Vzroki$vzrok[Vzroki$vzrok == "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)"] <- "Simptomi, znaki in nenormalni klinični in laboratorijski izvidi, ki niso uvrščeni drugje"



Zdrava <- read_csv("podatki/Zdrava_leta/podatki2.csv", na=",", locale=locale(encoding="Windows-1250"))
Prebivalstvo <- read_csv("podatki/Populacija/število_prebivalstva.csv", na=" ", locale=locale(encoding="Windows-1250"))
Postelje.v.bolnišnicah <- read_tsv("podatki/Postelje_v_bolnišnicah/postelje.tsv", na=",", locale=locale(encoding="Windows-1250"))
Nesreče.v.službah <- read_html("podatki/Nesreče/nesreče.html", locale=locale(encoding="Windows-1250"))


