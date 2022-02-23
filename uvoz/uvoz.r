# 2. faza: Uvoz podatkov
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

Vzroki <- read_csv("podatki/Vzroki/podatki1.csv", na=",", locale=locale(encoding="Windows-1250"), 
  col_types = cols(.default = col_guess(), 
  UNIT = col_skip(), 
  AGE = col_skip(), 
  RESID = col_skip(),
  FlagandFootnotes = col_skip()
))

Vzroki <- Vzroki %>% relocate(obmocje = GEO, leto = TIME, vzrok = ICD10, spol = SEX, stevilo.prebivalcev = Value)

Vzroki <- Vzroki[!grepl('Serbia', Vzroki$obmocje),] 
Vzroki <- Vzroki[!grepl('Turkey', Vzroki$obmocje),]
Vzroki <- Vzroki[!grepl('France (metropolitan)', Vzroki$obmocje),] 
# Ne vem zakaj noče odstrant France (metropolitana)...

Vzroki$spol[Vzroki$spol == "Total"] <- "Skupaj"
Vzroki$spol[Vzroki$spol == "Males"] <- "Moški"
Vzroki$spol[Vzroki$spol == "Females"] <- "Ženske"

Vzroki$vzrok[Vzroki$vzrok == "Certain infectious and parasitic diseases (A00-B99)"] <- "Nekatere nalezljive in parazitske bolezni"
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

Prebivalstvo <- read_csv("podatki/Populacijan/populacija.csv", na=" ", locale=locale(encoding="Windows-1250"),
                         col_types = cols(.default = col_guess(),
                                          FlagandFootnotes = col_skip()))

Prebivalstvo <- Prebivalstvo %>% relocate(obmocje = GEO, leto = TIME, spol = INDIC_DE, stevilo.prebivalcev = Value)

Prebivalstvo <- Prebivalstvo[!grepl('Germany including former GDR', Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl('European Economic Area (EU28 - 2013-2020 and IS, LI, NO)', Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl('European Economic Area (EU27 - 2007-2013 and IS, LI, NO)', Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl('European Free Trade Association', Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl('European Free Trade Association', Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Montenegro", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("North Macedonia", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Albania", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Serbia", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Turkey", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Andorra", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Belarus", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Bosnia and Herzegovina", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Kosovo (under United Nations Security Council Resolution 1244/99)", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Moldova", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Monaco", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Russia", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("San Marino", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Ukraine", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Armenia", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Azerbaijan", Prebivalstvo$obmocje),]
Prebivalstvo <- Prebivalstvo[!grepl("Georgia" , Prebivalstvo$obmocje),]                                                                                                                                                                                                                                                                                                                                                                                                                                                          

Prebivalstvo$spol[Prebivalstvo$spol == "Average population - total"] <- "Skupaj"
Prebivalstvo$spol[Prebivalstvo$spol == "Average population - males"] <- "Moški"
Prebivalstvo$spol[Prebivalstvo$spol == "Average population - females"] <- "Ženske"

# Naredi 3 tabele za vzroke (skupau, moški, ženske) jih združi z populacijo in na koncu skupaj

Vzrokis <-Vzroki %>% filter(grepl("Skupaj", spol))
Vzrokiz <-Vzroki %>% filter(grepl("Ženske", spol))
Vzrokim <-Vzroki %>% filter(grepl("Moški", spol))

Prebivalstvos <- Prebivalstvo %>% filter(grepl("Skupaj", spol))


Zdrava <- read_csv("podatki/Zdrava_leta/podatki2.csv", na=",", locale=locale(encoding="Windows-1250"))

Postelje.v.bolnišnicah <- read_tsv("podatki/Postelje_v_bolnišnicah/postelje.tsv", na=",", locale=locale(encoding="Windows-1250"))
Nesreče.v.službah <- read_html("podatki/Nesreče/nesreče.html", locale=locale(encoding="Windows-1250"))

Vzroki$obmocje[Vzroki$obmocje == "European Union - 28 countries (2013-2020)"] <- "Evropska unija"
Vzroki$obmocje[Vzroki$obmocje == "Belgium"] <- "Belgija"
Vzroki$obmocje[Vzroki$obmocje == "Bulgaria"] <- "Bulgarija"
Vzroki$obmocje[Vzroki$obmocje == "Czechia"] <- "Češka"
Vzroki$obmocje[Vzroki$obmocje == "Denmark"] <- "Danska"
Vzroki$obmocje[Vzroki$obmocje == "Germany (until 1990 former territory of the FRG)"] <- "Nemčija"
Vzroki$obmocje[Vzroki$obmocje == "Estonia"] <- "Estonija"
Vzroki$obmocje[Vzroki$obmocje == "Ireland"] <- "Irska"
Vzroki$obmocje[Vzroki$obmocje == "Greece"] <- "Grčija"
Vzroki$obmocje[Vzroki$obmocje == "Spain"] <- "Španija"
Vzroki$obmocje[Vzroki$obmocje == "France"] <- "Francija" 
Vzroki$obmocje[Vzroki$obmocje == "Croatia"] <- "Hrvaška"
Vzroki$obmocje[Vzroki$obmocje == "Italy"] <- "Italija"
Vzroki$obmocje[Vzroki$obmocje == "Cyprus"] <- "Ciper"
Vzroki$obmocje[Vzroki$obmocje == "Latvia"] <- "Latvija"
Vzroki$obmocje[Vzroki$obmocje == "Lithuania"] <- "Litva"
Vzroki$obmocje[Vzroki$obmocje == "Luxembourg"] <- "Luksemburg"
Vzroki$obmocje[Vzroki$obmocje == "Hungary"] <- "Madžarska"
Vzroki$obmocje[Vzroki$obmocje == "Netherlands"] <- "Nizozemska"
Vzroki$obmocje[Vzroki$obmocje == "Austria"] <- "Avstrija"
Vzroki$obmocje[Vzroki$obmocje == "Poland"] <- "Poljska"
Vzroki$obmocje[Vzroki$obmocje == "Portugal"] <- "Portugalska"
Vzroki$obmocje[Vzroki$obmocje == "Romania"] <- "Romunija"
Vzroki$obmocje[Vzroki$obmocje == "Slovenia"] <- "Slovenija"
Vzroki$obmocje[Vzroki$obmocje == "Slovakia"] <- "Slovaška"
Vzroki$obmocje[Vzroki$obmocje == "Finland"] <- "Finska"
Vzroki$obmocje[Vzroki$obmocje == "Sweden"] <- "Švedska"
Vzroki$obmocje[Vzroki$obmocje == "Iceland"] <- "Islandija"
Vzroki$obmocje[Vzroki$obmocje == "Norway"] <- "Norveška"
Vzroki$obmocje[Vzroki$obmocje == "Switzerland"] <- "Švica"
Vzroki$obmocje[Vzroki$obmocje == "United Kingdom"] <- "Združeno kraljestvo"
