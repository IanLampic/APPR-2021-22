# 2. faza: Uvoz podatkov
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

Vzroki <- read_csv("podatki/Vzroki/podatki3.csv", na=",", locale=locale(encoding="Windows-1250"), 
  col_types = cols(.default = col_guess(), 
  UNIT = col_skip(), 
  RESID = col_skip(),
  AGE = col_skip(), 
  FlagandFootnotes = col_skip()
))

Vzroki <- Vzroki %>% relocate(obmocje = GEO, leto = TIME, vzrok = ICD10, spol = SEX, stevilo.prebivalcev = Value)

Vzroki <- Vzroki[!grepl('metropolitan', Vzroki$obmocje),] 
Vzroki <- Vzroki[!grepl('Serbia', Vzroki$obmocje),] 
Vzroki <- Vzroki[!grepl('Turkey', Vzroki$obmocje),]

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
Prebivalstvo <- Prebivalstvo[!grepl('Economic', Prebivalstvo$obmocje),]
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
Prebivalstvo <- Prebivalstvo[!grepl("Kosovo", Prebivalstvo$obmocje),]
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

Vzroki.s <-Vzroki %>% filter(grepl("Skupaj", spol)) 
Vzroki.z <-Vzroki %>% filter(grepl("Ženske", spol))
Vzroki.m <-Vzroki %>% filter(grepl("Moški", spol))

Prebivalstvo.s <- Prebivalstvo %>% filter(grepl("Skupaj", spol))
Prebivalstvo.z <- Prebivalstvo %>% filter(grepl("Ženske", spol))
Prebivalstvo.m <- Prebivalstvo %>% filter(grepl("Moški", spol))

vzroki.prebivalstvo.s <- left_join(Vzroki.s, Prebivalstvo.s, by=c("obmocje", "leto"))
vzroki.prebivalstvo.z <- left_join(Vzroki.z, Prebivalstvo.z, by=c("obmocje", "leto"))
vzroki.prebivalstvo.m <- left_join(Vzroki.m, Prebivalstvo.m, by=c("obmocje", "leto"))

vzroki.prebivalstvo.s <- subset (vzroki.prebivalstvo.s, select = -spol.y)
vzroki.prebivalstvo.s <- vzroki.prebivalstvo.s %>% relocate(leto = leto, obmocje = obmocje, spol = spol.x, vzrok = vzrok,  ljudje.z.boleznijo = stevilo.prebivalcev.x, stevilo.prebivalcev = stevilo.prebivalcev.y)
vzroki.prebivalstvo.z <- subset (vzroki.prebivalstvo.z, select = -spol.y)
vzroki.prebivalstvo.z <- vzroki.prebivalstvo.z %>% relocate(leto = leto, obmocje = obmocje, spol = spol.x, vzrok = vzrok, ljudje.z.boleznijo = stevilo.prebivalcev.x, stevilo.prebivalcev = stevilo.prebivalcev.y)
vzroki.prebivalstvo.m <- subset (vzroki.prebivalstvo.m, select = -spol.y)
vzroki.prebivalstvo.m <- vzroki.prebivalstvo.m %>% relocate(leto = leto, obmocje = obmocje, spol = spol.x, vzrok = vzrok, ljudje.z.boleznijo = stevilo.prebivalcev.x, stevilo.prebivalcev = stevilo.prebivalcev.y)

Skupaj <- rbind(vzroki.prebivalstvo.s, vzroki.prebivalstvo.z, vzroki.prebivalstvo.m)

Skupaj$obmocje[Skupaj$obmocje == "European Union - 28 countries (2013-2020)"] <- "Evropska unija"
Skupaj$obmocje[Skupaj$obmocje == "Belgium"] <- "Belgija"
Skupaj$obmocje[Skupaj$obmocje == "Bulgaria"] <- "Bulgarija"
Skupaj$obmocje[Skupaj$obmocje == "Czechia"] <- "Češka"
Skupaj$obmocje[Skupaj$obmocje == "Denmark"] <- "Danska"
Skupaj$obmocje[Skupaj$obmocje == "Germany (until 1990 former territory of the FRG)"] <- "Nemčija"
Skupaj$obmocje[Skupaj$obmocje == "Estonia"] <- "Estonija"
Skupaj$obmocje[Skupaj$obmocje == "Ireland"] <- "Irska"
Skupaj$obmocje[Skupaj$obmocje == "Greece"] <- "Grčija"
Skupaj$obmocje[Skupaj$obmocje == "Spain"] <- "Španija"
Skupaj$obmocje[Skupaj$obmocje == "France"] <- "Francija" 
Skupaj$obmocje[Skupaj$obmocje == "Croatia"] <- "Hrvaška"
Skupaj$obmocje[Skupaj$obmocje == "Italy"] <- "Italija"
Skupaj$obmocje[Skupaj$obmocje == "Cyprus"] <- "Ciper"
Skupaj$obmocje[Skupaj$obmocje == "Latvia"] <- "Latvija"
Skupaj$obmocje[Skupaj$obmocje == "Lithuania"] <- "Litva"
Skupaj$obmocje[Skupaj$obmocje == "Luxembourg"] <- "Luksemburg"
Skupaj$obmocje[Skupaj$obmocje == "Hungary"] <- "Madžarska"
Skupaj$obmocje[Skupaj$obmocje == "Netherlands"] <- "Nizozemska"
Skupaj$obmocje[Skupaj$obmocje == "Austria"] <- "Avstrija"
Skupaj$obmocje[Skupaj$obmocje == "Poland"] <- "Poljska"
Skupaj$obmocje[Skupaj$obmocje == "Portugal"] <- "Portugalska"
Skupaj$obmocje[Skupaj$obmocje == "Romania"] <- "Romunija"
Skupaj$obmocje[Skupaj$obmocje == "Slovenia"] <- "Slovenija"
Skupaj$obmocje[Skupaj$obmocje == "Slovakia"] <- "Slovaška"
Skupaj$obmocje[Skupaj$obmocje == "Finland"] <- "Finska"
Skupaj$obmocje[Skupaj$obmocje == "Sweden"] <- "Švedska"
Skupaj$obmocje[Skupaj$obmocje == "Iceland"] <- "Islandija"
Skupaj$obmocje[Skupaj$obmocje == "Norway"] <- "Norveška"
Skupaj$obmocje[Skupaj$obmocje == "Switzerland"] <- "Švica"
Skupaj$obmocje[Skupaj$obmocje == "United Kingdom"] <- "Združeno kraljestvo"

Skupaj <- type.convert(Skupaj, na.strings = ":", as.is = 0)
Skupaj <- as.data.frame(Skupaj)
Skupaj[is.na(Skupaj)] <- 0

################################################################################################################################################################
Zdrava <- read_csv("podatki/Zdrava_leta/podatki2.csv", na=",", locale=locale(encoding="Windows-1250"),
                   col_types = cols(.default = col_guess(), 
                                    UNIT = col_skip(), 
                                    FlagandFootnotes = col_skip(),
                                    INDIC_HE = col_skip()
                   ))

Zdrava <- Zdrava %>% relocate(obmocje = GEO, leto = TIME, spol = SEX)
class(Zdrava$"leto") = "double"

Zdrava$obmocje[Zdrava$obmocje == "European Union - 28 countries (2013-2020)"] <- "Evropska unija"
Zdrava$obmocje[Zdrava$obmocje == "Belgium"] <- "Belgija"
Zdrava$obmocje[Zdrava$obmocje == "Bulgaria"] <- "Bulgarija"
Zdrava$obmocje[Zdrava$obmocje == "Czechia"] <- "Češka"
Zdrava$obmocje[Zdrava$obmocje == "Denmark"] <- "Danska"
Zdrava$obmocje[Zdrava$obmocje == "Germany (until 1990 former territory of the FRG)"] <- "Nemčija"
Zdrava$obmocje[Zdrava$obmocje == "Estonia"] <- "Estonija"
Zdrava$obmocje[Zdrava$obmocje == "Ireland"] <- "Irska"
Zdrava$obmocje[Zdrava$obmocje == "Greece"] <- "Grčija"
Zdrava$obmocje[Zdrava$obmocje == "Spain"] <- "Španija"
Zdrava$obmocje[Zdrava$obmocje == "France"] <- "Francija" 
Zdrava$obmocje[Zdrava$obmocje == "Croatia"] <- "Hrvaška"
Zdrava$obmocje[Zdrava$obmocje == "Italy"] <- "Italija"
Zdrava$obmocje[Zdrava$obmocje == "Cyprus"] <- "Ciper"
Zdrava$obmocje[Zdrava$obmocje == "Latvia"] <- "Latvija"
Zdrava$obmocje[Zdrava$obmocje == "Lithuania"] <- "Litva"
Zdrava$obmocje[Zdrava$obmocje == "Luxembourg"] <- "Luksemburg"
Zdrava$obmocje[Zdrava$obmocje == "Hungary"] <- "Madžarska"
Zdrava$obmocje[Zdrava$obmocje == "Netherlands"] <- "Nizozemska"
Zdrava$obmocje[Zdrava$obmocje == "Austria"] <- "Avstrija"
Zdrava$obmocje[Zdrava$obmocje == "Poland"] <- "Poljska"
Zdrava$obmocje[Zdrava$obmocje == "Portugal"] <- "Portugalska"
Zdrava$obmocje[Zdrava$obmocje == "Romania"] <- "Romunija"
Zdrava$obmocje[Zdrava$obmocje == "Slovenia"] <- "Slovenija"
Zdrava$obmocje[Zdrava$obmocje == "Slovakia"] <- "Slovaška"
Zdrava$obmocje[Zdrava$obmocje == "Finland"] <- "Finska"
Zdrava$obmocje[Zdrava$obmocje == "Sweden"] <- "Švedska"
Zdrava$obmocje[Zdrava$obmocje == "Iceland"] <- "Islandija"
Zdrava$obmocje[Zdrava$obmocje == "Norway"] <- "Norveška"
Zdrava$obmocje[Zdrava$obmocje == "Switzerland"] <- "Švica"
Zdrava$obmocje[Zdrava$obmocje == "United Kingdom"] <- "Združeno kraljestvo"

Zdrava.z <- Zdrava %>% filter(grepl("Females", spol)) 
Zdrava.z <- subset(Zdrava.z, select = -c(spol) ) 
names(Zdrava.z)[names(Zdrava.z) == "Value"] <- "pojav.zdr.tezav.pri.z"
Zdrava.m <- Zdrava  %>% filter(grepl("Males", spol)) 
Zdrava.m <- subset(Zdrava.m, select = -c(spol) )
names(Zdrava.m)[names(Zdrava.m) == "Value"] <- "pojav.zdr.tezav.pri.m"
Zdrava.s <- Zdrava  %>% filter(grepl("Total", spol)) 
Zdrava.s <- subset(Zdrava.s, select = -c(spol) )
names(Zdrava.s)[names(Zdrava.s) == "Value"] <- "pojav.zdr.tezav.skupaj"

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
Postelje.v.bolnišnicah <- read_tsv("podatki/Postelje_v_bolnišnicah/postelje.tsv", na=",", locale=locale(encoding="Windows-1250"))
Postelje.v.bolnišnicah <- Postelje.v.bolnišnicah  %>% relocate(obmocje = GEOE)
Postelje.v.bolnišnicah <- pivot_longer(Postelje.v.bolnišnicah, !obmocje, names_to = "leto", values_to = "število.postelj")


Postelje.v.bolnišnicah$število.postelj <- gsub("e","",as.character(Postelje.v.bolnišnicah$število.postelj))
Postelje.v.bolnišnicah$število.postelj <- gsub("b","",as.character(Postelje.v.bolnišnicah$število.postelj))
Postelje.v.bolnišnicah$število.postelj <- gsub("p","",as.character(Postelje.v.bolnišnicah$število.postelj))

p <- unlist(Postelje.v.bolnišnicah$število.postelj)
Postelje.v.bolnišnicah$število.postelj <-  as.numeric(gsub(",", "", p))

Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "European Union - 28 countries (2013-2020),Number,Available beds in hospitals (HP.1)"] <- "Evropska unija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Belgium,Number,Available beds in hospitals (HP.1)"] <- "Belgija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Bulgaria,Number,Available beds in hospitals (HP.1)"] <- "Bulgarija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Czechia,Number,Available beds in hospitals (HP.1)"] <- "Češka"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Denmark,Number,Available beds in hospitals (HP.1)"] <- "Danska"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Germany (until 1990 former territory of the FRG),Number,Available beds in hospitals (HP.1)"] <- "Nemčija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Estonia,Number,Available beds in hospitals (HP.1)"] <- "Estonija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Ireland,Number,Available beds in hospitals (HP.1)"] <- "Irska"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Greece,Number,Available beds in hospitals (HP.1)"] <- "Grčija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Spain,Number,Available beds in hospitals (HP.1)"] <- "Španija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "France,Number,Available beds in hospitals (HP.1)"] <- "Francija" 
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Croatia,Number,Available beds in hospitals (HP.1)"] <- "Hrvaška"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Italy,Number,Available beds in hospitals (HP.1)"] <- "Italija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Cyprus,Number,Available beds in hospitals (HP.1)"] <- "Ciper"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Latvia,Number,Available beds in hospitals (HP.1)"] <- "Latvija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Lithuania,Number,Available beds in hospitals (HP.1)"] <- "Litva"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Luxembourg,Number,Available beds in hospitals (HP.1)"] <- "Luksemburg"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Hungary,Number,Available beds in hospitals (HP.1)"] <- "Madžarska"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Netherlands,Number,Available beds in hospitals (HP.1)"] <- "Nizozemska"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Austria,Number,Available beds in hospitals (HP.1)"] <- "Avstrija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Poland,Number,Available beds in hospitals (HP.1)"] <- "Poljska"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Portugal,Number,Available beds in hospitals (HP.1)"] <- "Portugalska"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Romania,Number,Available beds in hospitals (HP.1)"] <- "Romunija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Slovenia,Number,Available beds in hospitals (HP.1)"] <- "Slovenija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Slovakia,Number,Available beds in hospitals (HP.1)"] <- "Slovaška"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Finland,Number,Available beds in hospitals (HP.1)"] <- "Finska"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Sweden,Number,Available beds in hospitals (HP.1)"] <- "Švedska"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Iceland,Number,Available beds in hospitals (HP.1)"] <- "Islandija"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Norway,Number,Available beds in hospitals (HP.1)"] <- "Norveška"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "Switzerland,Number,Available beds in hospitals (HP.1)"] <- "Švica"
Postelje.v.bolnišnicah$obmocje[Postelje.v.bolnišnicah$obmocje == "United Kingdom,Number,Available beds in hospitals (HP.1)"] <- "Združeno kraljestvo"

Postelje.v.bolnišnicah <- Postelje.v.bolnišnicah[!grepl("Malta", Postelje.v.bolnišnicah$obmocje),]
Postelje.v.bolnišnicah <- Postelje.v.bolnišnicah[!grepl("Liechtenstein", Postelje.v.bolnišnicah$obmocje),]
Postelje.v.bolnišnicah <- Postelje.v.bolnišnicah[!grepl("Macedonia", Postelje.v.bolnišnicah$obmocje),]
Postelje.v.bolnišnicah <- Postelje.v.bolnišnicah[!grepl("Albania" , Postelje.v.bolnišnicah$obmocje),] 
Postelje.v.bolnišnicah <- Postelje.v.bolnišnicah[!grepl("Serbia" , Postelje.v.bolnišnicah$obmocje),]
Postelje.v.bolnišnicah <- Postelje.v.bolnišnicah[!grepl("Turkey" , Postelje.v.bolnišnicah$obmocje),]
Postelje.v.bolnišnicah <- Postelje.v.bolnišnicah[!grepl("Montenegro" , Postelje.v.bolnišnicah$obmocje),]

Postelje.v.bolnišnicah <- type.convert(Postelje.v.bolnišnicah, na.strings = ":", as.is = 0)
Postelje.v.bolnišnicah$število.postelj <- as.integer(Postelje.v.bolnišnicah$število.postelj)
Postelje.v.bolnišnicah <- Postelje.v.bolnišnicah %>% replace_na(list(število.postelj = 0))

class(Postelje.v.bolnišnicah$"leto") = "integer"
postelje.in.tezave <- left_join(Postelje.v.bolnišnicah, Zdrava.z, by=c("obmocje","leto"))
postelje.in.tezave <- left_join(postelje.in.tezave, Zdrava.m, by=c("obmocje","leto"))
postelje.in.tezave <- left_join(postelje.in.tezave, Zdrava.s, by=c("obmocje","leto"))

postelje.in.tezave <- type.convert(postelje.in.tezave, na.strings = ":", as.is = 0)
postelje.in.tezave <- as.data.frame(postelje.in.tezave)
postelje.in.tezave[is.na(postelje.in.tezave)] <- 0

################################################################################################################################################################

Nesreče.v.službah <- read_tsv("podatki/Nesreče/nesreče.tsv", na=",", locale=locale(encoding="Windows-1250"))
Nesreče.v.službah <- Nesreče.v.službah  %>% relocate(obmocje = GEOE)
Nesreče.v.službah$"2013" <- as.character(Nesreče.v.službah$"2013") 
Nesreče.v.službah <- pivot_longer(Nesreče.v.službah, !obmocje, names_to = "leto", values_to = "število.nesreč")

Poskodba.stiri.ali.vec.dni <- Nesreče.v.službah %>% slice(1:568)

Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "European Union - 28 countries (2013-2020),Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Evropska unija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Belgium,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Belgija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Bulgaria,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Bulgarija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Czechia,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Češka"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Denmark,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Danska"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Germany (until 1990 former territory of the FRG),Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Nemčija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Estonia,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Estonija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Ireland,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Irska"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Greece,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Grčija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Spain,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Španija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "France,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Francija" 
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Croatia,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Hrvaška"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Italy,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Italija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Cyprus,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Ciper"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Latvia,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Latvija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Lithuania,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Litva"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Luxembourg,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Luksemburg"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Hungary,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Madžarska"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Netherlands,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Nizozemska"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Austria,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Avstrija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Poland,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Poljska"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Portugal,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Portugalska"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Romania,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Romunija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Slovenia,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Slovenija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Slovakia,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Slovaška"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Finland,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Finska"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Sweden,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Švedska"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Iceland,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Islandija"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Norway,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Norveška"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Switzerland,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Švica"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "United Kingdom,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Združeno kraljestvo"
Nesreče.v.službah$obmocje[Nesreče.v.službah$obmocje == "Malta,Number,Total,Total,4 days or over,Agriculture; industry and construction (except mining); services of the business economy"] <- "Malta"

class(Nesreče.v.službah$"leto") = "double"
Nesreče.v.službah <- Nesreče.v.službah[!grepl("Malta", Nesreče.v.službah$obmocje),]
Nesreče.v.službah["spol"] <-rep("Skupaj", 1674)

prebivalstvo.nesreče <- Prebivalstvo %>%
  slice(which(row_number() %% 3 == 1)) %>%
  slice(1:340)
prebivalstvo.nesreče <- prebivalstvo.nesreče[!grepl('metropolitan', prebivalstvo.nesreče$obmocje),] 
prebivalstvo.nesreče <- prebivalstvo.nesreče[!grepl('Liechtenstein', prebivalstvo.nesreče$obmocje),] 

prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "European Union - 28 countries (2013-2020)"] <- "Evropska unija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Belgium"] <- "Belgija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Bulgaria"] <- "Bulgarija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Czechia"] <- "Češka"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Denmark"] <- "Danska"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Germany (until 1990 former territory of the FRG)"] <- "Nemčija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Estonia"] <- "Estonija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Ireland"] <- "Irska"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Greece"] <- "Grčija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Spain"] <- "Španija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "France"] <- "Francija" 
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Croatia"] <- "Hrvaška"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Italy"] <- "Italija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Cyprus"] <- "Ciper"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Latvia"] <- "Latvija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Lithuania"] <- "Litva"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Luxembourg"] <- "Luksemburg"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Hungary"] <- "Madžarska"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Netherlands"] <- "Nizozemska"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Austria"] <- "Avstrija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Poland"] <- "Poljska"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Portugal"] <- "Portugalska"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Romania"] <- "Romunija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Slovenia"] <- "Slovenija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Slovakia"] <- "Slovaška"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Finland"] <- "Finska"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Sweden"] <- "Švedska"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Iceland"] <- "Islandija"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Norway"] <- "Norveška"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Switzerland"] <- "Švica"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "United Kingdom"] <- "Združeno kraljestvo"
prebivalstvo.nesreče$obmocje[prebivalstvo.nesreče$obmocje == "Finland"] <- "Finska"

nesrece.prebivalstvo <- left_join(Nesreče.v.službah, prebivalstvo.nesreče, by=c("obmocje","leto"))
nesrece.prebivalstvo <-  subset(nesrece.prebivalstvo, select = -c(spol.x,spol.y) ) %>%
  slice(1:279)

nesrece.prebivalstvo$število.nesreč <- gsub("e","",as.character(nesrece.prebivalstvo$število.nesreč))
nesrece.prebivalstvo$število.nesreč <- gsub("b","",as.character(nesrece.prebivalstvo$število.nesreč))
nesrece.prebivalstvo$število.nesreč <- gsub("p","",as.character(nesrece.prebivalstvo$število.nesreč))

pr <- unlist(nesrece.prebivalstvo$stevilo.prebivalcev)
nesrece.prebivalstvo$stevilo.prebivalcev <-  as.numeric(gsub(",", "", pr))

n <- unlist(nesrece.prebivalstvo$število.nesreč)
nesrece.prebivalstvo$število.nesreč <-  as.numeric(gsub(",", "", n))
nesrece.prebivalstvo <- type.convert(nesrece.prebivalstvo, na.strings = ":", as.is = 0)
nesrece.prebivalstvo$število.nesreč <- as.integer(nesrece.prebivalstvo$število.nesreč)
nesrece.prebivalstvo <- nesrece.prebivalstvo %>% replace_na(list(število.nesreč = 0))

################################################################################################################################################################


