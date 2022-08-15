# 2. faza: Uvoz podatkov
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#Vzroki
Vzroki <- read_csv("/Users/ianlampic/Desktop/APPR-2021-22/podatki/Vzroki/podatki3.csv", na=",", locale=locale(encoding="Windows-1250"), 
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
Vzroki <- Vzroki[!grepl('Malta', Vzroki$obmocje),]
Vzroki <- Vzroki[!grepl('Liechtenstein', Vzroki$obmocje),]

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

#Prebivalstvo
Prebivalstvo <- read_csv("/Users/ianlampic/Desktop/APPR-2021-22/podatki/Populacijan/populacija.csv", na=" ", locale=locale(encoding="Windows-1250"),
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
Prebivalstvo <- Prebivalstvo[!grepl("belarus", Prebivalstvo$obmocje),]
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
Prebivalstvo <- Prebivalstvo[!grepl('metropolitan', Prebivalstvo$obmocje),]

Prebivalstvo$spol[Prebivalstvo$spol == "Average population - total"] <- "Skupaj"
Prebivalstvo$spol[Prebivalstvo$spol == "Average population - males"] <- "Moški"
Prebivalstvo$spol[Prebivalstvo$spol == "Average population - females"] <- "Ženske"

pre <- unlist(Prebivalstvo$stevilo.prebivalcev)
Prebivalstvo$stevilo.prebivalcev <-  as.numeric(gsub(",", "", pre))
Prebivalstvo[is.na(Prebivalstvo)] <- 0

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

#Potrošnja per capita in PPP v US dolarjih
link <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_total_health_expenditure_per_capita")
tabela <- html_node(link, ".wikitable")
tabela = html_table(tabela, fill = TRUE)

names(tabela) <- c("obmocje", "potrošnja", "A", "B")
tabela1 <- rbind(tabela[2], data.frame(potrošnja = tabela$A))
tabela1 <- rbind(tabela1, data.frame(potrošnja = tabela$B))

tabela <- subset(tabela, select = -c(potrošnja, A, B))
obmocje<-rep(unlist(tabela$obmocje),times=3)
tabela<-data.frame(obmocje)

tabela$potrošnja <- tabela1$potrošnja
tabela$leto <- rep(2017,times=37)
tabela$leto[38:74] <- 2018
tabela$leto[75:111] <- 2019

tabela$obmocje <- gsub("^([[:alpha:]]*).*", "\\1", tabela$obmocje)

#Postelje
Postelje.v.bolnišnicah <- read_tsv("/Users/ianlampic/Desktop/APPR-2021-22/podatki/Postelje_v_bolnišnicah/postelje.tsv", na=",", locale=locale(encoding="Windows-1250"))
Postelje.v.bolnišnicah <- Postelje.v.bolnišnicah  %>% relocate(obmocje = GEOE)
Postelje.v.bolnišnicah <- pivot_longer(Postelje.v.bolnišnicah, !obmocje, names_to = "leto", values_to = "število.postelj")

Postelje.v.bolnišnicah$število.postelj <- gsub("e","",as.character(Postelje.v.bolnišnicah$število.postelj))
Postelje.v.bolnišnicah$število.postelj <- gsub("b","",as.character(Postelje.v.bolnišnicah$število.postelj))
Postelje.v.bolnišnicah$število.postelj <- gsub("p","",as.character(Postelje.v.bolnišnicah$število.postelj))

p <- unlist(Postelje.v.bolnišnicah$število.postelj)
Postelje.v.bolnišnicah$število.postelj <-  as.numeric(gsub(",", "", p))

Postelje.v.bolnišnicah$obmocje <- gsub(",.*", "", Postelje.v.bolnišnicah$obmocje)

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

Skupaj <- rbind(vzroki.prebivalstvo.s, vzroki.prebivalstvo.z, vzroki.prebivalstvo.m)
Skupaj <- left_join(Skupaj, tabela, , by = c("leto" = "leto",  "obmocje" = "obmocje"))
Skupaj <- left_join(Skupaj, Postelje.v.bolnišnicah, by=c("obmocje", "leto"))

Skupaj$obmocje[Skupaj$obmocje == "European Union - 28 countries (2013-2020)"] <- "EU"
Skupaj$obmocje[Skupaj$obmocje == "Belgium"] <- "BE"
Skupaj$obmocje[Skupaj$obmocje == "Bulgaria"] <- "BG"
Skupaj$obmocje[Skupaj$obmocje == "Czechia"] <- "CZ"
Skupaj$obmocje[Skupaj$obmocje == "Denmark"] <- "DK"
Skupaj$obmocje[Skupaj$obmocje == "Germany (until 1990 former territory of the FRG)"] <- "DE"
Skupaj$obmocje[Skupaj$obmocje == "Estonia"] <- "EE"
Skupaj$obmocje[Skupaj$obmocje == "Ireland"] <- "IE"
Skupaj$obmocje[Skupaj$obmocje == "Greece"] <- "GR"
Skupaj$obmocje[Skupaj$obmocje == "Spain"] <- "ES"
Skupaj$obmocje[Skupaj$obmocje == "France"] <- "FR" 
Skupaj$obmocje[Skupaj$obmocje == "Croatia"] <- "HR"
Skupaj$obmocje[Skupaj$obmocje == "Italy"] <- "IT"
Skupaj$obmocje[Skupaj$obmocje == "Cyprus"] <- "CY"
Skupaj$obmocje[Skupaj$obmocje == "Latvia"] <- "LV"
Skupaj$obmocje[Skupaj$obmocje == "Lithuania"] <- "LT"
Skupaj$obmocje[Skupaj$obmocje == "Luxembourg"] <- "LU"
Skupaj$obmocje[Skupaj$obmocje == "Hungary"] <- "HU"
Skupaj$obmocje[Skupaj$obmocje == "Netherlands"] <- "NL"
Skupaj$obmocje[Skupaj$obmocje == "Austria"] <- "AT"
Skupaj$obmocje[Skupaj$obmocje == "Poland"] <- "PL"
Skupaj$obmocje[Skupaj$obmocje == "Portugal"] <- "PT"
Skupaj$obmocje[Skupaj$obmocje == "Romania"] <- "RO"
Skupaj$obmocje[Skupaj$obmocje == "Slovenia"] <- "SI"
Skupaj$obmocje[Skupaj$obmocje == "Slovakia"] <- "SK"
Skupaj$obmocje[Skupaj$obmocje == "Finland"] <- "FI"
Skupaj$obmocje[Skupaj$obmocje == "Sweden"] <- "SE"
Skupaj$obmocje[Skupaj$obmocje == "Iceland"] <- "IS"
Skupaj$obmocje[Skupaj$obmocje == "Norway"] <- "NO"
Skupaj$obmocje[Skupaj$obmocje == "Switzerland"] <- "CH"
Skupaj$obmocje[Skupaj$obmocje == "United Kingdom"] <- "GB"

Skupaj <- type.convert(Skupaj, na.strings = ":", as.is = 0)
Skupaj <- as.data.frame(Skupaj)
Skupaj[is.na(Skupaj)] <- 0

Skupaj$ljudje.z.boleznijo <-  as.numeric(gsub(",", ".", gsub("\\.", "", Skupaj$ljudje.z.boleznijo)))

pr <- unlist(Skupaj$stevilo.prebivalcev)
Skupaj$stevilo.prebivalcev <-  as.numeric(gsub(",", "", pr))

samoSlo <- Skupaj[Skupaj$obmocje %in% c("SI"), ]

pot <- unlist(Skupaj$potrošnja)
Skupaj$potrošnja <-  as.numeric(gsub(",", "", pot))
Skupaj <- type.convert(Skupaj, na.strings = ":", as.is = 0)
Skupaj$potrošnja <- as.integer(Skupaj$potrošnja)
Skupaj <- Skupaj %>% replace_na(list(potrošnja = 0))
################################################################################################################################################################

Nesreče.v.službah <- read_tsv("/Users/ianlampic/Desktop/APPR-2021-22/podatki/Nesreče/nesreče.tsv", na=",", locale=locale(encoding="Windows-1250"))
Nesreče.v.službah <- Nesreče.v.službah  %>% relocate(obmocje = GEOE)
Nesreče.v.službah$"2013" <- as.character(Nesreče.v.službah$"2013") 
Nesreče.v.službah <- pivot_longer(Nesreče.v.službah, !obmocje, names_to = "leto", values_to = "število.nesreč")

Poskodba.stiri.ali.vec.dni <- Nesreče.v.službah %>% slice(1:568)

Nesreče.v.službah$obmocje <- gsub(",.*", "", Nesreče.v.službah$obmocje)

class(Nesreče.v.službah$"leto") = "double"
Nesreče.v.službah <- Nesreče.v.službah[!grepl("MT", Nesreče.v.službah$obmocje),]
Nesreče.v.službah["spol"] <-rep("Skupaj", 1674)

prebivalstvo.nesreče <- Prebivalstvo %>%
  slice(which(row_number() %% 3 == 1)) %>%
  slice(1:340)
prebivalstvo.nesreče <- prebivalstvo.nesreče[!grepl('metropolitan', prebivalstvo.nesreče$obmocje),] 
prebivalstvo.nesreče <- prebivalstvo.nesreče[!grepl('Liechtenstein', prebivalstvo.nesreče$obmocje),] 

nesrece.prebivalstvo <- left_join(Nesreče.v.službah, prebivalstvo.nesreče, by=c("obmocje","leto"))
nesrece.prebivalstvo <-  subset(nesrece.prebivalstvo, select = -c(spol.x,spol.y) ) %>%
  slice(1:279)

nesrece.prebivalstvo$število.nesreč <- gsub("e","",as.character(nesrece.prebivalstvo$število.nesreč))
nesrece.prebivalstvo$število.nesreč <- gsub("b","",as.character(nesrece.prebivalstvo$število.nesreč))
nesrece.prebivalstvo$število.nesreč <- gsub("p","",as.character(nesrece.prebivalstvo$število.nesreč))

Zdrava <- read_csv("/Users/ianlampic/Desktop/APPR-2021-22/podatki/Zdrava_leta/podatki2.csv", na=",", locale=locale(encoding="Windows-1250"),
                   col_types = cols(.default = col_guess(), 
                                    UNIT = col_skip(), 
                                    FlagandFootnotes = col_skip(),
                                    INDIC_HE = col_skip()
                   ))

Zdrava <- Zdrava %>% relocate(obmocje = GEO, leto = TIME, spol = SEX)
class(Zdrava$"leto") = "double"

Zdrava.z <- Zdrava %>% filter(grepl("Females", spol)) 
Zdrava.z <- subset(Zdrava.z, select = -c(spol) ) 
names(Zdrava.z)[names(Zdrava.z) == "Value"] <- "pojav.zdr.tezav.pri.z"
Zdrava.m <- Zdrava  %>% filter(grepl("Males", spol)) 
Zdrava.m <- subset(Zdrava.m, select = -c(spol) )
names(Zdrava.m)[names(Zdrava.m) == "Value"] <- "pojav.zdr.tezav.pri.m"
Zdrava.s <- Zdrava  %>% filter(grepl("Total", spol)) 
Zdrava.s <- subset(Zdrava.s, select = -c(spol) )
names(Zdrava.s)[names(Zdrava.s) == "Value"] <- "pojav.zdr.tezav.skupaj"

nesrece.prebivalstvo <- left_join(nesrece.prebivalstvo, Zdrava.z, by=c("obmocje","leto"))
nesrece.prebivalstvo <- left_join(nesrece.prebivalstvo, Zdrava.m, by=c("obmocje","leto"))
nesrece.prebivalstvo <- left_join(nesrece.prebivalstvo, Zdrava.s, by=c("obmocje","leto"))

nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "European Union - 28 countries (2013-2020)"] <- "EU"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Belgium"] <- "BE"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Bulgaria"] <- "BG"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Czechia"] <- "CZ"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Denmark"] <- "DK"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Germany (until 1990 former territory of the FRG)"] <- "DE"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Estonia"] <- "EE"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Ireland"] <- "IE"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Greece"] <- "GR"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Spain"] <- "ES"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "France"] <- "FR" 
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Croatia"] <- "HR"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Italy"] <- "IT"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Cyprus"] <- "CY"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Latvia"] <- "LV"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Lithuania"] <- "LT"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Luxembourg"] <- "LU"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Hungary"] <- "HU"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Netherlands"] <- "NL"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Austria"] <- "AT"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Poland"] <- "PL"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Portugal"] <- "PT"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Romania"] <- "RO"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Slovenia"] <- "SI"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Slovakia"] <- "SK"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Finland"] <- "FI"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Sweden"] <- "SE"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Iceland"] <- "IS"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Norway"] <- "NO"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "Switzerland"] <- "CH"
nesrece.prebivalstvo$obmocje[nesrece.prebivalstvo$obmocje == "United Kingdom"] <- "GB"

pr <- unlist(nesrece.prebivalstvo$stevilo.prebivalcev)
nesrece.prebivalstvo$stevilo.prebivalcev <-  as.numeric(gsub(",", "", pr))

n <- unlist(nesrece.prebivalstvo$število.nesreč)
nesrece.prebivalstvo$število.nesreč <-  as.numeric(gsub(",", "", n))
nesrece.prebivalstvo <- type.convert(nesrece.prebivalstvo, na.strings = ":", as.is = 0)
nesrece.prebivalstvo$število.nesreč <- as.integer(nesrece.prebivalstvo$število.nesreč)
nesrece.prebivalstvo <- nesrece.prebivalstvo %>% replace_na(list(število.nesreč = 0))

z <- unlist(nesrece.prebivalstvo$pojav.zdr.tezav.pri.z)
nesrece.prebivalstvo$pojav.zdr.tezav.pri.z <-  as.numeric(gsub(",", "", z))
nesrece.prebivalstvo <- type.convert(nesrece.prebivalstvo, na.strings = ":", as.is = 0)
nesrece.prebivalstvo$pojav.zdr.tezav.pri.z <- as.double(nesrece.prebivalstvo$pojav.zdr.tezav.pri.z)
nesrece.prebivalstvo <- nesrece.prebivalstvo %>% replace_na(list(pojav.zdr.tezav.pri.z = 0))

m <- unlist(nesrece.prebivalstvo$pojav.zdr.tezav.pri.m)
nesrece.prebivalstvo$pojav.zdr.tezav.pri.m <-  as.numeric(gsub(",", "", m))
nesrece.prebivalstvo <- type.convert(nesrece.prebivalstvo, na.strings = ":", as.is = 0)
nesrece.prebivalstvo$pojav.zdr.tezav.pri.m <- as.double(nesrece.prebivalstvo$pojav.zdr.tezav.pri.m)
nesrece.prebivalstvo <- nesrece.prebivalstvo %>% replace_na(list(pojav.zdr.tezav.pri.m = 0))

s <- unlist(nesrece.prebivalstvo$pojav.zdr.tezav.skupaj)
nesrece.prebivalstvo$pojav.zdr.tezav.skupaj <-  as.numeric(gsub(",", "", s))
nesrece.prebivalstvo <- type.convert(nesrece.prebivalstvo, na.strings = ":", as.is = 0)
nesrece.prebivalstvo$pojav.zdr.tezav.skupaj <- as.double(nesrece.prebivalstvo$pojav.zdr.tezav.skupaj)
nesrece.prebivalstvo <- nesrece.prebivalstvo %>% replace_na(list(pojav.zdr.tezav.skupaj = 0))




