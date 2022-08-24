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
Vzroki$spol[Vzroki$spol == "Males"] <- "Moski"
Vzroki$spol[Vzroki$spol == "Females"] <- "zenske"

Vzroki$vzrok[Vzroki$vzrok == "Certain infectious and parasitic diseases (A00-B99)"] <- "Nekn"
Vzroki$vzrok[Vzroki$vzrok == "Malignant neoplasms (C00-C97)"] <- "Mal"
Vzroki$vzrok[Vzroki$vzrok == "Non-malignant neoplasms (benign and uncertain)"] <- "Nem"
Vzroki$vzrok[Vzroki$vzrok == "Endocrine, nutritional and metabolic diseases (E00-E90)"] <- "End"
Vzroki$vzrok[Vzroki$vzrok == "Mental and behavioural disorders (F00-F99)"] <- "Dus"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the nervous system and the sense organs (G00-H95)"] <- "Bolz"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the circulatory system (I00-I99)"] <- "Bolc"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the respiratory system (J00-J99)"] <- "Bold"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the digestive system (K00-K93)"] <- "Bolp"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the skin and subcutaneous tissue (L00-L99)"] <- "Bolk"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the musculoskeletal system and connective tissue (M00-M99)"] <- "Bolm"
Vzroki$vzrok[Vzroki$vzrok == "Diseases of the genitourinary system (N00-N99)"] <- "Bolg"
Vzroki$vzrok[Vzroki$vzrok == "Pregnancy, childbirth and the puerperium (O00-O99)"] <- "Nos"
Vzroki$vzrok[Vzroki$vzrok == "Certain conditions originating in the perinatal period (P00-P96)"] <- "Neks"
Vzroki$vzrok[Vzroki$vzrok == "Congenital malformations, deformations and chromosomal abnormalities (Q00-Q99)"] <- "Pri"
Vzroki$vzrok[Vzroki$vzrok == "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)"] <- "Sim"

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
Prebivalstvo$spol[Prebivalstvo$spol == "Average population - males"] <- "Moski"
Prebivalstvo$spol[Prebivalstvo$spol == "Average population - females"] <- "zenske"

pre <- unlist(Prebivalstvo$stevilo.prebivalcev)
Prebivalstvo$stevilo.prebivalcev <-  as.numeric(gsub(",", "", pre))
Prebivalstvo[is.na(Prebivalstvo)] <- 0

Vzroki.s <-Vzroki %>% filter(grepl("Skupaj", spol)) 
Vzroki.z <-Vzroki %>% filter(grepl("zenske", spol))
Vzroki.m <-Vzroki %>% filter(grepl("Moski", spol))

Prebivalstvo.s <- Prebivalstvo %>% filter(grepl("Skupaj", spol))
Prebivalstvo.z <- Prebivalstvo %>% filter(grepl("zenske", spol))
Prebivalstvo.m <- Prebivalstvo %>% filter(grepl("Moski", spol))

vzroki.prebivalstvo.s <- left_join(Vzroki.s, Prebivalstvo.s, by=c("obmocje", "leto"))
vzroki.prebivalstvo.z <- left_join(Vzroki.z, Prebivalstvo.z, by=c("obmocje", "leto"))
vzroki.prebivalstvo.m <- left_join(Vzroki.m, Prebivalstvo.m, by=c("obmocje", "leto"))

vzroki.prebivalstvo.s <- subset (vzroki.prebivalstvo.s, select = -spol.y)
vzroki.prebivalstvo.s <- vzroki.prebivalstvo.s %>% relocate(leto = leto, obmocje = obmocje, spol = spol.x, vzrok = vzrok,  ljudje.z.boleznijo = stevilo.prebivalcev.x, stevilo.prebivalcev = stevilo.prebivalcev.y)
vzroki.prebivalstvo.z <- subset (vzroki.prebivalstvo.z, select = -spol.y)
vzroki.prebivalstvo.z <- vzroki.prebivalstvo.z %>% relocate(leto = leto, obmocje = obmocje, spol = spol.x, vzrok = vzrok, ljudje.z.boleznijo = stevilo.prebivalcev.x, stevilo.prebivalcev = stevilo.prebivalcev.y)
vzroki.prebivalstvo.m <- subset (vzroki.prebivalstvo.m, select = -spol.y)
vzroki.prebivalstvo.m <- vzroki.prebivalstvo.m %>% relocate(leto = leto, obmocje = obmocje, spol = spol.x, vzrok = vzrok, ljudje.z.boleznijo = stevilo.prebivalcev.x, stevilo.prebivalcev = stevilo.prebivalcev.y)

#Potrosnja per capita in PPP v US dolarjih (2017-2019)
link <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_total_health_expenditure_per_capita")
tabela <- html_node(link, ".wikitable")
tabela = html_table(tabela, fill = TRUE)

names(tabela) <- c("obmocje", "potrosnja", "A", "B")
tabela1 <- rbind(tabela[2], data.frame(potrosnja = tabela$A))
tabela1 <- rbind(tabela1, data.frame(potrosnja = tabela$B))

tabela <- subset(tabela, select = -c(potrosnja, A, B))
obmocje<-rep(unlist(tabela$obmocje),times=3)
tabela<-data.frame(obmocje)

tabela$potrosnja <- tabela1$potrosnja
tabela$leto <- rep(2017,times=37)
tabela$leto[38:74] <- 2018
tabela$leto[75:111] <- 2019

tabela$obmocje <- gsub("^([[:alpha:]]*).*", "\\1", tabela$obmocje)

tab <- unlist(tabela$potrosnja)
tabela$potrosnja <-  as.numeric(gsub(",", "", tab))

#Postelje
Postelje.v.bolnisnicah <- read_tsv("/Users/ianlampic/Desktop/APPR-2021-22/podatki/Postelje_v_bolnisnicah/postelje.tsv", na=",", locale=locale(encoding="Windows-1250"))
Postelje.v.bolnisnicah <- Postelje.v.bolnisnicah  %>% relocate(obmocje = GEOE)
Postelje.v.bolnisnicah <- pivot_longer(Postelje.v.bolnisnicah, !obmocje, names_to = "leto", values_to = "stevilo.postelj")

Postelje.v.bolnisnicah$stevilo.postelj <- gsub("e","",as.character(Postelje.v.bolnisnicah$stevilo.postelj))
Postelje.v.bolnisnicah$stevilo.postelj <- gsub("b","",as.character(Postelje.v.bolnisnicah$stevilo.postelj))
Postelje.v.bolnisnicah$stevilo.postelj <- gsub("p","",as.character(Postelje.v.bolnisnicah$stevilo.postelj))

p <- unlist(Postelje.v.bolnisnicah$stevilo.postelj)
Postelje.v.bolnisnicah$stevilo.postelj <-  as.numeric(gsub(",", "", p))

Postelje.v.bolnisnicah$obmocje <- gsub(",.*", "", Postelje.v.bolnisnicah$obmocje)

Postelje.v.bolnisnicah <- Postelje.v.bolnisnicah[!grepl("Malta", Postelje.v.bolnisnicah$obmocje),]
Postelje.v.bolnisnicah <- Postelje.v.bolnisnicah[!grepl("Liechtenstein", Postelje.v.bolnisnicah$obmocje),]
Postelje.v.bolnisnicah <- Postelje.v.bolnisnicah[!grepl("Macedonia", Postelje.v.bolnisnicah$obmocje),]
Postelje.v.bolnisnicah <- Postelje.v.bolnisnicah[!grepl("Albania" , Postelje.v.bolnisnicah$obmocje),] 
Postelje.v.bolnisnicah <- Postelje.v.bolnisnicah[!grepl("Serbia" , Postelje.v.bolnisnicah$obmocje),]
Postelje.v.bolnisnicah <- Postelje.v.bolnisnicah[!grepl("Turkey" , Postelje.v.bolnisnicah$obmocje),]
Postelje.v.bolnisnicah <- Postelje.v.bolnisnicah[!grepl("Montenegro" , Postelje.v.bolnisnicah$obmocje),]

class(Postelje.v.bolnisnicah$"leto") = "integer"

Skupaj <- rbind(vzroki.prebivalstvo.s, vzroki.prebivalstvo.z, vzroki.prebivalstvo.m)
Skupaj2 <- left_join(Skupaj, Postelje.v.bolnisnicah, by=c("obmocje", "leto"))
Skupaj <- left_join(Skupaj, tabela, by = c("leto" = "leto",  "obmocje" = "obmocje"))

Skupaj$ljudje.z.boleznijo <-  as.numeric(gsub(",", ".", gsub("\\.", "", Skupaj$ljudje.z.boleznijo)))

analiza <- Skupaj
analiza$obmocje[analiza$obmocje  == "Germany (until 1990 former territory of the FRG)"] <- "Germany"

vsota.s <- analiza %>% filter(spol %in% c("Skupaj")) %>%
  dplyr::group_by(leto,obmocje) %>%
  dplyr::summarise(
    ljudje.z.boleznijo.skupno = sum(ljudje.z.boleznijo, na.rm = TRUE)
  )
Skupaj$obmocje[Skupaj$obmocje == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
Skupaj <- left_join(vsota.s, Skupaj, by=c("obmocje", "leto"))
Skupaj2 <- left_join(Skupaj2, vsota.s, by=c("obmocje", "leto"))
Skupaj2 <- left_join(Skupaj2, tabela, by=c("obmocje", "leto"))
vsota.s <- left_join(vsota.s, tabela)
vsota.s <- left_join(vsota.s, Prebivalstvo)

###
#Mapping preden sem skrajsal imena drzav
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
Skupaj$obmocje[Skupaj$obmocje == "Czechia"] <- "Czech Republic"

lvls <-(Europe$sovereignt)
primerjava <- data.frame(obmocje = lvls) %>% left_join(Skupaj, by = "obmocje")
Novosku <- Skupaj %>% filter(spol %in% c("Skupaj")) %>% filter(leto %in% c(2016)) %>% filter(vzrok %in% c("Nos"))
###

Skupaj$obmocje[Skupaj$obmocje == "European Union - 28 countries (2013-2020)"] <- "EU"
Skupaj$obmocje[Skupaj$obmocje == "Belgium"] <- "BE"
Skupaj$obmocje[Skupaj$obmocje == "Bulgaria"] <- "BG"
Skupaj$obmocje[Skupaj$obmocje == "Czech Republic"] <- "CZ"
Skupaj$obmocje[Skupaj$obmocje == "Denmark"] <- "DK"
Skupaj$obmocje[Skupaj$obmocje == "Germany"] <- "DE"
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

ana <- unlist(analiza$stevilo.prebivalcev)
analiza$stevilo.prebivalcev <- as.numeric(gsub(",","",ana))

pr <- unlist(Skupaj$stevilo.prebivalcev)
Skupaj$stevilo.prebivalcev <-  as.numeric(gsub(",", "", pr))

Koncna <- Skupaj %>% filter(spol %in% c("Skupaj"))
################################################################################################################################################################

Nesrece.v.sluzbah <- read_tsv("/Users/ianlampic/Desktop/APPR-2021-22/podatki/Nesrece/nesrece.tsv", na=",", locale=locale(encoding="Windows-1250"))
Nesrece.v.sluzbah <- Nesrece.v.sluzbah  %>% relocate(obmocje = GEOE)
Nesrece.v.sluzbah$"2013" <- as.character(Nesrece.v.sluzbah$"2013") 
Nesrece.v.sluzbah <- pivot_longer(Nesrece.v.sluzbah, !obmocje, names_to = "leto", values_to = "stevilo.nesrec")

Poskodba.stiri.ali.vec.dni <- Nesrece.v.sluzbah %>% slice(1:568)

Nesrece.v.sluzbah$obmocje <- gsub(",.*", "", Nesrece.v.sluzbah$obmocje)

class(Nesrece.v.sluzbah$"leto") = "double"
Nesrece.v.sluzbah <- Nesrece.v.sluzbah[!grepl("Malta", Nesrece.v.sluzbah$obmocje),]
Nesrece.v.sluzbah["spol"] <-rep("Skupaj", 1674)

prebivalstvo.nesrece <- Prebivalstvo %>%
  slice(which(row_number() %% 3 == 1)) %>%
  slice(1:340)
prebivalstvo.nesrece <- prebivalstvo.nesrece[!grepl('metropolitan', prebivalstvo.nesrece$obmocje),] 
prebivalstvo.nesrece <- prebivalstvo.nesrece[!grepl('Liechtenstein', prebivalstvo.nesrece$obmocje),] 

nesrece.prebivalstvo <- left_join(Nesrece.v.sluzbah, prebivalstvo.nesrece, by=c("obmocje","leto"))
nesrece.prebivalstvo <-  subset(nesrece.prebivalstvo, select = -c(spol.x,spol.y) ) %>%
  slice(1:279)

nesrece.prebivalstvo$stevilo.nesrec <- gsub("e","",as.character(nesrece.prebivalstvo$stevilo.nesrec))
nesrece.prebivalstvo$stevilo.nesrec <- gsub("b","",as.character(nesrece.prebivalstvo$stevilo.nesrec))
nesrece.prebivalstvo$stevilo.nesrec <- gsub("p","",as.character(nesrece.prebivalstvo$stevilo.nesrec))

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

zadnja.n <- left_join(nesrece.prebivalstvo, Skupaj2, by = c('leto', 'obmocje'))

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

nes <- unlist(nesrece.prebivalstvo$stevilo.nesrec)
nesrece.prebivalstvo$stevilo.nesrec <-  as.numeric(gsub(",", "", nes))

class(nesrece.prebivalstvo$pojav.zdr.tezav.pri.m) <- "double"
class(nesrece.prebivalstvo$pojav.zdr.tezav.pri.z) <- "double"
class(nesrece.prebivalstvo$pojav.zdr.tezav.skupaj) <- "double"
nesrece.prebivalstvo2 <- janitor::clean_names(nesrece.prebivalstvo)


