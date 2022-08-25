# 3. faza: Vizualizacija podatkov
  options(scipen=999)
  
#Primerjava v Slo v letu 2011 in 2019, skupno
  Slo.skupaj.dve <- Skupaj[Skupaj$obmocje %in% c("SI"), ] %>% filter(spol %in% c("Skupaj"))

  graf1 <- ggplot(data.frame(Slo.skupaj.dve)) +  aes(x = leto, y = ljudje.z.boleznijo, color = vzrok) + geom_line() + xlab("") + ylab("Število bolezni") +
                   ggtitle("Število bolezni po Sloveniji od 2011 do 2019") + scale_x_continuous(breaks= c(2011,2012,2013,2014,2015,2016,2017,2018,2019)) + scale_color_discrete(name = 'Bolezni')

#Razporeditev stevila ljudi glede na bolezen v letu 2016
  brezEU <- Skupaj[!(Skupaj$obmocje == "EU"),]
  
  #brezEU$vzrok[brezEU$vzrok =="Bolc"] <- "Bolezni cirkulacijskega sistema"        
  #brezEU$vzrok[brezEU$vzrok == "Mal"] = "MalMaligne neoplazme"
  #brezEU$vzrok[brezEU$vzrok == "Bolp"] = "Bolezni prebavnega sistema"
  #brezEU$vzrok[brezEU$vzrok == "Bolz"] = "Bolezni živčnega sistema in čutnih organov"
  #brezEU$vzrok[brezEU$vzrok == "End"] = "Endokrine, prehranske in presnovne bolezni"
  #brezEU$vzrok[brezEU$vzrok == "Bolg"] = "Bolezni genitourinarnega sistema"
  #brezEU$vzrok[brezEU$vzrok == "Dus"] = "Duševne in vedenjske motnje"
  #brezEU$vzrok[brezEU$vzrok == "Nekn"] = "Nekatere nalezljive in parazitske bolezni"
  #brezEU$vzrok[brezEU$vzrok == "Bolk"] = "Bolezni kože in podkožnega tkiva"
  #brezEU$vzrok[brezEU$vzrok == "Bolm"] = "Bolezni mišično-skeletnega sistema in vezivnega tkiva"
  #brezEU$vzrok[brezEU$vzrok == "Neks"] = "Določena stanja, ki izvirajo iz perinatalnega obdobja"
  #brezEU$vzrok[brezEU$vzrok == "Nem"] = "Nemaligne neoplazme"
  #brezEU$vzrok[brezEU$vzrok == "Nos"] = "Nosečnost, porod in puerperij"
  #brezEU$vzrok[brezEU$vzrok == "Pri"] = "Prirojene malformacije, deformacije in kromosomske nepravilnosti"
  #brezEU$vzrok[brezEU$vzrok == "Sim"] = "Drugi simptomi"
  #brezEU$vzrok[brezEU$vzrok == "Bold"] = "Bolezni dihalnega sistema"
  

  graf2 <- ggplot(
  data = (brezEU %>% filter(spol %in% c("Skupaj")) %>% filter(leto %in% c(2017))) %>% filter(obmocje %in% c("DE", "FR", "IT", "ES", "GB", "PL")) %>% filter(vzrok %in% c("Bolc", "Bolp", "Bold", "Bolz", "Dus", "Mal", "Sim")) %>%
    dplyr::group_by(obmocje)
  ,
  mapping = aes(
    x = vzrok,
    y = ljudje.z.boleznijo,
    fill = ljudje.z.boleznijo
  )
  ) +
  geom_bar(stat = "identity") +
  facet_wrap(~ obmocje, ncol = 3) +
  labs(x = "", y = "Število bolezni") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "None") +
  ggtitle("")


#Graf stevila nesrec brez Evropske unije, Nemčije, Italije, Španije, VB, Francije   (da se bolj nazorno vidi)

  graf3 <- ggplot(
  data = (nesrece.prebivalstvo[!(nesrece.prebivalstvo$obmocje == "EU" | nesrece.prebivalstvo$obmocje == "DE" | nesrece.prebivalstvo$obmocje == "GB" | nesrece.prebivalstvo$obmocje == "ES" |  nesrece.prebivalstvo$obmocje == "FR" | nesrece.prebivalstvo$obmocje == "IT") ,]) %>%
    dplyr::group_by(obmocje),
  mapping = aes(
    x=leto, 
    y=stevilo.nesrec, 
    color = obmocje)
  ) + 
  geom_line() + 
  facet_wrap(facets = vars(obmocje)) +
  xlab("") + 
  ylab("") + 
  ggtitle("Število nesreč, zaradi katerih so bili v bolnici vec kot 4 dni") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_x_continuous(breaks= c(2011,2012,2013,2014,2015,2016,2017,2018,2019)) + scale_color_discrete(name = "")

  #in pa skupaj na enem grafu (zaniv padec Nizozemske)
  graf32 <- ggplot(aes(x=leto, y=stevilo.nesrec, color=obmocje), data = nesrece.prebivalstvo[!(nesrece.prebivalstvo$obmocje == "EU" | nesrece.prebivalstvo$obmocje == "DE" | nesrece.prebivalstvo$obmocje == "GB" | nesrece.prebivalstvo$obmocje == "ES" |  nesrece.prebivalstvo$obmocje == "FR" | nesrece.prebivalstvo$obmocje == "IT") ,] ) + geom_line() + xlab("leto") + ylab("stevilo.nesrec") + ggtitle("stevilo.nesrec.v.drzavah.po.letih")
  
#Stevilo nesrec v zgoraj izvzetih drzavah
  
  graf4 <- ggplot(
    data = (nesrece.prebivalstvo %>% filter(obmocje %in% c("EU", "FR", "IT", "GB", "ES", "DE"))) %>%
      dplyr::group_by(obmocje),
    mapping = aes(
      x=leto, 
      y=stevilo.nesrec, 
      color = obmocje)
  ) + 
    geom_line() + 
    facet_wrap(facets = vars(obmocje)) +
    xlab("leto") + 
    ylab("stevilo.nesrec") + 
    ggtitle("stevilo.nesrec.v.drzavah.po.letih") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  #se skupaj
  graf42 <- ggplot(aes(x=leto, y=stevilo.nesrec, color=obmocje), data = nesrece.prebivalstvo %>% filter(obmocje %in% c("EU", "FR", "IT", "GB", "ES", "DE"))) + geom_line() + xlab("") + ylab("") + ggtitle("Število nesreč v državah od leta 2011 do 2019") + scale_x_continuous(breaks= c(2011,2012,2013,2014,2015,2016,2017,2018,2019)) + scale_color_discrete(name = "")


#Stevilo ljudi z boleznijo po drzavah
  
  pospos <- (vsota.s[!(vsota.s$obmocje == "European Union - 28 countries (2013-2020)" | vsota.s$obmocje == "DE" | vsota.s$obmocje == "GB" | vsota.s$obmocje == "ES" |  vsota.s$obmocje == "FR" | vsota.s$obmocje == "IT") ,]) %>% filter(spol %in% c("Skupaj"))
  graf5 <- pospos %>%
    ggplot(
      mapping = aes(x = leto, y = ljudje.z.boleznijo.skupno, color = obmocje)
    ) +
    geom_point(position = position_jitter(width = 0.25)
    ) + xlab('') + ylab('') +
    ggtitle("Skupno število ljudi z boleznijo po državah")  + scale_x_continuous(breaks= c(2011,2012,2013,2014,2015,2016,2017,2018,2019))

#Mediana izdatkov za zdravje per capita po drzavah
  zat <- Skupaj %>% filter(leto %in% c(2017,2018,2019)) %>% filter(spol %in% c("Skupaj"))
  s <- unlist(zat$potrosnja)
  zat$potrosnja <-  as.numeric(gsub(",", "", s))
  zat$potrosnja <- type.convert(zat$potrosnja, na.strings = ":", as.is = 0)
  
  graf6 <- zat %>% 
    ggplot(
      mapping = aes(x = obmocje, y = potrosnja)
    ) +
    geom_boxplot() + xlab("") + ylab("Izdatki per capita") +
    ggtitle("Mediana izdatkov za zdravje per capita po državah")

#V katerem letu starosti se v povprecju pojavijo zdravstvene tezave
  nesrece.prebivalstvo$leto2 <- as.character(nesrece.prebivalstvo$leto)
  nesrece.prebivalstvo$pojav.zdr.težav.skupaj <- type.convert(nesrece.prebivalstvo$pojav.zdr.težav.skupaj, na.strings = ":", as.is = 0)
  class(nesrece.prebivalstvo$pojav.zdr.težav.skupaj) <- "Double"
  
  graf7 <- ggplot(
    data = nesrece.prebivalstvo %>% filter(obmocje %in% c("DE", "FR", "IT", "ES", "GB", "PL", "SI", "AT")) %>%
      dplyr::group_by(obmocje)
    ,
    mapping = aes(
      x = leto2,
      y = pojav.zdr.težav.skupaj,
      fill = (pojav.zdr.težav.skupaj > 60.5)
    )
  ) +
    geom_bar(stat = "identity") +
    facet_wrap(~ obmocje, ncol = 4) +
    labs(x = "", y = "Povprečna starost pojava hujših zdravstvenih težav") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    ggtitle("Povprečno leto starosti, ko se pojavijo hujše zdravstvene težave")

#graf stevila nesrec glede na stevilo prebivalstva
  
  vsota <- Skupaj
  vsota[vsota == 0] <- NA
  vsota$st.nesrec.glede.na.prebivalca <- vsota$ljudje.z.boleznijo / vsota$stevilo.prebivalcev 
  vsota <- vsota %>%
  dplyr::group_by(obmocje, leto) %>%
  dplyr::summarize(
    st.nesrec.glede.na.prebivalca = sum(st.nesrec.glede.na.prebivalca, na.rm = TRUE)
  )
  vsota[vsota == 0] <- NA
  graf8 <- ggplot(aes(x=leto, y=st.nesrec.glede.na.prebivalca, color=obmocje), data = vsota) + geom_line() + xlab("leto") + ylab("st.nesrec.glede.na.prebivalca") + ggtitle("st.nesrec.glede.na.prebivalca")
  #Zanimivo zakaj Estoniji pade tako zelo v zadnjem letu

#stevilo prebivalcev glede na postelje,
  
  Skupaj2$preb.glede.na.postelje <- Skupaj2$stevilo.postelj / Skupaj2$stevilo.prebivalcev
  vsotap <- Skupaj2 %>%
  dplyr::group_by(obmocje, leto) %>%
  dplyr::summarize(
    preb.glede.na.postelje = sum(preb.glede.na.postelje, na.rm = TRUE)
  )
  vsotap[vsotap == 0] <- NA
  vsotap[] <- Map(function(x) replace(x, is.infinite(x), NA), vsotap)
  graf9 <- ggplot(
    data = vsotap,
    aes(x=leto, y=preb.glede.na.postelje, color = obmocje), 
    ) + 
      geom_line() + 
      xlab("leto") + 
      ylab("stevilo prebivalcev glede na postelje") + 
      ggtitle("Kaksno je razmerje med stevilom postelj v bolnisnicah in stevilo prebivalcev")
  
  #Nemcija ma kr dobro, Bulgariji se veca, Estoniji ful pade, Veliki Britaniji zelo pade

#tortni diagram bolnih ljudi glede na vzroke bolezni
                                      
  
 Skupaj$bolezni.glede.na.preb <- Skupaj$ljudje.z.boleznijo / Skupaj$ljudje.z.boleznijo.skupno
 Skupaj$bolezni.glede.na.preb <- as.double(Skupaj$bolezni.glede.na.preb)
 vec1 = Skupaj %>% filter(obmocje %in% c("SI")) %>% filter(leto %in% c(2016)) %>% filter(spol %in% c("Skupaj"))
 vec2 = Skupaj$vzrok[1:16]
 vec1 %>%
   arrange(desc(ljudje.z.boleznijo)) %>%
   mutate(prop = percent(ljudje.z.boleznijo / (ljudje.z.boleznijo.skupno))) -> vec1
 
 drugo <- sum(vec1$bolezni.glede.na.preb[16],vec1$bolezni.glede.na.preb[15], vec1$bolezni.glede.na.preb[14], vec1$bolezni.glede.na.preb[13], vec1$bolezni.glede.na.preb[12], vec1$bolezni.glede.na.preb[11], vec1$bolezni.glede.na.preb[10])
 vec1$bolezni.glede.na.preb[3] <- drugo
 vec1$vzrok[3] <- "Drugo"
 vec1 <- vec1[-c(10,11,12,13,14,15,16),]   
 
 vec1$vzrok[vec1$vzrok =="Bolc"] <- "Bolezni cirkulacijskega sistema"        
 vec1$vzrok[vec1$vzrok == "Mal"] = "Maligne neoplazme"
vec1$vzrok[vec1$vzrok == "Drugo"] = "Drugo"
vec1$vzrok[vec1$vzrok == "Bolp"] = "Bolezni prebavnega sistema"
vec1$vzrok[vec1$vzrok == "Bolz"] = "Bolezni živčnega sistema in čutnih organov"
vec1$vzrok[vec1$vzrok == "End"] = "Endokrine, prehranske in presnovne bolezni"
vec1$vzrok[vec1$vzrok == "Bolg"] = "Bolezni genitourinarnega sistema"
vec1$vzrok[vec1$vzrok == "Dus"] = "Duševne in vedenjske motnje"
vec1$vzrok[vec1$vzrok == "Nekn"] = "Nekatere nalezljive in parazitske bolezni"
 
 library(ggrepel)
 
 graf10 <- ggplot(
   data = vec1,
   aes(
     x = "",
     y = bolezni.glede.na.preb,
     fill = vzrok
   )
 ) + 
   geom_bar(stat="identity", width=1) +
   #scale_color_manual(values = c("Bolc" = "darkolivegreen1", "Bold" = "darkolivegreen3", "Bolg" = "darkolivegreen4", "Bolm" = "darkslategrey", "Bolp" =  "gold", "Bolz" = "gold4", "Drugo" = "dimgrey", "End" = "blueviolet", "Nekn" = "darkslategray1", "Dus" = "darkblue", "Mal" = "darkmagenta")) +
   
   coord_polar("y", start = 0) + xlab("") + ylab("") + 
   geom_text(aes(x=1.6, label=paste0(round(bolezni.glede.na.preb*100), "%")),
             position = position_stack(vjust=0.5)) +
   ggtitle("Delitev števila ljudi glede na bolezni v Sloveniji leta 2016") + 
   theme_void() + scale_fill_discrete('Bolezni')
                                                                                   
   


#razmerje med stevilom postelj in ljudmi z boleznijo
#values = c("Bolc" = "darkolivegreen1", "Bold" = "darkolivegreen3", "Bolg" = "darkolivegreen4", "Bolm" = "darkslategrey", "Bolp" =  "gold", "Bolz" = "gold4", "Drugo" = "dimgrey", "End" = "blueviolet", "Nekn" = "darkslategray1", "Dus" = "darkblue", "Mal" = "darkmagenta")
  novaa <- Skupaj2[ , c("obmocje", "leto", "ljudje.z.boleznijo.skupno","stevilo.postelj")] 
  novaa = novaa[seq(1, nrow(novaa), 16), ]
  novaa[novaa == 0] <- NA
  novaa$razmerje <- novaa$stevilo.postelj / novaa$ljudje.z.boleznijo.skupno
  novaa<- novaa[order(novaa$leto),]
 
  graf11 <- ggplot(aes(x=leto, y=razmerje, color=obmocje), data = novaa) + geom_line() + xlab("leto") + ylab("razmerje med stevilom postelj in vseh ljudi z boleznijo") + ggtitle("stevilo.postelj.glede.na.ljudi.z.boleznijo")
  #Pri večini trend navzdol, mozno je pojav covida, ko je veliko vec ljudi bilo (vsaj zabelezeno) bolni

#Razmerje med izdatki za zdravstvo in bolnimi ljudmi

  potr <- Skupaj2 %>% filter(vzrok %in% c("Nekn"))
  potr <- potr[ , c("obmocje", "leto", "potrosnja")]
  novaa <- novaa[seq(1, nrow(potr), 3), ]
  potrosnja.bolezni <- left_join(potr, novaa) %>% filter(leto %in% c(2017,2018,2019))
  #potrosnja.bolezni <- potrosnja.bolezni[seq(1, nrow(potr), 3), ]
  potrosnja.bolezni = na.omit(potrosnja.bolezni)
  potrosnja.bolezni$razmerje.med.potrosnjo.in.bolnimi.ljudmi <- potrosnja.bolezni$potrosnja / potrosnja.bolezni$ljudje.z.boleznijo.skupno
  
  novagraf12 <- potrosnja.bolezni 
  novagraf12<- novagraf12[!(novagraf12$obmocje == "Iceland" | novagraf12$obmocje == "Luxembourg"),]
  
  graf12 <- ggplot(
    data = (novagraf12) %>% 
      dplyr::group_by(obmocje),
    mapping = aes(
      x=leto, 
      y=razmerje.med.potrosnjo.in.bolnimi.ljudmi, 
      fill = obmocje)
  ) + 
    geom_bar(stat = "identity") + 
    facet_wrap(facets = vars(obmocje)) +
    xlab("") + 
    ylab("Razmerje") + 
    ggtitle("Razmerje med izdatki za zdravstvo in bolnimi ljudmi") +
    theme(legend.position = "None", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_fill_discrete(name = "")
  #Islandija in Luksemburg imata zelo dobro razmerje, torej, prebivalstvo veliko izdatkov nameni zdravju, torej lahko bolj ucinkovito zdravijo bolezni, odraza se boljsi standard zivljenja teh dveh drzav v povprecju z drugimi


graf122 <- ggplot(
  data = (potrosnja.bolezni) %>% filter(obmocje %in% c("Iceland", "Luxembourg")) %>%
    dplyr::group_by(obmocje),
  mapping = aes(
    x=leto, 
    y=razmerje.med.potrosnjo.in.bolnimi.ljudmi, 
    fill = obmocje)
) + 
  geom_bar(stat = "identity") + 
  facet_wrap(facets = vars(obmocje)) +
  xlab("") + 
  ylab("Razmerje") + 
  ggtitle("Razmerje med izdatki za zdravstvo in bolnimi ljudmi") +
  theme(legend.position = "None", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_fill_discrete(name = "")


#Islandija in Luksemburg imata zelo dobro razmerje, torej, prebivalstvo veliko izdatkov nameni zdravju, torej lahko bolj ucinkovito zdravijo bolezni, odraza se boljsi standard zivljenja teh dveh drzav v povprecju z drugimi

  
#Mapping  
  #Prvi del je zaradi poimenovanja v uvozu
  manjkajoci <- primerjava[is.na(primerjava$leto), ]
  
  df = data.frame(obmocje = manjkajoci$obmocje, Country = c("Albania", 
                                                           "Andorra",
                                                           "Bosnia and Herzegovina",
                                                           "Belarus",
                                                           "Kosovo",
                                                           "Liechtenstein",
                                                           "Monaco",
                                                           "Moldova",
                                                           "Macedonia",
                                                           "Malta",
                                                           "Montenegro",
                                                           "Russia",
                                                           "San Marino",
                                                           "Republic of Serbia",
                                                           "Ukraine",
                                                           "Vatican"))
  
  Novosku <- Novosku %>% left_join(df) %>% mutate(Country=ifelse(is.na(obmocje), Country, obmocje))
  Novosku <- Novosku %>% dplyr::select(-obmocje)
  Novosku$razmerje <- Novosku$ljudje.z.boleznijo.skupno / Novosku$stevilo.prebivalcev
  n2 <- Novosku %>% dplyr::group_by(sovereignt = Country)
  m <- merge(Europe, n2)
  graf13 <- ggplot(m) +
    geom_sf() +
    coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) +
    aes(fill = razmerje) +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    ) +
    scale_fill_gradient(low="white", high="blue") + ggtitle('Razmerje med vsoto ljudi z boleznijo in številom prebivalstva') + labs(fill = "")

class(nesrece.prebivalstvo$pojav.zdr.težav.skupaj) <- "double"
