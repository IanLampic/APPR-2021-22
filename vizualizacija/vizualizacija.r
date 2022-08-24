# 3. faza: Vizualizacija podatkov
  options(scipen=999)
  
#Primerjava v Slo v letu 2011 in 2019, skupno
  Slo.skupaj.dve <- Skupaj[Skupaj$obmocje %in% c("SI"), ] %>% filter(spol %in% c("Skupaj"))

  graf1 <- ggplot(data.frame(Slo.skupaj.dve)) +  aes(x = vzrok, y = ljudje.z.boleznijo, fill = leto) + geom_bar(stat="identity") + xlab("Bolezni") + ylab("stevilo") +
                   ggtitle("Bolezni po Sloveniji od 2011 do 2019")
  
  
#Razporeditev stevila ljudi glede na bolezen v letu 2016
  brezEU <- Skupaj[!(Skupaj$obmocje == "EU"),]

  graf2 <- ggplot(
  data = (brezEU %>% filter(spol %in% c("Skupaj")) %>% filter(leto %in% c(2016))) %>%
    dplyr::group_by(obmocje)
  ,
  mapping = aes(
    x = vzrok,
    y = ljudje.z.boleznijo,
    fill = ljudje.z.boleznijo
  )
  ) +
  geom_bar(stat = "identity") +
  facet_wrap(~ obmocje, ncol = 5) +
  labs(x = "bolezni", y = "stevilo bolezni") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Razporeditev stevila ljudi glede na bolezen v letu 2016")

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
  xlab("leto") + 
  ylab("stevilo.nesrec") + 
  ggtitle("Stevilo nesrec, zaradi katerih so bili v bolnici vec kot 4 dni") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

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
  graf42 <- ggplot(aes(x=leto, y=stevilo.nesrec, color=obmocje), data = nesrece.prebivalstvo %>% filter(obmocje %in% c("EU", "FR", "IT", "GB", "ES", "DE"))) + geom_line() + xlab("leto") + ylab("stevilo.nesrec") + ggtitle("stevilo.nesrec.v.drzavah.po.letih")

#Stevilo ljudi z boleznijo po drzavah
  
  pospos <- (Skupaj[!(Skupaj$obmocje == "EU" | Skupaj$obmocje == "DE" | Skupaj$obmocje == "GB" | Skupaj$obmocje == "ES" |  Skupaj$obmocje == "FR" | Skupaj$obmocje == "IT") ,])
  graf5 <- pospos %>%
    ggplot(
      mapping = aes(x = vzrok, y = ljudje.z.boleznijo, color = obmocje)
    ) +
    geom_point(position = position_jitter(width = 0.25)
    ) +
    ggtitle("Stevilo ljudi z boleznijo po drzavah")
 
#Mediana izdatkov za zdravje per capita po drzavah
  zat <- Skupaj %>% filter(leto %in% c(2017,2018,2019)) %>% filter(spol %in% c("Skupaj"))
  s <- unlist(zat$potrosnja)
  zat$potrosnja <-  as.numeric(gsub(",", "", s))
  zat$potrosnja <- type.convert(zat$potrosnja, na.strings = ":", as.is = 0)
  
  graf6 <- zat %>% 
    ggplot(
      mapping = aes(x = obmocje, y = potrosnja)
    ) +
    geom_boxplot() +
    ggtitle("Mediana izdatkov za zdravje per capita po drzavah")

#V katerem letu starosti se v povprecju pojavijo zdravstvene tezave
  nesrece.prebivalstvo$leto2 <- as.character(nesrece.prebivalstvo$leto)
  nesrece.prebivalstvo$pojav.zdr.tezav.skupaj <- type.convert(nesrece.prebivalstvo$pojav.zdr.tezav.skupaj, na.strings = ":", as.is = 0)
  class(nesrece.prebivalstvo$pojav.zdr.tezav.skupaj) <- "Double"
  
  
  graf7 <- ggplot(
    data = nesrece.prebivalstvo %>%
      dplyr::group_by(obmocje)
    ,
    mapping = aes(
      x = leto2,
      y = pojav.zdr.tezav.skupaj,
      fill = (pojav.zdr.tezav.skupaj > 62)
    )
  ) +
    geom_bar(stat = "identity") +
    facet_wrap(~ obmocje, ncol = 4) +
    labs(x = "leto", y = "pojav zdravstvenih tezav") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    ggtitle("V katerem letu starosti se v povprecju pojavijo zdravstvene tezave")
  
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

 Skupaj$bolezni.glede.na.preb <- Skupaj$ljudje.z.boleznijo.skupno / Skupaj$stevilo.prebivalcev
 Skupaj$bolezni.glede.na.preb <- as.double(Skupaj$bolezni.glede.na.preb)
 vec1 = Skupaj %>% filter(obmocje %in% c("SI")) %>% filter(leto %in% c(2016)) %>% filter(spol %in% c("Skupaj"))
 vec2 = Skupaj$vzrok[1:16]
 vec1$bolezni.glede.na.preb <- vec1$bolezni.glede.na.preb * 10000
 
 drugo <- sum(vec1$bolezni.glede.na.preb[3],vec1$bolezni.glede.na.preb[10], vec1$bolezni.glede.na.preb[16], vec1$bolezni.glede.na.preb[15], vec1$bolezni.glede.na.preb[14], vec1$bolezni.glede.na.preb[13])
 vec1$bolezni.glede.na.preb[3] <- drugo
 vec1$vzrok[3] <- "Drugo"
 vec1 <- vec1[-c(10,16,14,13,15),]   

 graf10 <- ggplot(
   data = vec1,
   aes(
     x = "",
     y = bolezni.glede.na.preb,
     fill = vzrok
   )
 ) + 
   geom_bar(stat="identity", width=1) +
   scale_fill_manual("legend", values = c("Bolc" = "darkolivegreen1", "Bold" = "darkolivegreen3", "Bolg" = "darkolivegreen4", "Bolm" = "darkslategrey", "Bolp" =  "gold", "Bolz" = "gold4", "Drugo" = "dimgrey", "End" = "blueviolet", "Nekn" = "darkslategray1", "Dus" = "darkblue", "Mal" = "darkmagenta")) + 
   coord_polar("y", start = 0) + 
   ggtitle("Delitev bolnih ljudi glede na bolezni v Sloveniji leta 2016")

#razmerje med stevilom postelj in ljudmi z boleznijo

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
  
  graf12 <- ggplot(
    data = (potrosnja.bolezni) %>%
      dplyr::group_by(obmocje),
    mapping = aes(
      x=leto, 
      y=razmerje.med.potrosnjo.in.bolnimi.ljudmi, 
      fill = obmocje)
  ) + 
    geom_bar(stat = "identity") + 
    geom_line() +
    facet_wrap(facets = vars(obmocje)) +
    xlab("leto") + 
    ylab("razmerje") + 
    ggtitle("Razmerje med izdatki za zdravstvo in bolnimi ljudmi") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
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
    scale_fill_gradient(low="white", high="blue") + ggtitle('Razmerje med vsoto ljudi z boleznijo in stevilom prebivalstva') + labs(fill = "")

class(nesrece.prebivalstvo$pojav.zdr.tezav.skupaj) <- "double"
