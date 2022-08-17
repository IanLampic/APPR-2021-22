# 3. faza: Vizualizacija podatkov
  options(scipen=999)

#Olepšana y os
  fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
  }
#Primerjava v Slo v letu 2011 in 2019, skupno
  Slo.skupaj.dve <- samoSlo %>% filter(spol %in% c("Skupaj"))

  graf1 <- ggplot(data.frame(Slo.skupaj.dve)) +  aes(x = vzrok, y = ljudje.z.boleznijo, fill = leto) + geom_bar(stat="identity") + xlab("Bolezni") + ylab("stevilo") +
                   ggtitle("Bolezni po Sloveniji od 2011 do 2019")

  print(graf1)
  print(graf1+coord_flip())

#Mogoce se kaksna primerjava recimo spolov, malo se poigraj z barvami
#Mogoce lahko da das samo 2011 in 2019, ampak oba spola in to primerjas
#Zacni razmislat potem se o povprecjih, max/min, itd.,
  brezEU <- Skupaj[!(Skupaj$obmocje == "EU"),]

  graf2 <- ggplot(
  data = (brezEU %>% filter(spol %in% c("Skupaj")) %>% filter(leto %in% c(2016))) %>%
    group_by(obmocje)
  ,
  mapping = aes(
    x = vzrok,
    y = ljudje.z.boleznijo,
    fill = (ljudje.z.boleznijo > 4250)
  )
  ) +
  geom_bar(stat = "identity") +
  facet_wrap(~ obmocje, ncol = 3) +
  labs(x = "bolezni", y = "stevilo bolezni") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45))

  print(graf2)

#Graf stevila nesrec brez Evropske unije, Nemčije, Italije, Španije, VB, Francije   (da se bolj nazorno vidi)

  graf3 <- ggplot(
  data = (nesrece.prebivalstvo[!(nesrece.prebivalstvo$obmocje == "EU" | nesrece.prebivalstvo$obmocje == "DE" | nesrece.prebivalstvo$obmocje == "GB" | nesrece.prebivalstvo$obmocje == "ES" |  nesrece.prebivalstvo$obmocje == "FR" | nesrece.prebivalstvo$obmocje == "IT") ,]) %>%
    group_by(obmocje),
  mapping = aes(
    x=leto, 
    y=stevilo.nesrec, 
    color = obmocje)
  ) + 
  geom_line() + 
  facet_wrap(facets = vars(obmocje)) +
  scale_y_continuous(labels=fancy_scientific) +
  xlab("leto") + 
  ylab("stevilo.nesrec") + 
  ggtitle("stevilo.nesrec.v.drzavah.po.letih") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  print(graf3)
  
  #in pa skupaj na enem grafu (zaniv padec Nizozemske)
  graf32 <- ggplot(aes(x=leto, y=stevilo.nesrec, color=obmocje), data = nesrece.prebivalstvo[!(nesrece.prebivalstvo$obmocje == "EU" | nesrece.prebivalstvo$obmocje == "DE" | nesrece.prebivalstvo$obmocje == "GB" | nesrece.prebivalstvo$obmocje == "ES" |  nesrece.prebivalstvo$obmocje == "FR" | nesrece.prebivalstvo$obmocje == "IT") ,] ) + geom_line() + xlab("leto") + ylab("stevilo.nesrec") + ggtitle("stevilo.nesrec.v.drzavah.po.letih")
  plot(graf32)
  
#Stevilo nesrec v zgoraj izvzetih drzavah
  
  graf4 <- ggplot(
    data = (nesrece.prebivalstvo %>% filter(obmocje %in% c("EU", "FR", "IT", "GB", "ES", "DE"))) %>%
      group_by(obmocje),
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

  print(graf4)

#Stevilo ljudi z boleznijo po drzavah
  
  pospos <- (Skupaj[!(Skupaj$obmocje == "EU" | Skupaj$obmocje == "DE" | Skupaj$obmocje == "GB" | Skupaj$obmocje == "ES" |  Skupaj$obmocje == "FR" | Skupaj$obmocje == "IT") ,])
  graf5 <- pospos %>%
    ggplot(
      mapping = aes(x = vzrok, y = ljudje.z.boleznijo, color = obmocje)
    ) +
    geom_point(position = position_jitter(width = 0.25)
    ) +
    ggtitle("Stevilo ljudi z boleznijo po drzavah")
  
  print(graf5)  
  
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

  print(graf6)

#V katerem letu starosti se v povprecju pojavijo zdravstvene tezave
  nesrece.prebivalstvo$leto2 <- as.character(nesrece.prebivalstvo$leto)
  
  
  graf7 <- ggplot(
    data = nesrece.prebivalstvo  %>% filter(obmocje %in% c("SI", "GB", "EU", "DE")) %>%
      group_by(obmocje)
    ,
    mapping = aes(
      x = leto2,
      y = pojav.zdr.tezav.skupaj,
      fill = (pojav.zdr.tezav.skupaj > 62)
    )
  ) +
    geom_bar(stat = "identity") +
    facet_wrap(~ obmocje, ncol = 3) +
    labs(x = "leto", y = "pojav.zdr.tezav") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    ggtitle("V katerem letu starosti se v povprecju pojavijo zdravstvene tezave")
  
  plot(graf7)
  #samo še nekaj glede yosi

#graf stevila nesrec glede na stevilo prebivalstva
  
  Skupaj$st.nesrec.glede.na.prebivalca <- Skupaj$ljudje.z.boleznijo / Skupaj$stevilo.prebivalcev 
  vsota <- Skupaj %>%
  group_by(obmocje, leto) %>%
  summarize(
    st.nesrec.glede.na.prebivalca = sum(st.nesrec.glede.na.prebivalca, na.rm = TRUE)
  )
  vsota[vsota == 0] <- NA
  graf8 <- ggplot(aes(x=leto, y=st.nesrec.glede.na.prebivalca, color=obmocje), data = vsota) + geom_line() + xlab("leto") + ylab("vsota vseh ljudi z boleznijo") + ggtitle("vsota ljudi, ki so bolni po letih")
  plot(graf8)
  #Zanimivo zakaj Estoniji pade tako zelo v zadnjem letu

#stevilo prebivalcev glede na postelje,
  
  Skupaj$preb.glede.na.postelje <- Skupaj$stevilo.postelj / Skupaj$stevilo.prebivalcev / 16
  vsotap <- Skupaj %>%
  group_by(obmocje, leto) %>%
  summarize(
    preb.glede.na.postelje = sum(preb.glede.na.postelje, na.rm = TRUE)
  )
  vsotap[vsotap == 0] <- NA
  graf9 <- ggplot(
    data = vsotap,
    aes(x=leto, y=preb.glede.na.postelje, color = obmocje), 
    ) + 
      geom_line() + 
      xlab("leto") + 
      ylab("stevilo prebivalcev glede na postelje") + 
      ggtitle("kaksno je razmerje med stevilom postelj v bolnisnicah in stevilo prebivalcev")
  
  plot(graf9)
  #Nemcija ma kr dobro, Bulgariji se veca, Estoniji ful pade, Veliki Britaniji zelo pade
  
#tortni diagram bolnih ljudi glede na vzroke bolezni

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
   coord_polar("y", start = 0) + 
   ggtitle("Delitev bolnih ljudi glede na bolezni v Sloveniji leta 2016")
 
 plot(graf10)

#razmerje med stevilom postelj in ljudmi z boleznijo

  novaa <- Skupaj[ , c("obmocje", "leto", "ljudje.z.boleznijo.skupno","stevilo.postelj")] 
  novaa = novaa[seq(1, nrow(novaa), 48), ]
  novaa[novaa == 0] <- NA
  novaa$razmerje <- novaa$stevilo.postelj / novaa$ljudje.z.boleznijo.skupno
  novaa<- novaa[order(novaa$leto),]
 
  graf11 <- ggplot(aes(x=leto, y=razmerje, color=obmocje), data = novaa) + geom_line() + xlab("leto") + ylab("razmerje med stevilom postelj in vseh ljudi z boleznijo") + ggtitle("stevilo.postelj.glede.na.ljudi.z.boleznijo")
  plot(graf11)
  #Pri večini trend navzdol, mozno je pojav covida, ko je veliko vec ljudi bilo (vsaj zabelezeno) bolni

#Razmerje med izdatki za zdravstvo in bolnimi ljudmi

  potr <- Skupaj[ , c("obmocje", "leto", "potrosnja")] %>% filter(leto %in% c(2017,2018,2019))
  potr = potr[seq(1, nrow(potr), 48), ]
  potrosnja.bolezni <- left_join(novaa, potr, by=c("obmocje", "leto"))
  potrosnja.bolezni = na.omit(potrosnja.bolezni)
  potrosnja.bolezni$razmerje.med.potrosnjo.in.bolnimi.ljudmi <- potrosnja.bolezni$potrosnja / potrosnja.bolezni$ljudje.z.boleznijo.skupno
  
  graf12 <- ggplot(
    data = (potrosnja.bolezni) %>%
      group_by(obmocje),
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
  print(graf12)
  #Islandija in Luksemburg imata zelo dobro razmerje, torej, prebivalstvo veliko izdatkov nameni zdravju, torej lahko bolj ucinkovito zdravijo bolezni, odraza se boljsi standard zivljenja teh dveh drzav v povprecju z drugimi

  
#Mapping  
  #Prvi del je zaradi poimenovanja v uvozu
  manjkajoci <- primerjava[is.na(primerjava$leto), ]
  
  df = data.frame(obmocje = manjkajoci$obmocje, Country = c("Albania", 
                                                           "Andorra",
                                                           "Bosnia and Herzegovina",
                                                           "Belarus",
                                                           "Czechia",
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
  
  Novosku <- Skupaj %>% filter(spol %in% c("Skupaj")) %>% filter(leto %in% c(2016)) %>% filter(vzrok %in% c("Nos"))
  Novosku <- Novosku %>% left_join(df) %>% mutate(Country=ifelse(is.na(obmocje), Country, obmocje))
  Novosku <- Novosku %>% dplyr::select(-obmocje)
  Novosku$razmerje <- Novosku$ljudje.z.boleznijo.skupno / Novosku$stevilo.prebivalcev
  n2 <- Novosku %>% group_by(sovereignt = Country)
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
print(graf13)  

  