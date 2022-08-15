# 3. faza: Vizualizacija podatkov
options(scipen=999)

#Primerjava v Slo v letu 2011 in 2019, skupno
Slo.skupaj.dve <- samoSlo %>% filter(spol %in% c("Skupaj"))

bolezni <- c("Nekn", "Mal", "Nem", "End", "Duš", "Bolž", "Bolc", "Bold", "Bolp", "Bolk", "Bolm", "Bolg", "Nos", "Nekp","Pri","Sim")

graf1 <- ggplot(data.frame(Slo.skupaj.dve,bolezni)) +  aes(x = bolezni, y = ljudje.z.boleznijo, fill = leto) + geom_bar(stat="identity") + xlab("Bolezni") + ylab("Število") +
                   ggtitle("Bolezni po Sloveniji od 2011 do 2019")

#+ scale_fill_manual("leto", values = c("aliceblue","aquamarine4","bisque4","red","darkblue","coral4","darkgreen","deepskyblue3","gray55"))

print(graf1)
print(graf1+coord_flip())

#Mogoče še kakšna primerjava recimo spolov, malo se poigraj z barvami
#Mogoče lahko da daš samo 2011 in 2019, ampak oba spola in to primerjaš
#Začni razmišlat potem še o povprečjih, max/min, itd.,

graf12 <- ggplot(
  data = (samoSlo %>% filter(spol %in% c("Skupaj")) %>% filter(leto %in% c(2019))) %>%
    group_by(obmocje)
  ,
  mapping = aes(
    x = bolezni,
    y = ljudje.z.boleznijo,
    fill = (ljudje.z.boleznijo > 4250)
  )
) +
  geom_bar(stat = "identity") +
  facet_wrap(~ obmocje, ncol = 3) +
  labs(x = "bolezni", y = "število bolezni") +
  theme(legend.position = "bottom")

    
print(graf12)

#probi naredit tabelo kjer so na x osi bolezni na y pa države

Skupaj %>% filter(spol %in% c("Skupaj")) %>% filter(leto %in% c(2019)) %>%
  ggplot(
    mapping = aes(x = obmocje, y = ljudje.z.boleznijo, fill = vzrok)
  ) +
  geom_bar(
    position = position_dodge()
  )
#Neki naredi glede y osi

#Graf števila nesreč samo evropske unije
graf3 <- ggplot(aes(x=leto, y=število.nesreč, color=obmocje), data = nesrece.prebivalstvo %>% filter(obmocje %in% c("EU"))) + geom_line() + xlab("leto") + ylab("število.nesreč") + ggtitle("število.nesreč.v.državah.po.letih")
plot(graf3)
#Graf števila nesreč vseh držav in EU (ugotovi, kako odstraniti EU)
graf4 <- ggplot(aes(x=leto, y=število.nesreč, color=obmocje), data = nesrece.prebivalstvo %>% filter(spol %in% c("Skupaj"))) + geom_line() + xlab("leto") + ylab("število.nesreč") + ggtitle("število.nesreč.v.državah.po.letih")
plot(graf4)


#Isto k uzgori samo da naredis za pojav zdr tezav
#Še graf:
#5. število nesreč/st prebivalstva
Skupaj$st.nesrec.glede.na.prebivalca <- Skupaj$ljudje.z.boleznijo / Skupaj$stevilo.prebivalcev 
vsota <- Skupaj %>%
  group_by(obmocje, leto) %>%
  summarize(
    st.nesrec.glede.na.prebivalca = sum(st.nesrec.glede.na.prebivalca, na.rm = TRUE)
  )

graf9 <- ggplot(aes(x=leto, y=st.nesrec.glede.na.prebivalca, color=obmocje), data = vsota %>% filter(obmocje %in% c("SI", "EU", "DE", "FR", "BE", "HR"))) + geom_line() + xlab("leto") + ylab("vsota vseh ljudi z boleznijo") + ggtitle("vsota ljudi, ki so bolni po letih")
plot(graf9)
#Mogoče kej glede Francije, k je deljneje z 0

#6. pojav zdr tezav (geom_line) po državah, mogoče skupaj moški in ženske al pa ce rata tist k je več grafov neaenkrat
#7. število prebivalcev glede na postelje, mgoče meja nad 1, se prai vsaj ena postelja na prebivalca
Skupaj$preb.glede.na.postelje <- Skupaj$število.postelj / Skupaj$stevilo.prebivalcev
vsotap <- Skupaj %>%
  group_by(obmocje, leto) %>%
  summarize(
    preb.glede.na.postelje = sum(preb.glede.na.postelje, na.rm = TRUE)
  )

graf10 <- ggplot(aes(x=leto, y=preb.glede.na.postelje, color=obmocje), data = vsotap %>% filter(obmocje %in% c("SI", "EU", "DE", "FR", "BE", "HR"))) + geom_line() + xlab("leto") + ylab("vsota vseh ljudi z boleznijo") + ggtitle("vsota ljudi, ki so bolni po letih")
plot(graf10)

#8.sum vseh bolezni po država(preveč držav, zato sem si jih izbral 5)

Vsota.skupaj <- Skupaj %>%
group_by(obmocje, leto) %>%
  summarize(
    ljudje.z.boleznijo = sum(ljudje.z.boleznijo, na.rm = TRUE)
  )

graf5 <- ggplot(aes(x=leto, y=ljudje.z.boleznijo, color=obmocje), data = Vsota.skupaj %>% filter(obmocje %in% c("SI", "EU", "DE", "GB", "BE"))) + geom_line() + xlab("leto") + ylab("vsota vseh ljudi z boleznijo") + ggtitle("vsota ljudi, ki so bolni po letih")
plot(graf5)

#9. mogoče rast prebivalstva(spet za 5 držav)
graf6 <- ggplot(aes(x=leto, y=stevilo.prebivalcev, color=obmocje), data = Prebivalstvo.m %>% filter(leto %in% c(2011,2012,2013,2014,2015,2016,2017,2018,2019)) %>% filter(obmocje %in% c("Slovenia", "Denmark", "Germany (until 1990 former territory of the FRG)", "France", "Belgium"))) + 
  geom_line() + xlab("leto") + ylab("stevilo moskih") + ggtitle("stevilo.moskih.po.letih")
plot(graf6)

graf7 <- ggplot(aes(x=leto, y=stevilo.prebivalcev, color=obmocje), data = Prebivalstvo.z %>% filter(leto %in% c(2011,2012,2013,2014,2015,2016,2017,2018,2019)) %>% filter(obmocje %in% c("Slovenia", "Denmark", "Germany (until 1990 former territory of the FRG)", "France", "Belgium"))) + 
  geom_line() + xlab("leto") + ylab("stevilo zensk") + ggtitle("stevilo.zensk.po.letih")
plot(graf7)

graf8 <- ggplot(aes(x=leto, y=stevilo.prebivalcev, color=obmocje), data = Prebivalstvo.s %>% filter(leto %in% c(2011,2012,2013,2014,2015,2016,2017,2018,2019)) %>% filter(obmocje %in% c("Slovenia", "Denmark", "Germany (until 1990 former territory of the FRG)", "France", "Belgium"))) + 
  geom_line() + xlab("leto") + ylab("skupno stevilo") + ggtitle("skupno.stevilo.ljudi.po.letih")
plot(graf8)
#Mogoče še kako bi jih dal skupaj + kej bolj elegantega glede let-zapis

#10. bolezni v državi, kjer je največ bolnih
#11. postelje glede na bolezni
postelje.skupno.bolezni <- left_join(Vsota.skupaj, Postelje.v.bolnišnicah, by=c("obmocje", "leto"))
postelje.skupno.bolezni$razmerje <- postelje.skupno.bolezni$število.postelj / postelje.skupno.bolezni$ljudje.z.boleznijo
Vsotapb <- postelje.skupno.bolezni %>%
  group_by(obmocje, leto) %>%
  summarize(
    razmerje = sum(razmerje, na.rm = TRUE)
  )
Vsotapb<- Vsotapb[order(Vsotapb$leto),]

graf11 <- ggplot(aes(x=leto, y=razmerje, color=obmocje), data = head(Vsotapb,217) %>% filter(obmocje %in% c("SI", "EU", "DE", "GB", "BE"))) + geom_line() + xlab("leto") + ylab("vsota vseh ljudi z boleznijo") + ggtitle("število.postelj.glede.na.bolezni")
plot(graf11)

#12. izdatki za zdravje per capita glede na bolezni
potrošnja.bolezni <- left_join(Vsota.skupaj, Skupaj$potrošnja %>% filter(spol %in% c('Skupaj')), by=c("obmocje", "leto"))
#to razmisli še kako bi
