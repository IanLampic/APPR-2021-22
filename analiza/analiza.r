# 4. faza: Napredna analiza podatkov
#glej po tem kako so si države podobne po strukturi bolezni, osredotočim se kje imajo iste bolezni
obrisi = function(podatki, hc = TRUE, od = 2, do = NULL) {
  n = nrow(podatki)
  if (is.null(do)) {
    do = n - 1
  }
  
  razdalje = dist(podatki)
  
  k.obrisi = tibble()
  for (k in od:do) {
    if (hc) {
      o.k = hclust(razdalje) %>%
        cutree(k) %>%
        silhouette(razdalje)
    } else {
      set.seed(42) # zato, da so rezultati ponovljivi
      o.k = kmeans(podatki, k)$cluster %>%
        silhouette(razdalje)
    }
    k.obrisi = k.obrisi %>% bind_rows(
      tibble(
        k = rep(k, n),
        obrisi = o.k[, "sil_width"]
      )
    )
  }
  k.obrisi$k = as.ordered(k.obrisi$k)
  
  k.obrisi
}

obrisi.povprecje = function(k.obrisi) {
  k.obrisi.povprecje = k.obrisi %>%
    dplyr::group_by(k) %>%
    dplyr::summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    dplyr::summarize(k = min(k)) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

diagram.obrisi = function(k.obrisi) {
  ggplot() +
    geom_boxplot(
      data = k.obrisi,
      mapping = aes(x = k, y = obrisi)
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = k, y = obrisi),
      color = "red"
    ) +
    geom_line(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = as.integer(k), y = obrisi),
      color = "red"
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi) %>%
        filter(obrisi == max(obrisi)) %>%
        filter(k == min(k)),
      mapping = aes(x = k, y = obrisi),
      color = "blue"
    ) +
    xlab("število skupin (k)") +
    ylab("obrisi (povprečje obrisov)") +
    ggtitle(paste("Maksimalno povprečje obrisov pri k =", obrisi.k(k.obrisi))) +
    theme_classic()
}

bolezni <- analiza %>% dplyr::select(leto, obmocje, spol, vzrok, ljudje.z.boleznijo) 
  bolezni[is.na(bolezni)] = 0
  bolezni <- bolezni %>%
  filter(leto %in% c(2019)) %>% filter(spol %in% c('Skupaj')) %>%
  dplyr::select(-leto) %>%
  pivot_wider(
    names_from = vzrok,
    values_from = ljudje.z.boleznijo
  )

drzave <- bolezni[, 1] %>% unlist()
razdalje <- bolezni[,c(-1,-2)] %>% dist()
dendrogram = razdalje %>% hclust(method = "ward.D")
#dendrogram <- plot(
#  dendrogram,
#  labels = drzave,
#  ylab = 'višina',
#  main = NULL
#)
#Drevo
hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  # k.visina je tabela s štirimi stolpci
  # (1) k, število skupin
  # (2) višina združevanja
  # (3) sprememba višine pri združevanju
  # (4) koleno: ali je točka koleno?
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}
# iz tabele k.visina vrne seznam vrednosti k,
# pri katerih opazujemo koleno

hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# izračunamo tabelo s koleni za dendrogram
r = hc.kolena(dendrogram)

diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red"
    ) +
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red"
    ) +
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 2
    ) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}
#Kolena
ana1 <- diagram.kolena(r)

diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    dplyr::rename(skupina = ...4)
  
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = x, y = y, color = skupina
      )
    ) +
    geom_point() +
    geom_label(label = oznake, size = 2) +
    scale_color_hue() +
    theme_classic()
  
  for (i in 1:k) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }
  d
}


b <- transform(bolezni, Nekn = as.numeric(unlist(bolezni[,3])),
               Mal = as.numeric(unlist(bolezni[,4])),
               Nem = as.numeric(unlist(bolezni[,5])),
               End = as.numeric(unlist(bolezni[,6])),
               Duš = as.numeric(unlist(bolezni[,7])),
               Bolž = as.numeric(unlist(bolezni[,8])),
               Bolc = as.numeric(unlist(bolezni[,9])),
               Bold = as.numeric(unlist(bolezni[,10])),
               Bolp = as.numeric(unlist(bolezni[,11])),
               Bolk = as.numeric(unlist(bolezni[,12])),
               Bolm = as.numeric(unlist(bolezni[,13])),
               Bolg = as.numeric(unlist(bolezni[,14])),
               Nos = as.numeric(unlist(bolezni[,15])),
               Nekp = as.numeric(unlist(bolezni[,16])),
               Pri = as.numeric(unlist(bolezni[,17])),
               Sim = as.numeric(unlist(bolezni[,18]))) %>% dplyr::select(Nekn,Mal, Nem, End, Duš, Bolž, Bolc, Bold, Bolp, Bolk, Bolm, Bolg, Nos, Nekp, Pri, Sim)

skupine = b %>%
  kmeans(centers = 2) %>%
  getElement("cluster") %>%
  as.ordered()



r.hc = bolezni[, -c(1,2)] %>% obrisi(hc = TRUE)

ana2 <- diagram.obrisi(r.hc)
#Maksimalno povprečje obrisov
drzave.x.y =
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(drzave) %>%
  dplyr::select(obmocje = ...3, x = V1, y = V2)

k = obrisi.k(r.hc)
skupine = bolezni[, c(-1,-2)] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()
ana3 <- diagram.skupine(drzave.x.y, drzave.x.y$obmocje, skupine, k)

set.seed(50)
skupine = bolezni[, c(-1,-2)] %>%
  kmeans(centers = 2) %>%
  getElement("cluster") %>%
  as.ordered()
skup <- diagram.skupine(drzave.x.y, drzave.x.y$obmocje, skupine, 2)

#Enako kot zgoraj
###########################################################################
download.file(url='https://kt.ijs.si/~ljupco/lectures/appr/zemljevidi/svet/TM_WORLD_BORDERS-0.3.shp',
              destfile='TM_WORLD_BORDERS-0.3.shp', method='curl')
download.file(url='https://kt.ijs.si/~ljupco/lectures/appr/zemljevidi/svet/TM_WORLD_BORDERS-0.3.dbf',
              destfile='TM_WORLD_BORDERS-0.3.dbf', method='curl')
download.file(url='https://kt.ijs.si/~ljupco/lectures/appr/zemljevidi/svet/TM_WORLD_BORDERS-0.3.shx',
              destfile='TM_WORLD_BORDERS-0.3.shx', method='curl')
svet.sp <- readOGR(getwd(), "TM_WORLD_BORDERS-0.3", verbose=FALSE)
svet.sp = gBuffer(svet.sp, byid = TRUE, width = 0)

svet.map <- st_as_sf(x = svet.sp, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
svet.centroidi = read_csv("podatki/drzave-centroidi.csv")
evropske.drzave = tibble(
  drzava = c(
    "Albania", "Andorra", "Armenia",
    "Austria", "Azerbaijan", "Belarus",
    "Belgium", "Bosnia and Herzegovina",
    "Bulgaria", "Croatia", "Cyprus",
    "Czechia", "Denmark", "Estonia",
    "Finland", "France", "Georgia",
    "Germany", "Greece", "Hungary",
    "Iceland", "Ireland", "Italy",
    "Kazakhstan", "Latvia",
    "Liechtenstein", "Lithuania",
    "Luxembourg", "Malta", "Moldova",
    "Monaco", "Montenegro",
    "Netherlands", "North Macedonia",
    "Norway", "Poland", "Portugal",
    "Romania", "Russia", "San Marino",
    "Serbia", "Slovakia", "Slovenia",
    "Spain", "Sweden", "Switzerland",
    "Turkey", "Ukraine", "United Kingdom",
    "Holy See (Vatican City)"
  )
)


evropa.izsek = as(extent(-25, 60, 30, 75), "SpatialPolygons")
sp::proj4string(evropa.izsek) <- sp::proj4string(svet.sp)

evropske.drzave = tibble(
  drzava = c(
    "Albania", "Andorra", "Armenia",
    "Austria", "Azerbaijan", "Belarus",
    "Belgium", "Bosnia and Herzegovina",
    "Bulgaria", "Croatia", "Cyprus",
    "Czechia", "Denmark", "Estonia",
    "Finland", "France", "Georgia",
    "Germany", "Greece", "Hungary",
    "Iceland", "Ireland", "Italy",
    "Kazakhstan", "Latvia",
    "Liechtenstein", "Lithuania",
    "Luxembourg", "Malta", "Moldova",
    "Monaco", "Montenegro",
    "Netherlands", "North Macedonia",
    "Norway", "Poland", "Portugal",
    "Romania", "Russia", "San Marino",
    "Serbia", "Slovakia", "Slovenia",
    "Spain", "Sweden", "Switzerland",
    "Turkey", "Ukraine", "United Kingdom",
    "Holy See (Vatican City)"
  )
)

# Evropa se po zemljepisni dolžini razteza
# od -25 do 60, po širini pa od 30 do 75
evropa.izsek = as(extent(-25, 60, 30, 75), "SpatialPolygons")
sp::proj4string(evropa.izsek) <- sp::proj4string(svet.sp)
colnames(evropske.drzave)[1] <- "NAME"
evropa.poligoni = svet.sp %>% crop(evropa.izsek) %>% fortify() %>% tibble() %>%
  left_join(
    rownames_to_column(svet.map),
    by = c("id" = "rowname")
  )
colnames(svet.centroidi)[1] <- "NAME"
evropa.centroidi = evropske.drzave %>%
  left_join(
    svet.centroidi,
    by = "NAME"
  )


colnames(evropa.poligoni)[12] <- "drzava"
colnames(evropa.centroidi)[1] <- "drzava"

prostorski.diagram.skupine = function(drzave, skupine, k) {
  drzave %>%
    bind_cols(skupine) %>%
    dplyr::select(drzava = ...1, skupina = ...2) %>%
    left_join(
      evropa.poligoni,
      by = "drzava"
    ) %>%
    ggplot() +
    geom_polygon(
      mapping = aes(long, lat, group = group, fill = skupina),
      color = "grey"
    ) +
    scale_fill_brewer() +
    coord_map() +
    xlim(-25, 50) +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

set.seed(42)
skupine = bolezni[, c(-1,-2)] %>%
  kmeans(centers = 2) %>%
  getElement("cluster") %>%
  as.ordered()

ana4 <- prostorski.diagram.skupine(drzave, skupine, 2)



podatki.ucni <- zadnja.n %>% filter(spol %in% c('Skupaj')) %>%
  filter(leto == 2017) %>% filter(vzrok %in% c('Nekn')) %>%
  dplyr::select(-leto,-spol,-stevilo.prebivalcev.y,-pojav.zdr.tezav.pri.z,-pojav.zdr.tezav.pri.m, -vzrok,-ljudje.z.boleznijo)

nesss <- unlist(podatki.ucni$stevilo.nesrec)
podatki.ucni$stevilo.nesrec <-  as.numeric(gsub(",", "", nesss))
podatki.ucni <- podatki.ucni[-c(1,3,4,11,13,16,22,30,20),]
g <- ggplot(podatki.ucni, aes(x=potrosnja, y=ljudje.z.boleznijo.skupno)) + geom_point()
ana5 <- g + geom_smooth(method="lm", fullrange=TRUE) + xlim(2000,6500) + xlab("Potrošnja") + ylab("Skupno število ljudi z boleznijo")

############################################################################

library(iml)
library (tidyverse)

X = podatki.ucni %>% dplyr::select(ljudje.z.boleznijo.skupno,stevilo.prebivalcev.x, stevilo.nesrec)
model <- lm(data = podatki.ucni, formula =  potrosnja ~ ljudje.z.boleznijo.skupno + stevilo.nesrec + stevilo.prebivalcev.x)
podatki.ucni = podatki.ucni[c(-3,-8,-9,-15,-20,-22),]
X = X[c(-3,-8,-9,-15,-20,-22),]


pfun = function(model, newdata) {
  predict(model, data = newdata)
}

reg.pred = Predictor$new(
  model,
  data = X, y = podatki.ucni$potrosnja,
  predict.fun = pfun
)

# na koncu uporabimo funkcijo FeatureImp$new
reg.moci = FeatureImp$new(reg.pred, loss = "mse")

grafmoci <- plot(reg.moci)
###############################################################################################
slo <- zadnja.n %>% filter(obmocje == "Slovenia")  %>% filter(spol %in% c("Skupaj"))  %>% filter(vzrok %in% c("Mal"))
slo <- slo[,-c(1,6,5,9,10,11,12,13,14)]
ness <- unlist(slo$stevilo.nesrec)
slo$stevilo.nesrec <-  as.numeric(gsub(",", "", ness))
slo[, c(2,4)] <- sapply(slo[, c(2,4)], as.numeric)
class(slo$"pojav.zdr.težav.skupaj") = "double"
#slo <- slo[-c(1:4),]


CAC <- slo[,4]
CACs <- slo[,c(1,4)]
#CACs %>% ggplot() +
#  geom_line(
#    mapping = aes(x = leto, y = pojav.zdr.težav.skupaj),
#    color = "navyblue"
#  )
library(ranger)
Lag <- function(x, n){
  (c(rep(NA, n), x)[1 : length(x)] )
}
naredi.df <- function(x){data.frame(CAC = x,
                                    CAC1 = Lag(x, 1),
                                    CAC2 = Lag(x, 2) ,
                                    CAC3 = Lag(x, 3),
                                    CAC4 = Lag(x, 4)
)
}
df <- naredi.df(CAC$pojav.zdr.težav.skupaj)
model.bi = ranger(CAC ~ CAC1 + CAC2 + CAC3 + CAC4, data=df %>% drop_na())
n <- nrow(df)
for (i in 1:5){
  df <- naredi.df(c(df$CAC, NA))
  napoved = predict(model.bi,  data = df[n + i, ] )$predictions
  df[n+i, 1] = napoved
}

napovedi = df[c(10,11,12,13,14), 1]
CACs2 <- CACs
CACs2[c(10,11,12,13,14),2] = napovedi
CACs2[c(10,11,12,13,14),1] = c(2020, 2021, 2022, 2023, 2024)

nap <- ggplot(CACs2) + geom_point(aes(x = leto, y = pojav.zdr.težav.skupaj, colour = leto > 2019)) +
  scale_colour_manual(name = 'Napovedi', values = setNames(c('red','navyblue'),c(T, F))) +
  xlab('Leto') + ylab('Povprečna starost pojava hujših bolezni')
ana7 <- nap

class(nesrece.prebivalstvo$pojav.zdr.težav.skupaj) <- "double"
