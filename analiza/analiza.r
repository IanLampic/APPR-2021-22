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
    group_by(k) %>%
    summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    summarize(k = min(k)) %>%
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

bolezni <- Skupaj %>% select(leto, obmocje, spol, vzrok, ljudje.z.boleznijo) %>%
  filter(leto %in% c(2019)) %>% filter(spol %in% c('Skupaj')) %>%
  select(-leto) %>%
  pivot_wider(
    names_from = vzrok,
    values_from = ljudje.z.boleznijo
  )

drzave <- bolezni[, 1] %>% unlist()
razdalje <- bolezni[,-1] %>% dist()
dendrogram = razdalje %>% hclust(method = "ward.D")
dendrogramcek <- plot(
  dendrogram,
  labels = drzave,
  ylab = 'višina',
  main = NULL
)

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
diagram.kolena(r)

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


#k-ti voditelji

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
               Sim = as.numeric(unlist(bolezni[,18]))) %>% select(Mal, Nem, End, Duš, Bolž, Bolc, Bold, Bolp, Bolk, Bolm, Bolg, Nos, Nekp, Pri, Sim)

skupine = b %>%
  kmeans(centers = 3) %>%
  getElement("cluster") %>%
  as.ordered()

print(skupine)
r.hc = bolezni[, -c(1,2)] %>% obrisi(hc = TRUE)
r.km = bolezni[, -c(1,2)] %>% obrisi(hc = FALSE)

diagram.obrisi(r.hc)
diagram.obrisi(r.km)

#Optimalno število skupin je torej 2 ali 4/5


