library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)
library(readr)
library(dplyr)
library(httr)
library(XML)
library(stringr)
library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(cluster)
library(scales)
library(ggalt)
library(ggplot2)
library(sf)


options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("/Users/ianlampic/Desktop/APPR-2021-22/lib/uvozi.zemljevid.r", encoding="UTF-8")

