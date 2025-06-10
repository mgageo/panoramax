# <!-- coding: utf-8 -->
#
# quelques fonctions exif
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===========À====================================================
#
varDir <- sprintf("%s/web.tmp/baba", Drive)
mobile <- "Pixel"
mobile <- "Redmi"
exif_dsn <- sprintf("%s/exif_%s.rds", varDir, mobile)
posts_dsn <- sprintf("%s/posts_%s.rds", varDir, mobile)
#
# lecture des données exif des fichiers d'un dossier
# source("geo/scripts/panoramax.R");exif_get()
exif_get <- function() {
  library(tidyverse)
  library(exiftoolr)
  files_dir <- sprintf("%s/%s/Pictures/Baba", varDir, mobile)
  carp("liste des fichiers: %s", files_dir)
  files <- list.files(
    files_dir,
    pattern = "jpg$",
    all.files = TRUE,
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = FALSE
  )
  nb_files <- length(files)
  print(sprintf("nb_files: %s", format(nb_files, big.mark = " ")))
  #  df <- exif_read(path = files) %>%
  #    glimpse()
  tranche <- 1000
  debuts <- seq(1, nb_files, tranche)
  exif.df <- tibble()
  # https://exiftool.org/TagNames/
  tags <- c("sourcefile",
    "filename",
    "FileCreateDate",
    "ModifyDate",
    "DateTimeOriginal",
    "filesize",
    "imagesize",
    "make",
    "model",
    "imagewidth",
    "imageheight",
    "megapixels",
    "warning",
    "GPSLatitude",
    "GPSLongitude",
    "GPSImgDirection"
  )
  carp("lecture des exifs")
  for (debut in debuts) {
    print(sprintf("debut: %s/%s", format(debut, big.mark = " "), format(nb_files, big.mark = " ")))
    fin <- min((debut + tranche - 1), nb_files)
    f <- files[debut:fin] %>%
      glimpse()
    df <- exif_read(path = f, tags = tags) %>%
      glimpse()
    exif.df <- bind_rows(exif.df, df)
    #    break
  }
  exif.df <- exif.df
  saveRDS(exif.df, file = exif_dsn)
  carp("fin nrow: %s", nrow(exif.df))
}
#
# lecture des données exif des fichiers d'un dossier
# source("geo/scripts/panoramax.R");exif_stat()
exif_stat <- function() {
  library(tidyverse)
  library(sf)
  library(lubridate)
  library(tmap);tmap_mode("plot")
  exif.df <- readRDS(file = exif_dsn) %>%
    rowid_to_column(var = "index") %>%
    mutate(time = ymd_hms(DateTimeOriginal)) %>%
    mutate(
      time_diff = as.numeric(difftime(time, dplyr::lag(time), units = "secs"))
    )
  df1 <- exif.df %>%
    filter(time_diff > 10) %>%
    glimpse()

  df2 <- exif.df %>%
    filter(time >= ymd_hms("2025-06-05 01:02:00")) %>%
    glimpse()
  carp("fin nrow: %s", nrow(df2))
  df3 <- exif.df %>%
    group_by(GPSLatitude, GPSLongitude) %>%
    summarize(nb = n()) %>%
    filter(nb != 1) %>%
    glimpse()
  nc1 <- st_as_sf(df2, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326, remove = FALSE) %>%
    glimpse()
  nc2 <- st_combine(nc1)
  nc3 <- st_cast(nc2, "LINESTRING")
  print(st_length(nc3))
  ligne.sf <- nc3 %>%
    st_transform(2154)
  bbox.sf <- ligne.sf %>%
    st_buffer(200)
  points.sf <- nc1 %>%
    st_transform(2154)
  carte1 <- tm_shape(bbox.sf) +
    tm_shape(ligne.sf) +
    tm_shape(points.sf) +
    tm_borders( )+
    tm_basemap("OpenStreetMap") +
    tm_title("exif")
  print(carte1)
}
