# <!-- coding: utf-8 -->
#
# quelques fonctions pour Baba, prise de photos  et téléversement sur panoramax
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
# lecture d'un fichier Chucker
#
# version en .har
# install.packages("remotes")
# remotes::install_github("USDA-ERS/MTED-HARr")
baba_transactions_har <- function() {
  library(HARr)
  dsn <- sprintf("%s/transactions.har", varDir)
  read_har(dsn) %>%
    glimpse()
}
#
# version en .txt
# source("geo/scripts/panoramax.R");baba_posts_get()
baba_posts_get <- function() {
  library(tidyverse)
  dsn <- sprintf("%s/%s/Pictures/transactions.txt", varDir, mobile)
  carp("dsn: %s", dsn)
  lignes <- readLines(dsn, encoding = "UTF-8") %>%
    glimpse()
# pour le premier bloc
  lignes[1] <- "=================="
  blocs <- grep("^==================", lignes)
# pour le dernier bloc
  blocs <- c(blocs, length(lignes) + 1) %>%
    glimpse()
  blocs.list <- list()
  for (i in seq_along(blocs[-length(blocs)])) {
    debut <- blocs[i]
    fin <- blocs[i + 1] - 1
#    carp("debut: %s, fin: %s", debut, fin)
    lignes[debut] <- sprintf("%s-%s", debut, fin)
    blocs.list[[i]] <- lignes[debut:fin]
  }
  j <- 0
  posts.list <- list()
  for (i in 1:length(blocs.list)) {
    lignes <- blocs.list[[i]]
# que les transferts d'images
    blocs <- any(grepl("^URL.*items$", lignes))
    if (blocs != TRUE) {
      next
    }
    writeLines(lignes[1:6])
    j <- j + 1
    posts.list[[j]] <- paste(lignes[1:16], collapse = "€€€")
#    break
  }
  carp("j: %s", j)
  glimpse(posts.list)
  saveRDS(posts.list, file = posts_dsn)
  return(invisible(posts.list))
}
#
# analyse des posts
# source("geo/scripts/panoramax.R");baba_posts_stat()
baba_posts_stat <- function() {
  library(tidyverse)
  library(sf)
  library(lubridate)
  posts.list <- readRDS(file = unlist(posts_dsn))
  df1 <- tibble(post = unlist(posts.list)) %>%
    separate_wider_regex(
      post, too_few = "debug",
      c(
        lignes = "^.*",
        "€€€.*collections/",
        collection = ".*",
        "/items€€€.*",
        "Status: ",
        status = ".*",
        "€€€Response: ",
        response = "[^€]+",
        ".*€€€Request time: ",
        request_time = "[^€]+",
        ".*$")
    ) %>%
    glimpse()
  df2 <- df1 %>%
    group_by(collection, response) %>%
    summarize(nb = n())
  misc_print(df2)
  df3 <- df1 %>%
    filter(is.na(response)) %>%
    glimpse()
}
#
# lecture des données exif des fichiers d'un dossier
# source("geo/scripts/panoramax.R");baba_exif_get()
baba_exif_get <- function() {
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
  saveRDS(exif.df, file = exif_dsn)
  carp("fin nrow: %s", nrow(exif.df))
}
#
# lecture des données exif des fichiers d'un dossier
# source("geo/scripts/panoramax.R");baba_exif_stat()
baba_exif_stat <- function() {
  library(tidyverse)
  library(sf)
  library(lubridate)
  exif.df <- readRDS(file = exif_dsn) %>%
    mutate(time =  ymd_hms(DateTimeOriginal)) %>%
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
    filter(nb == 1) %>%
    glimpse()
  stop()
  nc1 <- st_as_sf(df2, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326, remove = FALSE) %>%
    glimpse()
  nc2 <- st_combine(nc1)
  nc3 <- st_cast(nc2, "LINESTRING")
  print(st_length(nc3))
}