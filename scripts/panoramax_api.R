# <!-- coding: utf-8 -->
#
# quelques fonctions pour Panoramax
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# utilisation de l'api
# https://docs.panoramax.fr/api/api/api/#oauth-flow
#
panoramax_id <- FALSE
# mga
mga_collection <- "893971d5-19eb-43c4-80f3-82493addcb75"
# mga_geo
geo_collection <- "e03a9073-a706-4e33-a5c4-31d75b68e862"
#
#
# source("geo/scripts/panoramax_api.R"); resp <- api_test() %>% glimpse()
api_test <- function() {
  library(httr2)
  library(jsonlite)
  carp("début")
  panoramax <<- config::get(file = "geo/panoramax.yml")
#  api_id()
#  resp <- api_collections()
  mga.df <- api_items(mga_collection)
  geo.df <- api_items(geo_collection)
  df1 <- mga.df %>%
    group_by(`$geometry$coordinates`) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  return(invisible())
}
#
# récupération de l'id
api_id <- function() {
  library(httr2)
  library(jsonlite)
  carp("début")
  url <- sprintf("%s/api/users/me", panoramax$server)
  response <- request(url) |>
    req_headers(Authorization = paste("Bearer", panoramax_token)) |>
    req_perform()
# la réponse
  resp <- resp_body_json(response)
  panoramax_id <<- resp$id
  return(invisible(panoramax_id))
}
#
# récupération des collections
api_collections <- function() {
  library(httr2)
  library(jsonlite)
  carp("début")
  url <- sprintf("%s/api/users/%s/collection?limit=1", panoramax$server, panoramax_id)
  response <- request(url) |>
    req_headers(Authorization = paste("Bearer", panoramax_token)) |>
    req_perform()
  mime <- resp_content_type(response)
  if (! grepl("application/json", mime)) {
    stop("*** réponse json: %s", url)
  }
  writeLines(resp_body_string(response), "d:/tmp/api_collections.json")
# la réponse
  resp <- resp_body_json(response)
  links <- resp$links[-(1:5)]
  df <- data.frame()
  for (item in links) {
    df1 <<- bind_rows(item)
    if (nrow(df1) > 1) {
      print(item)
      df2 <<- as.data.frame(t(df1)) %>%
      misc_print(df2)
      confess("****** trop de lignes: %s", nrow(df1))
    }
    df <- bind_rows(df, df1)
  }
  return(invisible(resp))
}
#
# liste des photos d'une collection
#
# récupération des items
# https://panoramax.openstreetmap.fr/api/collections/bbe8ae11-df4b-4bff-8e45-b01eb8de314c/items
# https://panoramax.openstreetmap.fr/api/collections/bbe8ae11-df4b-4bff-8e45-b01eb8de314c/items
api_items <- function(collection) {
  library(httr2)
  library(jsonlite)
  library(purrr)
  carp("début")
  url <- sprintf("%s/api/collections/%s/items", panoramax$server, collection)
  response <- request(url) |>
    req_perform()
  status_code <- resp_status(response)
  mime <- resp_content_type(response)
  carp("status_code: %s mime: %s", status_code, mime)
  if (! grepl("application/.*json", mime)) {
    confess("*** réponse json: %s", url)
  }
  writeLines(resp_body_string(response), "d:/tmp/api_items.json")
  elements <- "
$assets$hd$href
$geometry$coordinates
$properties$exif$Exif.Image.DateTime
$properties$exif$Exif.Image.ImageLength
$properties$exif$Exif.Image.ImageWidth
$properties$exif$Exif.GPSInfo.GPSImgDirection
"
  lignes <- unlist(strsplit(elements, split = "\r?\n"))
  lignes <- lignes[nzchar(trimws(lignes))]
  resp <- resp_body_json(response)
  df1 <- data.frame()
  for (i in 1:length(resp$features)) {
    liste <- resp$features[[i]]
    for (ligne in lignes) {
      champs <- unlist(strsplit(ligne, split = "\\$"))
      if (length(champs) == 3) {
        res <- paste(pluck(liste, champs[2], champs[3]), collapse = ",")
#        carp("ligne: %s res: %s", ligne, res)
      }
      if (length(champs) == 4) {
        res <- pluck(liste, champs[2], champs[3], champs[4])
#        carp("ligne: %s res: %s", ligne, res)
      }
      df1[i, ligne] <- res
    }
  }
  glimpse(df1)
  return(invisible(df1))
}