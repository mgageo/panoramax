# <!-- coding: utf-8 -->
#
# quelques fonctions pour Panoramax
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
Drive <- substr( getwd(),1,2)
baseDir <- sprintf("%s/web", Drive)
cfgDir <- sprintf("%s/web/geo/PANORAMAX", Drive)
setwd(baseDir)
dir.create(cfgDir, showWarnings = FALSE, recursive = TRUE)
DEBUG <- FALSE
source("geo/scripts/mga.R")
source("geo/scripts/misc.R")
source("geo/scripts/panoramax_api.R"); # le dialogue avec l'api
source("geo/scripts/panoramax_baba.R"); # avec Baba sur Androïd
source("geo/scripts/panoramax_cli.R"); # le script en ligne de commandes
source("geo/scripts/panoramax_exif.R"); # les données exif des photos
if ( interactive() ) {
  DEBUG <- TRUE
  graphics.off()
} else {
}
