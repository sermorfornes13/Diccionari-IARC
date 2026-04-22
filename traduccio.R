########################################################################################################
#
# 22/4/26: traducció i formateig de les dades d'EPIC Espanya per incorporar grups de càncer de la IARC
#
########################################################################################################

#### Llibreries ####
loadpkg <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}

loadpkg("RODBC")
loadpkg("readxl")
loadpkg("ImportExport")
loadpkg("sqldf")
loadpkg("lubridate")
loadpkg("data.table")
loadpkg("Hmisc")
# loadpkg(gmodels)
loadpkg("stringr")
loadpkg("gtools")
loadpkg("dplyr")
loadpkg("tidyr")
loadpkg("tidyverse")
loadpkg("readr")
loadpkg("openxlsx")
loadpkg("rstudioapi")   ## showPrompt
loadpkg("log4r")        ## logging
loadpkg("splines")
loadpkg("survival")
# loadpkg("Design")
loadpkg("pspline")


## RESET
rm(list = ls())
cat("\014")
gc()

## Settings
options(scipen = 999)


## USUARI ####
# canviar per cada usuari

user <- 'Sergio' # user <- 'Maria'
usuaris <- data.table(usuari = c('Sergio', 'Maria'), 
                      folder = c('S:/UNAC/DATA_MANAGEMENT/Endpoint/', 'S:/grups/UNAC/DATA_MANAGEMENT/Endpoint/'))
folder <- usuaris[usuari == user, folder]

# per a les dades, hem de carregar cadascun dels centres per separat i després ajuntar-ho tot en una sola taula
# a més a més, hem de tindre en compte que, a data de creació del codi, hi ha taules que estan actualitzades
# en una carpeta a banda, pel que s'hauran de carregar aquestes primer i posteriorment carregar la resta de dades
# de la carpeta amb les dades corregides en la seua darrera versió

# en total hem de tenir 4 taules per cada centre: candiag (càncer incident), canpreva (càncer prevalent),
# caumort (causa de mort) i esvital (estatus vital)

ruta_new <- paste0(folder, 'ENDPOINT2022/7_QC_IARC_Març26/Resolució queries/')
ruta_act <- paste0(folder, 'ENDPOINT2022/6_QC_IARC_Nov25/Resolució queries/')

centre <- c('ASTURIES', 'GRANADA', 'MURCIA', 'NAVARRA', 'PAIS VASCO')
taules <- c('CANDIAG', 'CANPREVA', 'CAUMORT', 'ESVITAL')

for(i in centre){
  taules_new <- list.files(paste0(ruta_new, i, '/01_Enviar IARC'))
  taules_new <- taules_new[grepl(paste0('^', taules, collapse = '|'), taules_new, ignore.case = T)]
  
  taules_act <- list.files(paste0(ruta_act, i, '/03_Enviar IARC'))
  taules_act <- taules_act[grepl(paste0('^', taules, collapse = '|'), taules_act, ignore.case = T)]
  taules_act <- taules_act[grepl(paste0(taules[!taules %in% unlist(strsplit(taules_new, split = '_'))], collapse = '|'),
                                  taules_act, ignore.case = T)]

  # llegim les taules que hi ha a la carpeta amb dades amb darrera revisió enviada a la iarc
  for(j in taules_new){
    nom <- tolower(unlist(strsplit(j, split = '_'))[1])
    a <- setDT(read.xlsx(paste0(ruta_new, i, '/01_Enviar IARC/', j)))
    assign(paste0(substr(tolower(i), 1, 3), '_', nom), a, envir = globalenv())
  }
  
  # carreguem ara les taules que falten de cada centre
  for(k in taules_act){
    nom <- tolower(unlist(strsplit(k, split = '_'))[1])
    a <- setDT(read.xlsx(paste0(ruta_act, i, '/03_Enviar IARC/', k)))
    assign(paste0(substr(tolower(i), 1, 3), '_', nom), a, envir = globalenv())
  }
  
  rm(taules_act, taules_new, nom, a)
  
}

# agrupem les taules segons categoria
candiag <- Reduce(function(...) unique(rbind(..., fill = T)), mget(ls(pattern = 'candiag')))[order(ID_1)]
canpreva <- Reduce(function(...) unique(rbind(..., fill = T)), mget(ls(pattern = 'canpreva')))[order(ID_1)]
caumort <- Reduce(function(...) unique(rbind(..., fill = T)), mget(ls(pattern = 'caumort')))[order(ID_1)]
esvital <- Reduce(function(...) unique(rbind(..., fill = T)), mget(ls(pattern = 'esvital')))[order(ID_1)]

rm(list = c(ls(pattern = 'ast'), ls(pattern = 'gra'), ls(pattern = 'mur'), 
            ls(pattern = 'nav'), ls(pattern = 'pai')))

setnames(candiag, names(candiag), gsub('.', '_', names(candiag), fixed = T))

# canviem els noms per identificar millor cada variable
setnames(candiag, names(candiag), c('id', 'sexe', 'datanaix', 'datarecl', 'ordretum', 'datadiag', 'icdo', 'locatum', 'morfotum', 'tipustum', 'grautum', 
                                    'tnmclas', 'tnmestat', 'tnmedic', paste0('defcancer', 1:3), paste0('fontcancer', 1:5), 'cribrat', 'datacribrat',
                                    paste0('fontcrib', 1:3), 'resumestadi', 'dukes', 'gleason1', 'gleason2', 'gleasonsum', 'psa', 
                                    paste0('estrogen', c('recestatus', 'recscore', 'recscorecat')), paste0('progesteron', c('recestatus', 'recscore', 'recscorecat')), 
                                    'herp2estatus', 'herp2score', 'fishtest', 'ki67', 'figoestat', 'figoclas', 'arborlugano', 'ESTADTUM'))
setnames(canpreva, names(canpreva), c('id', 'sexe', 'datanaix', 'datarecl', 'numtumor', 'datadiag', 'icdo', 'locatum', 'morfotum', 'tipustum', 'font'))
setnames(caumort, names(caumort), c('id', 'sexe', 'datanaix', 'datarecl', 'datamort', paste0(rep(c('causamort', 'tipuscausa'), 17), sort(rep(1:17,2))),
                                    'fontcausamort', 'altrafont', 'menciocancer', 'autopsia', paste0('autopdiag', 1:3), 'cancerautop', 'llocmort', 'cie'))
setnames(esvital, names(esvital), c('id', 'sexe', 'datanaix', 'datarecl', 'dataxequeig', 'estatus_vital', 
                                    'font1', 'font2', 'ultimcontacte', 'datacenscancer'))

# carreguem el diccionari que ens permetrà seleccionar els càncers i les seues subcategories

dicc_can <- setDT(read.xlsx('WG specific variables_final_202604.xlsx'))
setnames(dicc_can, names(dicc_can), gsub('.', '_', names(dicc_can), fixed = T))
names(dicc_can) <- c('wg_abbrev', 'wg_cancersite', 'code', 'subcategory', 'definition', 'comments')

vars_to_modif <- names(dicc_can)[1:3]

dicc_can <- setDT(dicc_can %>% 
  fill(vars_to_modif) %>% 
  fill(vars_to_modif, .direction = 'down') %>%
  distinct)

# comencem a treballar amb les definicions de càncer
# seguirem l'ordre del diccionari per fer les definicions

candiag[, `:=`(clas_iarc = as.character(NA), subcateg = as.character(NA), inc_excl = as.character(NA))]
canpreva[, `:=`(clas_iarc = as.character(NA), subcateg = as.character(NA), inc_excl = as.character(NA))]

# qualsevol cancer (tots excepte aquells que siguin C44 amb morfologia 809, 810 i 811)
candiag[!(grepl('^C44', locatum, ignore.case = T) & grepl('^809|^810|^811', morfotum, ignore.case = T)), clas_iarc := 'Anyc']
canpreva[!(grepl('^C44', locatum, ignore.case = T) & grepl('^809|^810|^811', morfotum, ignore.case = T)), clas_iarc := 'Anyc']

# bufeta
candiag[grepl('^C67', locatum, ignore.case = T), clas_iarc := 'Blad']
canpreva[grepl('^C67', locatum, ignore.case = T), clas_iarc := 'Blad']

# colorectal

# treballem primer amb la taula candiag
{
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  clrt <- copy(candiag[grepl('^C18|^C19|^C20', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, morfotum, tipustum, clas_iarc = 'Clrt')])
  
  # malignant
  cat <- dicc_can[wg_abbrev == 'Clrt' & subcategory == '01-Malignant tumour']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  clrt <- merge(clrt, morfo[, .(morfotum = V1, tipustum = V2, malignant = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # in situ
  cat <- dicc_can[wg_abbrev == 'Clrt' & subcategory == '02-In-situ tumour']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  clrt <- merge(clrt, morfo[, .(morfotum = V1, tipustum = V2, insitu = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # morfologia exclosa
  cat <- dicc_can[wg_abbrev == 'Clrt' & subcategory == '03-Exc-Morphology excluded']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  clrt <- merge(clrt, morfo[, .(morfotum = V1, tipustum = V2, morpho_excl = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # morfologia ineligible
  cat <- dicc_can[wg_abbrev == 'Clrt' & subcategory == '04-Exc-Morphology ineligible']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  morfo[V1 == V2, V2 := NA]
  
  clrt <- merge(clrt, morfo[, .(morfotum = V1, tipustum = V2, morpho_inel = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # morfologia missing
  clrt[, morpho_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  clrt[insitu == 1, `:=`(subcateg = 'In-situ tumour', inc_excl = 'Included')]
  clrt[malignant == 1 & grepl(paste0('^C18', 0:5, collapse = '|'), locatum, ignore.case = T), 
       `:=`(subcateg = 'Malignant Colon Proximal', inc_excl = 'Included')]
  clrt[malignant == 1 & grepl('^C186|^C187', locatum, ignore.case = T), 
       `:=`(subcateg = 'Malignant Colon Distal', inc_excl = 'Included')]
  clrt[malignant == 1 & grepl('^C188|^C189', locatum, ignore.case = T), 
       `:=`(subcateg = 'Malignant Colon Overlapping/Nos', inc_excl = 'Included')]
  clrt[malignant == 1 & grepl('^C199|^C209', locatum, ignore.case = T), 
       `:=`(subcateg = 'Malignant Rectum', inc_excl = 'Included')]
  clrt[morpho_excl == 1, `:=`(subcateg = 'Morphology excluded', inc_excl = 'Excluded')]
  clrt[morpho_inel == 1, `:=`(subcateg = 'Morphology ineligible', inc_excl = 'Excluded')]
  clrt[morpho_mis == 1, `:=`(subcateg = 'Morphology missing', inc_excl = 'Excluded')]
  
  # creem una categoria addicional per aquells que no s'han pogut classificar
  clrt[is.na(subcateg), `:=`(subcateg = 'Not classified', inc_excl = 'Doubt')]
  
  candiag <- merge(candiag, clrt[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

# ho repetim per a la taula canpreva
{
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  clrt <- copy(canpreva[grepl('^C18|^C19|^C20', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, morfotum, tipustum, clas_iarc = 'Clrt')])
  
  # malignant
  cat <- dicc_can[wg_abbrev == 'Clrt' & subcategory == '01-Malignant tumour']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  clrt <- merge(clrt, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), malignant = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # in situ
  cat <- dicc_can[wg_abbrev == 'Clrt' & subcategory == '02-In-situ tumour']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  clrt <- merge(clrt, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), insitu = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # morfologia exclosa
  cat <- dicc_can[wg_abbrev == 'Clrt' & subcategory == '03-Exc-Morphology excluded']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  clrt <- merge(clrt, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), morpho_excl = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # morfologia ineligible
  cat <- dicc_can[wg_abbrev == 'Clrt' & subcategory == '04-Exc-Morphology ineligible']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  morfo[V1 == V2, V2 := NA]
  
  clrt <- merge(clrt, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), morpho_inel = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # morfologia missing
  clrt[, morpho_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  clrt[insitu == 1, `:=`(subcateg = 'In-situ tumour', inc_excl = 'Included')]
  clrt[malignant == 1 & grepl(paste0('^C18', 0:5, collapse = '|'), locatum, ignore.case = T), 
       `:=`(subcateg = 'Malignant Colon Proximal', inc_excl = 'Included')]
  clrt[malignant == 1 & grepl('^C186|^C187', locatum, ignore.case = T), 
       `:=`(subcateg = 'Malignant Colon Distal', inc_excl = 'Included')]
  clrt[malignant == 1 & grepl('^C188|^C189', locatum, ignore.case = T), 
       `:=`(subcateg = 'Malignant Colon Overlapping/Nos', inc_excl = 'Included')]
  clrt[malignant == 1 & grepl('^C199|^C209', locatum, ignore.case = T), 
       `:=`(subcateg = 'Malignant Rectum', inc_excl = 'Included')]
  clrt[morpho_excl == 1, `:=`(subcateg = 'Morphology excluded', inc_excl = 'Excluded')]
  clrt[morpho_inel == 1, `:=`(subcateg = 'Morphology ineligible', inc_excl = 'Excluded')]
  clrt[morpho_mis == 1, `:=`(subcateg = 'Morphology missing', inc_excl = 'Excluded')]
  
  # creem una categoria addicional per aquells que no s'han pogut classificar
  clrt[is.na(subcateg), `:=`(subcateg = 'Not classified', inc_excl = 'Doubt')]
  
  canpreva <- merge(canpreva, clrt[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  canpreva[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

rm(clrt, morfo, cat)

# ronyó

# treballem primer amb la taula candiag
{
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  kidn <- copy(candiag[grepl('^C64|^C65', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, morfotum, tipustum, clas_iarc = 'Kidn')])
  
  # carcinoma de cèl·lules renals
  cat <- dicc_can[wg_abbrev == 'Kidn' & subcategory == '01-Renal cell carcinoma']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  kidn <- merge(kidn, morfo[, .(morfotum = V1, tipustum = V2, renal = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # altres càncers de ronyó
  cat <- dicc_can[wg_abbrev == 'Kidn' & subcategory == '02-Other Kidney tumors']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  kidn <- merge(kidn, morfo[, .(morfotum = V1, tipustum = V2, altres = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # pelvis
  cat <- dicc_can[wg_abbrev == 'Kidn' & subcategory == '03-Renal pelvis']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  morfo[, V2 := '3']
  
  kidn <- merge(kidn, morfo[, .(morfotum = V1, tipustum = V2, pelvis = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # # altra morfologia
  # cat <- dicc_can[wg_abbrev == 'Kidn' & subcategory == '04-Other specified morphology']$definition
  # cat <- gsub(')', '', gsub("','", '|', cat))
  # cat <- gsub("'", '', cat)
  # 
  # morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  # morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  # morfo[V1 == V2, V2 := NA]
  # 
  # kidn <- merge(kidn, morfo[, .(morfotum = V1, tipustum = V2, morpho_other = 1)], 
  #               by = c('morfotum', 'tipustum'), all.x = T)
  
  # morfologia missing
  kidn[, morpho_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  kidn[renal == 1, `:=`(subcateg = 'Renal cell carcinoma', inc_excl = 'Included')]
  kidn[altres == 1, `:=`(subcateg = 'Other Kidney tumors', inc_excl = 'Included')]
  kidn[pelvis == 1 | grepl('^C65', locatum, ignore.case = T), `:=`(subcateg = 'Renal pelvis', inc_excl = 'Included')]
  kidn[!is.na(morfotum) & is.na(renal) & is.na(altres) & is.na(pelvis), 
       `:=`(subcateg = 'Other specified morphology', inc_excl = 'Included')]
  kidn[morpho_mis == 1, `:=`(subcateg = 'Morphology missing', inc_excl = 'Included')]
  
  candiag <- merge(candiag, kidn[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

# ho repetim per a la taula canpreva
{
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  kidn <- copy(canpreva[grepl('^C64|^C65', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, morfotum, tipustum, clas_iarc = 'Kidn')])
  
  # carcinoma de cèl·lules renals
  cat <- dicc_can[wg_abbrev == 'Kidn' & subcategory == '01-Renal cell carcinoma']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  kidn <- merge(kidn, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), renal = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # altres càncers de ronyó
  cat <- dicc_can[wg_abbrev == 'Kidn' & subcategory == '02-Other Kidney tumors']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  kidn <- merge(kidn, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), altres = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # pelvis
  cat <- dicc_can[wg_abbrev == 'Kidn' & subcategory == '03-Renal pelvis']$definition
  cat <- gsub(')', '', gsub("','", '|', cat))
  cat <- gsub("'", '', cat)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  morfo[, V2 := '3']
  
  kidn <- merge(kidn, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), pelvis = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # # altra morfologia
  # cat <- dicc_can[wg_abbrev == 'Kidn' & subcategory == '04-Other specified morphology']$definition
  # cat <- gsub(')', '', gsub("','", '|', cat))
  # cat <- gsub("'", '', cat)
  # 
  # morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  # morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  # morfo[V1 == V2, V2 := NA]
  # 
  # kidn <- merge(kidn, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), morpho_other = 1)], 
  #               by = c('morfotum', 'tipustum'), all.x = T)
  
  # morfologia missing
  kidn[, morpho_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  kidn[renal == 1, `:=`(subcateg = 'Renal cell carcinoma', inc_excl = 'Included')]
  kidn[altres == 1, `:=`(subcateg = 'Other Kidney tumors', inc_excl = 'Included')]
  kidn[pelvis == 1 | grepl('^C65', locatum, ignore.case = T), `:=`(subcateg = 'Renal pelvis', inc_excl = 'Included')]
  kidn[!is.na(morfotum) & is.na(renal) & is.na(altres) & is.na(pelvis), 
       `:=`(subcateg = 'Other specified morphology', inc_excl = 'Included')]
  kidn[morpho_mis == 1, `:=`(subcateg = 'Morphology missing', inc_excl = 'Included')]
  
  canpreva <- merge(canpreva, kidn[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  canpreva[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

rm(kidn, morfo, cat)

# fetge

# treballem primer amb la taula candiag
{
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  live <- copy(candiag[grepl('^C22|^C23|^C24', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, morfotum, tipustum, clas_iarc = 'Live')])
  
  # carcinoma hepatocel·lular
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'HCC- Hepatocellular carcinoma']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or ', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  hcc <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, hcc[, .(locatum = var1, morfotum = var2, tipustum = var3, hcc = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # carcinoma hepatocel·lular, definició més laxa
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory %in% c('HCC- Hepatocellular carcinoma',
                  'HCC_Wide - Hepatocellular carcinoma (wider definition)')]$definition
  cat <- gsub('as above + ', '', cat, fixed = T)
  cat <- gsub('If ', '', cat, fixed = T)
  cat <- gsub(' and Lab_Afp* ge 12', '', cat, fixed = T)
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub(' and Basis_diag1 in (20,25,50,55,70)', '', cat, fixed = T)
  cat <- gsub('Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or ', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  hcc_w <- unique(morfo[, {
    val <- unlist(strsplit(V2, "\\|")) 
    rbindlist(lapply(val, function(x) {
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) 
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)])
  
  live <- merge(live, hcc_w[, .(locatum = var1, morfotum = var2, tipustum = var3, hcc_w = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # conducte biliar
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'IBD - Intrahepatic bile duct']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or ', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  ibd <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, ibd[, .(locatum = var1, morfotum = var2, tipustum = var3, ibd = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # vesícula biliar i conducte biliar
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'GBT - Gallbladder and biliary tract']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub(' and Basis_diag1 not in (.,27,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  gbt <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, gbt[, .(locatum = var1, morfotum = var2, tipustum = var3, gbt = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # conducte biliar extrahepàtic
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'EBD - Extra-hepatic bile duct']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  ebd <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, ebd[, .(locatum = var1, morfotum = var2, tipustum = var3, ebd = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # vesícula biliar
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'Gallblad- Gall bladder']$definition
  cat <- gsub(' and Basis_diag1 not in (.,27,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  gallblad <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, gallblad[, .(locatum = var1, morfotum = var2, tipustum = var3, gallblad = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # ampolla de vater
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'AOV- Ampulla of Vater']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  aov <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, aov[, .(locatum = var1, morfotum = var2, tipustum = var3, aov = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # colangiocarcinoma intrahepàtic
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'CCA - Intra-hepatic cholangiocarcinomas']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  cca <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, cca[, .(locatum = var1, morfotum = var2, tipustum = var3, cca = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # colangiocarcinomas extrahepàtic
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'CCA - Extrahepatic cholangiocarcinomas']$definition
  cat <- gsub(' and Basis_diag1 not in (.,27,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  cca_ex <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, cca_ex[, .(locatum = var1, morfotum = var2, tipustum = var3, cca_ex = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # colangiocarcinomas extrahepàtic perihilar
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'Perihilar extrahepatic cholangiocarcinomas']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  pec <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, pec[, .(locatum = var1, morfotum = var2, tipustum = var3, pec = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # colangiocarcinomas extrahepàtic distal
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'Distal extrahepatic cholangiocarcinomas ']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  dec <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, dec[, .(locatum = var1, morfotum = var2, tipustum = var3, dec = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # morfologia missing
  live[, morpho_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  live[hcc == 1, `:=`(subcateg = 'Hepatocellular carcinoma', inc_excl = 'Included')]
  live[hcc_w == 1, `:=`(subcateg = 'Hepatocellular carcinoma (wider definition)', inc_excl = 'Included')]
  live[ibd == 1, `:=`(subcateg = 'Intrahepatic bile duct', inc_excl = 'Included')]
  live[gbt == 1, `:=`(subcateg = 'Gallbladder and biliary tract', inc_excl = 'Included')]
  live[ebd == 1, `:=`(subcateg = 'Extra-hepatic bile duct', inc_excl = 'Included')]
  live[gallblad == 1, `:=`(subcateg = 'Gall bladder', inc_excl = 'Included')]
  live[aov == 1, `:=`(subcateg = 'Ampulla of Vater', inc_excl = 'Included')]
  live[cca == 1, `:=`(subcateg = 'Intra-hepatic cholangiocarcinomas', inc_excl = 'Included')]
  live[cca_ex == 1, `:=`(subcateg = 'Extra-hepatic cholangiocarcinomas', inc_excl = 'Included')]
  live[pec == 1, `:=`(subcateg = 'Perihilar extrahepatic cholangiocarcinomas', inc_excl = 'Included')]
  live[dec == 1, `:=`(subcateg = 'Distal extrahepatic cholangiocarcinomas', inc_excl = 'Included')]
  live[morpho_mis == 1, `:=`(subcateg = 'Morphology missing', inc_excl = 'Excluded')]
  live[ibd == 1, `:=`(subcateg = '', inc_excl = 'Included')]
  
  
  
  
  live[pelvis == 1 | grepl('^C65', locatum, ignore.case = T), `:=`(subcateg = 'Renal pelvis', inc_excl = 'Included')]
  live[!is.na(morfotum) & is.na(renal) & is.na(altres) & is.na(pelvis), 
       `:=`(subcateg = 'Other specified morphology', inc_excl = 'Included')]
  live[morpho_mis == 1, `:=`(subcateg = 'Morphology missing', inc_excl = 'Included')]
  
  candiag <- merge(candiag, live[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}


















