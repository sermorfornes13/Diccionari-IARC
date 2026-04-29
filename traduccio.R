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

vars_to_modif <- names(dicc_can)[1:4]

dicc_can <- setDT(dicc_can %>% 
  fill(vars_to_modif) %>% 
  fill(vars_to_modif, .direction = 'down') %>%
  distinct)

dicc_can <- dicc_can[!(wg_abbrev == 'Lymp' & is.na(definition))]

# comencem a treballar amb les definicions de càncer
# seguirem l'ordre del diccionari per fer les definicions

candiag[, `:=`(clas_iarc = as.character(NA), subcateg = as.character(NA), inc_excl = as.character(NA))]
canpreva[, `:=`(clas_iarc = as.character(NA), subcateg = as.character(NA), inc_excl = as.character(NA))]

#### qualsevol cancer ####
# (tots excepte aquells que siguin C44 amb morfologia 809, 810 i 811)
# no posem valor a inclusió/exclusió per precaució
candiag[!(grepl('^C44', locatum, ignore.case = T) & grepl('^809|^810|^811', morfotum, ignore.case = T)), clas_iarc := 'Anyc']
canpreva[!(grepl('^C44', locatum, ignore.case = T) & grepl('^809|^810|^811', morfotum, ignore.case = T)), clas_iarc := 'Anyc']


#### bufeta #### 
candiag[grepl('^C67', locatum, ignore.case = T), `:=`(clas_iarc = 'Blad', inc_excl = 'Included')]
canpreva[grepl('^C67', locatum, ignore.case = T), `:=`(clas_iarc = 'Blad', inc_excl = 'Included')]


#### colorectal #### 
# treballem primer amb la taula candiag
if(candiag[grepl('^C18|^C19|^C20', locatum, ignore.case = T), .N] > 0){
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
if(canpreva[grepl('^C18|^C19|^C20', locatum, ignore.case = T), .N] > 0){
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

# # comprovem si hi ha algun codi duplicat
# prova <- dicc_can[wg_abbrev == 'Clrt', definition][1:4]
# prova <- gsub("'", '', gsub(')', '', prova, fixed = T), fixed = T)
# prova <- gsub(",", '|', prova, fixed = T)
# 
# morfo <- unlist(strsplit(prova, split = '|', fixed = T))
# morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
# morfo[V1 == V2, V2 := NA]
# 
# morfo[,.N] == unique(morfo)[,.N] # són iguals
# rm(morfo, prova)


#### ronyó #### 
# treballem primer amb la taula candiag
if(candiag[grepl('^C64|^C65', locatum, ignore.case = T), .N] > 0){
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
if(canpreva[grepl('^C64|^C65', locatum, ignore.case = T), .N] > 0){
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

# # comprovem si hi ha algun codi duplicat
# prova <- dicc_can[wg_abbrev == 'Kidn', definition][1:3]
# prova <- gsub("'", '', gsub(')', '', prova, fixed = T), fixed = T)
# prova <- gsub(",", '|', prova, fixed = T)
# prova <- gsub("  or site in C65", '', prova, fixed = T)
# 
# morfo <- unlist(strsplit(prova, split = '|', fixed = T))
# morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
# morfo[V1 == V2, V2 := NA]
# 
# morfo[,.N] == unique(morfo)[,.N] # són iguals
# rm(morfo, prova)


#### fetge #### 
# treballem primer amb la taula candiag
if(candiag[grepl('^C22|^C23|^C24', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  live <- copy(candiag[grepl('^C22|^C23|^C24', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, morfotum, tipustum, defcancer1, clas_iarc = 'Live')])
  
  # carcinoma hepatocel·lular
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'HCC- Hepatocellular carcinoma']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or ', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  d_hcc <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, d_hcc[, .(locatum = var1, morfotum = var2, tipustum = var3, hcc = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)

  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(hcc) & locatum == 'C220' & morfotum %in% d_hcc[var1 == 'C220']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), hcc := 0]
  live[!is.na(hcc) & locatum == 'C221' & morfotum %in% d_hcc[var1 == 'C221']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), hcc := 0]
  
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
  
  d_hcc_w <- unique(morfo[, {
    val <- unlist(strsplit(V2, "\\|")) 
    rbindlist(lapply(val, function(x) {
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) 
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)])
  
  # 23/4/26: hi ha un comentari en què diu que s'ha d'afegit un codi concret a la taula en absència d'una variable
  d_hcc_w <- unique(rbind(d_hcc_w, data.table(var1 = 'C220', var2 = c(8000, 8140), var3 = 3))[order(var1, var2)])
  d_hcc_w <- unique(rbind(d_hcc, d_hcc_w)[order(var1, var2, var3)])
  
  live <- merge(live, d_hcc_w[, .(locatum = var1, morfotum = var2, tipustum = var3, hcc_w = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(hcc_w) & locatum == 'C220' & morfotum %in% d_hcc_w[var1 == 'C220']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), hcc_w := 0]
  live[!is.na(hcc_w) & locatum == 'C221' & morfotum %in% d_hcc_w[var1 == 'C220']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), hcc_w := 0]
  live[!is.na(hcc_w) & locatum == 'C220' & morfotum %in% c(8000, 8010) & tipustum == 3 & 
         !defcancer1 %in% c(20,25,50,55,70), d_hcc_w := 0]
  
  # conducte biliar
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'IBD - Intrahepatic bile duct']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or ', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  d_ibd <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, d_ibd[, .(locatum = var1, morfotum = var2, tipustum = var3, ibd = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(ibd) & locatum == 'C220' & morfotum %in% d_ibd[var1 == 'C220']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), ibd := 0]
  live[!is.na(ibd) & locatum == 'C221' & morfotum %in% d_ibd[var1 == 'C221']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), ibd := 0]
  
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
  
  d_gbt <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, d_gbt[, .(locatum = var1, morfotum = var2, tipustum = var3, gbt = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(gbt) & locatum == 'C221' & morfotum %in% d_gbt[var1 == 'C221']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), gbt := 0]
  live[!is.na(gbt) & locatum == 'C239' & morfotum %in% 8000 & tipustum == 3 & 
         defcancer1 %in% c(27,53,54,56,60), gbt := 0]
  live[!is.na(gbt) & locatum == 'C239' & morfotum %in% d_gbt[var1 == 'C239' & var2 != 8000]$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), gbt := 0]
  live[!is.na(gbt) & locatum == 'C240' & morfotum %in% d_gbt[var1 == 'C240']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), gbt := 0]
  live[!is.na(gbt) & locatum == 'C248' & morfotum %in% d_gbt[var1 == 'C248']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), gbt := 0]

  # conducte biliar extrahepàtic
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'EBD - Extra-hepatic bile duct']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  d_ebd <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, d_ebd[, .(locatum = var1, morfotum = var2, tipustum = var3, ebd = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(ebd) & locatum == 'C240' & morfotum %in% d_ebd[var1 == 'C240']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), ebd := 0]
  
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
  
  d_gallblad <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, d_gallblad[, .(locatum = var1, morfotum = var2, tipustum = var3, gallblad = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(gallblad) & locatum == 'C239' & morfotum %in% 8000 & tipustum == 3 & 
         defcancer1 %in% c(27,53,54,56,60), gallblad := 0]
  live[!is.na(gallblad) & locatum == 'C239' & morfotum %in% d_gallblad[var1 == 'C239' & var2 != 8000]$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), gallblad := 0]
  
  # ampolla de vater
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'AOV- Ampulla of Vater']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  d_aov <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, d_aov[, .(locatum = var1, morfotum = var2, tipustum = var3, aov = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(aov) & locatum == 'C241' & morfotum %in% d_aov[var1 == 'C241']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), aov := 0]
  
  # colangiocarcinoma intrahepàtic
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'CCA - Intra-hepatic cholangiocarcinomas']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  d_cca <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, d_cca[, .(locatum = var1, morfotum = var2, tipustum = var3, cca = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(cca) & locatum == 'C220' & morfotum %in% d_cca[var1 == 'C220']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), cca := 0]
  live[!is.na(cca) & locatum == 'C221' & morfotum %in% d_cca[var1 == 'C221']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), cca := 0]
  
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
  
  d_cca_ex <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, d_cca_ex[, .(locatum = var1, morfotum = var2, tipustum = var3, cca_ex = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(cca_ex) & locatum == 'C221' & morfotum %in% d_cca_ex[var1 == 'C221'] & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), cca_ex := 0]
  live[!is.na(cca_ex) & locatum == 'C239' & morfotum %in% d_cca_ex[var1 == 'C239'] & tipustum == 3 & 
         defcancer1 %in% c(27,53,54,56,60), cca_ex := 0]
  live[!is.na(cca_ex) & locatum == 'C240' & morfotum %in% d_cca_ex[var1 == 'C240'] & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), cca_ex := 0]
  live[!is.na(cca_ex) & locatum == 'C248' & morfotum %in% d_cca_ex[var1 == 'C248'] & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), cca_ex := 0]
  live[!is.na(cca_ex) & locatum == 'C249' & morfotum %in% d_cca_ex[var1 == 'C249'] & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), cca_ex := 0]
  
  # colangiocarcinomas extrahepàtic perihilar
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'Perihilar extrahepatic cholangiocarcinomas']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  d_pec <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, d_pec[, .(locatum = var1, morfotum = var2, tipustum = var3, pec = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(pec) & locatum == 'C221' & morfotum %in% d_pec[var1 == 'C221']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), pec := 0]
  live[!is.na(pec) & locatum == 'C240' & morfotum %in% d_pec[var1 == 'C240']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), pec := 0]
  live[!is.na(pec) & locatum == 'C249' & morfotum %in% d_pec[var1 == 'C249']$var2 & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), pec := 0]
  
  # colangiocarcinomas extrahepàtic distal
  cat <- dicc_can[wg_abbrev == 'Live' & subcategory == 'Distal extrahepatic cholangiocarcinomas ']$definition
  cat <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', cat, fixed = T)
  cat <- gsub('If Site eq ', '', gsub('and Morpho in', '&', cat))
  cat <- gsub(' or', '|', gsub('\n', '', cat))
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  d_dec <- morfo[, {
    val <- unlist(strsplit(V2, "\\|")) # extrau els valors de cada part de la variable V2
    rbindlist(lapply(val, function(x) { # transforma la taula de manera que se'm queda per cada part
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) # el valor de la variable V1 com var1
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]])) # i cadascuna de les parts de V2 com var2 i var3 per separat
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)] # ens crea la variable nrow que no ens fa falta però és necessari crear-la per separar la taula
  
  live <- merge(live, d_dec[, .(locatum = var1, morfotum = var2, tipustum = var3, dec = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 28/4/26: incloem correccions per la variable basis_diag1 (defcancer1 per a nosaltres)
  live[!is.na(dec) & locatum == 'C239' & morfotum %in% d_dec[var1 == 'C239'] & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), dec := 0]
  live[!is.na(dec) & locatum == 'C240' & morfotum %in% d_dec[var1 == 'C240'] & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), dec := 0]
  live[!is.na(dec) & locatum == 'C248' & morfotum %in% d_dec[var1 == 'C248'] & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), dec := 0]
  live[!is.na(dec) & locatum == 'C249' & morfotum %in% d_dec[var1 == 'C249'] & tipustum == 3 & 
         defcancer1 %in% c(53,54,56,60), dec := 0]
  
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
  
  # classifiquem els que queden pendents com a morfologia ineligible segons el diccionari
  live[is.na(subcateg), `:=`(subcateg = 'Morphology ineligible', inc_excl = 'Excluded')]

  candiag <- merge(candiag, live[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
  rm(list = ls(pattern = 'd_'))
}

# ho repetim a la taula canpreva
if(canpreva[grepl('^C22|^C23|^C24', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  live <- copy(canpreva[grepl('^C22|^C23|^C24', locatum, ignore.case = T), 
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
  
  # 23/4/26: hi ha un comentari en què diu que s'ha d'afegit un codi concret a la taula en absència d'una variable
  hcc_w <- unique(rbind(hcc_w, data.table(var1 = 'C220', var2 = c(8000, 8140), var3 = 3))[order(var1, var2)])
  
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
  
  # classifiquem els que queden pendents com a morfologia ineligible segons el diccionari
  live[is.na(subcateg), `:=`(subcateg = 'Morphology ineligible', inc_excl = 'Excluded')]
  
  canpreva <- merge(canpreva, live[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  canpreva[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

rm(live, aov, cat, cca, cca_ex, dec, ebd, gallblad, gbt, hcc, hcc_w, ibd, morfo, pec, vars_to_modif)

# # comprovem si hi ha algun codi duplicat
# prova <- dicc_can[wg_abbrev == 'Live', definition][1:11]
# prova <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', prova, fixed = T)
# prova <- gsub(' and Basis_diag1 not in (.,27,53,54,56,60)', '', prova, fixed = T)
# prova <- gsub('Site eq ', '', gsub('and Morpho in', '&', prova))
# prova <- gsub(' or ', '|', gsub('\n', '', prova))
# prova <- gsub(' or', '|', prova)
# prova <- gsub('as above + ', '', prova, fixed = T)
# prova <- gsub('If ', '', prova, fixed = T)
# prova <- gsub(' and Lab_Afp* ge 12', '', prova, fixed = T)
# prova <- gsub(' and Basis_diag1 not in (.,53,54,56,60)', '', prova, fixed = T)
# prova <- gsub(' and Basis_diag1 in (20,25,50,55,70)', '', prova, fixed = T)
# 
# morfo <- unlist(strsplit(prova, split = '|', fixed = T))
# morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '&', fixed = T)))
# morfo[, V1 := gsub("'", '', V1)]
# morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
# morfo[, V2 := trimws(V2, which = 'both')]
# morfo[, clase := c(rep('HCC', 2), rep('HCC_Wide', 2), rep('IBD', 2), rep('GBT', 5), 'EBD',
#                    rep('Gallblad', 2), 'AOV', rep('CCA', 2), rep('CCA_ex', 5), rep('PEC', 3),
#                    rep('DEC',4))]
# 
# morfo <- morfo[, {
#   val <- unlist(strsplit(V2, "\\|"))
#   rbindlist(lapply(val, function(x) {
#     part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE)
#     data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]]), clase)
#   }))
# }, by = 1:nrow(morfo)][, .(var1, var2, var3, clase)]
# morfo <- unique(morfo)
# 
# morfo[,.N, .(var1, var2, var3)][N>1] # hi ha repetits
# morfo <- merge(morfo, morfo[,.N, .(var1, var2, var3)][N>1][, .(var1, var2, var3, rep = 1)], by = paste0('var', 1:3), all.x = T)
# morfo[rep == 1]
# 
# rm(morfo, prova)


#### pulmó #### 
candiag[grepl('^C34', locatum, ignore.case = T), `:=`(clas_iarc = 'Lung', inc_excl = 'Included')]
canpreva[grepl('^C34', locatum, ignore.case = T), `:=`(clas_iarc = 'Lung', inc_excl = 'Included')]


#### pàncrees #### 
candiag[grepl('^C25', locatum, ignore.case = T), `:=`(clas_iarc = 'Panc', inc_excl = 'Included')]
canpreva[grepl('^C25', locatum, ignore.case = T), `:=`(clas_iarc = 'Panc', inc_excl = 'Included')]


#### estòmac i esòfac #### 
# treballem primer amb la taula candiag
if(candiag[grepl('^C15|^C16', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  stom <- copy(candiag[grepl('^C15|^C16', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Stom')])
  
  # adenocarcinoma gàstric
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '01-Gastric adenocarcinoma']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = V2, gac = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # adenocarcinoma gàstric
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '02-Gastric carcinoma']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = V2, gcar = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # tumor gàstric endocrí
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '03-Gastric endocrine tumour']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = V2, get = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # linfoma gàstric
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '04-Gastric lymphoma']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = V2, gli = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # tumor mesenquimal i secundari
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '05-Mesenchymal and secondary tumour']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = V2, mes = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # gàstric inclassificable
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '07-Gastric unclassified']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = V2, gin = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # gàstric ineligible
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == "08-Gastric ineligible'"]$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("'", '', V2)]
  
  morfo <- morfo[, c("var2", "var3") := tstrsplit(V2, "/", fixed = TRUE, type.convert = TRUE)]
  morfo[, `:=`(var2 = as.character(var2), var3 = as.character(var3))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = var2, tipustum = var3, gil = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
 
  # gàstric missing
  stom[cie10 == 'C16', g_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # carcinoma d'esòfac de cèl·lules escamoses
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == "11-Esophagus squamous cell carcinoma'"]$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = V2, escc = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # adenocarcinoma d'esòfac
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '12-Esophagus adenocarcinoma']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = V2, eac = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # altre carcinoma d'esòfac
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '13-Esophagus other carcinoma']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = V2, eoc = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # esòfac inclassificable
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '17-Esophagus unclassified']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = V2, ein = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # esòfac ineligible
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == "18-Esophagus ineligible"]$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("'", '', V2)]
  
  morfo <- morfo[, c("var2", "var3") := tstrsplit(V2, "/", fixed = TRUE, type.convert = TRUE)]
  morfo[, `:=`(var2 = as.character(var2), var3 = as.character(var3))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = var2, tipustum = var3, eil = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # esòfac missing
  stom[cie10 == 'C15', e_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  stom[gac == 1, `:=`(subcateg = 'Gastric adenocarcinoma', inc_excl = 'Included')]
  stom[gcar == 1, `:=`(subcateg = 'Gastric carcinoma', inc_excl = 'Included')]
  stom[get == 1, `:=`(subcateg = 'Gastric endocrine tumour', inc_excl = 'Included')]
  stom[gli == 1, `:=`(subcateg = 'Gastric lymphoma', inc_excl = 'Included')]
  stom[mes == 1, `:=`(subcateg = 'Mesenchymal and secondary tumour', inc_excl = 'Included')]
  stom[gin == 1, `:=`(subcateg = 'Gastric unclassified', inc_excl = 'Included')]
  stom[gil == 1, `:=`(subcateg = 'Gastric ineligible', inc_excl = 'Included')]
  stom[g_mis == 1, `:=`(subcateg = 'Gastric missing', inc_excl = 'Included')]
  stom[escc == 1, `:=`(subcateg = 'Esophagus squamous cell carcinoma', inc_excl = 'Included')]
  stom[eac == 1, `:=`(subcateg = 'Esophagus adenocarcinoma', inc_excl = 'Included')]
  stom[eoc == 1, `:=`(subcateg = 'Esophagus other carcinoma', inc_excl = 'Included')]
  stom[ein == 1, `:=`(subcateg = 'Esophagus unclassified', inc_excl = 'Included')]
  stom[eil == 1, `:=`(subcateg = 'Esophagus ineligible', inc_excl = 'Included')]
  stom[e_mis == 1, `:=`(subcateg = 'Esophagus missing', inc_excl = 'Included')]
  
  # classifiquem els que queden pendents com a no classificables
  stom[is.na(subcateg), `:=`(subcateg = 'Not classified', inc_excl = 'Doubt')]
  
  candiag <- merge(candiag, stom[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

# ho repetim a la taula canpreva
if(canpreva[grepl('^C15|^C16', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  stom <- copy(canpreva[grepl('^C15|^C16', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Stom')])
  
  # adenocarcinoma gàstric
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '01-Gastric adenocarcinoma']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = as.numeric(V2), gac = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # adenocarcinoma gàstric
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '02-Gastric carcinoma']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = as.numeric(V2), gcar = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # tumor gàstric endocrí
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '03-Gastric endocrine tumour']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = as.numeric(V2), get = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # linfoma gàstric
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '04-Gastric lymphoma']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = as.numeric(V2), gli = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # tumor mesenquimal i secundari
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '05-Mesenchymal and secondary tumour']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = as.numeric(V2), mes = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # gàstric inclassificable
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '07-Gastric unclassified']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = as.numeric(V2), gin = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # gàstric ineligible
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == "08-Gastric ineligible'"]$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("'", '', V2)]
  
  morfo <- morfo[, c("var2", "var3") := tstrsplit(V2, "/", fixed = TRUE, type.convert = TRUE)]
  morfo[, `:=`(var2 = as.numeric(var2), var3 = as.numeric(var3))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = var2, tipustum = var3, gil = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # gàstric missing
  stom[cie10 == 'C16', g_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # carcinoma d'esòfac de cèl·lules escamoses
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == "11-Esophagus squamous cell carcinoma'"]$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = as.numeric(V2), escc = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # adenocarcinoma d'esòfac
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '12-Esophagus adenocarcinoma']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = as.numeric(V2), eac = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # altre carcinoma d'esòfac
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '13-Esophagus other carcinoma']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = as.numeric(V2), eoc = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # esòfac inclassificable
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == '17-Esophagus unclassified']$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  # morfo[, V2 := gsub("\\('", '', gsub("'\\)", '', gsub("','", '|', V2, fixed = T)))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = as.numeric(V2), ein = 1)], 
                by = c('cie10', 'morfotum'), all.x = T)
  
  # esòfac ineligible
  cat <- dicc_can[wg_abbrev == 'Stom' & subcategory == "18-Esophagus ineligible"]$definition
  cat <- gsub('Site ', '', gsub('+ ', '|', cat, fixed = T), fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V1 := gsub("'", '', V1)]
  morfo[, V2 := gsub("'", '', V2)]
  
  morfo <- morfo[, c("var2", "var3") := tstrsplit(V2, "/", fixed = TRUE, type.convert = TRUE)]
  morfo[, `:=`(var2 = as.numeric(var2), var3 = as.numeric(var3))]
  
  stom <- merge(stom, morfo[, .(cie10 = V1, morfotum = var2, tipustum = var3, eil = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # esòfac missing
  stom[cie10 == 'C15', e_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  stom[gac == 1, `:=`(subcateg = 'Gastric adenocarcinoma', inc_excl = 'Included')]
  stom[gcar == 1, `:=`(subcateg = 'Gastric carcinoma', inc_excl = 'Included')]
  stom[get == 1, `:=`(subcateg = 'Gastric endocrine tumour', inc_excl = 'Included')]
  stom[gli == 1, `:=`(subcateg = 'Gastric lymphoma', inc_excl = 'Included')]
  stom[mes == 1, `:=`(subcateg = 'Mesenchymal and secondary tumour', inc_excl = 'Included')]
  stom[gin == 1, `:=`(subcateg = 'Gastric unclassified', inc_excl = 'Included')]
  stom[gil == 1, `:=`(subcateg = 'Gastric ineligible', inc_excl = 'Included')]
  stom[g_mis == 1, `:=`(subcateg = 'Gastric missing', inc_excl = 'Included')]
  stom[escc == 1, `:=`(subcateg = 'Esophagus squamous cell carcinoma', inc_excl = 'Included')]
  stom[eac == 1, `:=`(subcateg = 'Esophagus adenocarcinoma', inc_excl = 'Included')]
  stom[eoc == 1, `:=`(subcateg = 'Esophagus other carcinoma', inc_excl = 'Included')]
  stom[ein == 1, `:=`(subcateg = 'Esophagus unclassified', inc_excl = 'Included')]
  stom[eil == 1, `:=`(subcateg = 'Esophagus ineligible', inc_excl = 'Included')]
  stom[e_mis == 1, `:=`(subcateg = 'Esophagus missing', inc_excl = 'Included')]
  
  # classifiquem els que queden pendents com a no classificable
  stom[is.na(subcateg), `:=`(subcateg = 'Not classified', inc_excl = 'Doubt')]
  
  canpreva <- merge(canpreva, stom[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  canpreva[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

rm(stom, cat, morfo)

# # comprovem si hi ha algun codi duplicat
# prova <- dicc_can[wg_abbrev == 'Stom']$definition[1:13]
# prova <- gsub('Site ', '', gsub('+ ', ' & ', prova, fixed = T), fixed = T)
# prova <- gsub("'", '', prova)
# 
# morfo <- as.data.table(do.call(rbind, strsplit(prova, split = '&', fixed = T)))
# morfo[, V1 := trimws(gsub("'", '', V1), which = 'both')]
# morfo[, V2 := trimws(gsub("'", '', gsub(',', '|', V2)), which = 'both')]
# morfo[V2 == 'missing', V2 := NA]
# 
# morfo <- morfo[, {
#   codigos <- unlist(strsplit(V2, "\\|"))
#   temp <- data.table(var1 = V1, codigo = codigos)
#   temp[, c("var2", "var3") := tstrsplit(codigo, "/", fixed = TRUE)]
#   temp[, var2 := as.numeric(var2)]
#   temp[, var3 := fifelse(is.na(var3), NA, var3)]  # NA a cadena vacía
#   temp[, codigo := NULL]
#   temp
# }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
# 
# morfo[,.N] == unique(morfo)[,.N] # són iguals
# 
# rm(prova, morfo)


#### tracte aerodigestiu superior #### 
# treballem primer amb la taula candiag
if(candiag[grepl(paste0(paste0('^C0', c(0:7,9), collapse = '|'), '|', paste0('^C', c(10:15,32), collapse = '|')), locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  uadt <- copy(candiag[grepl(paste0(paste0('^C0', c(0:7,9), collapse = '|'), '|', paste0('^C', c(10:15,32), collapse = '|')), locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Uadt')])
  
  cie <- c(paste0('C0', c(0:7,9)), paste0('C', c(10:15,32)))
  
  # carcinoma de cèl·lules escamoses
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "01-UADT squamous cell carcinoma"]$definition
  cat <- gsub('site ne ', '', gsub(' + ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  # 29/4/26: hem de canviar el valor de C15 per la resta de codis per eliminar aquest valor de la comparativa
  morfo <- rep(morfo, length(cie[cie != 'C15']))
  morfo[seq(1, length(morfo)-1, 2)] <- cie[cie != 'C15']
  
  morfo <- data.table(V1 = morfo[seq(1, length(morfo)-1, 2)], V2 = morfo[seq(2, length(morfo), 2)])
  morfo[, V2 := gsub("'", '', V2)]
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, "\\,"))
    rbindlist(lapply(val, function(x) {
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE)
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]]))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = var1, morfotum = var2, tipustum = var3, scc = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # altres uadt
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "02-UADT others"]$definition
  cat <- gsub('site ne ', '', gsub('+ ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  # 29/4/26: hem de canviar el valor de C15 per la resta de codis per eliminar aquest valor de la comparativa
  morfo <- rep(morfo, length(cie[cie != 'C15']))
  morfo[seq(1, length(morfo)-1, 2)] <- cie[cie != 'C15']
  
  morfo <- data.table(V1 = morfo[seq(1, length(morfo)-1, 2)], V2 = morfo[seq(2, length(morfo), 2)])
  morfo[, V2 := gsub("'", '', V2)]
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, "\\,"))
    rbindlist(lapply(val, function(x) {
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE)
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]]))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = var1, morfotum = var2, tipustum = var3, oth = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # uadt ineligible
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "08-UADT ineligible"]$definition
  cat <- gsub('site ne ', '', gsub(' + ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  # 29/4/26: hem de canviar el valor de C15 per la resta de codis per eliminar aquest valor de la comparativa
  morfo <- rep(morfo, length(cie[cie != 'C15']))
  morfo[seq(1, length(morfo)-1, 2)] <- cie[cie != 'C15']
  
  morfo <- data.table(V1 = morfo[seq(1, length(morfo)-1, 2)], V2 = morfo[seq(2, length(morfo), 2)])
  morfo[, V2 := gsub("'", '', V2)]
  
  morfo <- morfo[, {
    codigos <- unlist(strsplit(V2, "\\,"))
    temp <- data.table(var1 = V1, codigo = codigos)
    temp[, c("var2", "var3") := tstrsplit(codigo, "/", fixed = TRUE)]
    temp[, var2 := as.character(var2)]
    temp[, var3 := fifelse(is.na(var3), NA, var3)]  # NA a cadena vacía
    temp[, codigo := NULL]
    temp
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = var1, morfotum = var2, tipustum = var3, inel = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # uadt missing
  uadt[cie10 != 'C15', u_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # carcinoma de cèl·lules escamoses esòfac
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "11-Esophagus squamous cell carcinoma"]$definition
  cat <- gsub('site eq ', '', gsub(' + ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V2 := trimws(gsub("'", '', V2))]
  morfo <- morfo[, c("var2", "var3") := tstrsplit(V2, "/", fixed = TRUE, type.convert = TRUE)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = V1, morfotum = as.character(var2), tipustum = as.character(var3), e_scc = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # altres esòfac
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "12-Esophagus others"]$definition
  cat <- gsub('site eq ', '', gsub(' + ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V2 := trimws(gsub("'", '', V2))]
  morfo <- morfo[, c("var2", "var3") := tstrsplit(V2, "/", fixed = TRUE, type.convert = TRUE)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = V1, morfotum = as.character(var2), tipustum = as.character(var3), e_oth = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # esòfac ineligible
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "18-Esophagus ineligible"]$definition
  cat <- gsub('site eq ', '', gsub(' + ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V2 := trimws(gsub("'", '', V2))]
  morfo <- morfo[, c("var2", "var3") := tstrsplit(V2, "/", fixed = TRUE, type.convert = TRUE)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = V1, morfotum = as.character(var2), tipustum = as.character(var3), e_inel = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # esòfac missing
  uadt[cie10 == 'C15', e_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  uadt[scc == 1, `:=`(subcateg = 'UADT squamous cell carcinoma', inc_excl = 'Included')]
  uadt[oth == 1, `:=`(subcateg = 'UADT others', inc_excl = 'Included')]
  uadt[inel == 1, `:=`(subcateg = 'UADT inelegible', inc_excl = 'Included')]
  uadt[u_mis == 1, `:=`(subcateg = 'UADT missing', inc_excl = 'Excluded')]
  uadt[e_scc == 1, `:=`(subcateg = 'Esophagus squamous cell carcinoma', inc_excl = 'Included')]
  uadt[e_oth == 1, `:=`(subcateg = 'Esophagus others', inc_excl = 'Included')]
  uadt[e_inel == 1, `:=`(subcateg = 'Esophagus inelegible', inc_excl = 'Included')]
  uadt[e_mis == 1, `:=`(subcateg = 'Esophagus missing', inc_excl = 'Excluded')]
  
  # classifiquem els que queden pendents com a morfologia ineligible segons el diccionari
  uadt[is.na(subcateg) & cie10 != 'C15', `:=`(subcateg = 'UADT ineligible', inc_excl = 'Included')]
  uadt[is.na(subcateg) & cie10 == 'C15', `:=`(subcateg = 'Esophagus ineligible', inc_excl = 'Included')]
  
  candiag <- merge(candiag, uadt[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 29/4/26: classifiquem sols els que no estan classificats com a càncer d'esòfac en la categoria Stom
  candiag[!is.na(clas_iarc_cor) & clas_iarc != 'Stom', clas_iarc := clas_iarc_cor]
  
  # en el creuament de les variables subcateg tenim que hi ha alguns que poden aportar informació
  candiag[!is.na(subcateg_cor) & (clas_iarc != 'Stom' | (clas_iarc == 'Stom' & 
            subcateg == 'Not classified')), `:=`(clas_iarc = clas_iarc_cor, subcateg = subcateg_cor)]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor] # no hi ha problema perquè el que estava a dubte el corregim i la resta es mantenen igual
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

# ho repetim a la taula canpreva
if(canpreva[grepl(paste0(paste0('^C0', c(0:7,9), collapse = '|'), '|', paste0('^C', c(10:15,32), collapse = '|')), locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  uadt <- copy(canpreva[grepl(paste0(paste0('^C0', c(0:7,9), collapse = '|'), '|', paste0('^C', c(10:15,32), collapse = '|')), locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Uadt')])
  
  cie <- c(paste0('C0', c(0:7,9)), paste0('C', c(10:15,32)))
  
  # carcinoma de cèl·lules escamoses
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "01-UADT squamous cell carcinoma"]$definition
  cat <- gsub('site ne ', '', gsub(' + ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  # 29/4/26: hem de canviar el valor de C15 per la resta de codis per eliminar aquest valor de la comparativa
  morfo <- rep(morfo, length(cie[cie != 'C15']))
  morfo[seq(1, length(morfo)-1, 2)] <- cie[cie != 'C15']
  
  morfo <- data.table(V1 = morfo[seq(1, length(morfo)-1, 2)], V2 = morfo[seq(2, length(morfo), 2)])
  morfo[, V2 := gsub("'", '', V2)]
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, "\\,"))
    rbindlist(lapply(val, function(x) {
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE)
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]]))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = var1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), scc = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # altres uadt
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "02-UADT others"]$definition
  cat <- gsub('site ne ', '', gsub('+ ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  # 29/4/26: hem de canviar el valor de C15 per la resta de codis per eliminar aquest valor de la comparativa
  morfo <- rep(morfo, length(cie[cie != 'C15']))
  morfo[seq(1, length(morfo)-1, 2)] <- cie[cie != 'C15']
  
  morfo <- data.table(V1 = morfo[seq(1, length(morfo)-1, 2)], V2 = morfo[seq(2, length(morfo), 2)])
  morfo[, V2 := gsub("'", '', V2)]
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, "\\,"))
    rbindlist(lapply(val, function(x) {
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE)
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]]))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = var1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), oth = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # uadt ineligible
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "08-UADT ineligible"]$definition
  cat <- gsub('site ne ', '', gsub(' + ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  # 29/4/26: hem de canviar el valor de C15 per la resta de codis per eliminar aquest valor de la comparativa
  morfo <- rep(morfo, length(cie[cie != 'C15']))
  morfo[seq(1, length(morfo)-1, 2)] <- cie[cie != 'C15']
  
  morfo <- data.table(V1 = morfo[seq(1, length(morfo)-1, 2)], V2 = morfo[seq(2, length(morfo), 2)])
  morfo[, V2 := gsub("'", '', V2)]
  
  morfo <- morfo[, {
    codigos <- unlist(strsplit(V2, "\\,"))
    temp <- data.table(var1 = V1, codigo = codigos)
    temp[, c("var2", "var3") := tstrsplit(codigo, "/", fixed = TRUE)]
    temp[, var2 := as.character(var2)]
    temp[, var3 := fifelse(is.na(var3), NA, var3)]  # NA a cadena vacía
    temp[, codigo := NULL]
    temp
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = var1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), inel = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # uadt missing
  uadt[cie10 != 'C15', u_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # carcinoma de cèl·lules escamoses esòfac
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "11-Esophagus squamous cell carcinoma"]$definition
  cat <- gsub('site eq ', '', gsub(' + ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V2 := trimws(gsub("'", '', V2))]
  morfo <- morfo[, c("var2", "var3") := tstrsplit(V2, "/", fixed = TRUE, type.convert = TRUE)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = V1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), e_scc = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # altres esòfac
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "12-Esophagus others"]$definition
  cat <- gsub('site eq ', '', gsub(' + ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V2 := trimws(gsub("'", '', V2))]
  morfo <- morfo[, c("var2", "var3") := tstrsplit(V2, "/", fixed = TRUE, type.convert = TRUE)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = V1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), e_oth = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # esòfac ineligible
  cat <- dicc_can[wg_abbrev == 'Uadt' & subcategory == "18-Esophagus ineligible"]$definition
  cat <- gsub('site eq ', '', gsub(' + ', '|', cat, fixed = T))
  cat <- gsub(' +', '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = '|', fixed = T))
  morfo <- as.data.table(t(do.call(rbind, strsplit(morfo, split = ',', fixed = T))))
  morfo[, V2 := trimws(gsub("'", '', V2))]
  morfo <- morfo[, c("var2", "var3") := tstrsplit(V2, "/", fixed = TRUE, type.convert = TRUE)]
  
  uadt <- merge(uadt, morfo[, .(cie10 = V1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), e_inel = 1)], 
                by = c('cie10', 'morfotum', 'tipustum'), all.x = T)
  
  # esòfac missing
  uadt[cie10 == 'C15', e_mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  uadt[scc == 1, `:=`(subcateg = 'UADT squamous cell carcinoma', inc_excl = 'Included')]
  uadt[oth == 1, `:=`(subcateg = 'UADT others', inc_excl = 'Included')]
  uadt[inel == 1, `:=`(subcateg = 'UADT inelegible', inc_excl = 'Included')]
  uadt[u_mis == 1, `:=`(subcateg = 'UADT missing', inc_excl = 'Excluded')]
  uadt[e_scc == 1, `:=`(subcateg = 'Esophagus squamous cell carcinoma', inc_excl = 'Included')]
  uadt[e_oth == 1, `:=`(subcateg = 'Esophagus others', inc_excl = 'Included')]
  uadt[e_inel == 1, `:=`(subcateg = 'Esophagus inelegible', inc_excl = 'Included')]
  uadt[e_mis == 1, `:=`(subcateg = 'Esophagus missing', inc_excl = 'Excluded')]
  
  # classifiquem els que queden pendents com a morfologia ineligible segons el diccionari
  uadt[is.na(subcateg) & cie10 != 'C15', `:=`(subcateg = 'UADT ineligible', inc_excl = 'Included')]
  uadt[is.na(subcateg) & cie10 == 'C15', `:=`(subcateg = 'Esophagus ineligible', inc_excl = 'Included')]
  
  canpreva <- merge(canpreva, uadt[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # 29/4/26: classifiquem sols els que no estan classificats com a càncer d'esòfac en la categoria Stom
  canpreva[!is.na(clas_iarc_cor) & clas_iarc != 'Stom', clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor) & is.na(subcateg), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor] # no hi ha problema perquè el que estava a dubte el corregim i la resta es mantenen igual
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

rm(uadt, cat, morfo, cie)


#### tiroides #### 
# treballem primer amb la taula candiag
if(candiag[grepl('^C73', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  thyr <- copy(candiag[grepl('^C73', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Thyr')])
  
  # papilar
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '01-Papillary']$definition
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))

  thyr[morfotum %in% morfo, papilar := 1]
  
  # folicular
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '02-Follicular']$definition
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))

  thyr[morfotum %in% morfo, folicular := 1]
  
  # medular
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '03-Medullary']$definition
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  
  thyr[morfotum %in% morfo, medular := 1]
  
  # anaplàstic
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '04-Anaplastic']$definition
  cat <- gsub('(', '', gsub(')', '', cat, fixed = T), fixed = T)
  cat <- gsub(',', ' ', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ' ', fixed = T))
  
  # 28/4/26: segons la descripció, deuria ser des del 8020 fins al 8035 ambdós inclosos i la resta
  morfo <- as.numeric(morfo[!is.na(as.numeric(morfo))])
  morfo <- c(morfo[1]:morfo[2], morfo[!c(1:2)])
  
  thyr[morfotum %in% morfo, anaplas := 1]
  
  # linfoma
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '05-Thyroid lymphoma']$definition
  
  thyr[morfotum %in% cat, linfoma := 1]
  
  # sense especificar
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '04-Anaplastic']$definition
  cat <- gsub('(', '', gsub(')', '', cat, fixed = T), fixed = T)
  cat <- gsub(',', ' ', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ' ', fixed = T))
  morfo <- as.numeric(morfo[!is.na(as.numeric(morfo))])
  
  thyr[morfotum %in% morfo, noesp := 1]
  
  # tiroides missing
  thyr[, mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  thyr[papilar == 1, `:=`(subcateg = 'Papillary', inc_excl = 'Included')]
  thyr[folicular == 1, `:=`(subcateg = 'Follicular', inc_excl = 'Included')]
  thyr[medular == 1, `:=`(subcateg = 'Medullary', inc_excl = 'Included')]
  thyr[anaplas == 1, `:=`(subcateg = 'Anaplastic', inc_excl = 'Included')]
  thyr[linfoma == 1, `:=`(subcateg = 'Thyroid lymphoma', inc_excl = 'Included')]
  thyr[noesp == 1, `:=`(subcateg = 'Not otherwise specified', inc_excl = 'Included')]
  thyr[mis == 1, `:=`(subcateg = 'Thyroid missing', inc_excl = 'Included')]
  
  # classifiquem els que queden pendents com a no classificables
  thyr[is.na(subcateg), `:=`(subcateg = 'Thyroid unclassified', inc_excl = 'Included')]
  
  candiag <- merge(candiag, thyr[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

# ho repetim a la taula canpreva
if(canpreva[grepl('^C73', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  thyr <- copy(canpreva[grepl('^C73', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Thyr')])
  
  # papilar
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '01-Papillary']$definition
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  
  thyr[morfotum %in% morfo, papilar := 1]
  
  # folicular
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '02-Follicular']$definition
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  
  thyr[morfotum %in% morfo, folicular := 1]
  
  # medular
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '03-Medullary']$definition
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  
  thyr[morfotum %in% morfo, medular := 1]
  
  # anaplàstic
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '04-Anaplastic']$definition
  cat <- gsub('(', '', gsub(')', '', cat, fixed = T), fixed = T)
  cat <- gsub(',', ' ', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ' ', fixed = T))
  morfo <- as.numeric(morfo[!is.na(as.numeric(morfo))])
  
  thyr[morfotum %in% morfo, anaplas := 1]
  
  # linfoma
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '05-Thyroid lymphoma']$definition
  
  thyr[morfotum %in% cat, linfoma := 1]
  
  # sense especificar
  cat <- dicc_can[wg_abbrev == 'Thyr' & subcategory == '04-Anaplastic']$definition
  cat <- gsub('(', '', gsub(')', '', cat, fixed = T), fixed = T)
  cat <- gsub(',', ' ', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ' ', fixed = T))
  morfo <- as.numeric(morfo[!is.na(as.numeric(morfo))])
  
  thyr[morfotum %in% morfo, noesp := 1]
  
  # tiroides missing
  thyr[, mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  thyr[papilar == 1, `:=`(subcateg = 'Papillary', inc_excl = 'Included')]
  thyr[folicular == 1, `:=`(subcateg = 'Follicular', inc_excl = 'Included')]
  thyr[medular == 1, `:=`(subcateg = 'Medullary', inc_excl = 'Included')]
  thyr[anaplas == 1, `:=`(subcateg = 'Anaplastic', inc_excl = 'Included')]
  thyr[linfoma == 1, `:=`(subcateg = 'Thyroid lymphoma', inc_excl = 'Included')]
  thyr[noesp == 1, `:=`(subcateg = 'Not otherwise specified', inc_excl = 'Included')]
  thyr[mis == 1, `:=`(subcateg = 'Thyroid missing', inc_excl = 'Included')]
  
  # classifiquem els que queden pendents com a no classificables
  thyr[is.na(subcateg), `:=`(subcateg = 'Thyroid unclassified', inc_excl = 'Included')]
  
  canpreva <- merge(canpreva, thyr[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  canpreva[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

rm(thyr, cat, morfo)


#### mama #### 
# treballem primer amb la taula candiag
if(candiag[grepl('^C50', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  brea <- copy(candiag[grepl('^C50', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Brea')])
  
  # epitelial invasiu
  cat <- dicc_can[wg_abbrev == 'Brea' & subcategory == '01-Breast invasive epithelial']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  brea <- merge(brea, morfo[, .(morfotum = V1, tipustum = V2, epitelial = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # in situ
  cat <- dicc_can[wg_abbrev == 'Brea' & subcategory == '02-Breast in-situ']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  brea <- merge(brea, morfo[, .(morfotum = V1, tipustum = V2, insitu = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # ineligible
  cat <- dicc_can[wg_abbrev == 'Brea' & subcategory == '08 - Excl - Breast ineligible']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  brea <- merge(brea, morfo[, .(morfotum = V1, tipustum = V2, inelig = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # exclosos
  cat <- dicc_can[wg_abbrev == 'Brea' & subcategory == '09 - Excl - Breast excluded']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  brea <- merge(brea, morfo[, .(morfotum = V1, tipustum = V2, exclos = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # mama missing
  brea[, mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  brea[epitelial == 1, `:=`(subcateg = 'Breast invasive epithelial', inc_excl = 'Included')]
  brea[insitu == 1, `:=`(subcateg = 'Breast in-situ', inc_excl = 'Included')]
  brea[inelig == 1, `:=`(subcateg = 'Breast ineligible', inc_excl = 'Excluded')]
  brea[exclos == 1, `:=`(subcateg = 'Breast excluded', inc_excl = 'Excluded')]
  brea[mis == 1, `:=`(subcateg = 'Breast missing', inc_excl = 'Excluded')]

  # classifiquem els que queden pendents com a no classificables
  brea[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  candiag <- merge(candiag, brea[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

# ho repetim a la taula canpreva
if(canpreva[grepl('^C50', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  brea <- copy(canpreva[grepl('^C50', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Brea')])
  
  # epitelial invasiu
  cat <- dicc_can[wg_abbrev == 'Brea' & subcategory == '01-Breast invasive epithelial']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  brea <- merge(brea, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), epitelial = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # in situ
  cat <- dicc_can[wg_abbrev == 'Brea' & subcategory == '02-Breast in-situ']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  brea <- merge(brea, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), insitu = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # ineligible
  cat <- dicc_can[wg_abbrev == 'Brea' & subcategory == '08 - Excl - Breast ineligible']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  brea <- merge(brea, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), inelig = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # exclosos
  cat <- dicc_can[wg_abbrev == 'Brea' & subcategory == '09 - Excl - Breast excluded']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  brea <- merge(brea, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), exclos = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # mama missing
  brea[, mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  brea[epitelial == 1, `:=`(subcateg = 'Breast invasive epithelial', inc_excl = 'Included')]
  brea[insitu == 1, `:=`(subcateg = 'Breast in-situ', inc_excl = 'Included')]
  brea[inelig == 1, `:=`(subcateg = 'Breast ineligible', inc_excl = 'Excluded')]
  brea[exclos == 1, `:=`(subcateg = 'Breast excluded', inc_excl = 'Excluded')]
  brea[mis == 1, `:=`(subcateg = 'Breast missing', inc_excl = 'Excluded')]
  
  # classifiquem els que queden pendents com a no classificables
  brea[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  canpreva <- merge(canpreva, brea[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  canpreva[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

rm(brea, cat, morfo)


#### cèrvix uterí #### 
candiag[grepl('^C53', locatum, ignore.case = T), `:=`(clas_iarc = 'Ceru', inc_excl = 'Included')]
canpreva[grepl('^C53', locatum, ignore.case = T), `:=`(clas_iarc = 'Ceru', inc_excl = 'Included')]


#### corpus uterí #### 
# treballem primer amb la taula candiag
if(candiag[grepl('^C54', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  coru <- copy(candiag[grepl('^C54', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Coru')])
  
  # tumor endometri
  cat <- dicc_can[wg_abbrev == 'Coru' & subcategory == '01-Endometrioid tumours']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  coru <- merge(coru, morfo[, .(morfotum = V1, tipustum = V2, endom = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # tumor cèl·lules seroses
  cat <- dicc_can[wg_abbrev == 'Coru' & subcategory == '02-Serous/Clear cell tumours']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  coru <- merge(coru, morfo[, .(morfotum = V1, tipustum = V2, serosa = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # altres
  cat <- dicc_can[wg_abbrev == 'Coru' & subcategory == '08-Others']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  coru <- merge(coru, morfo[, .(morfotum = V1, tipustum = V2, altres = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # exclosos
  cat <- dicc_can[wg_abbrev == 'Coru' & subcategory == '09-Excluded']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  coru <- merge(coru, morfo[, .(morfotum = V1, tipustum = V2, exclosos = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # corpus missing
  coru[, mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  coru[endom == 1, `:=`(subcateg = 'Endometrioid tumours', inc_excl = 'Included')]
  coru[serosa == 1, `:=`(subcateg = 'Serous/Clear cell tumours', inc_excl = 'Included')]
  coru[altres == 1, `:=`(subcateg = 'Others', inc_excl = 'Included')]
  coru[exclosos == 1, `:=`(subcateg = 'Excluded', inc_excl = 'Excluded')]
  coru[mis == 1, `:=`(subcateg = 'Missing', inc_excl = 'Excluded')]
  
  # classifiquem els que queden pendents com a no classificables
  coru[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  candiag <- merge(candiag, coru[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

# ho repetim a la taula canpreva
if(canpreva[grepl('^C54', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  coru <- copy(canpreva[grepl('^C54', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Coru')])
  
  # tumor endometri
  cat <- dicc_can[wg_abbrev == 'Coru' & subcategory == '01-Endometrioid tumours']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  coru <- merge(coru, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), endom = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # tumor cèl·lules seroses
  cat <- dicc_can[wg_abbrev == 'Coru' & subcategory == '02-Serous/Clear cell tumours']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  coru <- merge(coru, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), serosa = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # altres
  cat <- dicc_can[wg_abbrev == 'Coru' & subcategory == '08-Others']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  coru <- merge(coru, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), altres = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # exclosos
  cat <- dicc_can[wg_abbrev == 'Coru' & subcategory == '09-Excluded']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
  
  coru <- merge(coru, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), exclosos = 1)], 
                by = c('morfotum', 'tipustum'), all.x = T)
  
  # corpus missing
  coru[, mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  coru[endom == 1, `:=`(subcateg = 'Endometrioid tumours', inc_excl = 'Included')]
  coru[serosa == 1, `:=`(subcateg = 'Serous/Clear cell tumours', inc_excl = 'Included')]
  coru[altres == 1, `:=`(subcateg = 'Others', inc_excl = 'Included')]
  coru[exclosos == 1, `:=`(subcateg = 'Excluded', inc_excl = 'Excluded')]
  coru[mis == 1, `:=`(subcateg = 'Missing', inc_excl = 'Excluded')]
  
  # classifiquem els que queden pendents com a no classificables
  coru[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  canpreva <- merge(canpreva, coru[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  canpreva[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

rm(coru, cat, morfo)

#### ovari ####
# treballem primer amb la taula candiag
if(candiag[grepl('^C56|^C48|^C570', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  ovar <- copy(candiag[grepl('^C56|^C48|^C570', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Ovar')])
  
  # tumors serosos
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '01-Serous tumours']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))

  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]]))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = var2, tipustum = var3, serous = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # tumor mucinós
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '02-Mucinous tumours']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = var2, tipustum = var3, mucinous = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # tumor endometrioide
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '03-Endometrioid tumours']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = var2, tipustum = var3, endomet = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # tumor cèl·lules clares
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '04-Clear cell tumours']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  # 28/4/26: hem de traure el codi corresponent a C48 + 'xxxx/x' perquè d'acord amb el codi de la iarc
  # correspon al grup anterior
  
  morfo <- morfo[!(var1 == 'C48' & var2 == 'xxxx' & var3 == 'x')]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = var2, tipustum = var3, clear = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # no especificades enlloc
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '05-Not otherwise specified']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = var2, tipustum = var3, altres = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # no classificat
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '07-Ovarian unclassified']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = var2, tipustum = var3, noclas = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # exclosos
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '09-Ovarian excluded']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  cat <- gsub(' +', '|', cat, fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  # 28/4/26: pel tipus de funció, hem d'incorporar un valor fictici (que borrarem després) perquè es pugui executar
  morfo[, V2 := gsub('/,', '/999,', V2, fixed = T)]
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",", fixed = T)) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = F) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  morfo[var3 == 999, var3 := NA]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = var2, tipustum = var3, exclos = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # ovari missing
  ovar[, mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  ovar[serous == 1, `:=`(subcateg = 'Serous tumours', inc_excl = 'Included')]
  ovar[mucinous == 1, `:=`(subcateg = 'Mucinous tumours', inc_excl = 'Included')]
  ovar[endomet == 1, `:=`(subcateg = 'Endometrioid tumours', inc_excl = 'Included')]
  ovar[clear == 1, `:=`(subcateg = 'Clear cell tumours', inc_excl = 'Included')]
  ovar[altres == 1, `:=`(subcateg = 'Not otherwise specified', inc_excl = 'Included')]
  ovar[noclas == 1, `:=`(subcateg = 'Ovarian unclassified', inc_excl = 'Included')]
  ovar[exclos == 1, `:=`(subcateg = 'Ovarian excluded', inc_excl = 'Excluded')]
  ovar[mis == 1, `:=`(subcateg = 'Missing', inc_excl = 'Excluded')]
  
  # classifiquem els que queden pendents com a no classificables
  ovar[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  candiag <- merge(candiag, ovar[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

# ho repetim a la taula canpreva
if(canpreva[grepl('^C56|^C48|^C570', locatum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  ovar <- copy(canpreva[grepl('^C56|^C48|^C570', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Ovar')])
  
  # tumors serosos
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '01-Serous tumours']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), var3 = as.character(part[[2]]))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), serous = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # tumor mucinós
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '02-Mucinous tumours']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), mucinous = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # tumor endometrioide
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '03-Endometrioid tumours']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  ovar <- merge(ovar, morfo[!is.na(as.numeric(var2)), .(locatum = var1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), endomet = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # tumor cèl·lules clares
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '04-Clear cell tumours']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  # 28/4/26: hem de traure el codi corresponent a C48 + 'xxxx/x' perquè d'acord amb el codi de la iarc
  # correspon al grup anterior
  
  morfo <- morfo[!(var1 == 'C48' & var2 == 'xxxx' & var3 == 'x')]
  
  ovar <- merge(ovar, morfo[!is.na(as.numeric(var2)), .(locatum = var1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), clear = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # no especificades enlloc
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '05-Not otherwise specified']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), altres = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # no classificat
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '07-Ovarian unclassified']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",")) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = TRUE) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), noclas = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # exclosos
  cat <- dicc_can[wg_abbrev == 'Ovar' & subcategory == '09-Ovarian excluded']$definition
  cat <- gsub('Site ', '', gsub('site ', '', cat, fixed = T), fixed = T)
  cat <- gsub(' + ', '|', gsub("'", '', cat, fixed = T), fixed = T)
  cat <- gsub(' +', '|', cat, fixed = T)
  
  morfo <- as.data.table(do.call(rbind, strsplit(cat, split = '|', fixed = T)))
  
  # 28/4/26: pel tipus de funció, hem d'incorporar un valor fictici (que borrarem després) perquè es pugui executar
  morfo[, V2 := gsub('/,', '/999,', V2, fixed = T)]
  
  morfo <- morfo[, {
    val <- unlist(strsplit(V2, ",", fixed = T)) 
    rbindlist(lapply(val, function(x) { 
      part <- tstrsplit(x, "/", fixed = TRUE, type.convert = F) 
      data.table(var1 = trimws(V1, which = 'both'), var2 = as.character(part[[1]]), 
                 var3 = trimws(as.character(part[[2]]), which = 'both'))
    }))
  }, by = 1:nrow(morfo)][, .(var1, var2, var3)]
  morfo[var3 == 999, var3 := NA]
  
  ovar <- merge(ovar, morfo[, .(locatum = var1, morfotum = as.numeric(var2), tipustum = as.numeric(var3), exclos = 1)], 
                by = c('locatum', 'morfotum', 'tipustum'), all.x = T)
  
  # ovari missing
  ovar[, mis := ifelse(is.na(morfotum), 1, NA)]
  
  # codifiquem les subcategories
  ovar[serous == 1, `:=`(subcateg = 'Serous tumours', inc_excl = 'Included')]
  ovar[mucinous == 1, `:=`(subcateg = 'Mucinous tumours', inc_excl = 'Included')]
  ovar[endomet == 1, `:=`(subcateg = 'Endometrioid tumours', inc_excl = 'Included')]
  ovar[clear == 1, `:=`(subcateg = 'Clear cell tumours', inc_excl = 'Included')]
  ovar[altres == 1, `:=`(subcateg = 'Not otherwise specified', inc_excl = 'Included')]
  ovar[noclas == 1, `:=`(subcateg = 'Ovarian unclassified', inc_excl = 'Included')]
  ovar[exclos == 1, `:=`(subcateg = 'Ovarian excluded', inc_excl = 'Excluded')]
  ovar[mis == 1, `:=`(subcateg = 'Missing', inc_excl = 'Excluded')]
  
  # classifiquem els que queden pendents com a no classificables
  ovar[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  canpreva <- merge(canpreva, ovar[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  canpreva[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

rm(ovar, cat, morfo)


#### pròstata ####
# treballem primer amb la taula candiag
if(candiag[grepl('^C61', locatum, ignore.case = T),.N] > 0){
  # 28/4/26: per a la pròstata hem de definir primer la variable tnmtype, es defineix com segueix
  # tnmtype = 2: si tnmclas té T3 o T4 o tnmclas té N1, N2 o N3 o tnmclas té M1
  # tnmtype = 1: si tnmclas té T0, T1 o T2 o tnmclas té N0 i NX o tnmclas té M0
  # tnmtype = 9: en altre cas
  
  candiag[grepl('T3|T4|N1|N2|N3|M1', tnmclas, ignore.case = T), tnmtype := 2]
  candiag[grepl('T0|T1|T2|N0|NX|M0', tnmclas, ignore.case = T) & is.na(tnmtype), tnmtype := 1]
  candiag[is.na(tnmtype), tnmtype := 9]
  
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  pros <- copy(candiag[grepl('^C61', locatum, ignore.case = T), 
                       .(id, datadiag, locatum, tnmtype, ESTADTUM, clas_iarc = 'Pros')])
  
  # codifiquem les subcategories
  # sols podem incorporar 2 dels 3 criteris de codificació degut a què la variable resumestat està buida
  pros[tnmtype == 2 | (ESTADTUM %in% 3:5 & tnmtype == 9), `:=`(subcateg = 'Advanced', inc_excl = 'Included')]
  pros[tnmtype == 1 | (ESTADTUM %in% 1:2 & tnmtype == 9), `:=`(subcateg = 'Localised', inc_excl = 'Included')]
  pros[!((tnmtype == 2 | (ESTADTUM %in% 3:5 & tnmtype == 9)) | (tnmtype == 1 | (ESTADTUM %in% 1:2 & tnmtype == 9))), 
       `:=`(subcateg = 'Others', inc_excl = 'Included')]

  # classifiquem els que queden pendents com a no classificables
  pros[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  candiag <- merge(candiag, pros[, .(id, datadiag, locatum, tnmtype, ESTADTUM, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'tnmtype', 'ESTADTUM'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

# no es pot treballar amb la taula canpreva perquè no tenim informació de variables clau per definir-ho

rm(pros)


#### anogenital ####
candiag[grepl('^C21|^C51|^C52|^C53|^C60', locatum, ignore.case = T), `:=`(clas_iarc = 'Anog', inc_excl = 'Included')]
canpreva[grepl('^C21|^C51|^C52|^C53|^C60', locatum, ignore.case = T), `:=`(clas_iarc = 'Anog', inc_excl = 'Included')]


#### cervell ####
candiag[grepl('^C70|^C71|^C72', locatum, ignore.case = T), `:=`(clas_iarc = 'Brai', inc_excl = 'Included')]
canpreva[grepl('^C70|^C71|^C72', locatum, ignore.case = T), `:=`(clas_iarc = 'Brai', inc_excl = 'Included')]


#### unknown primary site ####
candiag[grepl('^C80', locatum, ignore.case = T), `:=`(clas_iarc = 'Cups', inc_excl = 'Included')]
canpreva[grepl('^C80', locatum, ignore.case = T), `:=`(clas_iarc = 'Cups', inc_excl = 'Included')]


#### intestí prim ####
candiag[grepl('^C17', locatum, ignore.case = T), `:=`(clas_iarc = 'Sint', inc_excl = 'Included')]
canpreva[grepl('^C17', locatum, ignore.case = T), `:=`(clas_iarc = 'Sint', inc_excl = 'Included')]


#### pell/melanoma ####
# treballem primer amb la taula candiag
if(candiag[grepl('^C440', locatum, ignore.case = T) | grepl(paste0('^', 872:879, collapse = '|'), morfotum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  skin <- copy(candiag[grepl('^C440', locatum, ignore.case = T) | grepl(paste0('^', 872:879, collapse = '|'), morfotum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Skin')])
  
  # carcinoma de cèl·lules escamoses
  cat <- dicc_can[wg_abbrev == 'Skin' & subcategory == '01-Squamous Cell Carcinoma']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))

  skin[grepl(paste0('^', morfo, collapse = '|'), morfotum, ignore.case = T), `:=`(subcateg = 'Squamous Cell Carcinoma', inc_excl = 'Included')]
  
  # carcinoma de cèl·lules basals
  cat <- dicc_can[wg_abbrev == 'Skin' & subcategory == '02-Basal Cell Carcinoma']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))

  skin[grepl(paste0('^', morfo, collapse = '|'), morfotum, ignore.case = T), `:=`(subcateg = 'Basal Cell Carcinoma', inc_excl = 'Included')]
  
  # melanoma
  cat <- dicc_can[wg_abbrev == 'Skin' & subcategory == '03-Melanoma']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))

  skin[grepl(paste0('^', morfo, collapse = '|'), morfotum, ignore.case = T), `:=`(subcateg = 'Melanoma', inc_excl = 'Included')]
  
  # pell no classificats
  skin[is.na(subcateg) & !is.na(morfotum), `:=`(subcateg = 'Skin unclassified', inc_excl = 'Included')]
  
  # pell missing
  skin[is.na(morfotum), `:=`(subcateg = 'Skin missing', inc_excl = 'Included')]
  
  # classifiquem els que queden pendents com a no classificables
  skin[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  candiag <- merge(candiag, skin[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

# ho repetim a la taula canpreva
if(canpreva[grepl('^C440', locatum, ignore.case = T) | grepl(paste0('^', 872:879, collapse = '|'), morfotum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  skin <- copy(canpreva[grepl('^C440', locatum, ignore.case = T) | grepl(paste0('^', 872:879, collapse = '|'), morfotum, ignore.case = T), 
                       .(id, datadiag, locatum, cie10 = substr(locatum, 1, 3), morfotum, tipustum, clas_iarc = 'Skin')])
  
  # carcinoma de cèl·lules escamoses
  cat <- dicc_can[wg_abbrev == 'Skin' & subcategory == '01-Squamous Cell Carcinoma']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  
  skin[grepl(paste0('^', morfo, collapse = '|'), morfotum, ignore.case = T), `:=`(subcateg = 'Squamous Cell Carcinoma', inc_excl = 'Included')]
  
  # carcinoma de cèl·lules basals
  cat <- dicc_can[wg_abbrev == 'Skin' & subcategory == '02-Basal Cell Carcinoma']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  
  skin[grepl(paste0('^', morfo, collapse = '|'), morfotum, ignore.case = T), `:=`(subcateg = 'Basal Cell Carcinoma', inc_excl = 'Included')]
  
  # melanoma
  cat <- dicc_can[wg_abbrev == 'Skin' & subcategory == '03-Melanoma']$definition
  cat <- gsub("'", '', cat, fixed = T)
  
  morfo <- unlist(strsplit(cat, split = ',', fixed = T))
  
  skin[grepl(paste0('^', morfo, collapse = '|'), morfotum, ignore.case = T), `:=`(subcateg = 'Melanoma', inc_excl = 'Included')]
  
  # pell no classificats
  skin[is.na(subcateg) & !is.na(morfotum), `:=`(subcateg = 'Skin unclassified', inc_excl = 'Included')]
  
  # pell missing
  skin[is.na(morfotum), `:=`(subcateg = 'Skin missing', inc_excl = 'Included')]
  
  # classifiquem els que queden pendents com a no classificables
  skin[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  canpreva <- merge(canpreva, skin[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  canpreva[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
}

rm(skin)


#### leucèmia ####
# extraiem la info de les morfologies que necessitarem

cat <- dicc_can[wg_abbrev == 'Leuk']$code
cat <- gsub("Morphology=", '', cat, fixed = T)
morfo <- unlist(strsplit(cat, split = ',', fixed = T))

candiag[grepl(paste0('^', morfo, collapse = '|'), morfotum, ignore.case = T), `:=`(clas_iarc = 'Leuk', inc_excl = 'Included')]
canpreva[grepl(paste0('^', morfo, collapse = '|'), morfotum, ignore.case = T), `:=`(clas_iarc = 'Leuk', inc_excl = 'Included')]

rm(cat, morfo)


#### linfoma ####
# extraiem la info de les morfologies que necessitarem

cat <- dicc_can[wg_abbrev == 'Lymp']$code
cat <- gsub("Morphology=", '', cat, fixed = T)
codis <- unlist(strsplit(cat, split = ',', fixed = T))

# treballem primer amb la taula candiag
if(candiag[grepl(paste0('^', codis, collapse = '|'), morfotum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  lymp <- copy(candiag[grepl(paste0('^', codis, collapse = '|'), morfotum, ignore.case = T), 
                       .(id, datadiag, locatum, morfotum, tipustum, clas_iarc = 'Lymp', 
                         subcateg, inc_excl)])
  
  dicc_can[wg_abbrev == 'Lymp', subcategory := gsub("'- '", '&', subcategory, fixed = T)]
  dicc_can[wg_abbrev == 'Lymp', subcategory := gsub("' -'", '&', subcategory, fixed = T)]
  dicc_can[wg_abbrev == 'Lymp', subcategory := gsub("' - '", '&', subcategory, fixed = T)]
  
  # com que tots tenen més o menys la mateixa forma, ho fem en una funció o bucle
  for(i in 1:(dicc_can[wg_abbrev == 'Lymp', .N]-1)){
    res <- dicc_can[wg_abbrev == 'Lymp']$subcategory[i]
    res <- unlist(strsplit(res, split = '&', fixed = T))
    res <- trimws(gsub("'", '', res[length(res)], fixed = T), which = 'both')
    if(res == 'Mature T- or NK-NHL')
      res <- 'NK-NHL'
    
    # seleccionem els casos per cadascun dels linfomes
    cat <- dicc_can[wg_abbrev == 'Lymp']$definition[i]
    cat <- gsub(' ', '&', gsub(', ', '&', gsub("'", '', cat, fixed = T), fixed = T), fixed = T)

    morfo <- unlist(strsplit(cat, split = '&', fixed = T))
    morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))

    lymp <- merge(lymp, morfo[, .(morfotum = V1, tipustum = V2, subcateg_cor = res, inc_excl_cor = 'Included')], 
                  by = c('morfotum', 'tipustum'), all.x = T)
    lymp[!is.na(subcateg_cor), subcateg := subcateg_cor]
    lymp[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
    lymp[, c('subcateg_cor', 'inc_excl_cor') := NULL]
  }
  
  # ajuntem la informació que hem generat 
  
  lymp[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  candiag <- merge(candiag, lymp[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  candiag[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  candiag[!is.na(subcateg_cor), subcateg := subcateg_cor]
  candiag[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  candiag[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]

}

# ho repetim a la taula canpreva
if(canpreva[grepl(paste0('^', codis, collapse = '|'), morfotum, ignore.case = T),.N] > 0){
  # per aquest començarem a incorporar les subcategories i inclusions/exclusions
  lymp <- copy(canpreva[grepl(paste0('^', codis, collapse = '|'), morfotum, ignore.case = T), 
                       .(id, datadiag, locatum, morfotum, tipustum, clas_iarc = 'Lymp', 
                         subcateg, inc_excl)])
  
  dicc_can[wg_abbrev == 'Lymp', subcategory := gsub("'- '", '&', subcategory, fixed = T)]
  dicc_can[wg_abbrev == 'Lymp', subcategory := gsub("' -'", '&', subcategory, fixed = T)]
  dicc_can[wg_abbrev == 'Lymp', subcategory := gsub("' - '", '&', subcategory, fixed = T)]
  
  # com que tots tenen més o menys la mateixa forma, ho fem en una funció o bucle
  for(i in 1:(dicc_can[wg_abbrev == 'Lymp', .N]-1)){
    res <- dicc_can[wg_abbrev == 'Lymp']$subcategory[i]
    res <- unlist(strsplit(res, split = '&', fixed = T))
    res <- trimws(gsub("'", '', res[length(res)], fixed = T), which = 'both')
    if(res == 'Mature T- or NK-NHL')
      res <- 'NK-NHL'
    
    # seleccionem els casos per cadascun dels linfomes
    cat <- dicc_can[wg_abbrev == 'Lymp']$definition[i]
    cat <- gsub(' ', '&', gsub(', ', '&', gsub("'", '', cat, fixed = T), fixed = T), fixed = T)
    
    morfo <- unlist(strsplit(cat, split = '&', fixed = T))
    morfo <- as.data.table(do.call(rbind, strsplit(morfo, split = '/', fixed = T)))
    
    lymp <- merge(lymp, morfo[, .(morfotum = as.numeric(V1), tipustum = as.numeric(V2), subcateg_cor = res, inc_excl_cor = 'Included')], 
                  by = c('morfotum', 'tipustum'), all.x = T)
    lymp[!is.na(subcateg_cor), subcateg := subcateg_cor]
    lymp[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
    lymp[, c('subcateg_cor', 'inc_excl_cor') := NULL]
  }
  
  # ajuntem la informació que hem generat 
  
  lymp[is.na(subcateg), `:=`(subcateg = 'Not unclassified', inc_excl = 'Doubt')]
  
  canpreva <- merge(canpreva, lymp[, .(id, datadiag, locatum, morfotum, tipustum, clas_iarc_cor = clas_iarc,
                                     subcateg_cor = subcateg, inc_excl_cor = inc_excl)],
                   by = c('id', 'datadiag', 'locatum', 'morfotum', 'tipustum'), all.x = T)
  canpreva[!is.na(clas_iarc_cor), clas_iarc := clas_iarc_cor]
  canpreva[!is.na(subcateg_cor), subcateg := subcateg_cor]
  canpreva[!is.na(inc_excl_cor), inc_excl := inc_excl_cor]
  canpreva[, c('clas_iarc_cor', 'subcateg_cor', 'inc_excl_cor') := NULL]
  
}

rm(cat, codis, morfo, lymp, res)


#### mesotelioma ####
candiag[grepl('^C384', locatum, ignore.case = T) & grepl('^905', morfotum, ignore.case = T), `:=`(clas_iarc = 'Meso', inc_excl = 'Included')]
canpreva[grepl('^C384', locatum, ignore.case = T) & grepl('^905', morfotum, ignore.case = T), `:=`(clas_iarc = 'Meso', inc_excl = 'Included')]








