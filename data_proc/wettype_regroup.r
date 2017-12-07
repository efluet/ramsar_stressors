

# filter ramsites by wet type and rt nodata
ramsites_wettypes <- ramsites %>%
  
  # remove spaces, and split classes by comma
  dplyr::select('SiteRef', "DomWetType") %>%
  
  # remove sites without a wet type
  filter(DomWetType != "", 
         DomWetType != 'no information available') 





saline <- class_equiv[which(class_equiv$lvl1=='Saline '),'ramsar_class_code']
brackish <- class_equiv[which(class_equiv$lvl1=='Brackish lake/marsh'),'ramsar_class_code']
estuarine <- class_equiv[which(class_equiv$lvl1=='Intertidal / Estuarine'),'ramsar_class_code']
flowing <- class_equiv[which(class_equiv$lvl1=='Flowing water'),'ramsar_class_code']
lakepond <- class_equiv[which(class_equiv$lvl1=="Lakes & ponds" ),'ramsar_class_code']
marsh <- class_equiv[which(class_equiv$lvl1=="Marshes (organic soil)" | 
                             class_equiv$lvl1=="Marshes (inorganic soil)"),'ramsar_class_code']



# reset column
ramsites_wettypes[, 'regroup'] <- NA


# palustrine wetland -----------------------------------------------------------
idx <- grepl(paste(marsh, collapse="|"), ramsites_wettypes$DomWetType)
ramsites_wettypes[idx, 'regroup'] <- "palustrine wetland"


# saline wetland -----------------------------------------------------------
idx <- grepl(paste(saline, collapse="|"), ramsites_wettypes$DomWetType)
ramsites_wettypes[idx, 'regroup'] <- "saline"


# estuarine wetland -----------------------------------------------------------
idx <- grepl(paste(estuarine, collapse="|"), ramsites_wettypes$DomWetType)
ramsites_wettypes[idx, 'regroup'] <- "estuarine wetland"


# lacustrine wetland -----------------------------------------------------------
idx <- grepl(paste(lakepond, collapse="|"), ramsites_wettypes$DomWetType) &
                  grepl(paste(marsh, collapse="|"), ramsites_wettypes$DomWetType)
ramsites_wettypes[idx, 'regroup'] <- "lake & lacustrine wetland"


# riverine wetland -------------------------------------------------------------
idx <- grepl(paste(flowing, collapse="|"), ramsites_wettypes$DomWetType) & 
                  grepl(paste(marsh, collapse="|"), ramsites_wettypes$DomWetType)
ramsites_wettypes[idx, 'regroup'] <- "river & riverine wetland"


# brackish wetland -------------------------------------------------------------
idx <- grepl(paste(brackish, collapse="|"), ramsites_wettypes$DomWetType)
ramsites_wettypes[idx, 'regroup'] <- "Brackish lake/marsh"



# peatland ---------------------------------------------------------------------
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="U"), 'regroup'] <- "peatland"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="Xp"), 'regroup'] <- "peatland"


# river ---------------------------------------------------------------------
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="L"), 'regroup'] <- "lake"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="M"), 'regroup'] <- "lake"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="N"), 'regroup'] <- "lake"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="M,L"), 'regroup'] <- "lake"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="M,N"), 'regroup'] <- "lake"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="N,M"), 'regroup'] <- "lake"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="N,P"), 'regroup'] <- "lake"


# river ---------------------------------------------------------------------
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="O"), 'regroup'] <- "river"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="P"), 'regroup'] <- "river"


# intertidal ---------------------------------------------------------------------
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="G"), 'regroup'] <- "intertidal"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="H"), 'regroup'] <- "intertidal"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="I"), 'regroup'] <- "intertidal"


# lagoon ---------------------------------------------------------------------
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="J"), 'regroup'] <- "lagoon"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="K"), 'regroup'] <- "lagoon"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="J,K"), 'regroup'] <- "lagoon"
ramsites_wettypes[which(ramsites_wettypes$DomWetType=="K,J"), 'regroup'] <- "lagoon"




# get unique combinations of 
ramsites_wettypes_u <- ramsites_wettypes %>%
                       distinct(DomWetType, regroup)




rm(marsh, saline, lvl1_tokeep, brackish, flowing, idx, estuarine, dom_types_tokeep, lakepond)
