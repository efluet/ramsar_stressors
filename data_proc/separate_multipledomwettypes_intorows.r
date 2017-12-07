
# remove spaces, and split classes by comma
domwettypes = strsplit(gsub(" ", "", ramsites_wettypes$DomWetType), ",")


# loop through the rows
for (r in seq(1, length(domwettypes))){
  
  # for sites with multiple domwettype
  if(length(unlist(domwettypes[r])) > 1){
    
    
    for (d in unlist(domwettypes[r])){
    #print(d)  
    
    outrow <- ramsites_wettypes[r, ]
    outrow[,'SingleDomWetType'] <- d
    ramsites_wettypes <- rbind(ramsites_wettypes, outrow)
       
    }
  }
}




ramsites_wettypes <- unique(ramsites_wettypes)



rm(d, r, domwettypes, outrow)
