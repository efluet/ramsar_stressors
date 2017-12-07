

domwettypes = strsplit(gsub(" ", "", ramsites$DomWetType), ",")

#sapply(strsplit(gsub(" ", "", ramsites$DomWetType), ","), "[[", 1))
#sapply(domwettypes, 1, function(x) length(unlist(x)))



# loop through the rows
for (r in seq(1, length(domwettypes))){
  
  # for sites with multiple domwettype
  if(length(unlist(domwettypes[r])) > 1){
    
    
    for (d in unlist(domwettypes[r])){
    #print(d)  
    
    outrow <- ramsites[r, ]
    outrow[,'SingleDomWetType'] <- d
    ramsites <- rbind(ramsites, outrow)
       
    }
  }
}




ramsites <- unique(ramsites)



rm(d, r, domwettypes, outrow)
