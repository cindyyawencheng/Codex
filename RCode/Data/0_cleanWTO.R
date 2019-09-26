# clean WTO membership data

if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}

# -------------------------
# Clean WTO Membership data
# --------------------------

wto = read.csv(paste0(pathMain, '/participation_development/mem-obs-list.csv'), stringsAsFactors = FALSE)
wto$date = as.Date(wto$Membership.Date, '%d %B %Y')
wto$year =  as.numeric(format(wto$date, "%Y"))

# observers = wto[166:188,]
# observers$Members[observers$Members == 'Iran'] = "Iran (Islamic Republic of)"
# observers$Members[observers$Members == "Lebanese Republic"] = "Lebanon"
# observers$Members[observers$Members == "Sao Tome and Principe"] = "Sao Tomé and Principe"
# observers$Members[observers$Members == "Sudan"] =  "Sudan (North + South)"
# 
# observers$observerDum = 1

wto = wto[1:164,]
wto$wtoDum = 1

wto$Members[wto$Members == "Bahrain, Kingdom of" ] = 'Bahrain'
wto$Members[wto$Members == "Bolivia, Plurinational State of" ] = "Bolivia"   
wto$Members[wto$Members == "Côte d’Ivoire" ] = "Côte d'Ivoire"  
wto$Members[wto$Members == "European Union (formerly EC)"] = "European Union"  
wto$Members[wto$Members == "Kuwait, the State of"] = "Kuwait"  
wto$Members[wto$Members == "Kyrgyz Republic"] = "Kyrgyzstan" 
wto$Members[wto$Members == "Lao People’s Democratic Republic"] = "Lao People's Democratic Republic" 
wto$Members[wto$Members == "Moldova, Republic of"] = "Republic of Moldova" 
wto$Members[wto$Members == "Saudi Arabia, Kingdom of"] = "Saudi Arabia"  
wto$Members[wto$Members == "Slovak Republic"] = "Slovakia"
wto$Members[wto$Members == "Chinese Taipei"] ="Taipei, Chinese" 
wto$Members[wto$Members == "Tanzania"] = "Tanzania, United Republic of" 
wto$Members[wto$Members == "United States"] = "United States of America" 
wto$Members[wto$Members == "Venezuela, Bolivarian Republic of"] = "Venezuela"
wto$Members[wto$Members == "North Macedonia"] = "Macedonia, The Former Yugoslav Republic of"


particip[which(particip$actor_name == 'European Union' & particip$event_short == 'CCEURO' & particip$year == "1996"  ),]
particip[which(part)]

wtoLong = expand.grid(wto$Members %>% unique() %>% sort(), 1964:2018) 
names(wtoLong) = c('Members', 'year')
wtoLong = wtoLong[order(wtoLong$Members, wtoLong$year),]
wtoLong$wtoDum = wto$wtoDum[match(paste0(wtoLong$Members, wtoLong$year), paste0(wto$Members, wto$year))]
wtoLong$wtoDum[which(is.na(wtoLong$wtoDum ))] = 0

wtoLong$wtoDum = unlist(lapply(split(wtoLong$wtoDum, wtoLong$Members), cumsum))


save(wtoLong, file = paste0(pathMain, '/participation_development/mem-obs-list_wtoclean.rda'))
