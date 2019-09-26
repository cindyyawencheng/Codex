rm(list= ls())
if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}

cleanFAOFigs = function(var, data){
  data[which(data[,var] == '...'), var] = NA
  data[which(data[,var] == '-'), var] = 0
  data[which(data[,var] == '0 0'), var] = 0
  data[,var] = gsub('F', '', data[,var]) %>% str_trim()
  data[,var] = as.numeric(data[,var])
}

sumNA = function(x){
  if(all(is.na(x)) == TRUE){
    sum(x)
  } else{
    sum(x, na.rm = TRUE)
  }
}


combineData = function(disaggCommodNames, aggCommodName, data){
  data$foodCat = NA
  data[which(data$item %in% disaggCommodNames), 'foodCat'] = aggCommodName
  data = data[which(data$foodCat == aggCommodName),]
  
  data = data %>% group_by(Country, year, foodCat) %>% summarise(expQuant = sumNA(expQuant),
                                                                 impQuant = sumNA(impQuant),
                                                                 expVal = sumNA(expVal),
                                                                 impVal = sumNA(impVal)) 
  data$tradeQuant = data$impQuant +data$expQuant
  data$tradeVal = data$impVal + data$expVal
  names(data)[which(names(data) == 'foodCat')] = 'item'
  return(data.frame(data))
}

  

reCombineData = function(disaggCommodNames, aggCommodName, newName, data){
  
  tradeCols = c( 'expQuant', 'expVal', 'impQuant','impVal', 'tradeQuant',  'tradeVal')
  aggData = data[which(data$item %in% aggCommodName), c('Country', 'year',  tradeCols) ] 
  
  if(length(aggCommodName) > 1){
    aggData = aggData %>% group_by(Country, year) %>% summarise(expQuant = sumNA(expQuant),
                                                                impQuant = sumNA(impQuant),
                                                                expVal = sumNA(expVal),
                                                                impVal = sumNA(impVal))
    
  }
  
  disAggData = data[which(data$item %in% disaggCommodNames),  c('Country', 'year',  tradeCols)]
  
  if(length(disaggCommodNames) > 1){
    disAggData = disAggData %>% group_by(Country, year) %>% summarise(expQuant = sumNA(expQuant),
                                                                      impQuant = sumNA(impQuant),
                                                                      expVal = sumNA(expVal),
                                                                      impVal = sumNA(impVal))}
  
  mergedData = merge(aggData, disAggData, by = c('Country', 'year'), all.x = TRUE)
  
  expQuant = mergedData$expQuant.x - mergedData$expQuant.y
  expVal = mergedData$expVal.x - mergedData$expVal.y
  impQuant = mergedData$impQuant.x - mergedData$impQuant.y
  impVal= mergedData$impVal.x - mergedData$impVal.y
  tradeQuant = expQuant + impQuant
  tradeVal = expVal + impVal
  data = data.frame(Country = mergedData$Country,
                    year = mergedData$year,
                    expQuant, expVal, impQuant, impVal, tradeQuant, tradeVal,
                    item = newName)
  data = data[, c('Country', 'year', 'item', tradeCols)]
  return(data)
}



# # -------------------------------------------------------------------
# # get trade data for food
# # -------------------------------------------------------------------
load(file = paste0(pathMain, '/participation_development/codexWide.rda'))
load(file = paste0(pathMain, '/participation_development/codex_participation_master.rda'))


trade = read.csv(paste0(pathMain, '/participation_development/faoTrade/Trade_Crops_Livestock_E_All_Data.csv'), stringsAsFactors = FALSE)
trade = select(trade, -Area.Code, -Item.Code, -Element.Code, -Unit, -ends_with('F')) %>%
        gather("year", "value", -Area, -Item, -Element)%>%
        spread(Element, value)
trade$year = gsub('Y', '', trade$year) %>% as.numeric()
trade = trade[-which(trade$year %in% 1961:1962),]
names(trade) = c('Country',   'item','year', 'expQuant', 'expVal', 'impQuant', 'impVal')
trade$tradeQuant = trade$expQuant + trade$impQuant
trade$tradeVal = trade$impVal + trade$expVal


# adjust data for collapse of communism
# czech
trade = trade[-which(trade$Country == "Czechia" & trade$year<1993  ), ] 
trade = trade[-which(trade$Country ==  "Czechoslovakia"  & trade$year>=1993  ), ]
trade[which(trade$Country =="Czechoslovakia" ), 'Country'] = "Czechia"


# russia
trade = trade[-which(trade$Country =="Russian Federation" & trade$year<1992  ), ]  
trade = trade[-which(trade$Country =="USSR"  & trade$year>=1992  ), ]  
trade[which(trade$Country =="USSR" ), 'Country'] = "Russian Federation"

# serbia
trade = trade[-c(which(trade$Country =="Serbia and Montenegro"  & trade$year>=2006),
                 which(trade$Country =="Serbia and Montenegro" & trade$year < 2003  )), ] 
trade = trade[-which(trade$Country =="Yugoslav SFR" & trade$year>=2003 ), ] 
trade = trade[-which(trade$Country =="Serbia" & trade$year<2006 ), ]  
trade[which(trade$Country %in% c("Serbia and Montenegro", "Yugoslav SFR") ), 'Country'] = "Serbia"

# # -------------------------------------------------------------------
# # get fisheries trade data
# #http://www.fao.org/fishery/statistics/global-commodities-production/en
# # -------------------------------------------------------------------
# ## build trade value data
# # disagg
faoFishDisaggV = read.csv(paste0(pathMain, '/participation_development/faoTrade/fao_fish_disagg_value.csv'), stringsAsFactors = FALSE)
faoFishDisaggV =faoFishDisaggV[-which(faoFishDisaggV$Unit == ''),]
faoFishDisaggV$Unit = 'Value'
names(faoFishDisaggV)[1:3] = c('Land.Area',  'Commodity', 'Trade.flow')
faoFishDisaggV$Trade.flow =  gsub('s', '', faoFishDisaggV$Trade.flow)


 
# # agg
expFishAgg = read.csv(paste0(pathMain, '/participation_development/faoTrade/fao_exportFisheriesAgg.csv'), stringsAsFactors = FALSE)
impFishAgg = read.csv(paste0(pathMain, '/participation_development/faoTrade/fao_importFisheriesAgg.csv'), stringsAsFactors = FALSE)
faoFishAggV = rbind(expFishAgg, impFishAgg)
faoFishAggV = select(faoFishAggV, -starts_with('S'))
faoFishAggV$Unit ='Value'

# combine
faoFishV = rbind(faoFishAggV[, names(faoFishDisaggV)], faoFishDisaggV )
faoFishV = faoFishV[-which(duplicated(faoFishV[, c('Land.Area', 'Trade.flow', 'Commodity')])), ]
 
## build trade quantity data
# disagg
faoFishDisaggQ = read.csv(paste0(pathMain, '/participation_development/faoTrade/fao_fish_disagg_quantity.csv'), stringsAsFactors = FALSE)
faoFishDisaggQ = faoFishDisaggQ[-which(faoFishDisaggQ$Unit == ''),]
faoFishDisaggQ = faoFishDisaggQ[-which(faoFishDisaggQ$Trade.flow %in% c('Processed production', 'Reexports')),]
names(faoFishDisaggQ)[1:3] = c('Land.Area',  'Commodity', 'Trade.flow')
faoFishDisaggQ$Unit = 'Quant'
faoFishDisaggQ$Trade.flow =  gsub('s', '', faoFishDisaggQ$Trade.flow)

# agg
expFishAggQ = read.csv(paste0(pathMain, '/participation_development/faoTrade/fao_exportFisheriesAggQuant.csv'), stringsAsFactors = FALSE)
impFishAggQ = read.csv(paste0(pathMain, '/participation_development/faoTrade/fao_importFisheriesAggQuant.csv'), stringsAsFactors = FALSE)
faoFishAggQ = rbind(impFishAggQ, expFishAggQ)
faoFishAggQ = select(faoFishAggQ, -starts_with('S'))
faoFishAggQ$Unit = 'Quant'

# combine
faoFishQ = rbind(faoFishAggQ[, names(faoFishDisaggQ)], faoFishDisaggQ )
faoFishQ = faoFishQ[-which(duplicated(faoFishQ[, c('Land.Area', 'Trade.flow', 'Commodity')])), ]


## combine all and reshape
faoFish = rbind(faoFishV, faoFishQ)
faoFish$Trade_Unit = paste0(faoFish$Trade.flow, ' ', faoFish$Unit)

faoFish = select(faoFish, -Unit,-Trade.flow) %>%
  gather("year", "value", -Land.Area, -Commodity,  -Trade_Unit)%>%
  spread(Trade_Unit, value)

faoFish$year = gsub('X', '', faoFish$year) %>% as.numeric()
names(faoFish) = c('Country', 'item', 'year', 'expQuant', 'expVal', 'impQuant', 'impVal')

faoFish$expQuant = cleanFAOFigs('expQuant', faoFish)
faoFish$impQuant = cleanFAOFigs('impQuant', faoFish)
faoFish$expVal = cleanFAOFigs('expVal', faoFish)
faoFish$impVal = cleanFAOFigs('impVal', faoFish)
faoFish$tradeVal = faoFish$impVal + faoFish$expVal
faoFish$tradeQuant = faoFish$impQuant + faoFish$expQuant




# adjust data for collapse of communism
# czech
faoFish[which(trade$Country == "Czechia" & faoFish$year == 1976  ), ] %>% summary()

faoFish[which(faoFish$Country ==  "Czechoslovakia"  & faoFish$year == 2017  ), ] %>% summary()
trade[which(trade$Country =="Czechoslovakia" ), 'Country'] = "Czechia"


# russia
faoFish[which(faoFish$Country =="Russian Federation" & faoFish$year<1992  ), ]  %>% dim()
faoFish[which(faoFish$Country =="Russian Federation" & faoFish$year>=1992  ), ]  %>% dim()
faoFish[which(faoFish$Country =="Un. Sov. Soc. Rep."   & faoFish$year>=1992  ), ]  %>% dim()
faoFish[which(faoFish$Country =="Un. Sov. Soc. Rep."   & faoFish$year<1992  ), ]  %>% dim()
faoFish[which(faoFish$Country =="Un. Sov. Soc. Rep."   ), ] %>% dim()
faoFish[which(faoFish$Country =="Czechoslovakia"   ), ] %>% dim()
t2= faoFish[which(faoFish$Country =="Yugoslavia SFR"   ), 'item ']  
t3 = faoFish[which(faoFish$Country =="Serbia"   ), 'item' ]  
t1 = faoFish[which(faoFish$Country == "Serbia and Montenegro"  ), 'item']  
                    

# Yugoslavia does not have any items in overlap with Serbia or Serbia and Montenegro
 
intersect(t1, t2)
intersect(t3, t2)
intersect(t3, t1)

unique(t3) %>% length()

trade[which(trade$Country =="USSR" ), 'Country'] = "Russian Federation"

# serbia
faoFish[c(which(faoFish$Country =="Serbia and Montenegro"  & faoFish$year>=2006),
                 which(faoFish$Country =="Serbia and Montenegro" & faoFish$year < 2003  )), ] 
faoFish[which(faoFish$Country =="Yugoslav SFR" & faoFish$year>=2003 ), ] 
faoFish[which(faoFish$Country =="Serbia" & faoFish$year<2006 ), ]  

faoFish[which(faoFish$Country =="Serbia and Montenegro"  & faoFish$year<=2006 & faoFish$year>2003 ), 'item'] %>% unique()

faoFish[which(faoFish$Country == 'Serbia and Montenegro' & faoFish$tradeVal == 843336),]
faoFish[which(faoFish$Country == 'Serbia' & faoFish$item == 'Sardines, sardinellas, brisling or sprats, prep. or pres., not minced, nei' & faoFish$year == 2011),]

trade[which(trade$Country %in% c("Serbia and Montenegro", "Yugoslav SFR") ), 'Country'] = "Serbia"
 
# combine fisheries data with all other commodites data
trade = rbind(trade, faoFish[, names(trade)])


# fix country names to be compatablie with codex and with each other
codexCountries = unique(particip$actor_name[which(particip$actor_type == 'state')]) %>% sort()
codexCountries[grep('Serb', codexCountries)]
unique(trade$Country) %>% sort()

trade[which(trade$Country %in% c("Bolivia (Plurinational State of)", "Bolivia (Plurinat.State)")), 'Country'] = "Bolivia" 
trade[which(trade$Country == "C\xf4te d'Ivoire"), 'Country'] =  "Côte d'Ivoire"
trade[which(trade$Country == "China, mainland"  ), 'Country'] = "China"
trade[which(trade$Country == "China, Hong Kong SAR"  ), 'Country'] = "Hong Kong, China" 
trade[which(trade$Country == "China, Macao SAR"   ), 'Country'] =  "Macao, China" 
trade[which(trade$Country == "Congo, Dem. Rep. of the"), 'Country'] = "Democratic Republic of the Congo"
trade[which(trade$Country %in% c("Democratic People's Republic of Korea", "Korea, Dem. People's Rep")), 'Country'] =  "Korea, Democratic People's Republic of"
trade[which(trade$Country == "Republic of Korea"), 'Country'] = "Korea, Republic of" 
trade[which(trade$Country == "Ethiopia PDR"), 'Country'] =  "Ethiopia"  # CHECK
trade[which(trade$Country == "Falkland Is.(Malvinas)" ), 'Country'] =  "Falkland Islands (Malvinas)" # CHECK


trade[which(trade$Country == "Lao People's Dem. Rep."), 'Country'] =  "Lao People's Democratic Republic" 
trade[which(trade$Country == "Iran (Islamic Rep. of)"), 'Country'] = "Iran (Islamic Republic of)"
trade[which(trade$Country == "Moldova, Republic of"), 'Country'] = "Republic of Moldova"
trade[which(trade$Country == "Micronesia, Fed.States of"), 'Country'] =  "Micronesia (Federated States of)"

trade[which(trade$Country == "Netherlands Antilles (former)"), 'Country'] = "Netherlands Antilles" # CHECK

trade[which(trade$Country == "Northern Mariana Is."), 'Country'] = "Northern Mariana Islands" # CHECK

trade[which(trade$Country %in% c("Palestine", "Occupied Palestinian Territory" )), 'Country'] = "Palestine, State of" 
trade[which(trade$Country %in% c("China, Taiwan Province of" ,"Taiwan Province of China"  )), 'Country'] = "Taipei, Chinese"

trade[which(trade$Country %in% c("Saint Vincent/Grenadines" )), 'Country'] = "Saint Vincent and the Grenadines"
trade[which(trade$Country %in% c("Turks and Caicos Is." )), 'Country'] = "Turks and Caicos Islands"

trade[which(trade$Country %in% c("United Republic of Tanzania", "Tanzania, United Rep. of" )), 'Country'] = "Tanzania, United Republic of"
trade[which(trade$Country %in% c( "USSR", "Un. Sov. Soc. Rep.")), 'Country'] = "Russian Federation" 
trade[which(trade$Country %in% c( "Venezuela (Bolivarian Republic of)", "Venezuela, Boliv Rep of")), 'Country'] =  "Venezuela"  

setdiff(trade$Country, unique(particip$actor_name[which(particip$actor_type == 'state')]))

trade[which(trade$Country ==  "Yugoslavia SFR" & trade$year>1993), ]  %>% head()
trade[which(trade$Country ==  "Serbia and Montenegro" & trade$year>1993), ]  %>% summary()
trade[which(trade$Country ==  "Serbia" & trade$year<1990), ]  %>% summary()

"South Sudan" 
# make aggregated trade data where necessary 
agProds = trade$item %>% unique() 

aggTrade = reCombineData("Oil, rapeseed", "Rape+Mustard Oils" ,"Oil, mustardseed", trade )

aggTrade = rbind(aggTrade, 
                 reCombineData("Egg products", "Dairy Products+Eggs"  ,"Dairy products", trade ))

 

aggTrade = rbind(aggTrade, combineData(c('Crustaceans, frozen',
                     'Crustaceans, not frozen',
                     'Crustaceans, prepared or preserved',
                     'Fish fillets, frozen',
                     'Fish meat, whether or not minced, and fillets, fresh or chilled',
                     'Fish meat, whether or not minced, frozen',
                     'Fish prepared or preserved',
                     'Fish, dried, salted or smoked',
                     'Fish, fresh or chilled, excluding fillets and meat',
                     'Fish, frozen, excluding fillets and meat',
                     'Fish, live',
                     'Other products',
                     'Molluscs and other aquatic invertebrates, live, fresh or chilled',
                     'Molluscs and other aquatic invertebrates, other than live, fresh or chilled',
                     'Molluscs and other aquatic invertebrates, prepared or preserved'
), 'Seafood', trade))

 
 
aggTrade = rbind(aggTrade, combineData(c(
                         'Fish fillets, frozen',
                         'Fish meat, whether or not minced, and fillets, fresh or chilled',
                         'Fish meat, whether or not minced, frozen',
                         'Fish prepared or preserved',
                         'Fish, dried, salted or smoked',
                         'Fish, fresh or chilled, excluding fillets and meat',
                         'Fish, frozen, excluding fillets and meat',
                         'Fish, live'), 'Fish, All', trade))


 

mollusc =  trade$item[grep('Abalone|abalone|clam|Clam|Conch|conch|shell|Univalve|Bivalve|Mussel|musselOyster|oyster|Scallop|scallop|venus|Venus|Coquille|coquille', trade$item)] %>% unique()
mollusc = mollusc[-grep('Shrimp|crab|pearl|shelled|in shell|Trochus|Cuttle|coral|with shell|Sea snail|Powder|Squid', mollusc)]
aggTrade = rbind(aggTrade, combineData(c(mollusc),  "Molluscan shellfish", trade))

 

molluscLive = mollusc[grep('live', mollusc)]
molluscLive = molluscLive[-grep('other than live', molluscLive)]


fishAndShellfish = c(mollusc,   'Fish fillets, frozen',
                     'Fish meat, whether or not minced, and fillets, fresh or chilled',
                     'Fish meat, whether or not minced, frozen',
                     'Fish prepared or preserved',
                     'Fish, dried, salted or smoked',
                     'Fish, fresh or chilled, excluding fillets and meat',
                     'Fish, frozen, excluding fillets and meat',
                     'Fish, live')

aggTrade = rbind(aggTrade, combineData(c(
  'Fish fillets, frozen',
  'Fish meat, whether or not minced, and fillets, fresh or chilled',
  'Fish meat, whether or not minced, frozen',
  'Fish prepared or preserved',
  'Fish, dried, salted or smoked',
  'Fish, fresh or chilled, excluding fillets and meat',
  'Fish, frozen, excluding fillets and meat',
  'Fish, live'), 'Fish and Shellfish', trade))

aggTrade = rbind(aggTrade, combineData(molluscLive, "Molluscan shellfish, live", trade))


aggTrade = rbind(aggTrade, combineData(c( "Meat, beef and veal sausages", 
                          "Meat, pig sausages"), "Meat, sausages", trade))


aggTrade = rbind(aggTrade,
                 combineData(c( "Vegetables, dried nes", 
                                "Fruit, dried nes" ),"Fruits and vegetables, dried", trade) )
aggTrade = rbind(aggTrade,
                 combineData(c( "Eggs, dried", 
                                "Eggs, hen, in shell", 
                                'Eggs, liquid', 
                                'Eggs, other bird, in shell'),"Egg products", trade) )
aggTrade = rbind(aggTrade,
                 combineData(c("Anise, badian, fennel, coriander", 
                               "Spices, nes", 
                               'Nutmeg, mace and cardamoms', 
                               "Ginger",  "Peppermint" ),"Spices", trade) )

aggTrade = rbind(aggTrade,
                 combineData(c('Almonds shelled',
                               "Brazil nuts, shelled",
                               'Cashew nuts, shelled', 
                               'Cashew nuts, with shell', 
                               'Hazelnuts, shelled', 
                               'Walnuts, shelled' , 
                               'Walnuts, with shell',
                               'Nuts, nes'),"Treenuts", trade) )


aggTrade = rbind(aggTrade,
                 combineData(c("Apricots",
                               "Peaches and nectarines" ,
                               "Plums and sloes",
                               "Cherries" ,
                               "Cherries, sour",
                               'Mangoes, mangosteens, guavas' ),"Stone fruit", trade) )

aggTrade = rbind(aggTrade,
                 combineData(c("Cocoa, beans",
                               "Cocoa, butter",
                               "Cocoa, powder & cake",
                               "Cocoa, paste" ),"Cocoa", trade) )

aggTrade = rbind(aggTrade,
                 combineData(c("Potatoes, frozen",
                               "Sweet corn frozen",
                               "Vegetables, frozen",
                               "Vegetables, preserved, frozen",
                               "Meat Fresh+Ch+Frozen" ),"Frozen Foods", trade) )
aggTrade = rbind(aggTrade,
                 combineData(c("Animal Oil+Fat+Grs",
                               'Animal Vegetable Oil' ),"Edible Fats and Oils", trade) )

aggTrade = rbind(aggTrade,
                 combineData(c("Fish, fresh or chilled, excluding fillets and meat",
                               "Fish, live"  ),"Fresh fish", trade) )
aggTrade = rbind(aggTrade,
                 combineData(c("Anise, badian, fennel, coriander", 
                               "Spices, nes", 
                               'Nutmeg, mace and cardamoms',
                               "Ginger", 
                               "Peppermint",
                               'Canned Meat nes','Meat Prepared Pres' ),"Spices in processed meat", trade) )

aggTrade = rbind(aggTrade,
                 combineData(c('Bovine Meat',
                               "Meat of Swine"),"Meat, beef and pork", trade) )
aggTrade = rbind(aggTrade,
                 combineData(c("Meat, chicken",
                               "Meat, chicken, canned" ),"Meat, chicken", trade) )

aggTrade = rbind(aggTrade,
                 combineData(c("Anise, badian, fennel, coriander", 
                               "Spices, nes", 
                               'Nutmeg, mace and cardamoms',
                               "Ginger", 
                               "Peppermint",
                               'Canned Meat nes','Meat Prepared Pres' ),"Spices in processed meat", trade) )
aggTrade = rbind(aggTrade,
                 combineData(c("Margarine, liquid", 
                               "Margarine, short" ),"Margarine" , trade) )

aggTrade = rbind(aggTrade,
                 combineData(c("Meat Bovine Fresh",
                               "Meat Poultry Fresh",
                               "Meat Sheep Fresh"  ),"Meat, fresh" , trade) ) # pork?
aggTrade = rbind(aggTrade,
                 combineData(c("Cashew nuts, shelled",   
                               "Cashew nuts, with shell")  ,"Cashew nuts"  , trade) ) 

aggTrade = rbind(aggTrade,
                 combineData(c("Juice, citrus, concentrated",   
                                "Juice, citrus, single strength",
                                'Juice, fruit nes',
                                'Juice, grape',
                                'Juice, grapefruit',
                                'Juice, grapefruit, concentrated',
                                'Juice, orange, concentrated',
                                'Juice, orange, single strength',
                                'Juice, pineapple',
                                'Juice, pineapple, concentrated',
                                'Juice, tomato',
                                'Juice, lemon, concentrated',
                                'Juice, plum, concentrated',
                                'Juice, plum, single strength')  ,"Juices, all"   , trade) ) 


aggTrade = rbind(aggTrade,
                 combineData(c("Soya curd", 'Soya curd'),"Soy protein"  , trade))  


aggTrade = rbind(aggTrade,
                 combineData(c("Soya curd", 
                               'Soya curd') ,"Soy protein"  , trade) ) 

aggTrade = rbind(aggTrade,
                 combineData(c("Beans, green",
                               'Lentils',
                               'Peas, green',
                               'Chick peas'),"Certain pulses", trade)) 

aggTrade = rbind(aggTrade,
                 combineData(c("Lard", 
                               'Tallow', 
                               "Oils, fats of animal nes"),"Named animal fat" , trade))  

minced = agProds[grep('minced', agProds)]
minced = minced[-grep('not minced', minced)]
aggTrade = rbind(aggTrade,
                 combineData(c(minced)  ,"Minced fish"    , trade))  

codHaddock = agProds[grep('Cod|cod|Haddock|haddock', agProds)]
codHaddockFillets = codHaddock[grep('fillet', codHaddock)]
codHaddockFilletsFrozen = codHaddockFillets [grep('frozen', codHaddockFillets )]
aggTrade = rbind(aggTrade,
                 combineData(c(codHaddockFilletsFrozen) ,"Cod and Haddock Fillets, Frozen"     , trade) )


aggTrade = rbind(aggTrade,
                 combineData(c("Flatfish fillets in blocks, frozen, nei",  
                               "Flatfish nei, fillets, frozen") ,"Flatfish Fillets, Frozen"    , trade))

aggTrade = rbind(aggTrade,
                 combineData(c(agProds[grep('Crab|crab', agProds)])  , "Crabs"    , trade))  


lobster = agProds[grep('Lobster|lobster', agProds)]
aggTrade = rbind(aggTrade,
                 combineData(c( lobster ), "Lobsters"    , trade))  

lobsterFrozen = lobster[grep('frozen', lobster)]
lobsterFrozen = lobsterFrozen [-grep('not frozen', lobsterFrozen )]
aggTrade = rbind(aggTrade,
                 combineData(c( lobsterFrozen ), "Lobster, frozen"     , trade))  

smoked = agProds[grep('smoked', agProds)]
smoked = smoked[-grep('not smoked', smoked)]
smokedFish = smoked[-grep('Abalone|lobster|Lobster|lobster|Cephal|Clam|shrimp|Shrimp|Crab|crab|Crayfish|Crust|Cuttle|Eel|Squid|squid|crayfish|Jellyfish|Libver|Invert|invert|Octopus|Scallop|Sea-|Mollusc|Mussel|Oyster|Shark|Stromboid|Univalve', smoked)]

aggTrade = rbind(aggTrade,
                 combineData(c( smokedFish ), "Smoked Fish"     , trade))  

aggTrade = rbind(aggTrade,
                 combineData(c( 'Fish, frozen, excluding fillets and meat',
                                'Fish fillets, frozen',
                                'Fish meat, whether or not minced, frozen' ), "Frozen Fish"      , trade))  

salted = agProds[grep(' salted', agProds)]
saltedFish = salted[-grep('roe|Fish, dried, salted or smoked|Hides|Tusk|Skins|Abalone|lobster|Lobster|lobster|Cephal|Clam|shrimp|Shrimp|Crab|crab|Crayfish|Crust|Cuttle|Eel|Squid|squid|crayfish|Jellyfish|Libver|Invert|invert|Octopus|Scallop|Sea-|Mollusc|Mussel|Oyster|Shark|Stromboid|Univalve', salted)]

aggTrade = rbind(aggTrade,
                 combineData(c( saltedFish), "Salted Fish" , trade))  

crab = agProds[grep('Crab|crab', agProds)]
crabCanned = crab[grep('pres', crab)]
aggTrade = rbind(aggTrade,
                 combineData(c( crabCanned), "Crab, canned", trade))  

mackerl = agProds[grep('mack|Mack', agProds)]
mackerlCanned = mackerl[grep('pres', mackerl)]
aggTrade = rbind(aggTrade,
                 combineData(c(mackerlCanned), "Mackerl, canned" , trade))  

salmon = agProds[grep('Salmon|salmon', agProds)]
salmonCanned = salmon[grep('airtight', salmon)]
aggTrade = rbind(aggTrade,
                 combineData(c(salmonCanned  ), "Salmon, canned" , trade))  

sardine = agProds[grep('Sardine|sardine', agProds)]
sardineCanned = sardine[grep('prep', sardine)]
aggTrade = rbind(aggTrade,
                 combineData(c(sardineCanned  ), "Sardines, canned" , trade))  



shrimp = agProds[grep('Shrimp|shrimp|Prawn|prawn', agProds)]
shrimpCanned = shrimp[grep('airtight', shrimp )]
aggTrade = rbind(aggTrade,
                 combineData(c(shrimpCanned  ), "Shrimps and prawns, canned" , trade))  


shrimpFrozen = shrimp[grep('frozen', shrimp )]
shrimpFrozen = shrimpFrozen[-grep('not frozen', shrimpFrozen)]
aggTrade = rbind(aggTrade,
                 combineData(c(shrimpFrozen  ), "Shrimps and prawns, frozen" , trade))  

tuna = agProds[grep('Tuna|tuna|bonito|Bonito', agProds)]
tunaCanned = tuna[grep('airtight', tuna)]
aggTrade = rbind(aggTrade,
                 combineData(c(tunaCanned   ), "Tuna, canned"  , trade))  


 
scallop = agProds[grep('Scallop|scallop', agProds)]
scallopFreshFrozen = scallop[grep('frozen|fresh', scallop)]
aggTrade = rbind(aggTrade,
                 combineData(c(scallopFreshFrozen   ), "Scallop, fresh or frozen"   , trade))  


aggTrade = rbind(aggTrade,
                 combineData(c("Shark fins, dried, unsalted" , 
                               "Shark fins, dried, whether or not salted, etc."  ), "Shark fins, dried"  , trade))  

aggTrade = rbind(aggTrade,
                 combineData(c(agProds[grep('stick', agProds)] ), "Fish sticks"   , trade))  

 

aggTrade = rbind(aggTrade,
                 combineData(c("Abalone (Haliotis spp.), frozen", 
                               "Abalones, shucked or not, live,fresh or chilled",  
                               "Abalones, shucked or not, other than live,fresh or chilled" ), "Abalone, fresh or frozen"    , trade))  

aggTrade = rbind(aggTrade,
                 combineData(c("Atlantic herring, not minced, prep. or pres., nei" , 
                               "Sprat, prepared or preserved, not minced" ), "Atlantic herring and sprat, salted"     , trade))  

aggTrade = rbind(aggTrade,
                 combineData(c("Gadiformes nei, dried and salted",
                               "Gadiformes, dried, whether or not salted, but not smoked",
                               "Gadiformes, salted or in brine, nei",
                               "Gadiforms, dried, whether or not salted, not smoked (excl. fillets offal and cod)" ), "Gadidforms, salted"     , trade))  

squid = agProds[grep('Squid|squid', agProds)]
squid = squid[-grep('Cutt', squid)]
squidFrozen = squid[grep('frozen', squid)]

aggTrade = rbind(aggTrade,
                 combineData(c(squidFrozen ), "Squid, frozen"      , trade))  

cephalapod = agProds[grep('Squid|squid|Cuttle|cuttle|Ceph|Octopus|octo|ceph', agProds)]
 

aggTrade = rbind(aggTrade,
                 combineData(c(cephalapod ), "Cephalapod"      , trade))  

cannedFish = agProds[grep('pres', agProds)]
cannedFish = cannedFish[-grep('Cuttle|Shrimp|Crab|eels|Scallop|Crusta|Jelly|Mollusc|Mussel|Octo|Other|Clam|Olive|Sweet|Veget|Crab|Crayfish|Shark|crab|lobster|Lobster|Sea-|Abalo|Univa|Aquat|Loins|Oyster|Ceph|Squid', cannedFish)]
aggTrade = rbind(aggTrade,
                 combineData(c(cannedFish ), "Fish, canned"      , trade))  



frozenFish = agProds[grep('frozen', agProds)]
frozenFish = frozenFish[-grep('not frozen', frozenFish)]
frozenFish = frozenFish[-grep('Fish meat, whether or not minced, frozen|Fish, frozen, excluding fillets and meat|Fish fillets, frozen|Cuttle|cuttle|Shrimp|shrimp|Crab|crab|eel|Eel|Shark|shark|Crustacean|crustacean|Squid|squid|Scallop|Jelly|Mollusc|Mussel|Octopus|Other|Clam|Olive|Crayfish|lobster|Lobster|Oyster|oyster|Cephalapod|Sea|Abalone|Univa|Arkshell|Krill|Conger|Bivalve|Ray|Aquatic|Coquille|Loins|roe|Sweet|Vegetable|Potato', frozenFish)]
aggTrade = rbind(aggTrade,
                 combineData(c(frozenFish ), "Fish, frozen"      , trade))  

frozenFishFilletMinced = frozenFish[grep('fillet|mince', frozenFish)]
aggTrade = rbind(aggTrade,
                 combineData(c(frozenFishFilletMinced), "Fish fillets or minced, frozen"      , trade)) 


fishOils = c("Fish body oils, nei",
             "Fish liver oils, nei",
             "Cod liver oil",
             "Halibuts, liver oils",
             "Herring oil",
             "Sardine oil",
             "Anchoveta oil",
             "Capelin oil",
             "Redfish oil",
             "Menhaden oil",
             "Clupeoid oils, nei",
             "Gadiformes, oil, nei") 

aggTrade = rbind(aggTrade,
                 combineData(c(fishOils), "Fish, oil"      , trade))  


vegetableOils = c("Oil, coconut (copra)",
                  "Oil, groundnut",
                  "Oil, cottonseed",
                  "Oil, linseed",
                  "Rape+Mustard Oils",
                  "Oil, maize",
                  "Oil, palm",
                  "Oil, palm kernel",
                  "Oil, sesame",
                  "Oil, soybean",
                  "Oil, sunflower",
                  "Oil, rice bran",
                  "Oil, safflower")

aggTrade = rbind(aggTrade,
                 combineData(c(vegetableOils), "Named Vegetable Oils"      , trade))  

aggTrade = rbind(aggTrade,
                 combineData(c("Cereals", "Pulses"), "Cereals and Pulses"      , trade))  

cannedSeafood = agProds[grep('pres', agProds)]
cannedSeafood = cannedSeafood[-grep('Vegetables|not in airtight|Olives|Sweet corn', cannedSeafood)]
cannedFoods = c(cannedSeafood, 
                "Canned Meat nes",
                "Meat, chicken, canned",
                "Mushrooms, canned",
                "Pineapples canned",
                "Vegetables, canned nes",
                "Olives preserved",
                "Sweet corn prep or preserved")

aggTrade = rbind(aggTrade,
                 combineData(c(cannedFoods), "Canned Food"      , trade)) 

aggTrade = rbind(aggTrade,
                 combineData(c( "Laver, dry",  "Laver, nei",  "Green laver"), "Laver Products"      , trade)) 

aggTrade = rbind(aggTrade,
                 combineData(c("Bovine Meat", "Meat of Swine"), "Meat, Beef and Pork"      , trade)) 

aggTrade = rbind(aggTrade,
                 combineData(c("Meat, goat", "Meat, sheep"), "Meat, Ovis"      , trade)) 


aggTrade = rbind(aggTrade,
                 combineData(c("Hake nei fillets, frozen",
                               "Hake fillets in blocks, frozen",
                               "Argentinian hake fillets, frozen",
                               "Cape hake fillets, frozen",
                               "European hake fillets, frozen",
                               "South Pacific hake fillets, frozen" ), "Hake Fillets, Frozen"      , trade)) 



aggTrade = rbind(aggTrade,
                 combineData(c('Crustaceans, frozen',
                                 'Crustaceans, not frozen',
                                 'Crustaceans, prepared or preserved',
                                 'Fish fillets, frozen',
                                 'Fish meat, whether or not minced, and fillets, fresh or chilled',
                                 'Fish meat, whether or not minced, frozen',
                                 'Fish prepared or preserved',
                                 'Fish, dried, salted or smoked',
                                 'Fish, fresh or chilled, excluding fillets and meat',
                                 'Fish, frozen, excluding fillets and meat',
                                 'Fish, live',
                                 'Other products',
                                 'Molluscs and other aquatic invertebrates, live, fresh or chilled',
                                 'Molluscs and other aquatic invertebrates, other than live, fresh or chilled',
                                 'Molluscs and other aquatic invertebrates, prepared or preserved',
                                 "Food Excl Fish"), "Food, all"      , trade)) 

 
# combine aggregated and disaggregated trade data
trade = rbind(trade, aggTrade)

 
dim(trade)

save(trade, file = paste0(pathMain, '/participation_development/tradeClean.rda'))

# Czechia, Chzechosolvakia
# -----------------------------------------------------
# make committee weighted trade data
# -----------------------------------------------------

load(paste0(pathMain, '/participation_development/codex_participation_clean.rda'))
unique(trade$Country)

setdiff(trade$Country, particip$actor_name)
intersect(trade$Country, particip$actor_name)
head(particip)

# -----------------------------------------------------

# -----------------------------------------------------

adminStd = c("Regional Guidelines for Codex Contact Points and National Codex Committees (Africa)",                                                                                                                
             "Regional Guidelines for Codex Contact Points and National Codex Committees (Asia)",                                                                                                                  
             "Regional Guidelines for Codex Contact Points and National Codex Committees (Near East)")


noInfo = c("Concentrated blackcurrant juice" ,
           "General Guidelines for the Utilization of Vegetable Protein Products (VPP) in Foods",
           "General Standard for Vegetable Protein Products (VPP)",
           "Pulpy nectars of certain small fruits",
           "Specified Animal or Mixed Animal and vegetable Fat Products",
           "Regional Standard for Non-Fermented Soybean Products" ,
           
           "Standard for a Blend of Skimmed Milk and Vegetable Fat in Powdered Form" ,
           "Standard for Aqueous Coconut Products - Coconut Milk and Coconut Cream"  ,
           "Standard for Crackers from Marine and Freshwater Fish, Crustaceans and Molluscan Shellfish",  
           "Standard for Fat Spreads and Blended Spreads",
           "Standard for Food Grade Salt",
           "Unadopted Proposed Draft Regional Standard for Gnetum Spp. Leaves32",
           "Unadopted Proposed Draft Standard for Mixed Vegetable and Animal Ghee",
           "." ,
           codexWide$Title[grep('Title', codexWide$Title)]
)


codexWide$foodCat = NA

codexWide[which(codexWide$Title == "Advisory Lists of Nutrient Compounds for Use in Foods for Special Dietary Uses intented for Infants and Young Children"), 'foodCat'] = "Infant food" 
codexWide[which(codexWide$Title == "Apple juice" ), 'foodCat'] = "Apple Juice" # no data for this
codexWide[which(codexWide$Title == "Apricot, peach and pear nectars"), 'foodCat'] = "Juice, fruit nes" 
codexWide[which(codexWide$Title == "Blackcurrant juice"), 'foodCat'] = "Juice, fruit nes" 
codexWide[which(codexWide$Title == "Canned apricots"), 'foodCat'] = "Apricots" 
codexWide[which(codexWide$Title == "Canned asparagus" ), 'foodCat'] = "Vegetables, canned nes"
codexWide[which(codexWide$Title == "Canned carrots" ), 'foodCat'] = "Vegetables, canned nes"
codexWide[which(codexWide$Title == "Canned grapefruit"), 'foodCat'] = "Grapefruit (inc. pomelos)" 
codexWide[which(codexWide$Title == "Canned green beans and wax beans" ), 'foodCat'] = "Vegetables, canned nes"
codexWide[which(codexWide$Title == "Canned green peas" ), 'foodCat'] = "Vegetables, canned nes"
codexWide[which(codexWide$Title == "Canned mandarin oranges" ), 'foodCat'] = "Oranges"   
codexWide[which(codexWide$Title == "Canned Mangoes"), 'foodCat'] = "Mangoes, mangosteens, guavas"   
codexWide[which(codexWide$Title == "Canned mature processed peas"), 'foodCat'] = "Vegetables, canned nes"
codexWide[which(codexWide$Title == "Canned mushrooms"), 'foodCat'] = "Mushrooms, canned"
codexWide[which(codexWide$Title == "Canned palmito" ), 'foodCat'] = "Vegetables, canned nes"
codexWide[which(codexWide$Title == "Canned peaches" ), 'foodCat'] = "Peaches and nectarines"
codexWide[which(codexWide$Title == "Canned Pears" ), 'foodCat'] =  "Pears"
codexWide[which(codexWide$Title == "Canned plums" ), 'foodCat'] = "Plums and sloes" 
codexWide[which(codexWide$Title == "Canned sweet corn"), 'foodCat'] = "Sweet corn prep or preserved"
codexWide[which(codexWide$Title == "Citrus marmalade" ), 'foodCat'] = "Fruit, prepared nes"  
codexWide[which(codexWide$Title == "Cocoa Butter Confectionery"    ), 'foodCat'] = "Cocoa, butter" 
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Bottled/Packaged Drinking Waters (Other than Natural Mineral Waters)"  ), 'foodCat'] = "Waters,ice etc"  
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Canned Fruit and Vegetable Products"    ), 'foodCat'] ="Vegetables, canned nes"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Collecting, Processing and Marketing of Natural Mineral Waters"     ), 'foodCat'] =   "Waters,ice etc"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Dehydrated Fruits and Vegetables including Edible Fungi"      ), 'foodCat'] = "Fruits and vegetables, dried"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Desiccated Coconut"), 'foodCat'] = "Coconuts, desiccated"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Dried Fruits" ), 'foodCat'] = "Fruit, dried nes" 
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Eggs and Egg Products"  ), 'foodCat'] = "Egg products"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Fresh Fruits and Vegetables"), 'foodCat'] = "Fruit and Vegetables"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Groundnuts (Peanuts)" ), 'foodCat'] = "Groundnuts Total Shelled"
codexWide[which(codexWide$Title ==  "Code of Hygienic Practice for Low and Acidified Low Acid Canned Foods" ), 'foodCat'] = "Canned Food"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Meat"  ), 'foodCat'] = "Total Meat"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Milk and Milk Products" ), 'foodCat'] = "Milk Condensed+Dry+Fresh"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Groundnuts (Peanuts)" ), 'foodCat'] = "Groundnuts Total Shelled"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Powdered Formulae for Infants and Young Children" ), 'foodCat'] = "Infant food"
codexWide[which(codexWide$Title ==  "Code of Hygienic Practice for Precooked and Cooked Foods in Mass Catering" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Spices and Dried Aromatic Herbs" ), 'foodCat'] = "Spices"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for the Processing of Frog Legs"  ), 'foodCat'] = "Meat, nes"
codexWide[which(codexWide$Title ==  "Code of Hygienic Practice for Refrigerated Packaged Foods with Extended Shelf Life" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title == "Code of Hygienic Practice for Tree Nuts"), 'foodCat'] = "Treenuts"
codexWide[which(codexWide$Title == "Code of Practice for Cephalopods"), 'foodCat'] = "Cephalapod"
codexWide[which(codexWide$Title == "Code of Practice for Fish and Fishery Products"), 'foodCat'] = "Fish, All"
codexWide[which(codexWide$Title == "Code of practice for Frozen Battered and/or Breaded Fishery Products"), 'foodCat'] = "Fish fillets, prep. or pres, incl. raw, coated in batter or breadcrum, cooked or not, frozen"
codexWide[which(codexWide$Title == "Code of Practice for Minced Fish"), 'foodCat'] = "Minced fish"
codexWide[which(codexWide$Title == "Code of Practice for Shrimps or Prawns"), 'foodCat'] = "Minced fish"
codexWide[which(codexWide$Title == "Code of Practice for the Packaging and Transport of Fresh Fruit and Vegetables"), 'foodCat'] = "Minced fish"
codexWide[which(codexWide$Title == "Code of Practice for the Prevention and Reduction of Aflatoxin Contamination in Dried Figs"), 'foodCat'] = "Figs dried"
codexWide[which(codexWide$Title == "Code of Practice for the Prevention and Reduction of Aflatoxin Contamination in Peanuts"), 'foodCat'] = "Groundnuts Total Shelled"
codexWide[which(codexWide$Title == "Code of Practice for the Prevention and Reduction of Aflatoxin Contamination in Tree Nuts" ), 'foodCat'] = "Treenuts"
codexWide[which(codexWide$Title == "Code of Practice for the Prevention and Reduction of Arsenic Contamination in Rice"), 'foodCat'] = "Rice"
codexWide[which(codexWide$Title == "Code of Practice for the Prevention and Reduction of Ethyl Carbamate Contamination in Stone Fruit Distillates" ), 'foodCat'] = "Stone fruit"
codexWide[which(codexWide$Title == "Code of Practice for the Prevention and Reduction of Mycotoxin Contamination in Cereals" ), 'foodCat'] = "Cereals"
codexWide[which(codexWide$Title == "Code of Practice for the Prevention and Reduction of Mycotoxins in Spices" ), 'foodCat'] = "Spices"
codexWide[which(codexWide$Title =="Code of Practice for the Prevention and Reduction of Ochratoxin A Contamination in Cocoa" ), 'foodCat'] = "Cocoa"
codexWide[which(codexWide$Title == "Code of Practice for the Prevention and Reduction of Ochratoxin A Contamination in Coffee" ), 'foodCat'] = "Coffee Green+Roast"
codexWide[which(codexWide$Title == "Code of Practice for the Prevention and Reduction of Ochratoxin A Contamination in Wine"         ), 'foodCat'] = "Wine"
codexWide[which(codexWide$Title == "Code of Practice for the Prevention and Reduction of Patulin Contamination in Apple Juice and Apple Juice Ingredients in Other Beverages"), 'foodCat'] ="Apple Juice" 
codexWide[which(codexWide$Title ==  "Code of Practice for the Prevention and Reduction of Inorganic Tin Contamination in Canned Foods"  ), 'foodCat'] = "Canned Food"
codexWide[which(codexWide$Title == "Code of Practice for the Processing and Handling of Quick Frozen Foods"), 'foodCat'] = "Frozen Foods"
codexWide[which(codexWide$Title =="Code of Practice for the Reduction of Aflatoxin B1 in Raw Materials and Supplemental Feedingstuffs for Milk-Producing Animals" ), 'foodCat'] = "Fodder & Feeding stuff"
codexWide[which(codexWide$Title == "Code of Practice for the Reduction of Hydrocyanic Acid (HCN) in Cassava and Cassava Products"  ), 'foodCat'] = "Cassava Equivalent"
codexWide[which(codexWide$Title == "Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk" ), 'foodCat'] = "Frozen Foods"
codexWide[which(codexWide$Title == "Code of Practice on Good Animal Feeding" ), 'foodCat'] = "Fodder & Feeding stuff"
codexWide[which(codexWide$Title == "Codex Code of Practice for Fresh Fish" ), 'foodCat'] = "Frozen Foods"
codexWide[which(codexWide$Title =="Codex maximum level for cadmium in cereals, pulses and legumes" ), 'foodCat'] = "Cereals and Pulses" 
codexWide[which(codexWide$Title == "Codex General Standard for Named Variety Process(ed) Cheese and Spreadable Process(ed) Cheese" ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title == "Codex General Standard for Process(ed) Cheese and Spreadable process(ed) Cheese" ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title == "Codex General Standard for Process(ed) Cheese Preparations, Process(ed) Cheese Food, and Process(ed) Cheese" ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Codex General Standard for Vegetable Juices"  ), 'foodCat'] = "Juice of Vegetables nes"# ; note there is this category, but not fao data for it
codexWide[which(codexWide$Title == "Codex of Hygienic Practice for Dried Milk"  ), 'foodCat'] = "Milk Dry"
codexWide[which(codexWide$Title == "Codex of Hygienic Practice for Mulluscan Shellfish" ), 'foodCat'] = "Molluscan shellfish" 
codexWide[which(codexWide$Title == "Codex of Practice for Canned Fish" ), 'foodCat'] = 'Fish prepared or preserved'
codexWide[which(codexWide$Title == "Codex of Practice for Minced Fish prepared by Mechanical Separation" ), 'foodCat'] = "Minced fish"
codexWide[which(codexWide$Title == "Codex Standard for Edible Palm Stearin" ), 'foodCat'] = "Oil, palm"
codexWide[which(codexWide$Title == "Codex Standard for Specified Vegetable Fat Products" ), 'foodCat'] = "Fixed Vegetable Oils"
codexWide[which(codexWide$Title == "Composite and filled chocolate" ), 'foodCat'] = "Chocolate products nes"
codexWide[which(codexWide$Title == "Concentrated apple juice"  ), 'foodCat'] = "Apple Juice" 
codexWide[which(codexWide$Title == "Concentrated grape juice"), 'foodCat'] = "Juice, grape"
codexWide[which(codexWide$Title == "Concentrated labrusca type grape juice, sweetened"), 'foodCat'] = "Juice, grape"
codexWide[which(codexWide$Title == "Concentrated orange juice"  ), 'foodCat'] = "Juice, orange, concentrated"
codexWide[which(codexWide$Title == "Concentrated pineapple juice preserved exclusively by physical means" ), 'foodCat'] = "Juice, pineapple, concentrated"
codexWide[which(codexWide$Title == "Concentrated pineapple juice with preservatives for manufacturing"  ), 'foodCat'] = "Juice, pineapple, concentrated"
codexWide[which(codexWide$Title == "Dextrose anhydrous"), 'foodCat'] = "Glucose and dextrose"
codexWide[which(codexWide$Title == "Dextrose monohydrate"), 'foodCat'] = "Glucose and dextrose"
codexWide[which(codexWide$Title == "Dried glucose syrup"), 'foodCat'] = "Glucose and dextrose"
codexWide[which(codexWide$Title == "Edible arachis oil"), 'foodCat'] = "Oil, groundnut"
codexWide[which(codexWide$Title == "Edible babassu oil"), 'foodCat'] = "Oil, palm"
codexWide[which(codexWide$Title == "Edible coconut oil"), 'foodCat'] = "Oil, coconut (copra)"
codexWide[which(codexWide$Title == "Edible cottonseed oil"), 'foodCat'] = "Oil, cottonseed"
codexWide[which(codexWide$Title == "Edible grape seed oil"), 'foodCat'] = "Oilseeds nes"
codexWide[which(codexWide$Title == "Edible Ices"), 'foodCat'] = "Waters,ice etc"
codexWide[which(codexWide$Title == "Edible low erucic acid rapeseed oil" ), 'foodCat'] = "Oil, rapeseed"
codexWide[which(codexWide$Title == "Edible maize oil" ), 'foodCat'] = "Oil, maize" 
codexWide[which(codexWide$Title == "Edible palm kernel oil" ), 'foodCat'] = "Oil, palm kernel"
codexWide[which(codexWide$Title == "Edible palm oil" ), 'foodCat'] = "Oil, palm"
codexWide[which(codexWide$Title == "Edible rapeseed oil" ), 'foodCat'] = "Oil, rapeseed"
codexWide[which(codexWide$Title == "Edible safflower seed oil" ), 'foodCat'] = "Oil, maize"
codexWide[which(codexWide$Title == "Edible sesame seed oil" ), 'foodCat'] = "Oil, sesame"
codexWide[which(codexWide$Title == "Edible soya bean oil" ), 'foodCat'] = "Oil, soybean"
codexWide[which(codexWide$Title == "Edible sunflower seed oil"  ), 'foodCat'] = "Oil, sunflower"
codexWide[which(codexWide$Title == "Edible tallow" ), 'foodCat'] = "Tallow" # check in standard if this includes vegetables
codexWide[which(codexWide$Title == "Fructose"  ), 'foodCat'] = "Fructose and syrup, other"
codexWide[which(codexWide$Title == "General Standard for Bottled/Packaged Drinking Waters (Other Than Natural Mineral Waters)" ), 'foodCat'] = "Waters,ice etc"
codexWide[which(codexWide$Title == "General Standard for Cheese"   ), 'foodCat'] = "Cheese and Curd"
codexWide[which(codexWide$Title == "General Standard for Contaminants and Toxins in Food and Feed"  ), 'foodCat'] = "Fodder & Feeding stuff"
codexWide[which(codexWide$Title == "General Standard for Edible Palm Olein" ), 'foodCat'] = "Oil, palm"
codexWide[which(codexWide$Title == "General Standard for Fruit Juices and Nectars"  ), 'foodCat'] = "Juices, all" 
codexWide[which(codexWide$Title == "General Standard for Fruit Juices not covered by Individual Standards"  ), 'foodCat'] =  'Juice, fruit nes' 
codexWide[which(codexWide$Title == "General Standard for Fruit Nectars not covered by Individual Standards"  ), 'foodCat'] =  'Juice, fruit nes'  
codexWide[which(codexWide$Title ==  "General Standard for the Use of Dairy Terms" ), 'foodCat'] = "Dairy products"
codexWide[which(codexWide$Title == "GENERAL PRINCIPLES OF MEAT HYGIENE" ), 'foodCat'] =  "Total Meat" 
codexWide[which(codexWide$Title == "Glucose syrup"), 'foodCat'] = "Glucose and dextrose"
codexWide[which(codexWide$Title == "Grape juice"), 'foodCat'] = "Juice, grape" 
codexWide[which(codexWide$Title == "Grapefruit juice" ), 'foodCat'] = "Juice, grapefruit"
codexWide[which(codexWide$Title == "Group Standard for Cheeses in Brine"  ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title == "Group Standard for Unripened Cheese including Fresh Cheese"  ), 'foodCat'] = "Cheese, whole cow milk"
codexWide[which(codexWide$Title == "Guava nectar"  ), 'foodCat'] = "Mangoes, mangosteens, guavas"
codexWide[which(codexWide$Title == "Guidance for Governments on Prioritizing Hazards in Feed" ), 'foodCat'] = "Fodder & Feeding stuff" 
codexWide[which(codexWide$Title == "Guide for the Microbiological Quality of Spices and Herbs Used in Processed Meat and Poultry Products"   ), 'foodCat'] = "Spices in processed meat"
codexWide[which(codexWide$Title == "Guideline Procedures for the Visual Inspection of Lots of Canned Foods for Unacceptable Defects"  ), 'foodCat'] = "Spices in processed meat"
codexWide[which(codexWide$Title == "Guidelines for Mixed Fruit Juices"   ), 'foodCat'] = "Juice, fruit nes"
codexWide[which(codexWide$Title == "Guidelines for Mixed Fruit Nectars"    ), 'foodCat'] = "Juice, fruit nes"
codexWide[which(codexWide$Title == "Guidelines for Packing Media for Canned Fruits"    ), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title == "Guidelines for the Control of Campylobacter and Salmonella in Chicken Meat"    ), 'foodCat'] = "Meat, chicken"
codexWide[which(codexWide$Title == "Guidelines for the Control of Nontyphoidal Salmonella spp. in Beef and Pork Meat"     ), 'foodCat'] = "Meat, beef and pork"
codexWide[which(codexWide$Title == "Guidelines for the Control of Taenia Saginata in Meat of Domestic Cattle"     ), 'foodCat'] = "Bovine Meat"
codexWide[which(codexWide$Title == "Guidelines for the Control of Trichinella Spp. in Meat of Suidae"  ), 'foodCat'] = "Total Meat"
codexWide[which(codexWide$Title == "Guidelines for the Preservation of Raw Milk by Use of the Lactoperoxidase System"  ), 'foodCat'] = "Milk Fresh"
codexWide[which(codexWide$Title == "Guidelines for the Sensory Evaluation of Fish and Shellfish in Laboratories"   ), 'foodCat'] =  'Fish and Shellfish'
codexWide[which(codexWide$Title == "Guidelines for Vitamin and Mineral Food Supplements"), 'foodCat'] = "Vitamins"
codexWide[which(codexWide$Title == "Guidelines on Formulated Complementary Foods for Older Infants and Young Children" ), 'foodCat'] = "Infant food"
codexWide[which(codexWide$Title == "Guidelines on the Application of Risk Assessment for Feed"  ), 'foodCat'] = "Fodder & Feeding stuff" 
codexWide[which(codexWide$Title == "Guideline Levels for Methylmercury in Fish"  ), 'foodCat'] = "Fish, All"
codexWide[which(codexWide$Title == "Guidelines on Performance Criteria for Methods of Analysis for the Determination of Pesticide Residues in Food and Feed"    ), 'foodCat'] = "Fodder & Feeding stuff"
codexWide[which(codexWide$Title == "Guidelines on the Application of General Principles of Food Hygiene to the Control of Pathogenic Vibrio Species in Seafood" ), 'foodCat'] = "Seafood"
codexWide[which(codexWide$Title == "Guidelines for the Use of Non-Meat Protein Products in Processed Meat and Poultry Products"), 'foodCat'] = "Total Meat"
codexWide[which(codexWide$Title == "Jams (fruit preserves) and jellies" ), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title == "Lactose" ), 'foodCat'] = "Vitamins"
codexWide[which(codexWide$Title == "Lard" ), 'foodCat'] = "Lard"
codexWide[which(codexWide$Title == "Lemon juice" ), 'foodCat'] = "Juice, lemon, concentrated"
codexWide[which(codexWide$Title == "Liquid pulpy mango products"   ), 'foodCat'] = "Mangoes, mangosteens, guavas"
codexWide[which(codexWide$Title == "Margarine"  ), 'foodCat'] = "Margarine" 
codexWide[which(codexWide$Title == "Mayonnaise" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title == "Minarine"), 'foodCat'] ="Margarine" 
codexWide[which(codexWide$Title == "Model Certificate for Fish and Fishery Products"  ), 'foodCat'] = "Fish, All"
codexWide[which(codexWide$Title == "Model Export Certificate for Milk and Milk Products" ), 'foodCat'] = "Dairy products"
codexWide[which(codexWide$Title == "Mustard seed oil"), 'foodCat'] = "Oil, mustardseed"
codexWide[which(codexWide$Title == "Nectars of certain citrus fruits" ), 'foodCat'] = "Juice, citrus, concentrated"
codexWide[which(codexWide$Title == "Non-pulpy blackcurrant nectar" ), 'foodCat'] = "Juice, fruit nes"
codexWide[which(codexWide$Title == "Orange juice"), 'foodCat'] = "Juice, orange, single strength"
codexWide[which(codexWide$Title == "Perch, Ocean, quick-frozen filltes"  ), 'foodCat'] = "Pacific redfishes (=ocean perch), frozen" 
codexWide[which(codexWide$Title == "Pineapple juice")  , 'foodCat'] = "Juice, pineapple"
codexWide[which(codexWide$Title == "Powdered dextrose (icing dextrose)"   ), 'foodCat'] = "Glucose and dextrose"
codexWide[which(codexWide$Title == "Powered sugar (icing sugar)"), 'foodCat'] = "Sugar confectionery"
codexWide[which(codexWide$Title == "Premier jus"), 'foodCat'] = "Tallow"
codexWide[which(codexWide$Title ==  "Pulpy nectars of certain small fruits" ), 'foodCat'] = NA
codexWide[which(codexWide$Title == "Quick Frozen Carrots"), 'foodCat'] = "Vegetables, frozen"
codexWide[which(codexWide$Title == "Quick Frozen Corn-on-the-Cob"), 'foodCat'] =  "Vegetables, frozen"
codexWide[which(codexWide$Title == "Quick Frozen Leeks" ), 'foodCat'] =  "Vegetables, frozen"
codexWide[which(codexWide$Title == "Quick Frozen Whole Kernel Corn" ), 'foodCat'] =  "Vegetables, frozen"
codexWide[which(codexWide$Title == "Quick-Frozen Fillet of Cod and Haddock" ), 'foodCat'] = "Cod and Haddock Fillets, Frozen"
codexWide[which(codexWide$Title == "Quick-Frozen Fillets of Flat Fish" ), 'foodCat'] ="Flatfish Fillets, Frozen"
codexWide[which(codexWide$Title == "Quick-Frozen Fillets of Hake" ), 'foodCat'] = "Hake Fillets, Frozen"
codexWide[which(codexWide$Title == "Recommended International Code of Hygienic Practice for Crabs" ), 'foodCat'] = "Crabs"
codexWide[which(codexWide$Title == "Recommended International Code of Hygienic Practice for Foods for Infants and Children"), 'foodCat'] = "Infant food"
codexWide[which(codexWide$Title == "Recommended International Code of Hygienic Practice for Fresh Meat" ), 'foodCat'] = "Meat, fresh"
codexWide[which(codexWide$Title == "Recommended International Code of Hygienic Practice for Game"  ), 'foodCat'] = "Meat, game"
codexWide[which(codexWide$Title == "Recommended International Code of Hygienic Practice for Lobsters"    ), 'foodCat'] = "Lobsters"
codexWide[which(codexWide$Title == "Recommended International Code of Hygienic Practice for Processed Meat and Poultry Products"      ), 'foodCat'] = "Meat Prepared Pres"
codexWide[which(codexWide$Title == "Recommended International Code of Hygienic Practice for Smoked Fish"   ), 'foodCat'] = "Smoked Fish"
codexWide[which(codexWide$Title ==  "Recommended Code of Hygienic Practice for Poultry Processing"  ), 'foodCat'] = "Poultry Meat" 
codexWide[which(codexWide$Title == "Recommended International Code of Practice for Frozen Fish"    ), 'foodCat'] = "Frozen Fish"
codexWide[which(codexWide$Title == "Recommended International Code of Practice for Salted Fish"  ), 'foodCat'] = "Salted Fish"
codexWide[which(codexWide$Title == "Regional Code of Hygienic Practice for Street-Vended Foods in Asia"), 'foodCat'] =  "Food prep nes"
codexWide[which(codexWide$Title == "Regional Code of Practice for Street-vended Foods (Near East)"), 'foodCat'] =  "Food prep nes"
codexWide[which(codexWide$Title ==  "Regional Code of Hygienic Practice for the Preparation and Sale of Street Foods (Latin America and the Caribbean)" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title == "Regional Guidelines for the Design of Control Measures for Street-Vended Foods (Africa)"), 'foodCat'] =  "Food prep nes"
codexWide[which(codexWide$Title == "Regional Standard for Canned Foul Medames"  ), 'foodCat'] = "Vegetables, canned nes"
codexWide[which(codexWide$Title == "Regional Standard for Canned Humus with Tehena" ), 'foodCat'] = "Vegetables, canned nes"
codexWide[which(codexWide$Title == "Regional Standard for Chilli Sauce"  ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title == "Regional Standard for Culantro Coyote (LAC)"  ), 'foodCat'] = "Anise, badian, fennel, coriander"
codexWide[which(codexWide$Title == "Regional Standard for Doogh"), 'foodCat'] =  "Food prep nes"
codexWide[which(codexWide$Title == "Regional Standard for Date Paste (Near East)"  ), 'foodCat'] = "Dates"
codexWide[which(codexWide$Title == "Regional Standard for Edible Sago Flour (Asia)" ), 'foodCat'] = "Roots and tubers, nes"
codexWide[which(codexWide$Title == "Regional Standard for Fermented Soybean Paste (Asia)"), 'foodCat'] = "Soya paste"
codexWide[which(codexWide$Title == "Regional standard for Ginseng Products"  ), 'foodCat'] = "Spices, nes"
codexWide[which(codexWide$Title == "Regional Standard for Gochujang"   ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title == "Regional Standard for Halwa Tehenia" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title == "Regional Standard for Harissa (Red Hot Pepper Paste)"   ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==  "Regional Standard for Laver Products"  ), 'foodCat'] = "Laver Products"
codexWide[which(codexWide$Title == "Regional Standard for Lucuma"  ), 'foodCat'] = "Fruit, tropical fresh nes"
codexWide[which(codexWide$Title == "Regional Standard for Non-Fermented Soybean Products"   ), 'foodCat'] = NA
codexWide[which(codexWide$Title == "Regional Standard for Tehena"  ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title == "Regional Standard for Tempe"   ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title == "Regional Standard for Unrefined Shea Butter" ), 'foodCat'] = "Nuts, nes"
codexWide[which(codexWide$Title ==  "Regional Standard for Yacon"  ), 'foodCat'] = "Roots and tubers, nes"
codexWide[which(codexWide$Title == "Rendered pork fat"  ), 'foodCat'] = "Fat, pigs"
codexWide[which(codexWide$Title == "Revised Sampling Plan for Peanuts Intended for Further Processing"  ), 'foodCat'] = "Groundnuts Total Shelled"
codexWide[which(codexWide$Title == "Sampling Plans for Prepackaged Foods"  ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title == "Soft sugars"    ), 'foodCat'] = "Sugar refined"
codexWide[which(codexWide$Title ==  "Specified Animal or Mixed Animal and vegetable Fat Products"  ), 'foodCat'] = NA
codexWide[which(codexWide$Title ==  "Standard for a Blend of Evaporated Skimmed Milk and Vegetable Fat"   ), 'foodCat'] = "Milk, reconstituted"
codexWide[which(codexWide$Title == "Standard for Aubergines"  ), 'foodCat'] = "Eggplants (aubergines)"
codexWide[which(codexWide$Title ==  "Standard for a Blend of Skimmed Milk and Vegetable Fat in Powdered Form"   ), 'foodCat'] = NA
codexWide[which(codexWide$Title ==   "Standard for a Blend of Sweetened Condensed Skimmed Milk and Vegetable Fat" ), 'foodCat'] = "Milk, reconstituted"
codexWide[which(codexWide$Title ==   "Standard for Apples"   ), 'foodCat'] = "Apples"
codexWide[which(codexWide$Title ==  "Standard for Aqueous Coconut Products - Coconut Milk and Coconut Cream"   ), 'foodCat'] = NA
codexWide[which(codexWide$Title ==   "Standard for Asparagus"  ), 'foodCat'] = "Asparagus"
codexWide[which(codexWide$Title ==   "Standard for Avocado" ), 'foodCat'] = "Avocados"
codexWide[which(codexWide$Title ==   "Standard for Baby Corn"   ), 'foodCat'] = "Maize"
codexWide[which(codexWide$Title ==   "Standard for Bananas"  ), 'foodCat'] = "Bananas"
codexWide[which(codexWide$Title ==   "Standard for Bitter Cassava"  ), 'foodCat'] = "Cassava"
codexWide[which(codexWide$Title == "Standard for Black, White and Green Peppers"   ), 'foodCat'] =  "Pepper (piper spp.)"
codexWide[which(codexWide$Title ==   "Standard for Boiled Dried Salted Anchovies" ), 'foodCat'] = "Anchovies, salted or in brine"
codexWide[which(codexWide$Title ==   "Standard for Bouillons and Consommés"    ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==   "Standard for Brie"   ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==   "Standard for Butter"   ), 'foodCat'] = "Butter"
codexWide[which(codexWide$Title ==   "Standard for Camembert" ), 'foodCat'] =  "Cheese, processed"
codexWide[which(codexWide$Title ==   "Standard for Canned Applesauce"     ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==   "Standard for Canned Baby Foods"  ), 'foodCat'] = "Infant food"
codexWide[which(codexWide$Title ==   "Standard for Canned Bamboo Shoots"   ), 'foodCat'] = "Vegetables, canned nes"
codexWide[which(codexWide$Title ==   "Standard for Canned Chestnuts and Canned Chestnut Purée" ), 'foodCat'] = "Vegetables, canned nes"
codexWide[which(codexWide$Title ==   "Standard for Canned Crab Meat"  ), 'foodCat'] = "Crab meat, in airtight containers, prep.or pres."
codexWide[which(codexWide$Title ==   "Standard for Canned Finfish"   ), 'foodCat'] = "Fish, canned"
codexWide[which(codexWide$Title ==   "Standard for Canned Fruit Cocktail"  ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==   "Standard for Canned Mackerel and Jack Mackerel"  ), 'foodCat'] =  "Mackerl, canned"
codexWide[which(codexWide$Title ==   "Standard for Canned Pineapple"  ), 'foodCat'] = "Pineapples canned"
codexWide[which(codexWide$Title ==   "Standard for Canned Raspberries"  ), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title ==   "Standard for Canned Salmon" ), 'foodCat'] = "Salmon, canned"
codexWide[which(codexWide$Title ==   "Standard for Canned Sardines and Sardine-Type Products"), 'foodCat'] = "Sardines, canned"
codexWide[which(codexWide$Title ==   "Standard for Canned Shrimps or Prawns" ), 'foodCat'] = "Shrimps and prawns, canned"
codexWide[which(codexWide$Title ==   "Standard for Canned Stone Fruits"  ), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title ==   "Standard for Canned Strawberries"  ), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title ==   "Standard for Canned Tropical Fruit Salad" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==   "Standard for Canned Tuna and Bonito"), 'foodCat'] =  "Tuna, canned"
codexWide[which(codexWide$Title ==   "Standard for Cape Gooseberry"  ), 'foodCat'] =  "Gooseberries" 
codexWide[which(codexWide$Title ==  "Standard for Carambola"  ), 'foodCat'] = "Fruit, tropical fresh nes"
codexWide[which(codexWide$Title ==  "Standard for Certain Canned Citrus Fruits"), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title ==  "Standard for Certain Canned Fruits"  ), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title ==  "Standard for Certain Canned Vegetables"  ), 'foodCat'] = "Vegetables, preserved nes"
codexWide[which(codexWide$Title ==  "Standard for Certain Pulses"), 'foodCat'] = "Certain pulses"
codexWide[which(codexWide$Title ==  "Standard for Chayotes"), 'foodCat'] = "Pumpkins, squash and gourds"
codexWide[which(codexWide$Title ==  "Standard for Cheddar"), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Chilli Peppers" ), 'foodCat'] = "Chillies and peppers, green"
codexWide[which(codexWide$Title ==  "Standard for Chocolate and Chocolate Products"   ), 'foodCat'] = "Chocolate products nes"
codexWide[which(codexWide$Title ==  "Standard for Cocoa (Cacao) Mass (Cocoa/Chocolate Liquor) and Cocoa Cake"  ), 'foodCat'] = "Cocoa, powder & cake"
codexWide[which(codexWide$Title ==  "Standard for Cocoa Butter" ), 'foodCat'] = "Cocoa, butter"
codexWide[which(codexWide$Title ==  "Standard for Cocoa powders (cocoas) and dry mixtures of cocoa and sugars" ), 'foodCat'] = "Cocoa, powder & cake"
codexWide[which(codexWide$Title ==  "Standard for Cooked Cured Chopped Meat"  ), 'foodCat'] = "Bacon and ham"
codexWide[which(codexWide$Title ==  "Standard for Cooked Cured Ham" ), 'foodCat'] = "Bacon and ham"
codexWide[which(codexWide$Title ==  "Standard for Cooked Cured Pork Shoulder"   ), 'foodCat'] = "Bacon and ham"
codexWide[which(codexWide$Title ==  "Standard for Corned Beef"    ), 'foodCat'] = "Meat, beef, preparations"
codexWide[which(codexWide$Title ==  "Standard for Cottage Cheese"  ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Coulommiers"  ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Couscous"   ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==  "Standard for Crackers from Marine and Freshwater Fish, Crustaceans and Molluscan Shellfish"  ), 'foodCat'] = NA
codexWide[which(codexWide$Title ==  "Standard for Cream and Prepared Creams"  ), 'foodCat'] = "Cream fresh"
codexWide[which(codexWide$Title == "Standard for Cream Cheese" ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title == "Standard for Cumin"  ), 'foodCat'] = "Anise, badian, fennel, coriander"
codexWide[which(codexWide$Title ==  "Standard for Dairy Fat Spreads" ), 'foodCat'] = "Dairy products"
codexWide[which(codexWide$Title ==  "Standard for Dairy Permeate Powders" ), 'foodCat'] = "Dairy products"
codexWide[which(codexWide$Title ==  "Standard for Danbo"  ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Dates"   ), 'foodCat'] = "Dates"
codexWide[which(codexWide$Title ==  "Standard for Degermed Maize (Corn) Meal and Maize (Corn) Grits" ), 'foodCat'] = "Flour, maize"
codexWide[which(codexWide$Title ==  "Standard for Desiccated Coconut" ), 'foodCat'] = "Coconuts, desiccated"
codexWide[which(codexWide$Title ==  "Standard for Dried Apricots"), 'foodCat'] = "Apricots, dry"
codexWide[which(codexWide$Title ==  "Standard for Dried Edible Fungi" ), 'foodCat'] = "Vegetables, dried nes"
codexWide[which(codexWide$Title == "Standard for Dried Shark Fins"  ), 'foodCat'] = "Shark fins, dried"
codexWide[which(codexWide$Title == "Standard for Dried Thyme"   ), 'foodCat'] = "Spices, nes"
codexWide[which(codexWide$Title ==  "Standard for Durian")  , 'foodCat'] = "Fruit, tropical fresh nes"
codexWide[which(codexWide$Title ==  "Standard for Edam" ), 'foodCat'] =  "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Durum Wheat Semolina and Durum Wheat Flour" ), 'foodCat'] = "Flour, wheat"
codexWide[which(codexWide$Title ==  "Standard for Edible Casein Products"), 'foodCat'] = "Milk, products of natural constituents nes"
codexWide[which(codexWide$Title ==  "Standard for Edible Cassava Flour"  ), 'foodCat'] = "Starch, cassava"
codexWide[which(codexWide$Title ==  "Standard for Edible Fats and Oils not Covered by Individual Standards"), 'foodCat'] = "Total Meat"
codexWide[which(codexWide$Title == "Standard for Edible Fungi and Fungus Products"  ), 'foodCat'] = "Mushrooms and truffles"
codexWide[which(codexWide$Title ==  "Standard for Emmental"   ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Evaporated Milks"  ), 'foodCat'] = "Milk, whole evaporated"
codexWide[which(codexWide$Title ==  "Standard for Extra Hard Grating Cheese" ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Fermented Milks"  ), 'foodCat'] = "Yoghurt, concentrated or not"
codexWide[which(codexWide$Title ==  "Standard for Fish Oils"  ), 'foodCat'] = "Fish, oil" 
codexWide[which(codexWide$Title ==  "Standard for Fish Sauce"  ), 'foodCat'] = "Fish, sauce"
codexWide[which(codexWide$Title ==  "Standard for Follow-up formula"   ), 'foodCat'] = "Infant food"
codexWide[which(codexWide$Title ==  "Standard for Food Grade Salt"   ), 'foodCat'] = NA
codexWide[which(codexWide$Title == "Standard for Fresh and Quick Frozen Raw Scallop Products"   ), 'foodCat'] = "Scallop, fresh or frozen"
codexWide[which(codexWide$Title ==  "Standard for Fresh Fungus \"Chanterelle\""  ), 'foodCat'] =  "Mushrooms and truffles"
codexWide[which(codexWide$Title ==  "Standard for Gari" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==  "Standard for Ginger"   ), 'foodCat'] = "Ginger"
codexWide[which(codexWide$Title ==  "Standard for Ginseng Products"  ), 'foodCat'] = "Spices, nes"
codexWide[which(codexWide$Title ==  "Standard for Gouda" ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title == "Standard for Grapefruits" ), 'foodCat'] = "Grapefruit (inc. pomelos)"
codexWide[which(codexWide$Title ==  "Standard for Guavas"  ), 'foodCat'] = "Mangoes, mangosteens, guavas"
codexWide[which(codexWide$Title ==  "Standard for Havarti"  ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Honey" ), 'foodCat'] = "Honey, natural"
codexWide[which(codexWide$Title ==  "Standard for Infant Formula" ), 'foodCat'] = "Infant food"
codexWide[which(codexWide$Title ==  "Standard for Infant Formula and Formulas for Special Medical Purposes Intended for Infants"), 'foodCat'] = "Infant food"
codexWide[which(codexWide$Title ==  "Standard for Instant Noodles" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==  "Standard for Jams, Jellies and Marmalades" ), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title ==  "Standard for Kimchi" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==  "Standard for Limes"  ), 'foodCat'] = "Lemons and limes"
codexWide[which(codexWide$Title ==  "Standard for Litchi"  ), 'foodCat'] = "Fruit, tropical fresh nes"
codexWide[which(codexWide$Title ==  "Standard for Live Abalone and for Raw Fresh Chilled or Frozen Abalone for Direct Consumption or for further Processing" ), 'foodCat'] =  "Abalone, fresh or frozen"
codexWide[which(codexWide$Title ==  "Standard for Live and Raw Bivalve Molluscs" ), 'foodCat'] = "Molluscan shellfish, live"
codexWide[which(codexWide$Title ==  "Standard for Longans" ), 'foodCat'] = "Fruit, tropical fresh nes"
codexWide[which(codexWide$Title ==  "Standard for Luncheon Meat" ), 'foodCat'] = "Bacon and ham"
codexWide[which(codexWide$Title ==  "Standard for Maize (Corn)" ), 'foodCat'] = "Maize"
codexWide[which(codexWide$Title ==  "Standard for Mango Chutney"  ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==  "Standard for Mangoes" ), 'foodCat'] =  "Mangoes, mangosteens, guavas"
codexWide[which(codexWide$Title ==  "Standard for Mangosteens" ), 'foodCat'] =  "Mangoes, mangosteens, guavas"
codexWide[which(codexWide$Title ==  "Standard for Mexican Limes" ), 'foodCat'] = "Lemons and limes"
codexWide[which(codexWide$Title ==  "Standard for Milk Powders and Cream Powder" ), 'foodCat'] = "Milk, whole dried"
codexWide[which(codexWide$Title ==  "Standard for Milkfat Products" ), 'foodCat'] = "Ghee, of buffalo milk"
codexWide[which(codexWide$Title ==  "Standard for Mozzarella" ), 'foodCat'] = "Cheese, whole cow milk"
codexWide[which(codexWide$Title ==  "Standard for Named Animal Fats"), 'foodCat'] = "Named animal fat"
codexWide[which(codexWide$Title ==  "Standard for Named Vegetable Oils" ), 'foodCat'] = "Named Vegetable Oils"  
codexWide[which(codexWide$Title ==  "Standard for Natural Mineral Waters" ), 'foodCat'] = "Waters,ice etc"
codexWide[which(codexWide$Title ==  "Standard for Nopal" ), 'foodCat'] = "Vegetables, fresh nes"
codexWide[which(codexWide$Title ==  "Standard for Oats" ), 'foodCat'] = "Oats"
codexWide[which(codexWide$Title ==  "Standard for Okra" ), 'foodCat'] = "Vegetables, fresh nes" 
codexWide[which(codexWide$Title ==  "Standard for Olive Oils and Olive Pomace Oils"  ), 'foodCat'] = "Olive Oil,Total"
codexWide[which(codexWide$Title ==  "Standard for Oranges" ), 'foodCat'] = "Oranges"
codexWide[which(codexWide$Title ==  "Standard for Papaya"  ), 'foodCat'] = "Papayas"
codexWide[which(codexWide$Title ==  "Standard for Passion Fruit"), 'foodCat'] = "Fruit, tropical fresh nes"
codexWide[which(codexWide$Title ==  "Standard for Peanuts"  ), 'foodCat'] = "Groundnuts Total Shelled"
codexWide[which(codexWide$Title ==  "Standard for Pearl Millet Flour" ), 'foodCat'] = "Flour, cereals"
codexWide[which(codexWide$Title ==  "Standard for Pickled Cucumbers (Cucumber Pickles)" ), 'foodCat'] = "Vegetables in vinegar"
codexWide[which(codexWide$Title ==  "Standard for Pickled Fruits and Vegetables" ), 'foodCat'] = "Vegetables in vinegar"
codexWide[which(codexWide$Title ==  "Standard for Pineapples" ), 'foodCat'] = "Pineapples"
codexWide[which(codexWide$Title ==  "Standard for Pitahayas" ), 'foodCat'] = "Fruit, tropical fresh nes"
codexWide[which(codexWide$Title ==  "Standard for Pomegranate" ), 'foodCat'] = "Fruit, fresh nes"
codexWide[which(codexWide$Title ==  "Standard for Preserved Tomatoes" ), 'foodCat'] = "Vegetables in vinegar"
codexWide[which(codexWide$Title ==  "Standard for Prickly Pear"  ), 'foodCat'] = "Fruit, fresh nes"
codexWide[which(codexWide$Title ==  "Standard for Processed Cereal-Based Foods for Infants and Young Children" ), 'foodCat'] = "Infant food"
codexWide[which(codexWide$Title ==  "Standard for Processed Tomato Concentrates"  ), 'foodCat'] = "Tomatoes, paste"
codexWide[which(codexWide$Title ==  "Standard for Provolone"  ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Pummelos"  ), 'foodCat'] = "Grapefruit (inc. pomelos)"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Bilberries"  ), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Blocks of Fish Fillets, Minced Fish Flesh and Mixtures of Fillets and Minced Fish Flesh"  ), 'foodCat'] = "Fish fillets or minced, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Blueberries"  ), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Broccoli"  ), 'foodCat'] = "Vegetables, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Brussels Sprouts"   ), 'foodCat'] = "Vegetables, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Cauliflower"   ), 'foodCat'] = "Vegetables, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Finfish, Uneviscerated and Eviscerated"  ), 'foodCat'] = "Fish, frozen" 
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Fish Fillets"  ), 'foodCat'] = "Fish fillets, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Fish Sticks (Fish Fingers), Fish Portions and Fish Fillets - Breaded or in Batter"  ), 'foodCat'] = "Fish sticks"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen French Fried Potatoes" ), 'foodCat'] = "Vegetables, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Green and Wax Beans" ), 'foodCat'] = "Vegetables, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Lobsters" ), 'foodCat'] = "Lobster, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Peaches"  ), 'foodCat'] = "Fruit, prepared nes"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Peas"), 'foodCat'] =  "Vegetables, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Raspberries" ), 'foodCat'] =  "Fruit, prepared nes"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Raw Squid"), 'foodCat'] = "Squid, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Shrimps or Prawns" ), 'foodCat'] = "Shrimps and prawns, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Spinach" ), 'foodCat'] =  "Vegetables, frozen"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Strawberries" ), 'foodCat'] =  "Fruit, prepared nes"
codexWide[which(codexWide$Title ==  "Standard for Quick Frozen Vegetables" ), 'foodCat'] =  "Vegetables, frozen"
codexWide[which(codexWide$Title ==  "Standard for Raisins" ), 'foodCat'] = "Raisins"
codexWide[which(codexWide$Title ==  "Standard for Rambutan" ), 'foodCat'] = "Fruit, tropical fresh nes"
codexWide[which(codexWide$Title ==  "Standard for Raw Cane Sugar" ), 'foodCat'] = "Sugar,Total (Raw Equiv.)"
codexWide[which(codexWide$Title ==  "Standard for Rice"  ), 'foodCat'] = "Rice"
codexWide[which(codexWide$Title ==  "Standard for Saint-Paulin" ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Salted Atlantic Herring and Salted Sprat"  ), 'foodCat'] = "Atlantic herring and sprat, salted"
codexWide[which(codexWide$Title ==  "Standard for Salted Fish and Dried Salted Fish of the Gadidae Family of Fishes" ), 'foodCat'] = "Gadidforms, salted"
codexWide[which(codexWide$Title ==  "Standard for Samso"  ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Smoked Fish, Smoke-Flavoured Fish and Smoke-Dried Fish" ), 'foodCat'] = "Smoked Fish"
codexWide[which(codexWide$Title ==  "Standard for Sorghum Flour" ), 'foodCat'] = "Sorghum"
codexWide[which(codexWide$Title ==  "Standard for Sorghum Grains"  ), 'foodCat'] = "Sorghum"
codexWide[which(codexWide$Title ==  "Standard for Soy Protein Products"  ), 'foodCat'] = "Soy protein"
codexWide[which(codexWide$Title ==  "Standard for Sturgeon Caviar"  ), 'foodCat'] = "Caviar"
codexWide[which(codexWide$Title ==  "Standard for Sugars" ), 'foodCat'] = "Sugar,Total (Raw Equiv.)"
codexWide[which(codexWide$Title ==  "Standard for Sweet Cassava" ), 'foodCat'] = "Cassava"
codexWide[which(codexWide$Title ==  "Standard for Sweetened Condensed Milks"  ), 'foodCat'] = "Milk, whole condensed"
codexWide[which(codexWide$Title ==   "Standard for Table Grapes" ), 'foodCat'] = "Grapes"
codexWide[which(codexWide$Title ==   "Standard for Table Olives"  ), 'foodCat'] = "Olives preserved"
codexWide[which(codexWide$Title ==  "Standard for Tannia"   ), 'foodCat'] = "Yautia" # no fao data for this
codexWide[which(codexWide$Title ==  "Standard for Tilsiter" ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Standard for Tomatoes"  ), 'foodCat'] = "Tomatoes"
codexWide[which(codexWide$Title ==  "Standard for Tree Tomatoes" ), 'foodCat'] = "Tomatoes"
codexWide[which(codexWide$Title ==  "Standard for Unshelled Pistachio Nuts"), 'foodCat'] = "Pistachios"
codexWide[which(codexWide$Title ==   "Standard for Wheat and Durum Wheat"  ), 'foodCat'] = "Wheat"
codexWide[which(codexWide$Title ==  "Standard for Wheat Flour"), 'foodCat'] = "Flour, wheat"
codexWide[which(codexWide$Title == "Standard for Wheat Protein Products Including Wheat Gluten"  ), 'foodCat'] = "Flour, wheat"  
codexWide[which(codexWide$Title ==   "Standard for Whey Cheeses"), 'foodCat'] = "Whey, Cheese" # category exists but data doesn't
codexWide[which(codexWide$Title ==  "Standard for Whey Powders" ), 'foodCat'] = "Whey, dry"
codexWide[which(codexWide$Title ==  "Standard for Whole Maize (Corn) Meal" ), 'foodCat'] = "Flour, maize"
codexWide[which(codexWide$Title == "Standard for Whole and Decorticated Pearl Millet Grains" ), 'foodCat'] =  "Millet" 
codexWide[which(codexWide$Title ==  "System for the Description of Carcasses of Bovine and Porcine Species" ), 'foodCat'] = "Meat, Beef and Pork" 
codexWide[which(codexWide$Title ==  "Tomato juice") , 'foodCat'] = "Juice, tomato"
codexWide[which(codexWide$Title == "Unadopted Proposed Draft Code of Hygienic Practice for Dry and Semi-dry Sausages"), 'foodCat'] = "Meat, sausages" 
codexWide[which(codexWide$Title == "Unadopted Proposed Draft Codex Standard for Parmesan"), 'foodCat'] = "Cheese, processed" 
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Codification of Carcases of the Species Ovis"), 'foodCat'] = "Meat, Ovis" 
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Glossary of Scientific and Common Names for Tropical Fresh Fruits and Vegetables" ), 'foodCat'] = "Fruit, tropical fresh nes"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Guidelines for the Inspection and Certification of Fresh Fruits and Vegetables for Conformity to Quality Standards"), 'foodCat'] = "Fruit and Vegetables"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Regional Standard for Fermented Cooked Cassava Based Products"), 'foodCat'] = "Cassava"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Regional Standard for Gnetum Spp. Leaves32"), 'foodCat'] = NA
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Ayran" ), 'foodCat'] = "Yoghurt, concentrated or not"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Cashew Kernels" ), 'foodCat'] = "Cashew nuts" 
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Edible Ices and Ice Mixes"), 'foodCat'] = "Waters,ice etc"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Mango Juice" ), 'foodCat'] = "Juice, fruit nes"
codexWide[which(codexWide$Title ==   "Unadopted Proposed Draft Standard for Mayonnaise" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Mixed Vegetable and Animal Ghee"), 'foodCat'] = NA
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Non-centrifugated Dehydrated Sugar Cane Juice"), 'foodCat'] = "Juice, fruit nes"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Panela" ), 'foodCat'] = "Sugar,Total (Raw Equiv.)"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Processed Cheese" ), 'foodCat'] = "Cheese, processed"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Soups and Broths"), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Soy Sauce" ), 'foodCat'] = "Soya sauce"
codexWide[which(codexWide$Title ==  "Unadopted Proposed Draft Standard for Vegetable Ghee"), 'foodCat'] = "Total Meat"
codexWide[which(codexWide$Title ==  "Vinegar" ), 'foodCat'] = "Food prep nes"
codexWide[which(codexWide$Title ==  "White sugar"), 'foodCat'] = "Sugar refined"
codexWide[which(codexWide$Title ==  "General Standard for the Labelling of Prepackaged Foods" ), 'foodCat'] = "Food prep nes"

codexWide[which(codexWide$Title ==  "Descriptions of Cutting Methods of Commercial Units of Carcasses Moving in International Trade ("), 'foodCat'] = "Total Meat" 
codexWide[which(codexWide$Title ==  "Recommended International Code for Ante-Mortem and Post-Mortem Inspection of Slaughter Animals and for Ante-Mortem and Post-Mortem Judgement of Slaughter Animals and Meat"), 'foodCat'] = "Total Meat" 
codexWide[which(codexWide$Title ==  "Recommended International Code of Practice for Ante-Mortem and Post-Mortem Judgement of Slaughter Animals and Meat"), 'foodCat'] = "Total Meat" 
codexWide[which(codexWide$Title ==  "Code of Ante Mortem and Post Mortem inspection of Slaughter Animals"), 'foodCat'] = "Total Meat" 

 
save(codexWide, file = paste0(pathMain, '/participation_development/codexWideTrade.rda'))


