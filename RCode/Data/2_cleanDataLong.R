rm(list = ls())
if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}
# amended, revised, revocation 
# centrality measures
# 
# -------------------------
# Load Dates of Meetings
# --------------------------
load(file = paste0(pathMain, '/participation_development/codex_event_dates_master.rda'))

# -------------------------
# Load Codex Event Data from 2_cleanDataWide.R
# --------------------------
load(file = paste0(pathMain, '/participation_development/codexEvent.rda'))
 
# -------------------------
# Add events data for revoked standards
# --------------------------
codexWideXtra = read.csv(paste0(pathMain, '/participation_development/codexWide_edited_sk.csv'), stringsAsFactors = FALSE)

# add info 
oilStds = codexWideXtra[grep('arachis|babassu|coconut oil|cottonseed oil|grape seed|maize oil|Mustard|palm kernel|Olein|Stearin|rapeseed|safflower|sesame|sunflower|soya', codexWideXtra$Title), 'Title' ]
fatStds = codexWideXtra[grep('Lard|Rendered|jus|tallow', codexWideXtra$Title),'Title' ]

meatStds = c('Recommended International Code of Hygienic Practice for Fresh Meat',
             'Recommended International Code of Hygienic Practice for Game',
             'Recommended International Code for Ante-Mortem and Post-Mortem Inspection of Slaughter Animals and for Ante-Mortem and Post-Mortem Judgement of Slaughter Animals and Meat',
             'Recommended International Code of Hygienic Practice for Processed Meat and Poultry Products',
             'Recommended Code of Hygienic Practice for Poultry Processing',
             'Recommended International Code of Practice for the Production, Storage and Composition of Mechanically Separated Meat Intended for Further Processing')


codexWideXtra[which(codexWideXtra$Title %in% c(oilStds, fatStds, meatStds,  'Fructose', 'Lactose')), 'droppedDummy' ] = 0
codexWideXtra[which(codexWideXtra$Title %in% c(oilStds,  fatStds, meatStds, 'Fructose', 'Lactose')), 'supersededDummy' ] = 1
codexWideXtra[which(codexWideXtra$Title %in% c(oilStds, fatStds,  meatStds, 'Fructose', 'Lactose')), 'revoked_Seb' ] = 1

codexWideXtra[which(codexWideXtra$Title %in% c('Fructose', 'Lactose')), 'Reference_superseding_standard' ] = 'CODEX STAN 212-1999'
codexWideXtra[which(codexWideXtra$Title %in% oilStds), 'Reference_superseding_standard' ] = 'CODEX STAN 210-1999'
codexWideXtra[which(codexWideXtra$Title %in% c(fatStds)), 'Reference_superseding_standard' ] = 'CODEX STAN 211-1999'
codexWideXtra[which(codexWideXtra$Title %in% c(meatStds)), 'Reference_superseding_standard' ] = 'CAC/RCP 58-2005'

codexWideXtra[which(codexWideXtra$Title %in% c(oilStds, fatStds, 'Fructose', 'Lactose')), "revoked_CAC_Seb"  ] = 'CAC23'
codexWideXtra[which(codexWideXtra$Title %in% c(meatStds)), "revoked_CAC_Seb"  ] = 'CAC28'


# select only standards that were revoked
codexWideXtra = codexWideXtra[which(codexWideXtra$revoked_Seb == 1),]
 
# remove standards having exclusively to do with residue levels
codexWideXtra = codexWideXtra[-which(codexWideXtra$Title %in% c("Maximum levels for cadmium" )),]

# remove standard that we have no adoption data for
codexWideXtra = codexWideXtra[-which(codexWideXtra$Title %in% c("System for the Description of Carcasses of Bovine and Porcine Species" )),]

 
# change variable names
names(codexWideXtra)[which(names(codexWideXtra) %in% c("Reference_new_Seb", "revoked_CAC_Seb", 'committee'))] = c('Reference_new', 'cac', 'committee_raw')

# match committee to cacs
codexWideXtra$committee = dates_all$event_number[match(paste0(codexWideXtra$cac, codexWideXtra$committee_raw), paste0(dates_all$event_number_cac, dates_all$event_short))]
codexWideXtra[which(codexWideXtra$committee_raw == 'CCPFV' & codexWideXtra$cac == 'CAC27'), 'committee'] = 'CCPFV21'
codexWideXtra[which(codexWideXtra$committee_raw == 'CCS' & codexWideXtra$cac == 'CAC23'),'committee'] = 'CCS6'
codexWideXtra[which(codexWideXtra$committee_raw == 'CCFAC' & codexWideXtra$cac == 'CAC31'), 'committee'] = 'CCFA40'
codexWideXtra[which(codexWideXtra$committee_raw == 'CCFAC' & codexWideXtra$cac == 'CAC35'), 'committee'] = 'CCCF6'
codexWideXtra[which(codexWideXtra$committee_raw == 'CGECPMMP' & codexWideXtra$cac == 'CAC33'),'committee'] = 'CCMMP9'
codexWideXtra[which(codexWideXtra$committee_raw == 'GEFJ' & codexWideXtra$cac == 'CAC28'),'committee'] = 'TFFJ4'
codexWideXtra[which(codexWideXtra$committee_raw == 'GEFJ' & codexWideXtra$cac == 'CAC26'),'committee'] = 'TFFJ2'
codexWideXtra[which(codexWideXtra$committee_raw == 'CCPMPP' & codexWideXtra$cac == 'CAC32'),'committee'] = 'CCEXEC62'

codexWideXtra[which(codexWideXtra$committee_raw == 'GEFJ' & codexWideXtra$cac == 'CAC26'),]
# add year, committee number variables, position
codexWideXtra$committee_num = gsub('\\d', '', codexWideXtra$committee) %>% as.numeric()
codexWideXtra$year = dates_all$year[match(codexWideXtra$cac, dates_all$event_number_cac)]
codexWideXtra$position = 'revoked'
 
# add meeting type and meeting status variables
codexWideXtra$meeting_type = dates_all$meeting_type[match(codexWideXtra$committee_raw, dates_all$event_short)]
codexWideXtra$meeting_status = dates_all$meeting_status[match(codexWideXtra$committee_raw, dates_all$event_short)]

# recode following standards as being superseded
sugarStds = codexWideXtra[which(codexWideXtra$committee_raw == 'CCS'),'Title']
codexWideXtra[which(codexWideXtra$Title %in% sugarStds),'supersededDummy'] = 1
codexWideXtra[which(codexWideXtra$Title %in% sugarStds),'droppedDummy'] = 0
codexWideXtra[which(codexWideXtra$Title %in% sugarStds),'Reference_superseding_standard'] = 'CODEX STAN 212-1999'


juiceStds = codexWideXtra[which(codexWideXtra$committee_raw == 'GEFJ' & codexWideXtra$cac == 'CAC28'),'Title']
codexWideXtra[which(codexWideXtra$Title %in% juiceStds),'droppedDummy'] = 0
codexWideXtra[which(codexWideXtra$Title %in% juiceStds),'Reference_superseding_standard'] = 'CODEX STAN 247-2005'


 
# add revocation data
codexEvent$revoked = 0
codexWideXtra$revoked = 1

# clean data
codexWideXtra[which(codexWideXtra$revoked == 1 & codexWideXtra$droppedDummy == 0 &codexWideXtra$revoked == 1 & codexWideXtra$supersededDummy == 0 ),'droppedDummy'] = 1
codexWideXtra[which(codexWideXtra$Title %in% c('Quick Frozen Carrots',
                                              'Quick Frozen Whole Kernel Corn',
                                              'Recommended International Code of Hygienic Practice for Fresh Meat') ),'droppedDummy'] = 0
 
codexEvent = rbind(codexEvent, codexWideXtra[, names(codexEvent)])



# clean up event data
codexEvent[which(codexEvent$position == 'end'), c('step', 'proposal_clean')] = NA
codexEvent[which(codexEvent$position != 'end'), c('adopted')] = NA
codexEvent$proposed = ifelse(codexEvent$position == 'start', 1, 0)


# -------------------------
# Add events data for amended/revised standards
# --------------------------
# load std data
load(paste0(pathMain, '/participation_development/codex_standard_development_master.rda'))

# select only amendments and revisions
std = std[which(std$revised == 1 |std$amended == 1),]

# remove standard data that aren't standards/are about residues
std = std[-which(std$Title %in% c('Classification of Foods and Animal Feeds',
                           'Food Control Laboratory Management: Reccomendations',
                           'Glossary of Terms and Definitions (Residues of Veterinary Drugs in Foods)',
                           'List of Codex Specifications for Food Additives',
                           "Maximum Residue Limits (MRLs) for Pesticides" ,
                           "Maximum Residue Limits (MRLs) and Risk Management Recommendations (RMRs) for Residues of Veterinary Drugs in Foods" ,
                           "Harmonized IUPAC Guidelines for Single-Laboratory Validation of Methods of Analysis",
                           "Harmonized Guidelines for Internal Quality Control in Analytical Chemistry Laboratories",
                            "Harmonized IUPAC Guidelines for the Use of Recovery Information in Analytical Measurement",
                           'Analysis of Pesticide Residues: Portion of Commodities to which Codex MRLS Apply and which is Analyzed', 
                           'List of Codex Specifications for Food Additives', # CAC MISC
                           "Statement on Infant Feeding")),]

# remove standards data that we don'th have info for
std = std[-which(std$Title %in% c('Recommended Methods of Analysis and Sampling',
                                  'System for the Description of Carcasses of Bovine and Porcine Species',
                                  "Standard for Canned Finfish",
                                  "Standard for Dairy Fat Spreads",
                                  "Standard for Honey",
                                  "Standard for Infant Formula and Formulas for Special Medical Purposes Intended for Infants",
                                  "Standard for Preserved Tomatoes",
                                  "Standard for Quick Frozen Finfish, Uneviscerated and Eviscerated",
                                  "Statement on Infant Feeding",
                                  "Principles and guidelines for the exchange of information between importing and exporting countries to support the trade in food",
                                  'Unadopted Proposed Draft Standard for Edible Ices and Ice Mixes', 
                                  'Unadopted Proposed Draft Standard for Soups and Broths',
                                  'Guideline Levels for Radionuclides in Foods following accidental Nuclear Contamination for use in International Trade')),]

 
 

# add info on new reference codes
codexWideXtra = read.csv(paste0(pathMain, '/participation_development/codexWide_edited_sk.csv'), stringsAsFactors = FALSE)
std$Reference_new = codexWideXtra$Reference_new[match(std$Reference, codexWideXtra$Reference)]



## merge amendment/revision info
codexEvent = merge(codexEvent, 
              std[, c('Reference', 'Reference_new', 'Title', 'year', 'revised', 'amended', 'meeting_cac', 'Meeting', "supersededDummy", 'droppedDummy')], 
              by.x = c('Reference','Reference_new', 'Title', 'year', 'cac', 'committee', "supersededDummy", 'droppedDummy'), 
              by.y = c('Reference', 'Reference_new','Title', 'year', 'meeting_cac', 'Meeting', "supersededDummy", 'droppedDummy'), all = TRUE)

# fill in missing info
codexEvent$proposed[which(is.na(codexEvent$proposed))] = 0
codexEvent$adopted[which(is.na(codexEvent$adopted))] = 0
codexEvent$revised[which(is.na(codexEvent$revised))] = 0
codexEvent$amended[which(is.na(codexEvent$amended))] = 0
codexEvent$revoked[which(is.na(codexEvent$revoked))] = 0
codexEvent$status[which(is.na(codexEvent$status))] = 0

codexEvent$committee_raw = gsub('\\d', '', codexEvent$committee)
codexEvent$committee_num = gsub('[A-Z]', '', codexEvent$committee) %>% as.numeric()
codexEvent$meeting_type = dates_all$meeting_type[match(codexEvent$committee, dates_all$event_number)]
codexEvent$meeting_status = dates_all$meeting_status[match(codexEvent$committee, dates_all$event_number)]

codexEvent = do.call(rbind, lapply(split(codexEvent, codexEvent$Title), function(x){
  x$discontinuedDummy = ifelse(all(is.na(x$discontinuedDummy)), 0, unique(na.omit(x$discontinuedDummy)))
  x$ongoingDummy = ifelse(all(is.na(x$ongoingDummy)), 0, unique(na.omit(x$ongoingDummy)))
  
  return(x)
})
)

rownames(codexEvent) = NULL

codexEvent[which(codexWide$revoked == 0 & codexEvent$droppedDummy == 1),]  %>% dim()


 
unlist(lapply(split(codexEvent$droppedDummy, codexEvent$Title), function(x) unique(na.omit(x)))) %>% table()
unlist(lapply(split(codexEvent$supersededDummy, codexEvent$Title), function(x) unique(na.omit(x)))) %>% table()

 


# -------------------------
# Expand dataset to long version
# --------------------------
multCommittees = c('Standard for Sorghum Grains', # complete 
                   'Standard for Sorghum Flour', # complete
                   'Standard for Maize (Corn)', # complete
                   'Standard for Boiled Dried Salted Anchovies', # complete
                   'Standard for Crackers from Marine and Freshwater Fish, Crustaceans and Molluscan Shellfish',# complete
                   'General Guidelines for Use of the Term "Halal"', # complete
                   'Standard for Aqueous Coconut Products - Coconut Milk and Coconut Cream', # complete
                   'Standard for Pickled Fruits and Vegetables', # complete
                   codexWideXtra[which(codexWideXtra$committee_raw == 'CCFAC' & codexWideXtra$cac == 'CAC31'), 'Title'],
                   codexWideXtra[which(codexWideXtra$committee_raw == 'CCFAC' & codexWideXtra$cac == 'CAC35'), 'Title'],
                   codexWideXtra[which(codexWideXtra$committee_raw == 'CGECPMMP' & codexWideXtra$cac == 'CAC33'),'Title'],
                   codexWideXtra[which(codexWideXtra$committee_raw == 'GEFJ' & codexWideXtra$cac == 'CAC28'),'Title'] ,
                   codexWideXtra[which(codexWideXtra$committee_raw == 'GEFJ' & codexWideXtra$cac == 'CAC26'),'Title'] ,
                   codexWideXtra[which(codexWideXtra$committee_raw == 'CCPMPP' & codexWideXtra$cac == 'CAC32'),'Title'] )

# expand dataset for event data 
dates_all$committee_num= gsub('[A-Z]', '', dates_all$event_number) %>% as.numeric()
names(dates_all)[which(names(dates_all) %in% c('event_number_cac'))] = 'cac'
 
codexExpand = do.call(rbind, lapply(split(codexEvent , codexEvent$Title), function(x){
  if (x$Title[1] %in% multCommittees){
    
    dates_sub_1 = dates_all[grep(x$committee_raw[1], dates_all$event_short),]
    dates_sub_1 = dates_sub_1[dates_sub_1$committee_num >= x$committee_num[1],]
    x$year = dates_all$year[match(paste0(x$committee_raw, x$committee_num), dates_all$event_number)]
    x = x[order(x$year),]
    
    if(length(which(x$committee_raw == x$committee_raw[1])) ==1 ){
      
      dates_sub_1 = dates_sub_1[which(dates_sub_1$year<x$year[2]),]
      dates_sub_2 = dates_all[grep(paste(x$committee_raw[2], collapse = '|'), dates_all$event_short),]
      dates_sub_2 = dates_sub_2[which(dates_sub_2$year >= x$year[2]),]
      
    } else if (length(which(x$committee_raw == x$committee_raw[1])) ==2 ){
      
      dates_sub_1 = dates_sub_1[which(dates_sub_1$year <= x$year[2]),]
      dates_sub_2 = dates_all[grep(paste(x$committee_raw[3], collapse = '|'), dates_all$event_short),]
      dates_sub_2 = dates_sub_2[which(dates_sub_2$year > x$year[2]),]
      
    }
    x = x[,-which(names(x) == 'year')]
    dates_sub_2 =  dates_sub_2[which( dates_sub_2$year<=x$year[3]),]
    dates_sub = rbind(dates_sub_1, dates_sub_2)
    
  } else{
     dates_sub = dates_all[grep(paste(unique(x$committee_raw), collapse = '|'), dates_all$event_short),]
    dates_sub = dates_sub[dates_sub$committee_num <= max(x$committee_num) & dates_sub$committee_num >= min(x$committee_num) ,]
    
  } 
  
  expand = merge(x, dates_sub[, c('event_number',  'cac', 'committee_num')], by.x = c('committee', 'cac', 'committee_num'), by.y =c('event_number','cac', 'committee_num' ), all = TRUE)
  expand$year = dates_all$year[match(expand$committee, dates_all$event_number)]
  expand = expand[order(expand$year),]

  expand$Reference = unique(na.omit(expand$Reference))
  expand$Reference_new = unique(na.omit(expand$Reference_new))
  expand$Title = unique(na.omit(expand$Title))
  expand$proposal_clean = unique(na.omit(expand$proposal_clean))
  expand$Reference_superseding_standard = ifelse(length(unique(na.omit(expand$Reference_superseding_standard)) ) == 0, NA, unique(na.omit(expand$Reference_superseding_standard)))
  expand$status = unique(na.omit(expand$status))
 
  expand$droppedDummy = unique(na.omit(expand$droppedDummy))
  expand$supersededDummy = unique(na.omit(expand$supersededDummy))
  expand$discontinuedDummy = unique(na.omit(expand$discontinuedDummy))
  expand$ongoingDummy = unique(na.omit(expand$ongoingDummy))
 

  expand = expand[, -which(names(expand) %in% c('committee_raw', 'position'))]
  expand$cac_num = gsub('CAC', '', expand$cac) %>% as.numeric()

  return(expand )
  
}))
rownames(codexExpand) = NULL

 
 
# fill in missing info
codexExpand$proposed[which(is.na(codexExpand$proposed))] = 0
codexExpand$adopted[which(is.na(codexExpand$adopted))] = 0
codexExpand$revised[which(is.na(codexExpand$revised))] = 0
codexExpand$amended[which(is.na(codexExpand$amended))] = 0
codexExpand$revoked[which(is.na(codexExpand$revoked))] = 0
codexExpand$status[which(is.na(codexExpand$status))] = 0


codexExpand$committee_raw = gsub('\\d', '', codexExpand$committee)
codexExpand$meeting_type = dates_all$meeting_type[match(codexExpand$committee, dates_all$event_number)]
codexExpand$meeting_status = dates_all$meeting_status[match(codexExpand$committee, dates_all$event_number)]


 
# create varaiable that combines possible outcomes (proposed, adopted, amended, revised, revoked)
codexExpand$outcome = ifelse(codexExpand$proposed == 1, 'proposed', ifelse(
                              codexExpand$adopted == 1, 'adopted', ifelse(
                              codexExpand$amended == 1, 'amended', ifelse(
                                codexExpand$revised == 1, 'revised', ifelse(
                                  codexExpand$revoked == 1, 'revoked', 0
                                )
                              )
                              )))


# ---------------------------------------
# add centrality
# --------------------------------------
load(file = paste0(pathMain, '/participation_development/centralityMeetingMeasure.rda'))
codexExpand= merge(codexExpand, centralityMeeting[, -1], by.x = c('committee'), by.y = c('event_number'), all.x = TRUE)
 
 
# ---------------------------------------
# add participant data
# --------------------------------------
load(paste0(pathMain, '/participation_development/codex_participation_master.rda'))
codexExpand = merge(codexExpand, dplyr:::select(participAgg,   -year), by.x = 'committee' , by.y = 'event_number', all.x = TRUE)
 
# merge in particpant data for cac meetings
participAgg_cac = participAgg[grep('CAC', participAgg$event_short),]
names(participAgg_cac)[c(3:547)]  = paste0(names(participAgg_cac)[c(3:547)], '_cac')
 
 
codexExpand  = merge(codexExpand, dplyr:::select(participAgg_cac,   -onlyChairDum, -chairButNoDelegatesDum, -infoDum, -missingDelegInfo ), by.x = 'cac' , by.y = 'event_number', all.x = TRUE)

codexLongAll = codexExpand
 
save(codexLongAll, file = paste0(pathMain, '/participation_development/codexLongAll.rda'))
 
load(file = paste0(pathMain, '/participation_development/codexLongAll.rda'))
names(codexLongAll)

# think about how to account for standards are dropped or superseded but don't have data on when they were revoked
# probably should just remove these from the dataset?


# Separately, something to think about is how we want to deal with the standards that we know are revoked but we don't have the revocation date for.
# I'm leaning towards just throwing these standards out of the dataset since they're so few (either 9 or 13 depending on how we code the fish fillets)?
# Shouldn't bias our results too much. But maybe before we do the analysis, we should take an inventory of
# how many standards we have missing information for (in terms of propsal data, revocation, titles etc)


codexWide[which(codexWide$revoked == 0 & codexWide$droppedDummy ==1|codexWide$revoked == 0 & codexWide$supersededDummy == 1),'revoked'] = NA
codexWide[which(codexWide$revoked == 0 & codexWide$droppedDummy ==1|codexWide$revoked == 0 & codexWide$supersededDummy == 1),'revoked_CAC'] = NA


