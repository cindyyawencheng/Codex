# rm(list= ls())
# if(Sys.info()['user'] == 'cindycheng'){
#   source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
# }
#   
# -------------------------
# Load Dates of Meetings
# --------------------------
load(file = paste0(pathMain, '/participation_development/codex_event_dates_master.rda'))


# -------------------------
# Load centrality data
# --------------------------
load(file = paste0(pathMain, '/participation_development/centralityMeetingMeasure.rda'))


# # -------------------------
# # Load Standards Data
# # --------------------------
# # load std data
# load(paste0(pathMain, '/participation_development/codex_standard_development_master.rda'))
# std = std[which(std$adopted == 1),]
# 
# # -------------------------
# # Load Proposals Data
# # --------------------------
# load(file = paste0(pathMain, '/participation_development/proposals_master.rda'))
# proposedAll$meetingCount = gsub("[^0-9.-]",  "", proposedAll$Meeting)
# 
# # -------------------------
# # create preliminary long version of dataset
# # -------------------------
# codexLong = merge(proposedAll, std[, c('year_cac', 'Reference', 'adopted',  'Title', 'meetingCount_cac', 'meeting_cac', 'supersededDummy', 'droppedDummy', 'Meeting')],
#                   by.x = c('year', 'Reference', 'Title', 'meetingCount_cac', 'meeting_cac', 'supersededDummy', 'droppedDummy', 'Meeting'),
#                   by.y = c('year_cac', 'Reference', 'Title', 'meetingCount_cac', 'meeting_cac', 'supersededDummy', 'droppedDummy', 'Meeting'), all = TRUE)
# 
# codexLong  = codexLong[order(codexLong$Reference, codexLong$year),]
# codexLong$Title= as.character(codexLong$Title)
# codexLong$proposal = as.character(codexLong$proposal)
# 
# codexLong$Title = ifelse(codexLong$Title == 0, paste0('Unadopted ', codexLong$proposal_clean), codexLong$Title)
# codexLong$year = as.numeric(codexLong$year)
# codexLong$meetingCount_cac = as.numeric(codexLong$meetingCount_cac)
# 
# 
# 
# # add names for standards missing names
# codexLong$Title[which(codexLong$Title == '.')] = paste0('Title Missing, Reference: ]', codexLong$Reference[which(codexLong$Title == '.')])
# 
# 
# # -------------------------
# # create preliminary Wide version of dataset
# # -------------------------
# # proposal - commitee
# codexWide =  do.call(rbind,  lapply(split(codexLong , codexLong$Title), function(x){
#   slice  = x
# 
#   slice$handcodedDummy = ifelse(dim(slice)[1]==1, slice$handcodedDummy[1], slice$handcodedDummy[-which(is.na(slice$handcodedDummy))] %>% unique())
# 
#   # fill in 'adopted' column for standards that were adopted but have missing references
#   if(length(intersect(grep('Adopted', slice$status), grep('Missing', slice$Reference)))>0){
#     slice$adopted[length(slice$adopted)] = 1
#   }
# 
# 
#   # if the standard has specifically been coded as adopted,
#   if (all(is.na(slice$adopted)) == FALSE){
#     slice = slice[1:which(slice$adopted ==1), ]}
# 
#     slice$committee = gsub('\\d', '', slice$Meeting[1])
#     slice$committee_start= slice$Meeting[1]
# 
#     if(is.na(slice$Meeting[length(slice$Meeting)]) == TRUE){
#       slice$committee_end= dates_all$event_number[match(paste0(slice$meeting_cac[length(slice$meeting_cac)], slice$committee[1]), paste0(dates_all$event_number_cac, dates_all$event_short))]
#     } else{
#       slice$committee_end = slice$Meeting[length(slice$Meeting)]
#     }
# 
#     slice$startYear = slice$year[1]
#     slice$endYear = slice$year[length(slice$year)]
#     #slice$endYear = dates_all$year[match(slice$committee_end[1], dates_all$event_number)]
#     slice$startCac = slice$meetingCount_cac[1]
#     slice$endCac = slice$meetingCount_cac[length(slice$meetingCount_cac)]
#     slice$yearsAfterProposal = slice$endYear - slice$startYear
#     slice$cacAfterProposal = slice$endCac - slice$startCac
#     slice$adopted[is.na(slice$adopted)] = 0
#     slice$handcodedDummy = ifelse(all(is.na(slice$handcodedDummy)), 0, slice$handcodedDummy[1])
#     slice$droppedDummy = ifelse(all(is.na(slice$droppedDummy)), 0, slice$droppedDummy[1])
#     slice$supersededDummy = ifelse(all(is.na(slice$supersededDummy)), 0, slice$supersededDummy[1])
#     slice$discontinuedDummy = ifelse(all(is.na(slice$discontinuedDummy)), 0, slice$discontinuedDummy[1])
#     slice$ongoingDummy = ifelse(all(is.na(slice$ongoingDummy)), 0, slice$ongoingDummy[1])
#     slice$status = ifelse(all(is.na(slice$status)), NA, slice$status[1])
#     slice$proposal_clean = ifelse(all(is.na(slice$proposal_clean)), NA, slice$proposal_clean[1])
#     slice$Reference_superseding_standard = ifelse(all(is.na(slice$Reference_superseding_standard)), NA, unique(na.omit(slice$Reference_superseding_standard)))
#   slice$step = ifelse(all(is.na(slice$step)), NA,
#                       ifelse(length(slice$step)==1, slice$step,
#                         ifelse(any(is.na(slice$step)), slice$step[-which(is.na(slice$step))]%>% unique(), slice$step[1]) ))
#   slice = slice[dim(slice)[1], -which(names(slice) %in% c('meetingCount_cac','proposal', 'meetingCount', 'missingDummy',  'Meeting', 'year'))]
#   return(slice)
# })
# )
# 
# 
# rownames(codexWide) = NULL
# 
# check =unlist(lapply(split(codexLong , codexLong$Title), function(x){
#   slice = x
#   if(sum(slice$adopted %>% as.numeric(), na.rm =TRUE)>1){
#     check = 'check OUT'
#   } else{
#     check = NULL
#   }
# }))
# 
# 
# 
# # remove non-standards
# codexWide$proposal_clean = as.character(codexWide$proposal_clean)
# nonStandards = c("Maximum Residue Limits (MRLs) for Pesticides" ,
#                  "Maximum Residue Limits (MRLs) and Risk Management Recommendations (RMRs) for Residues of Veterinary Drugs in Foods" ,
#                  "Harmonized IUPAC Guidelines for Single-Laboratory Validation of Methods of Analysis",
#                  "Harmonized Guidelines for Internal Quality Control in Analytical Chemistry Laboratories",
#                  "Food Control Laboratory Management: Reccomendations",
#                  "Harmonized IUPAC Guidelines for the Use of Recovery Information in Analytical Measurement",
#                  'Analysis of Pesticide Residues: Portion of Commodities to which Codex MRLS Apply and which is Analyzed',
#                  'List of Codex Specifications for Food Additives', # CAC MISC
#                  "Classification of Foods and Animal Feeds",
#                  "Glossary of Terms and Definitions (Residues of Veterinary Drugs in Foods)",
#                  "Statement on Infant Feeding",
#                  "Maximum level for aflatoxin M1 in milk",
#                  "Maximum Level and Sampling Plan for Total Aflatoxins in Peanuts intended for further processing"  )
# 
# codexWide = codexWide[-which(codexWide$Title %in% nonStandards),]
# 
# # remove proposals that were discontinued and continued under other standards
# codexWide = codexWide[-which(codexWide$status %in% c(
#   "Discontinued  as independent code; would be subsumed under Proposed Draft Code of Hygienic Practice for Milk and Milk Products",
#   "Discontinued; continued in 3 separate standards" ,
#   "Discontinued; continued under  initiative for preparation of a broader based standard or guideline to cover all fat spreads",
#   "Discontinued; continued under general standard covering vegetable protein products from all sources, including soya beans. "
# )),]
# 
# 
# # fill end year of ongoing standards with NA
# codexWide[which(codexWide$ongoingDummy == 1), c('committee_end', 'endYear', 'endCac', 'yearsAfterProposal', 'cacAfterProposal')]  = NA
# 
# 
# # fill start (year, cac, etc.) information missing standards with NA
#  missingStandardsAll = c('System for the Description of Carcasses of Bovine and Porcine Species',
#                       "Standard for Canned Finfish",
#                      "Standard for Dairy Fat Spreads",
#                      "Standard for Honey",
#                      "Standard for Infant Formula and Formulas for Special Medical Purposes Intended for Infants",
#                      "Standard for Preserved Tomatoes",
#                      "Standard for Quick Frozen Finfish, Uneviscerated and Eviscerated",
#                      "Statement on Infant Feeding",
#                      "Principles and guidelines for the exchange of information between importing and exporting countries to support the trade in food",
#                      "Recommended Methods of Analysis and Sampling",
#                      'Unadopted Proposed Draft Standard for Edible Ices and Ice Mixes',
#                      'Unadopted Proposed Draft Standard for Soups and Broths',
#                      'Guideline Levels for Radionuclides in Foods following accidental Nuclear Contamination for use in International Trade',
#                      codexWide[grep('Title', codexWide$Title), 'Title'])
# 
# 
# codexWide[which(codexWide$Title %in% missingStandardsAll),grep('start|end|yearsAfter|cacAfter|meeting_cac', names(codexWide))] = NA
# 
# # clean classes of variables
# codexWide$supersededDummy = as.numeric(codexWide$supersededDummy)
# codexWide$droppedDummy = as.numeric(codexWide$droppedDummy)
# codexWide$handcodedDummy = as.numeric(codexWide$handcodedDummy)
# codexWide$discontinuedDummy = as.numeric(codexWide$discontinuedDummy)
# codexWide$adopted  = as.numeric(codexWide$adopted )
# codexWide$ongoingDummy  = as.numeric(codexWide$ongoingDummy)
# 
# # include codebook of variable labels
# attributes(codexWide)$variable_labels = c('Reference for Standard/Guideline/Code of Practice if it exists',
#   'Title of Standard/Guideline/Code if it exists. If not, then will be preceded by the string "Unadopted"',
#   'CAC meeting',
#   'Dummy variable for if standard was superseded. Note that superseded standards are by definition dropped. However note, a standard is never coded as both dropped and superseded.',
#   'Dummy variable for if a standard was dropped',
#   'Cleaned name of propsoed standard',
#   'Dummy variable for if the proposal was hand coded',
#   'Step number of proposal',
#   'Reference code of the superseding standard',
#   'Notes whether a standard was discontinued; adopted but is missing a Reference; etc.',
#   'Dummy variable for if a propsoal was never adopted',
#   'Dummy variable for if a proposal is still being negotiated',
#   'Dummy variable for if a proposal was adopted',
#   'Committee where a proposal is developed',
#   'Committee number for the earliest date that we have information for when a proposal was developed at step 1',
#   'Committee number for when a proposal was either adopted or discontinued',
#   'Year for  the earliest date that we have information for when a proposal was developed at step 1',
#   'Year for when a proposal was either adopted or discontinued',
#   'CAC meeting for the earliest date that we have information for when a proposal was developed at step 1',
#   'CAC meeting for when a proposal was either adopted or discontinued',
#   'Number of years in between the beginning of the development to the end of the development of a proposal',
#   'Number of CAC meetings in between the begnning of the development to the end of the development of a proposal'
#   )
# 
# 
# # fill in end committee info for which there were previously no matches
# codexWide[which(codexWide$committee == 'CCCPC' & codexWide$endYear == 1985), 'committee_end'] = "CCCPC15"
# codexWide[which(codexWide$committee == 'CCFAC' & codexWide$endYear == 2009), 'committee_end'] = "CCFA41"
# codexWide[which(codexWide$committee == 'CCFAC' & codexWide$endYear == 2008), 'committee_end'] = "CCFA40"
# codexWide[which(codexWide$committee == 'CCFAC' & codexWide$endYear == 2007), 'committee_end'] = "CCFA39"
# codexWide[which(codexWide$committee == 'CCRVDF' & codexWide$endYear == 2004),'committee_end' ] = 'CCRVDF14'
# codexWide[which(codexWide$committee == 'CCS' & codexWide$endYear %in% c(1981, 1999)),'committee_end' ] = 'CCS6'
# codexWide[which(codexWide$committee == 'CCMPH' & codexWide$endYear == 1985),'committee_end' ] = 'CCMPH5'
# codexWide[which(codexWide$committee == 'CCASIA' & codexWide$endYear == 2006),'committee_end' ] = 'CCASIA14'
# codexWide[which(codexWide$committee == 'CCNMW' & codexWide$endYear == 1981), 'committee_end'] = 'CCNMW4'
# codexWide[which(codexWide$committee == 'CCEURO' & codexWide$endYear == 2004), 'committee_end'] = 'CCEURO24'
# 
# # add in meeting type
# codexWide$meeting_type = dates_all$meeting_type[match(codexWide$committee, dates_all$event_short)]
# 
# 
# # add in meeting status
# codexWide$meeting_status = dates_all$meeting_status[match(codexWide$committee, dates_all$event_short)]
# 
# # change variable names
# names(codexWide)[which(names(codexWide ) %in% c('startYear', 'endYear'))] = c('year_start', 'year_end')
# codexWide$cac_start = paste0('CAC', codexWide$startCac)
# codexWide$cac_end = paste0('CAC', codexWide$endCac)
# 
# 
# table(codexWide$yearsAfterProposal[which(codexWide$Reference == '0')])
# table(codexWide$yearsAfterProposal[which(codexWide$Reference != '0')])
# 
# 
# # ---------------------------------
# # add in data Seb collected on additional standards and on when standards were dropped/revoked
# # ---------------------------------
# load(file = paste0(pathMain, '/participation_development/codex_event_dates_master.rda'))
# codexWideXtra = read.csv(paste0(pathMain, '/participation_development/codexWide_edited_sk.csv'), stringsAsFactors = FALSE)
# names(codexWideXtra)[which(names(codexWideXtra) %in% c('startYear', 'endYear'))] = c('year_start', 'year_end')
# codexWideXtra$year_start = as.numeric(codexWideXtra$year_start)
# codexWideXtra$year_end = as.numeric(codexWideXtra$year_end)
# 
# codexWideXtra$cac_start = paste0('CAC', codexWideXtra$startCac)
# codexWideXtra$cac_end = paste0('CAC', codexWideXtra$endCac)
# codexWideXtra$revoked_Seb[which(is.na(codexWideXtra$revoked_Seb))] = 0
# 
# 
# #### add additional standards
# moreStandards = codexWideXtra[which(codexWideXtra$Added_by_Seb == 1),]
# moreStandards = moreStandards[-which(moreStandards$Title %in% c('Codex maximum level for cadmium in cereals, pulses and legumes',
#                                                                'Maximum level for aflatoxin M1 in milk',
#                                                                "Maximum Level and Sampling Plan for Total Aflatoxins in Peanuts intended for further processing")),]
# # add CAC info
# moreStandards$cac_start = dates_all$event_number_cac[match(moreStandards$committee_start, dates_all$event_number)]
# moreStandards$cac_end = dates_all$event_number_cac[match(moreStandards$committee_end, dates_all$event_number)]
# 
# moreStandards$startCac = as.numeric(gsub('CAC', '', moreStandards$cac_start))
# moreStandards$endCac = as.numeric(gsub('CAC', '', moreStandards$cac_end))
# 
# moreStandards$yearsAfterProposal = moreStandards$year_end - moreStandards$year_start
# moreStandards$cacAfterProposal = moreStandards$endCac - moreStandards$startCac
# 
# # add info on  meeting_type ; meeting_status
# moreStandards$meeting_type = dates_all$meeting_type[match(moreStandards$committee, dates_all$event_short)]
# moreStandards$meeting_status = dates_all$meeting_status[match(moreStandards$committee, dates_all$event_short)]
# 
# # add in dummy info
# moreStandards$supersededDummy = moreStandards$droppedDummy = moreStandards$discontinuedDummy = moreStandards$ongoingDummy = 0
# moreStandards$handcodedDummy =1
# 
# # fill in Reference that was previously missing
# codexWide[which(codexWide$Title == 'Regional Standard for Unrefined Shea Butter'), c('Reference', 'status')] = c(moreStandards[which(moreStandards$Title == 'Regional Standard for Unrefined Shea Butter'), c('Reference')], 0)
# 
# codexWide[which(codexWide$Title == 'Standard for Fish Oils'), c('Reference', 'status')] = c(moreStandards[which(moreStandards$Title == 'Standard for Fish Oils'), c('Reference')], 0)
# 
# 
# codexWide[which(codexWide$Title == 'Standard for Aubergines'), c('Reference', 'committee_end', 'year_end', 'cac_end', 'yearsAfterProposal', 'cacAfterProposal', 'status')] =
#         c(moreStandards[which(moreStandards$Title == 'Standard for Aubergines'), c('Reference', 'committee_end', 'year_end')], 'CAC41', 3, 4, 0)
# 
# 
# codexWide[which(codexWide$Title == 'Standard for Dairy Permeate Powders'), c('Reference', 'committee_start',  'committee_end', 'year_start', 'year_end', 'startCac', 'endCac', 'cac_start',  'cac_end', 'yearsAfterProposal', 'cacAfterProposal', 'status')] =
#   c(moreStandards[which(moreStandards$Title =='Standard for Dairy Permeate Powders'), c('Reference', 'committee_start', 'committee_end', 'year_start', 'year_end')],     21,           33,   'CAC21',   'CAC33',       23,                  12,               0)
# 
# 
# 
# # duplicated with Code of Practice for the Prevention and Reduction of Patulin Contamination in Apple Juice and Apple Juice Ingredients in Other Beverages
#                 # Standard for Extra Hard Grating Cheese
#                 # Regional Code of Practice for Street-vended Foods (Near East)
#                 # "Code of Practice for the Prevention and Reduction of Aflatoxin Contamination in Dried Figs"
#                 #  Regional Standard for Unrefined Shea Butter
#                 # 'Standard for Aubergines'
#                 # 'Standard for Dairy Permeate Powders'
# moreStandards = moreStandards[-which(moreStandards$Title %in% c('Maximum level for patulin in apple juice and apple juice ingredients and other beverages',
#                                                                 'Codex International Standard for Extra Hard Grating Cheese',
#                                                                 'Regional Code of Practice for Street-vended Foods (Near East)',
#                                                                 "Code of Practice for the Prevention and Reduction of Aflatoxin Contamination in Dried Figs",
#                                                                 'Regional Standard for Unrefined Shea Butter',
#                                                                 'Standard for Aubergines',
#                                                                 'Standard for Dairy Permeate Powders',
#                                                                 'Standard for Fish Oils')),]
# 
# 
# # remove standards that deal exclusively with contaminants
# moreStandards = moreStandards[-which(moreStandards$Title %in% c('Maximum Limits for Pesticide Residues 8th Series',
#                                                                'Codex Maximum Level for lead',
#                                                                'Maximum levels for cadmium')),]
# 
# 
# 
# 
# 
# # rbind
# codexWide = rbind(codexWide, moreStandards[, names(codexWide)])
# 
# # a few proposals not coded as being dropped even though they are; fix this
# codexWide[which(codexWide$Title %in% codexWideXtra[which(codexWideXtra$revoked_Seb==1 & codexWideXtra$droppedDummy==0 & codexWideXtra$supersededDummy == 0),'Title']),'droppedDummy'] = 1
# 
# # a few proposals that are coded as both dropped and superseded; fix this so that each standard is only one or the other
# codexWide[which(codexWide$droppedDummy ==1 & codexWide$supersededDummy==1), 'droppedDummy'] = 0
# 
# ## add new Reference
# codexWide$Reference_new = codexWideXtra$Reference_new_Seb[match(codexWide$Reference, codexWideXtra$Reference)]
# 
# 
# # codexDroppedYears = read.csv(paste0(pathMain, '/participation_development/standards_dropped_added_years.csv'), stringsAsFactors = FALSE)
# # codexDroppedYears$Reference[which(codexDroppedYears$Title == 'Standard for dried edible fungi')] = codexWide$Reference[which(codexWide$Title == 'Standard for Dried Edible Fungi')]
# # codexDroppedYears$Reference[which(codexDroppedYears$Title == 'Codex International Standard for Extra Hard Grating Cheese')] = codexWide$Reference[which(codexWide$Title ==  "Standard for Extra Hard Grating Cheese")]

# save(codexWide, file = paste0(pathMain, '/participation_development/codexWide.rda'))
# 

# see 0_cleanTrade.R for the addition of the 'foodCat' variable
load(file = paste0(pathMain, '/participation_development/codexWideTrade.rda')) 
# ---------------------------------
# create event data
# ---------------------------------

# convert codexWide back to codexLongClean
# year - proposal - commitee

# 1: get rid of observations that we have no data for
codexWide= codexWide[-which(codexWide$cac_start == 'CACNA'),]
codexWide= codexWide[-which(codexWide$cac_end == 'CACNA'),]
codexWide = codexWide[-which(codexWide$Title %in% c('.')),]

# 2: convert codexWide into event based data
codexEvent  = dplyr:::select(codexWide, -meeting_cac, -handcodedDummy, -committee, -startCac, -endCac, -yearsAfterProposal, -cacAfterProposal) %>%   
  gather(key, value, -Reference, -Reference_new,  -Title, -supersededDummy, -droppedDummy, -proposal_clean, -step, -Reference_superseding_standard, -status, -discontinuedDummy, -ongoingDummy, -adopted, -meeting_type, -meeting_status, -foodCat) %>%
   tidyr:::extract(key, c("meeting", "position"), "([[:alnum:]]+)_([[:alnum:]]+)") %>%
         spread(meeting, value)

 
codexEvent$committee_num = gsub('[A-Z]', '', codexEvent$committee) %>% as.numeric()
codexEvent$committee_raw = gsub('\\d|\\(|\\)', '', codexEvent$committee)

 
# 3: select out standards that went through two different committeees that weren't abolished/adjourned
# and reincorporate the information about when the handoff of the standard took place
multCommittees = c('Standard for Sorghum Grains', # complete 
                   'Standard for Sorghum Flour', # complete
                    'Standard for Maize (Corn)', # complete
                    'Standard for Boiled Dried Salted Anchovies', # complete
                    'Standard for Crackers from Marine and Freshwater Fish, Crustaceans and Molluscan Shellfish',# complete
                    'General Guidelines for Use of the Term "Halal"', # complete
                    'Standard for Aqueous Coconut Products - Coconut Milk and Coconut Cream', # complete
                    'Standard for Pickled Fruits and Vegetables') # complete


addMult = codexLong[which(codexLong$Title %in% multCommittees),]
addMult$position = NA
names(addMult)[which(names(addMult) %in% c('meeting_cac', 'Meeting', 'meetingCount'))] = c('cac', 'committee', 'committee_num')
addMult$committee_num = gsub('[A-Z]', '', addMult$committee) %>% as.numeric()
addMult$committee_raw = gsub('\\d', '', addMult$committee)
addMult$meeting_type = dates_all$meeting_type[match(addMult$committee_raw, dates_all$event_short)]
addMult$meeting_status= dates_all$meeting_status[match(addMult$committee_raw, dates_all$event_short)]
addMult$revoked = codexWide$revoked[match(addMult$Reference, codexWide$Reference)]
addMult$revoked_CAC = codexWide$revoked_CAC[match(addMult$Reference, codexWide$Reference)]
addMult$Reference_new = codexWide$Reference_new[match(addMult$Reference, codexWide$Reference)]
addMult = addMult[, names(codexEvent)]

 
codexEvent = codexEvent[-which(codexEvent$Title %in% multCommittees),]
codexEvent = rbind(codexEvent, addMult)


# recode the following as being superseded
sugarStds = c("Dried glucose syrup",
              "White sugar",
              "Powered sugar (icing sugar)",
              "Powdered dextrose (icing dextrose)",
              "Soft sugars",
              "Dextrose anhydrous",
              "Dextrose monohydrate",
              "Glucose syrup" ,
              "Fructose",
              "Lactose") 

#oil stds
oilStds = c("Edible low erucic acid rapeseed oil",   
            "Edible coconut oil",                    
            "Edible palm kernel oil",                
            "Edible grape seed oil",                 
            "Edible babassu oil",                    
            "General Standard for Edible Palm Olein",
            "Codex Standard for Edible Palm Stearin",
            "Edible soya bean oil",                  
            "Edible arachis oil",                    
            "Edible cottonseed oil",                 
            "Edible sunflower seed oil",             
            "Edible rapeseed oil",                   
            "Edible maize oil",                      
            "Edible sesame seed oil",                
            "Edible safflower seed oil",             
           "Mustard seed oil")

fatStds = c("Lard",              
            "Rendered pork fat", 
            "Premier jus" ,     
            "Edible tallow")

meatStds = c('Recommended International Code of Hygienic Practice for Fresh Meat',
             'Recommended International Code of Hygienic Practice for Game',
             'Recommended International Code for Ante-Mortem and Post-Mortem Inspection of Slaughter Animals and for Ante-Mortem and Post-Mortem Judgement of Slaughter Animals and Meat',
             'Recommended International Code of Hygienic Practice for Processed Meat and Poultry Products',
             'Recommended Code of Hygienic Practice for Poultry Processing',
             'Recommended International Code of Practice for the Production, Storage and Composition of Mechanically Separated Meat Intended for Further Processing')

 
codexEvent[which(codexEvent$Title %in% c(sugarStds, oilStds, fatStds, meatStds)),'supersededDummy'] = 1
codexEvent[which(codexEvent$Title %in% c(sugarStds, oilStds, fatStds, meatStds )),'droppedDummy'] = 0

codexEvent[which(codexEvent$Title %in% sugarStds),'Reference_superseding_standard'] = 'CODEX STAN 212-1999'
codexEvent[which(codexEvent$Title %in% oilStds),'Reference_superseding_standard'] = 'CODEX STAN 210-1999'
codexEvent[which(codexEvent$Title %in% fatStds),'Reference_superseding_standard'] = 'CODEX STAN 211-1999'
codexEvent[which(codexEvent$Title %in% meatStds),'Reference_superseding_standard'] = 'CAC/RCP 58-2005'

codexEvent[which(codexEvent$Title == 'Quick Frozen Carrots' ),c('committee', 'committee_num')][2,] =c("GEQFF14", 14)


save(codexEvent, file = paste0(pathMain, '/participation_development/codexEvent.rda'))


 

# ---------------------------------
# create proposal-year dataset
# ---------------------------------
 
# expand dataset for event data 
dates_all$committee_num= gsub('[A-Z]|\\(|\\)', '', dates_all$event_number) %>% as.numeric()
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
  
  if (x$Title[1] %in% c("Edible Ices", "Standard for Apples", "Standard for Bouillons and Consommés" )){ # these standards are missing either start or end dates of the committee
    dates_sub = dates_cac[as.numeric(gsub('[A-Z]', '', dates_cac$event_number_cac)) <= max(as.numeric(gsub('[A-Z]', '', x$cac))) & 
                            as.numeric(gsub('[A-Z]', '', dates_cac$event_number_cac)) >= min(as.numeric(gsub('[A-Z]', '', x$cac)))  ,]
    expand = merge(x, dates_sub[, c('year', 'event_number_cac')], by.x = c('cac', 'year'), by.y = c('event_number_cac', 'year'), all = TRUE)
  
  }
 
  expand = expand[order(expand$year),]

  expand$Reference = unique(na.omit(expand$Reference))
  expand$Reference_new = unique(na.omit(expand$Reference_new))
  expand$Title = unique(na.omit(expand$Title))
  expand$proposal_clean = unique(na.omit(expand$proposal_clean))
  expand$Reference_superseding_standard = ifelse(length(unique(na.omit(expand$Reference_superseding_standard)) ) == 0, NA, unique(na.omit(expand$Reference_superseding_standard)))
  expand$status = unique(na.omit(expand$status))
  expand$adopted = unique(na.omit(expand$adopted))

  expand$droppedDummy = unique(na.omit(expand$droppedDummy))
  expand$supersededDummy = unique(na.omit(expand$supersededDummy))
  expand$discontinuedDummy = unique(na.omit(expand$discontinuedDummy))
  expand$ongoingDummy = unique(na.omit(expand$ongoingDummy))
  #expand$foodCat= unique(na.omit(expand$foodCat))

  expand = expand[, -which(names(expand) %in% c('committee_raw', 'position'))]
  expand$cac_num = gsub('CAC', '', expand$cac) %>% as.numeric()

  return(x )
 
  }))
 
rownames(codexExpand)= NULL
 
# add meeting information
codexExpand$meeting_status = dates_all$meeting_status[match(codexExpand$committee, dates_all$event_number)] 
codexExpand$meeting_type = dates_all$meeting_type[match(codexExpand$committee, dates_all$event_number)] 

# add information as to how many proposals were considered in each meeting
codexExpand$tmp = 1
totProposals = unlist(lapply(split(codexExpand$tmp, codexExpand$committee), function(x){
  sum(x)
}))
codexExpand$numTotProposals = totProposals[match(codexExpand$committee, names(totProposals))]  
codexExpand = codexExpand[, -which(names(codexExpand) == 'tmp')]

# add centrality information
codexExpand = merge(codexExpand, centralityMeeting[, -1], by.x = c('committee'), by.y = c('event_number'), all.x = TRUE)
 
# double check centrality info
# meta_particip = read.csv(paste0(pathMain, '/participation_development/additional_coding_23_November_6_December_2018.csv'))
# names(meta_particip) = c('event_short', 'event_short_meeting', 'coded', 'onlyChairDum', 'chairButNoDelegatesDum', 'Notes')
# meta_particip$meetingCount = gsub('[A-Z]', '', meta_particip$event_short_meeting) %>% as.numeric()
# meta_particip$infoDum = ifelse(meta_particip$coded == 0, 1, 0)
# 
# # these are before 1962: "CGECPMMP1" "CGECPMMP2" "CGECPMMP3" "CGECPMMP4" "CGECPMMP5" 
# # no delegates info for "GEQFF14"     
# setdiff(codexExpand[which(is.na(codexExpand$committeeEigenCentState)),'committee'] %>% unique(),  unique(meta_particip$event_short_meeting))




# ---------------------------------
# add participant
# ---------------------------------
# merge in participant data
load(paste0(pathMain, '/participation_development/codex_participation_master.rda'))
codexExpand = merge(codexExpand, dplyr:::select(participAgg, -year), by.x = 'committee' , by.y = 'event_number', all.x = TRUE)
 
 
# merge in particpant data for cac meetings
participAgg_cac = participAgg[grep('CAC', participAgg$event_short),]
names(participAgg_cac)[c(3:547)]  = paste0(names(participAgg_cac)[c(3:547)], '_cac')
 
 
codexExpand  = merge(codexExpand, dplyr:::select(participAgg_cac,  -year,  -onlyChairDum, -chairButNoDelegatesDum, -infoDum, -missingDelegInfo ), by.x = 'cac' , by.y = 'event_number', all.x = TRUE)

# ---------------------------------
# add trade info
# --------------------------------- 
load(file = paste0(pathMain, '/participation_development/tradeClean.rda'))




# ---------------------------------
# create wide version with pariticipant data
# --------------------------------- 
codexWideAll = codexExpand %>% dplyr:::group_by(Reference, Title) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% data.frame()
codexWideAll = merge(codexWide, dplyr:::select(codexWideAll, -cac_num), by = c('Reference', 'Title'))
codexWideAll$handcodedDummy = ifelse(is.na(codexWideAll$step), 0, 1)
codexWideAll$cac_start_num = gsub('[A-Z]', '', codexWideAll$cac_start) %>% as.numeric()
codexWideAll$cac_end_num = gsub('[A-Z]', '', codexWideAll$cac_end) %>% as.numeric()
codexWideAll$committee_start_num = gsub('[A-Z]', '', codexWideAll$committee_start) %>% as.numeric()
codexWideAll$committee_end_num = gsub('[A-Z]', '', codexWideAll$committee_end) %>% as.numeric()
codexWideAll$wtoDummy = ifelse(codexWideAll$cac_start_num > 21, 1, 0)

# remove variables with no variation
removeNoVariablesWithNoVariation = apply(codexWideAll, 2, function(x){
  length(unique(na.omit(x)))
})
 

codexWideAll = codexWideAll[,-which(removeNoVariablesWithNoVariation  == 1)]

 

save(codexWideAll, file = paste0(pathMain, '/participation_development/codexWideParticip.rda'))



install.packages('glmnet')
library(glmnet)

options(max.print = 10000) 
 
test = codexWideAll[, c(4:5,7, 11:13,   20, 26:963, 966:1150)]

 
test = test[complete.cases(test),]
test$committee = as.factor(test$committee)
 
 

x = data.matrix(test[, -which(names(test) == 'yearsAfterProposal')] )
y = test$yearsAfterProposal
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]
 

lambda <- 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(x[train,], y[train], alpha = .5, lambda = lambda, family = "poisson")
cv.out <- cv.glmnet(x[train,], y[train], alpha = .5, family = "poisson")
bestlam <- cv.out$lambda.min
lasso.best <- glmnet(x[train,], y[train], alpha = .5, lambda = bestlam, family = "poisson")

rownames(coef(lasso.best))[which(coef(lasso.best)!=0)] %>% sort()
 

lasso.pred <- predict(lasso.best, s = bestlam, newx = x[test,])


names(codexWideAll)[1000:1100]

coef(lasso.best)
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)
names(lasso.coef)

head(coef(lasso.best))

mean((lasso.pred-ytest)^2)

small_lambda_index = which(lasso1$lambda == min(lasso1$lambda))

lasso1$lambda
small_lambda_betas = lasso1$beta[, small_lambda_index]
small_lambda_betas[which(small_lambda_betas>0)] %>%
 
dim(data.matrix)
test[1:5, 1:20]
(test)
dim(test)

head()
names(test)
dim(test)
names(codexWideAll)
lasso1 = cv.glmnet( data.matrix(codexWideAll[, c(4:5,7, 11:13,  27:898)]) ,  codexWideAll[, modDV], family = "poisson")

names(codexWideAll)
summary(codexWideAll[, c(27:100)])
head(data.matrix(codexWideAll[, c(4:7, 11:14, 17:20, 22:23, 26:1064)]) )
?cv.glmnet
dim(codexWideAll)

names(codexWideAll)[1:30]
head(codexWideAll$committee_start)
names(codexWideAll)
names(participAgg)
plot(log(codexWideAll$delegates), codexWideAll$yearsAfterProposal)
hist(codexWideAll$yearsAfterProposal)
summary(model2)
# get longitudinal data for superseded, dropped varibales
 
# think about a variable that can measure the technical complexity of a standard

# Chocolate
# Milk Additives

# private standards



  # Latin American vs African countries
  
 
 

### couldn't find earlier steps
# 'Standard for Cooked Cured Chopped Meat', step 6
# Standard for Cooked Cured Pork Shoulder at step 6


### Note Correctly specified as yearsAfterProposal = 0
# Standard for Dairy Permeate Powders
# General Methods for the Detection of Irradiated Foods

# apppears to be correct for tail ends of yearsAfterProposal
# Recommended Methods of Analysis of Pesticide Residues
# Standards for Apple
# Standard for Canned Shrimps or Prawns
# Standard for Certain Canned Citrus Fruits    
# Standard for Cocoa (Cacao) Mass (Cocoa/Chocolate Liquor) and Cocoa Cake 
# Standard for Fat Spreads and Blended Spreads
# Recommended Methods of Analysis of Pesticide Residues
# Canned asparagus
# Canned mushrooms
# Standard for Canned Applesauce
# Standard for Canned Fruit Cocktail
# Standard for Canned Pineapple
# Standard for Canned Strawberries
# Standard for Raisins
# Dextrose anhydrous
# Dextrose monohydrate
# Dried glucose syrup
# Powered sugar (icing sugar)
# White sugar
# Codex General Standard for Process(ed) Cheese and Spreadable process(ed) Cheese
# General Standard for Cheese
# "Recommended International Code for Ante-Mortem and Post-Mortem Inspection of Slaughter Animals and for Ante-Mortem and Post-Mortem Judgement of Slaughter Animals and Meat"
# Codex General Standard for Process(ed) Cheese and Spreadable process(ed) Cheese
 
# note correctly specified as cacAfterProposal = 0
# Standard for Tilsiter 
 

 
 
# missing = codexWide[which(codexWide$yearsAfterProposal == 0), 'Title']%>% sort()
# missingInfo = codexLong[which(codexLong$Title %in% c(missing)),]
# missingInfo = missingInfo[order(missingInfo$proposal_clean),]
# missingInfo$Title[grep('Unadopted', missingInfo$Title)] = 0
# missingInfo = missingInfo[mixedorder(missingInfo$Reference),]
# missingMeetings = codexLong[which(codexLong$Title %in% c(missingStandardsAll, missingStandardsCommitteeOnly )),]
# missingAll = rbind(missingInfo, codexLong[which(codexLong$Title == '.'),], missingMeetings) 
#write.csv(missingAll, file = paste0(pathMain, '/participation_development/missingAll.csv'))
# foo = codexWide[which(codexWide$yearsAfterProposal == 0 & codexWide$Reference==0), 'Title']%>% sort()
# missingUnadoptedProposals = codexLong[which(codexLong$Title %in% foo),]
# missingUnadoptedProposals = missingUnadoptedProposals[order(missingUnadoptedProposals$proposal_clean),]
# missingUnadoptedProposals$Title = 0
# write.csv(missingUnadoptedProposals, file = paste0(pathMain, '/participation_development/missingUnadoptedProposals.csv'))


# Codex Alimentarius in July
# meetings always adopted in CAC

# Two possible analyses

# Analyses 1 
# Survival analysis where:
# the DV is the length of time that a standard lingers in the draft stage
# and the censoring is whether the ultimately gets adopted or not
# This analyses is able to follow each standard in particular



# Analysis 2
# at the first stage, a standard is whether proposal is adopted not in the cac
# the second stage is then the time that it took for that specific standard to be adopted  


# Analysis 3
# at the first stage, a standard is whether proposal accepted or not in the cac
# the second stage is i) average number of times the proposal was amended or revised per year ii) total number of times the proposal was amended or revised


# Analysis  4 
# at the first stage, a standard is proposed or not within a particular committee
# the second stage is then the time until that specific standard is adopted in the CAC 

# Dataset then has to look like this:

# CCMC ; year X;  Standard X ; 10 years before adopted ; composition of delegates in CCMC ; composition of delegates in CAC when adopted ; composition of delegates over length of time to adoption
# CCMC ; year Y; No standard ; 0 ; 



# DOUBLE CHECK HOW DROPPED DUMMY/SUPERSEDED DUMMY are merged.....
 
 

# -------------------------
# Load particip agg data
# -------------------------
load(paste0(pathMain, '/participation_development/codex_participation_master.rda'))
 
# ---------------------------------------
# Merge Std and Participation Data Based on Specific Committees 
# ---------------------------------------
 
# find matches for which there is a direct year-committee match
directMatch = merge(participAgg, std, by = c('event_short', 'year'), all = FALSE)


directMatch = merge(participAgg, std, by = c('event_number'), all = FALSE)

# find standards for which there is no direct year-committee match with the participants data
stdMissMatch = std[which(std$std_id %in% setdiff(std$std_id, directMatch$std_id)),]

# identify standards for which lag time between last meeting and time to standard adoption/revision/amendment more than 1 year
stdMissMatch$year = stdMissMatch$year-1
ByOneMatch = merge(participAgg, stdMissMatch, by = c('event_short', 'year'), all = FALSE)
stdMatch = stdMissMatch[which(stdMissMatch$std_id %in% ByOneMatch$std_id),]

# combine ....
std2 = rbind(std[which(std$std_id %in% directMatch$std_id),],  # standards that have a direct match, 
      stdMatch, # standarads that have match difference by one year,
      std[-which(std$std_id %in% c(directMatch$std_id, ByOneMatch$std_id)),] #  unmatched standards
      )



std[-which(std$std_id %in% c(directMatch$std_id, ByOneMatch$std_id)),'Title']

std2 = std2[order(std2$event_short, std2$year),]
 
# standards for which lag time between last meeting and time to standard adoption/revision/amendment more than 1 year

# CCFFP revision in 1979, previous meeting was 1977
std2[which(std2$event_short == 'CCFFP' & std2$year_enact == 1979 ), 'year'] = 1977

# CCFO revision in 1984, previous meeting 1982
std2[which(std2$event_short == 'CCFO' & std2$year_enact == 1984 ), 'year'] = 1982

# CCFO revision in 1989, previous meeting 1987
std2[which(std2$event_short == 'CCFO' & std2$year_enact == 1989 ), 'year' ] = 1987

# CCGP revision in 1985, previous meeting 1981
std2[which(std2$event_short == 'CCGP' & std2$year_enact == 1985 ), 'year' ] = 1981 

# CCNMW adopted 1981, previous meeting was 197
std2[which(std2$event_short == 'CCNMW' & std2$year_enact == 1981 ), 'year' ]  = 1972

# CCPFV adopted in 1991, previous meeting was 1986
std2[which(std2$event_short == 'CCPFV' & std2$year_enact == 1991 ), 'year' ] = 1986

# CCPFV adopted in 1993, previous meeting was 1986
std2[which(std2$event_short == 'CCPFV' & std2$year_enact == 1993 ), 'year' ] = 1986

# CCS adopted in 1981, previous meeting 1974
std2[which(std2$event_short == 'CCS' & std2$year_enact == 1981 ), 'year' ] = 1974

# CCS revised in 1987, previous meeting was 1974
std2[which(std2$event_short == 'CCS' & std2$year_enact == 1987 ), 'year' ] = 1974

# CCS adopted 1999, previous meeting 1974
std2[which(std2$event_short == 'CCS' & std2$year_enact == 1999 ), 'year' ] = 1974

# TFAF amended in 2008, previous meeting 2004
std2[which(std2$event_short == 'TFAF' & std2$year_enact == 2008 ), 'year' ] = 2004

# merge participation and standards info
codexCommittee = merge(participAgg, std2, by = c('event_short', 'year'), all = TRUE)

# the executive committee doesn't pass standards; 
# The CAC gets its own column of variables (i.e. 'delegates_cac') so remove them from the rows
# codex = codex[-which(codex$event_short %in% c('CCEXEC', 'CAC')),]
codexCommittee = codexCommittee[-which(codexCommittee$event_short %in% c('CCEXEC')),]


 
# ---------------------------------------
# Merge Data Based on CAC Committee
# ---------------------------------------
cacAgg = participAgg[which(participAgg$event_short == 'CAC'), -grep('chair_|obs_', names(participAgg))]
names(cacAgg) = paste0(names(cacAgg), '_cac')
rownames(cacAgg ) = 0:c(dim(cacAgg )[1]-1)

 
# mismatch between year enacted and year CAC met; assume that the standard was voted on the meeting immediatly previous
std2[which(std2$year_cac %in% c(1967)), 'year_cac'] = '1966'
std2[which(std2$year_cac %in% c(1973)), 'year_cac'] = '1972'


### add meeting count data to st2
std2$meetingCount_cac = cacAgg$meetingCount_cac[match(paste0('CAC', std2$year_cac) ,paste0(cacAgg$event_short_cac, cacAgg$year_cac))]

# there were 2 codex meetings in 2003; all of the action took place at the 26 session
std2$meetingCount_cac[which(std2$year_cac == 2003)] = 26

 
codexCAC= merge(std2, cacAgg, by = c('meetingCount_cac', 'year_cac'), all = FALSE)
 
 
# merge specific committee participation and CAC info 
codex = merge(codexCommittee, codexCAC[, c( grep('deleg|std_id', names(codexCAC)))], by = 'std_id', all.x = TRUE)
 
codex$delegates[which(is.na(codex$delegates) & !is.na(codex$delegates_cac))] = codex$delegates_cac[which(is.na(codex$delegates) & !is.na(codex$delegates_cac))]
codex$delegations[which(is.na(codex$delegations) & !is.na(codex$delegations_cac))] = codex$delegations_cac[which(is.na(codex$delegations) & !is.na(codex$delegations_cac))] 
codex$delegConc[which(is.na(codex$delegConc) & !is.na(codex$delegConc_cac))] = codex$delegConc_cac[which(is.na(codex$delegConc) & !is.na(codex$delegConc_cac))]


# --------------------
## Check
# codex_min = merge(participAgg, std2, by = c('event_short', 'year'), all.x = TRUE)
# stdMissMatch2 = std2[which(std2$std_id %in% setdiff(std2$std_id, codex_min$std_id)),]
  
# Note when the relevant committee was adjourned/abolished, substituted the delegation measures for the CAC committee instead. These committees were the following:
  # CCCPC reported revising or amending standards from 2003 to 2016, however it stopped meeting in 2001
  # CCCPL reported adopting or ameding standards in 2003, 2013, 2016, but last meeting report we have for them is 1994
  # CCFFP reported revising/amending standards in 1979, 2017
  # CCMMP revision 2013, 2014, 2016, adjouned as a committee/last meeting.. in  2010; find out when actually adjou
  # CCNMW amended 2011, adjounred as a committee/last meeting 2008
  # CCPMPP revised 2014, 2015, abolished/last met in 1990
  # CCSB adopted, revised 1981, 2001, 2015, committee abolished/last met 1977
  # CCVP revised in 2001, committed adjourned prob 1989
  # TFFBT amended 2011, dissolved 2008
  # TFPHQFF adopted, revised 1976, 1978, 1983, first meeting recorded is 2008, since dissolved

# --------------------
## Create Variables ##

# get vector of relevant variable names
delegateNamesAll = names(codex)[grep('delegates|delegations', names(codex))]
delegateNames = delegateNamesAll[-grep('cac', delegateNamesAll)]
delegateNamesCAC = delegateNamesAll[grep('cac', delegateNamesAll)]


# create dummy for committees that were adjounred/abolished and subsequently had revisions to their standards 
codex$activeCommittee = ifelse(is.na(codex$meetingCount), 0, 1)
 
# set NAs equal to 0
codex$adopted[which(is.na(codex$adopted))] = 0
codex$revised[which(is.na(codex$revised))] = 0
codex$amended[which(is.na(codex$amended))] = 0
 

# create log number of delegates variable
ldeleg = apply(codex[, delegateNamesAll], 2, log0) %>% data.frame()
names(ldeleg) = paste0('l', names(ldeleg))


# create percentage variables
delegShare = apply(codex[, delegateNames[-c(1,2)] ], 2, varShare) %>% data.frame()
names(delegShare) = paste0(names(delegShare), '_share')

delegShareCAC = apply(codex[, delegateNamesCAC[-c(1,2)] ], 2, varShare, cac = TRUE) %>% data.frame()
names(delegShareCAC) = paste0(names(delegShareCAC), '_share')
 
codex$delegates_developedShare = rowSums(codex[, c('delegates_income_group_Highincome', 'delegates_income_group_Uppermiddleincome')] )/codex$delegates 
codex$delegates_developedShare_cac = rowSums(codex[, c('delegates_income_group_Highincome_cac', 'delegates_income_group_Uppermiddleincome_cac')] )/codex$delegates_cac 

# create delegation size variable
codex$delegationSize = codex$delegates/codex$delegations
codex$delegationSize_cac = codex$delegates_cac/codex$delegations_cac

codex = cbind(codex, ldeleg, delegShare, delegShareCAC) 

# create variable of number of times a standard has been amended or revised
codex$bothAmendedOrRevised = rowSums(codex[, c('amended', 'revised')], na.rm = TRUE)
 
# create dummy variable for if the WTO is is existence
codex$wtoDummy = ifelse(codex$year>=1995, 1, 0)


# add centrality measure


save(codex, file = paste0(pathMain, '/participation_development/codexData.rda'))


####
 



dim(codexWide)
missing = codexWide[which(is.na(codexWide$delegates_income_group_Lowincome)) ,] 
codexWide[which(codexWide$Title %in% setdiff(missing$Title, c(missingStandardsAll, missingStandardsCommitteeOnly))), 1:20]

codexLong[which(codexLong$proposal_Meeting == 'CCEXEC61'),]
proposedCleanAll[grep(',',  proposedCleanAll$proposal_Meeting), 'proposalMeeting']

codexWide[grep( "Standard for Preserved Tomatoes", codexWide$Title),]

#load(paste0(pathMain, '/participation_development/codexWide.rda'))

 
# CHECK 
#codexWide[codexWide$yearsAfterProposal > 10, c('yearsAfterProposal', 'Title')]
dim(codexWide)
head(codexWide)
codexWide[which(codexWide$Reference != 0), 'Meeting'] %>% table() 
 

codexWide[which(codexWide$Reference != 0 & codexWide$yearsAfterProposal == 0 ), c('proposal', 'Title')] 
codexWide[which(codexWide$Reference != 0 & codexWide$yearsAfterProposal == 0 & !is.na(codexWide$proposal)), 'Title'] %>% dim()
codexWide[which(codexWide$Reference != 0 & !is.na(codexWide$proposal)), 'proposal'] 
 
standardsMissingProposalsMay2019 = codexWide[which(codexWide$Reference != 0 & codexWide$yearsAfterProposal ==0), 'Title'] 

 

codexWide[which(codexWide$adopted ==0), 'startYear'] == codexWide[which(codexWide$adopted ==0), 'endYear']
codexWide[which(codexWide$adopted ==0), 'endYear'] == codexWide[which(codexWide$adopted ==0), 'endCac']

#write.csv(standardsMissingProposalsApril2019, paste0(pathMain, '/participation_development/standardsMissingProposalsApril2019.csv'))

# plot number of standards over time
codexWide$dummy = ifelse(codexWide$Reference ==0, 0, 1)
codexAgg = codexWide %>% dplyr:::group_by(startYear) %>% dplyr:::summarise(count = sum(dummy))
plot(codexAgg$startYear, codexAgg$count, type = 'l')
 

 
## merge with CAC delegate data
# codexCACFinal = merge(proposedAgg, cacAgg, by.x = c('meetingCount_cac'), by.y = 'start_cac')


# Ask Seb: CAC 31 Report, list of adopted standards.... #Practice for Fish and Fishery Products
# std[grep('Practice for Fish and Fishery Products', std$Title),]
# proposed[grep('Standard for Chilli Peppers', proposed$Title),]

# Ask Seb: titles to resolve
# setdiff(unique(proposed$Title), unique(std$Title))

# need to first match based on proposal stage at the committee level
# adoption stage is then matched at the CAC level
# the complicating factor is that the proposal data is only available at the CAC level
# so need to create a concordance between CAC and specific committees where possible


# note also that the same standard is proposed and drafted multple times as it is proposed
# and then subsequently revised
# so the more appropriate model would be one in which 

# data cleanup
# proposed[which(proposed$Title == 'CAC/RCP 15-1976'), c('Reference', 'Title')] #= c('CAC/RCP 15-1976', 'MISSING')

# incorporate year
# dates = read.dta13(paste0(pathMain, '/participation_development/codex_event_dates.dta'))
# dates_cac = dates[which(dates$event_short == 'CAC'),]
# dates_cac$meetingCount_cac = gsub('[A-Z]', '', dates_cac$event_number)
# proposed = merge(proposed, dates_cac[,c('year', 'meetingCount_cac')], by = c('meetingCount_cac'), all = TRUE)


#NOTE: Proposed Draft Standard for Processed Cheese stopped/ended in 2010 according to CCMMP 2010


length of time is a reflection of disagreement


 
