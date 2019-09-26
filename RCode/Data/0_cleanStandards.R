# Clean Codex standards

if(Sys.info()['user'] == 'cindycheng'){
  source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}

load(file = paste0(pathMain, '/participation_development/codex_event_dates_master.rda'))
# -------------------------
# Clean Standards Data
# --------------------------
# load std data
std = read.dta13(paste0(pathMain, '/participation_development/codex_standard_development.dta'))
std = std[order(std$event_short, std$year),]

# note have to change some commmittee names to be consistent as some were renamed and reestablished
std$event_short_raw = std$event_short

# Fixed/Renamed: CCCF and CCFA are now separate committees but prior to 2007 were effectively combined into one committee: CCFAC
std[which(std$event_short == 'CCCF' & std$year<2007),  c('event_short')] = 'CCFAC'
std[which(std$event_short == 'CCFA' & std$year<2007),  c('event_short')] = 'CCFAC'

# Fixed/Renamed: CGECPMMP was the adhoc working group for milk products, before it became a committee in 1994, CCMMP
std[which(std$event_short == 'CGECPMMP'),  c('event_short')] = 'CCMMP'

# `year_enact` is consistent with the year a standard is adopted, revised or amended
# consequently this means the 'year' variable is consistent with the year there is a relevant Codex Committee meeting 
std$year_enact = std$year

# `year_cac' will be consistent with the year that the closest CAC meeting prior to the year before the adoption/revision/amendment of the standard was held
# note: this variable will likely be refined as Sebastian collects data on the exact dates
std$year_cac = std$year


# looks like 'General Standard for Contaminants and Toxins in Food and Feed' has multiple Reference Codes, correct one is CODEX STAN 193-1995
std = std[-which(std$Reference %in% paste0('CODEX STAN 193-199', 6:9)),]

# looks like 'Standard for Edible Fats and Oils not Covered by Individual Standards' has multiple Reference Code, correct one is CODEX STAN 19-1981
std = std[-which(std$Reference %in% paste0('CODEX STAN 19-198', 2:4)),]

# add standard for Regional Standard for Non-Fermented Soybean Product
std = rbind(std, c('CODEX STAN 322R-2015', 'Regional Standard for Non-Fermented Soybean Products', 'CCASIA', 2015, 1, NA, NA, 'CCASIA', 2015, 2015))

 
std$Reference_superseding_standard = NA

# --------------------------
## add standards formally superseded
# -------------------------
load(file = paste0(pathMain, '/participation_development/codex_superseding_standards_clean.rda'))

# extract adoption year
superStds$year = superStds$year_cac = superStds$year_enact = str_sub(gsub(", Rev. 1-1993|, Rev. 1 1985", '', superStds$Reference_original_standard), 
                                                                     nchar(gsub(", Rev. 1-1993|, Rev. 1 1985", '', superStds$Reference_original_standard))-3,
                                                                     nchar(gsub(", Rev. 1-1993|, Rev. 1 1985", '', superStds$Reference_original_standard))) %>% as.numeric()
 
# extract event_short and event_short_raw
superStds$event_short_raw  = superStds$event_short = gsub('\\d', '', superStds$Proposal_meeting_original_standard)
superStds[which(superStds$Title_original_standard == "Recommended International Code of Hygienic Practice for Processed Meat and Poultry Products"),c('Proposal_meeting_original_standard', 'event_short', 'event_short_raw')] = c('CCPMPP9', 'CCPMPP', 'CCPMPP')

 
# create adopted/revised/amended variables
superStds$adopted = 1
superStds$revised = superStds$amended = NA


# change variable names
names(superStds)[which(names(superStds) %in% c( "Reference_original_standard", "Title_original_standard"))] = c("Reference", "Title")



# according to CAC20, Standard for Raw Cane sugar adopted at step 5 at CAC19
# but subsequently superseded by standard for sugars
superStds = rbind(superStds, data.frame(Reference_superseding_standard = 'CODEX STAN 212-1999',
           Reference = 'Missing', 
           Title = 'Standard for Raw Cane Sugar',
           Proposal_meeting_original_standard = 'CCS6', 
           year_enact = 1991,
           year_cac = 1991,
           year = 1991,
           event_short = 'CCS',
           event_short_raw = 'CCS', 
           adopted = '1',
           amended = NA,
           revised = NA))


# Combine superseded standards and regular standards
std = rbind(std, superStds[, names(std)])
std$supersededDummy = ifelse(is.na(std$Reference_superseding_standard), 0, 1)


# --------------------------
## incorporate information on standards now dropped
# -------------------------
droppedStds = read.csv(paste0(pathMain, '/participation_development/dropped_standards/standards_dropped.csv'), stringsAsFactors = FALSE)

# add in dummy info for if std was dropped
std$droppedDummy = droppedStds$Dropped[match(std$Reference, droppedStds$Reference)]
std$droppedDummy[which(std$Title %in% c('Guidelines for Mixed Fruit Nectars',
                                        'Recommended International Code of Hygienic Practice for Fresh Meat',
                                        'Recommended International Code of Hygienic Practice for Processed Meat and Poultry Products',
                                        'Recommended International Code of Hygienic Practice for Game',
                                        'Standard for Extra Hard Grating Cheese'))] = 1
std$droppedDummy[which(is.na(std$droppedDummy))] = 0

# keep only data on dropped stds that were not previously incorporated in 'std' dataset
droppedStds = droppedStds[which(droppedStds$Reference %in% c(setdiff(droppedStds$Reference, std$Reference))),]
droppedStds = droppedStds[-which(droppedStds$Title %in% c('GUIDELINES FOR MIXED FRUIT NECTARS',
                                                          'Recommended International Code of Hygienic Practice for Fresh Meat',
                                                          'Recommended International Code of Hygienic Practice for Processed Meat and Poultry Products',
                                                          'Codex of Hygienic Practice for Game',
                                                          'Codex International Standard for Extra Hard Grating Cheese',
                                                          "Standard for dried edible fungi"
                                      )),]

# remove maximum levels of residues
droppedStds = droppedStds[-grep('Maximum|MAXIMUM', droppedStds$Title),]

# clean Title names
droppedStds$Title[which(droppedStds$Title == 'Quick-Frozen Fillets of Hake Canned Sardines and Sardine-Type Products')] = 'Quick-Frozen Fillets of Hake'
droppedStds$Title = str_trim(droppedStds$Title)

# extract year
droppedStds$year = droppedStds$year_enact = droppedStds$year_cac = gsub('\\-', '', sub("^[^\\-]*", "", droppedStds$Reference)) %>% as.numeric()
droppedStds$adopted =droppedStds$droppedDummy= 1

droppedStds$event_short = droppedStds$event_short_raw = droppedStds$revised = droppedStds$amended = droppedStds$Reference_superseding_standard = NA
droppedStds$supersededDummy = 0
 
# regional standard for mayonnaise adopted at CCEURO17
droppedStds[grep('Mayonnaise', droppedStds$Title), c('event_short', 'event_short_raw')] = c('CCEURO') 

# regional standard for vinegar adopted at CCEURO15
droppedStds[grep('Vinegar', droppedStds$Title), c('event_short', 'event_short_raw')] = c('CCEURO') 

# add meeting for Recommended International Code of Practice for the Control of the Use of Veterinary Drugs
droppedStds[which(droppedStds$Title == 'Recommended International Code of Practice for the Control of the Use of Veterinary Drugs'), c('event_short', 'Adopted', 'year', 'year_cac')] = c('CCRVDF', '1', 1992, 1992)

 
# combine dropped standards with stds
std = rbind(std, droppedStds[, names(std)])

"CAC/RCP 41-1993" #std
"CAC/RCP 41" # dropped
# --------------

# make std id variable
std$RefID = factor(std$Reference)
levels(std$RefID) = 1:length(levels(std$RefID))
std$std_id = paste0(std$RefID, std$year)

 

# attach information on when standards were adopted, amended or revised, in each cac
load(file = paste0(pathMain, '/participation_development/codex_event_dates_cac.rda'))
std = std[order(std$year, std$Reference),]

dates_cac$meetingCount_cac = gsub('[A-Z]', '', dates_cac$event_number)
std$meetingCount_cac = dates_cac$meetingCount_cac[match( std$year_cac, dates_cac$year)]
std[which(is.na(std$meetingCount_cac)),'year_cac'] = std[which(is.na(std$meetingCount_cac)),'year_cac']%>% as.numeric()-1
std$meetingCount_cac = dates_cac$meetingCount_cac[match( std$year_cac, dates_cac$year)] %>% as.numeric()
std$meeting_cac = paste0('CAC', std$meetingCount_cac)
std$Meeting = dates_all$event_number[match(paste0(std$event_short_raw, std$meeting_cac), paste0(dates_all$event_short, dates_all$event_number_cac))]


# clean names
std$Title[which(std$Title == "Standard for Samsø")] = 'Standard for Samso'
std[which(std$Title == 'Edible Inces'),'Title'] =  'Edible Ices'

std[which(std$Title %in% c("Recommended International Code of Hygienic Practice for Foods for\nInfants and Children")), 'Title'] = "Recommended International Code of Hygienic Practice for Foods for Infants and Children"


# fix date for adoption of Quick Frozen Lobster
std[which(std$Title == 'Standard for Quick Frozen Lobsters' & std$adopted == 1), c('year', 'year_enact', 'year_cac', 'meetingCount_cac', 'meeting_cac')] = c(1977, 1977, 1978, 12, 'CAC12')



 # fix committees 
std[which(std$Title =='Code of Practice for the Processing and Handling of Quick Frozen Foods' ), c('event_short', 'event_short_raw')] = 'GEQFF'
std[which(std$Title =='Standard for Whole Maize (Corn) Meal' ), c('event_short', 'event_short_raw')] = 'CCAFRICA'
std[which(std$Title =='Standard for Gari' ), c('event_short', 'event_short_raw')] = 'CCAFRICA'
std[which(std$Title %in% c('Standard for Boiled Dried Salted Anchovies',
                           'Standard for Crackers from Marine and Freshwater Fish, Crustaceans and Molluscan Shellfish' )),
                        c('event_short', 'event_short_raw')] = 'CCASIA'




# fix committee meetings for quick frozen fruits
std$Meeting[which(std$Title %in% c('Quick Frozen Corn-on-the-Cob',
                           'Quick Frozen Whole Kernel Corn'))] = 'GEQFF13'
std$Meeting[which(std$Title %in% c('Standard for Quick Frozen Broccoli',
                           'Standard for Quick Frozen Cauliflower',
                           'Standard for Quick Frozen Brussels Sprouts',
                           'Standard for Quick Frozen Green and Wax Beans',
                           'Standard for Quick Frozen French Fried Potatoes'))] = 'GEQFF12'
std$Meeting[which(std$Title %in% c('Standard for Quick Frozen Blueberries',
                           'Quick Frozen Leeks'))] = 'GEQFF11'
std$Meeting[which(std$Title %in% c('Standard for Quick Frozen Spinach'))] = 'GEQFF10' 
std$Meeting[which(std$Title %in% c('Standard for Quick Frozen Peaches',
                           'Standard for Quick Frozen Bilberries'))] = 'GEQFF9'
std$Meeting[which(std$Title %in% c('Standard for Quick Frozen Raspberries'))] = 'GEQFF8' 
std$Meeting[which(std$Title %in% c('Standard for Quick Frozen Strawberries'))] = 'GEQFF6' 
std$Meeting[which(std$Title %in% c('Standard for Quick Frozen Peas'))] = 'GEQFF5' 
std$Meeting[which(std$Title %in% c('Quick Frozen Carrots'))] = 'GEQFF(14)' 


# fix committee meetings for code of hygenic practices
std$Meeting[which(std$Title %in% c('Code of Hygienic Practice for Canned Fruit and Vegetable Products', 
                                   'Code of Hygienic Practice for Dried Fruits'))] = 'CCFH5' 
std$Meeting[which(std$Title %in% c('Code of Hygienic Practice for Dehydrated Fruits and Vegetables including Edible Fungi',
                                   'Code of Hygienic Practice for Desiccated Coconut'))] = 'CCFH7' 
std$Meeting[which(std$Title %in% c('Code of Hygienic Practice for Tree Nuts'))] = 'CCFH9' 
std$Meeting[which(std$Title %in% c('Code of Hygienic Practice for Groundnuts (Peanuts)'))] = 'CCFH17' 
std[which(std$Title %in% c('Code of Hygienic Practice for Groundnuts (Peanuts)')), c('year_enact', 'meeting_cac')] = c(1981, 'CAC14')


# fix standards that were actually adopted in CCAFRICA not CCCPL
std[which(std$Title %in% c('Standard for Pearl Millet Flour',
                           'Standard for Whole and Decorticated Pearl Millet Grains')), c("Meeting") ] = c('CCAFRICA8')

std[which(std$Title %in% c('Standard for Edible Cassava Flour')), c("Meeting") ] = c('CCAFRICA9')



# fix committee meetings
std$Meeting[which(std$Title %in% c('General Standard for the Labelling of Food Additives when sold as such'))] = c('CCFAC13')



# fix cac meeting
std[which(std$Title %in% c('Standard for Bouillons and Consommés' )& std$adopted == 1) ,c('meeting_cac', 'year')]  = c('CAC13', 1979)


std[grep('GEQFF', std$Meeting), 'year_cac'] = dates_all$year[match(std[grep('GEQFF', std$Meeting), 'Meeting'], dates_all$event_number)]
std[grep('CCFH5|CCFH7|CCFH9|CCFH17', std$Meeting), 'year_cac'] = dates_all$year[match(std[grep('CCFH5|CCFH7|CCFH9|CCFH17', std$Meeting), 'Meeting'], dates_all$event_number)]

 
# CCFAC to CCFA
# aboblished: Edible Ices : substitue in CAC14 delegates here
# Standard for Bouillons and Consommés ; substitue CAC13 delegates here

# fix classes
std$year = as.numeric(std$year)
std$year_enact = as.numeric(std$year_enact)
std$year_cac = as.numeric(std$year_cac)
std$meetingCount_cac = as.numeric(std$meetingCount_cac)

head(std)

save(std, file = paste0(pathMain, '/participation_development/codex_standard_development_master.rda'))


# std = std[which(std$adopted == 1),]
# write.csv(std, file = paste0(pathMain, '/participation_development/codex_standard_yearCac.csv'))
# std[which(std$year_cac !=std$year),]
# dim(std)
# 
# 
# dim(codexWide[which(codexWide$adopted == 1),])


