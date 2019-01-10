if(Sys.info()['user'] == 'cindycheng'){
	source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}

library("tidystringdist")
 
# Codex Alimentarius in July
# meetings always adopted in CAC



# Two possible analyses

# Analyses 1 
# Survival analysis where:
# the DV is the length of time that a standard lingers in the draft stage
# and the censoring is whether the ultimately gets adopted or not
# This analyses is able to follow each standard in particular
 


# Analysis 2
# at the first stage, a standard is whether proposal accepted or not in the cac
# the second stage is then the time until that specific standard is adopted in the CAC 


# Analysis 3
# at the first stage, a standard is proposed or not within a particular committee
# the second stage is then the time until that specific standard is adopted in the CAC 

# Dataset then has to look like this:

# CCMC ; year X;  Standard X ; 10 years before adopted ; composition of delegates in CCMC ; composition of delegates in CAC when adopted ; composition of delegates over length of time to adoption
# CCMC ; year Y; No standard ; 0 ; 



# -------------------------
# Clean Standards Data
# --------------------------
# load std data
std = read.dta13(paste0(pathData, '/participation_development/codex_standard_development.dta'))
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

 
# make std id variable
std$RefID = factor(std$Reference)
levels(std$RefID) = 1:355
std$std_id = paste0(std$RefID, std$year)


# -------------------------
# Clean Participant Data
# --------------------------
# load and clean particip data
particip = read.dta13(paste0(pathData, '/participation_development/codex_participation_updated_chairs_observers.dta'))
names(particip)[which(names(particip) == 'weight')] = 'delegates'

# create variable of number of delegations
particip$delegations = ifelse(particip$delegates !=0, 1, 0)
 
### create measure of number of delegates per region, incom and interest grouping and of chairs/observers from various regions or incomes
participDummy = dummy_cols(particip, select_columns = c('region_group', 'income_group', 'interest_group'))
colStart = which(names(participDummy) == 'region_group_Europe & Central Asia')

	# delegates
	colEnd = which(names(participDummy) == 'interest_group_Media')

	disaggDelegates =   particip$delegates * as.matrix(participDummy[, colStart:colEnd])  %>% data.frame()
	names(disaggDelegates) =paste0('delegates_', names(participDummy)[colStart:colEnd])

	# chairs
	colEndIncome = which(names(participDummy) == 'income_group_Lower middle income')

	particip$chairDum = ifelse(particip$chair == 'chair', 1, 0)
	disaggChair =   particip$chairDum * as.matrix(participDummy[, colStart:colEndIncome])   %>% data.frame()
	names(disaggChair ) =paste0('chair_', names(participDummy)[colStart:colEndIncome])

	# observers
	particip$observeDum = ifelse(particip$observer == 'observer', 1, 0)
	disaggObserv =   particip$observeDum * as.matrix(participDummy[, colStart:colEndIncome])   %>% data.frame()
	names(disaggObserv ) =paste0('obs_', names(participDummy)[colStart:colEndIncome])

disagg = cbind(disaggDelegates, disaggChair, disaggObserv)  

# ask Seb about RIOPPAH; only actor that doesn't have a region type or income group
disagg = disagg[, -grep('n/a|interest_group_$|region_group_$|income_group_$', names(disagg))]
 
particip = cbind(particip, disagg) 

# ask seb: looks like a lot of IGOs are not classified for the interest group measure
# particip[which(particip$interest_group == ''), 'actor_type'] %>% table()
# particip[which(particip$interest_group == '' & particip$actor_type == 'IGO'), 'actor_name'] %>% table()
 
# create variable that meausures the square of number of delegates weighted by total number of delegates for that committee (delegateWt2) in order to later create herfindahl index of delegations (delegConc)
delegTotal = particip %>% group_by(year, event_short) %>% dplyr:::summarise(delegates = sum(delegates, na.rm = TRUE)) %>% data.frame()
particip$delegatesTotal = delegTotal$delegates[match(paste0(particip$event_short, particip$year), paste0(delegTotal$event_short, delegTotal$year))]
particip$delegateWt2 = c(particip$delegates/particip$delegatesTotal)^2

# aggregate data
participAgg = particip %>% 
				group_by(year, event_id, event_short) %>% 
				summarise_at( vars(matches('deleg|chair_|obs_')), sum, na.rm = TRUE) %>% data.frame()


names(participAgg)[which(names(participAgg) == 'delegateWt2')] = 'delegConc'				 
participAgg = participAgg[, - which(names(participAgg) == 'delegatesTotal')]
names(participAgg) = gsub('\\.', '', names(participAgg))

# incorporate dates
dates = read.dta13(paste0(pathData, '/participation_development/codex_event_dates.dta'))
dates$meetingCount = gsub('[A-Z]', '', dates$event_number)

participAgg = merge(participAgg, dates, by = c('event_short', 'year'), all= TRUE)


# # add in dummies for meetings for which there is no info
meta_particip = read.csv(paste0(pathData, '/participation_development/additional_coding_23_November_6_December_2018.csv'))
names(meta_particip) = c('event_short', 'event_short_meeting', 'coded', 'onlyChairDum', 'chairButNoDelegatesDum', 'Notes')

meta_particip$meetingCount = gsub('[A-Z]', '', meta_particip$event_short_meeting) %>% as.numeric()
meta_particip$infoDum = ifelse(meta_particip$coded == 0, 1, 0) 

participAgg$onlyChairDum = meta_particip$onlyChairDum[match(paste0(participAgg$event_short, participAgg$meetingCount), meta_particip$event_short_meeting)]
participAgg$chairButNoDelegatesDum = meta_particip$chairButNoDelegatesDum[match(paste0(participAgg$event_short, participAgg$meetingCount), meta_particip$event_short_meeting)]
participAgg$infoDum = meta_particip$infoDum[match(paste0(participAgg$event_short, participAgg$meetingCount), meta_particip$event_short_meeting)]


participAgg$onlyChairDum[which(is.na(participAgg$onlyChairDum))] = 0
participAgg$chairButNoDelegatesDum[which(is.na(participAgg$chairButNoDelegatesDum))] = 0
participAgg$infoDum[which(is.na(participAgg$infoDum))] = 0
participAgg$missingDelegInfo = ifelse(participAgg$onlyChairDum ==1|participAgg$chairButNoDelegatesDum ==1 |participAgg$infoDum ==1 , 1, 0)
 
 
# assume that if there are no delegates and we've not explicitely taken missing info into account, the committee didn't/doesn't exist
participAgg=participAgg[-which(participAgg$delegates == 0 & participAgg$missingDelegInfo == 0 ),]  

# check assumption
# split(participAgg$year, participAgg$event_short)



# ---------------------------------------
# Merge Data Based on Specific Committees 
# ---------------------------------------



# find matches for which there is a direct year-committee match
directMatch = merge(participAgg, std, by = c('event_short', 'year'), all = FALSE)
 
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
codex = merge(participAgg, std2, by = c('event_short', 'year'), all = TRUE)

# the executive committee doesn't pass standards; 
# The CAC gets its own column of variables (i.e. 'delegates_cac') so remove them from the rows
# codex = codex[-which(codex$event_short %in% c('CCEXEC', 'CAC')),]
codex = codex[-which(codex$event_short %in% c('CCEXEC')),]


# ---------------------------------------
# Merge Data Based on CAC Committee
# ---------------------------------------

cacAgg = participAgg[which(participAgg$event_short == 'CAC'), -grep('chair_|obs_', names(participAgg))]
names(cacAgg) = paste0(names(cacAgg), '_cac')

cacDirectMatch = merge(std, cacAgg, by = c('year_cac'), all = FALSE)


cacMissMatch = std[which(std$std_id %in% setdiff(std$std_id, cacDirectMatch$std_id)),]
cacMissMatch$year_cac = cacMissMatch$year_cac -1
cacByOneMatch = merge(cacAgg, cacMissMatch, by = c('year_cac'), all = FALSE) 


codexCAC = rbind(cacDirectMatch, cacByOneMatch)

 
# merge participation and CAC info 
codex = merge(codex, codexCAC[, c(12, grep('deleg', names(codexCAC)))], by = 'std_id', all.x = TRUE)
 
codex$delegates[which(is.na(codex$delegates) & !is.na(codex$delegates_cac))] = codex$delegates_cac[which(is.na(codex$delegates) & !is.na(codex$delegates_cac))]
codex$delegations[which(is.na(codex$delegations) & !is.na(codex$delegations_cac))] = codex$delegations_cac[which(is.na(codex$delegations) & !is.na(codex$delegations_cac))] 
codex$delegConc[which(is.na(codex$delegConc) & !is.na(codex$delegConc_cac))] = codex$delegConc_cac[which(is.na(codex$delegConc) & !is.na(codex$delegConc_cac))]




 

# ---------------------------------------
# Clean Standard Proposal Data
# ---------------------------------------
# load proposed standard data
proposed = read.csv(paste0(pathData, '/participation_development/CAC_standards.csv'), stringsAsFactors = FALSE)

# data cleanup
proposed[which(proposed$Title == 'CAC/RCP 15-1976'), c('Reference', 'Title')] = c('CAC/RCP 15-1976', 'MISSING')
proposed[which(proposed$standards_extracted == 'Proposed Draft Guidelines for the Inspection and Certification of Fresh Fruits and Vegetables for Conformity to Quality Standards'), c('Title')] = c(0)

# create variables
proposed$meetingCount_cac  = gsub('[A-Z]', '', proposed$meeting) %>% as.numeric()

# incorporate year
dates = read.dta13(paste0(pathData, '/participation_development/codex_event_dates.dta'))
dates_cac = dates[which(dates$event_short == 'CAC'),]
dates_cac$meetingCount_cac = gsub('[A-Z]', '', dates_cac$event_number)
proposed = merge(proposed, dates_cac[,c('year', 'meetingCount_cac')], by = c('meetingCount_cac'), all = TRUE)


# Double check with Seb ; these appear to be duplicates

# check
#test = proposed[which(duplicated(proposed[, c('Reference', 'year', 'Title')])& proposed$Reference!=0|duplicated(proposed[, c('Reference', 'year', 'Title')], fromLast = TRUE) & proposed$Reference!=0),] 
#split(test$standards_extracted, list(test$meeting, test$Reference))

proposed = proposed[-which(duplicated(proposed[, c('Reference', 'year', 'Title')])& proposed$Reference!=0),] 

# ------------------------------
# clean standards_extracted names
# -------------------------------
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

proposed$standards_extracted_clean = proposed$standards_extracted

proposed$standards_extracted_clean= tolower(proposed$standards_extracted_clean)
proposed$standards_extracted_clean = apply(proposed$standards_extracted_clean %>% data.frame(),1, function(x) simpleCap(x))
proposed$standards_extracted_clean = gsub(' For ', ' for ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' To ', ' to ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' The ', ' the ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' On ', ' on ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' In ', ' in ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' Of ', ' of ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' With ', ' with ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub(' And ', ' and ', proposed$standards_extracted_clean)
proposed$standards_extracted_clean = gsub("Dtaft'standdrd|Dtaft'Standdrds|Dtaft'Standdrds", 'Draft standard', proposed$standards_extracted_clean)
 
 
# for identifying near duplicates
proposedNearDupe = tidy_comb_all(unique(proposed$standards_extracted_clean)[-which(unique(proposed$standards_extracted_clean)=='0'|is.na(unique(proposed$standards_extracted_clean)))])
proposedNearDupeDist = tidy_stringdist(proposedNearDupe) %>% data.frame()
dim(proposedNearDupe)

proposedNearDupeDist[which(proposedNearDupeDist$cosine <.03),c('V1', 'V2')][190:195,]
 
#Proposed Draft Guidelines on Performance Criteria and Validation of Methods for Detection, Identification and Quantification of Specific Dna Sequences and Specific Proteins in Foods
proposed[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendments to the Codex General Standard for Food Additives: Annex to Table 3 (food Categories Or Individual Food Items Excluded From the General Conditions of Table 3)',
                                                         "Proposed Draft Food Additive Provisions of the General Standard for Food Additives (GSFA)")),]


proposed[grep('Nutrient Reference Values', proposed$standards_extracted_clean),]

proposed[grep('Sh', proposed$standards_extracted_clean), 'standards_extracted_clean'] %>% unique()

proposed[grep('Cereal-based|Cereal', proposed$standards_extracted_clean), ]  

unique(proposed$standards_extracted_clean)



proposed$standards_extracted_clean[grep('Gluten-free', proposed$standards_extracted_clean)] = 'Proposed Draft Standard for Gluten-Free Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standards for Composite and Filled Chocolate') )] = 'Proposed Draft Standard for Composite and Filled Chocolate'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Codex Standard for Oats.') )] =   'Proposed Draft Codex Standard for Oats'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Ginseng Product') )] = 'Proposed Draft Standard for Ginseng Products'

proposed$standards_extracted_clean[grep('Gadidae Family', proposed$standards_extracted_clean) ]  =  'Proposed Draft Amendment to the Standard for Salted Fish and Dried Salted Fish of the Gadidae Family (Sampling and Analysis)'

proposed$standards_extracted_clean[grep('Judgement of Equivalence of Sanitary Measures', proposed$standards_extracted_clean) ] =   'Proposed Draft Guidelines on the Judgement of Equivalence of Sanitary Measures Associated with Food Inspection and Certification Systems'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Levels for Doexynivalenol') )] = 'Proposed Draft Maximum Level for Deoxynivalenol'

proposed$standards_extracted_clean[grep('Processed Cheese', proposed$standards_extracted_clean) ] = "Proposed Draft Standard for Processed Cheese" 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendment to the Standard for Canned Sardines and Sardine-type Products',
                                                                                         'Proposed Draft Amendment to the Standard for Sardine and Sardine Type Products') ) ]  ='Proposed Draft Amendment to the Standard for Canned Sardine and Sardine Type Products'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for “bitter” Cassava') ) ]  = 'Proposed Draft Standard for Bitter Cassava'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Levels Hydrocyanic Acid in Cassava and Cassava Products') )] = 'Proposed Draft Maximum Levels for Hydrocyanic Acid in Cassava and Cassava Products'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Mrls for Bovine Somatotropins (bsts)',
                                                                                   'Proposed Draft for Mrl for Bovine Somatotropins (bsts)') )] =  'Proposed Draft for Mrl for Bovine Somatotropins (BSTS)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendments to Codex Standards Fot Food for Infanta and Children',
                                                                                   'Proposed Draft Revision of the Recommended International Code of Practice for Foods for Infants and Children (CAC/RCP 21-1979 – amended 1981)',
                                                                                   'Proposed Draft Atendtents to Codex Standards for Foods for Infants and Children') )] # = 'Proposed Draft Amendments to Codex Standards for Foods for Infants and Children'

 
proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendments to the International Numbering System for Food Additives',
                                                                                   'Proposed Draft Amendments to the International Numbering (ins) System for Food Additives',
                                                                                   'Proposed Draft Amendments to the International Numbering System (ins) for Food Additives',
                                                                                   'Proposed Draft Revision of the “class Names and International Numbering System for Food Additives - Cac/gl 36-1989',
                                                                                   'Proposed Draft Amendments to the International Numbering System for Food Additives (cac/gl 36- 1989)') )]  =  'Proposed Draft Amendments/Revisions to the International Numbering (INS) System for Food Additives'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Food Additives Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft for Food Additive Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additive Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additive Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additives Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft (step 3) and Draft (step 6) Food Additive Provisions of the Codex General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additive Provisions of the Gsfa',
                                                                                   'Proposed Draft Food-additive Provisions of the Gsfa',
                                                                                   'Proposed Draft Amendment to the General Standard for Food Additives: Preamble – Footnote',
                                                                                   'Proposed Draft Amendments to the Food Category System for the General Standard for Food Additives',
                                                                                   'Proposed Draft for Food Additive Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additive Provisions of the General Standard for Food Additives',
                                                                                   'Proposed Draft Codex General Standard for Food Additives',
                                                                                   'Proposed Draft General Standard on Food Additives',
                                                                                   'Proposed Draft Food Additive Provisions of the General Standard for Food Additives (gsfa)',
                                                                                   'Proposed Draft Food Additive Provisions of the General Standard for Food Additives (gsfa) (codex Stan 192-1995)',
                                                                                   'Draft and Proposed Draft Food Additive Provisions of the General Standard for Food Additives (gsfa) (codex Stan 192-1995)',
                                                                                   'Food Additive Provisions of the Gsfa (proposed Draft and Draft)',
                                                                                   'Proposed Draft Amendments to the Codex General Standard for Food Additives: Annex to Table 3 (food Categories Or Individual Food Items Excluded From the General Conditions of Table 3)',
                                                                                   'Food Additive Provisions of the Gsfa (draft and Proposed Draft)'))] = "Proposed Draft Food Additive Provisions of the General Standard for Food Additives (GSFA)"

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Maximum Residue Limits',
                                                                                    'Proposed Draft Maximum Residue Limits (mrls)',
                                                                                    "Proposed Draft Maximum Residue Limits for Pesticides",
                                                                                    'Proposed Draft Maximum Limits for Pesticide Residues',
                                                                                    'Proposed Draft Maximum Residue Limits for Pesticides (mrls)',
                                                                                    'Maximum Residue Limits for Pesticides (draft and Proposed Draft)'))] = "Proposed Draft Maximum Residue Limits for Pesticides (MRLs)"

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Codex Standard for Tiquisque (white and Lilac)',
                                                                                    'Proposed Draft Standards for Tiquisque (white and Lilac)') )]  ='Proposed Draft Codex Standard for Tiquisque (White and Lilac)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Standard for Quick Frozen Scallop Adductor Muscle Meat') )] = 'Proposed Draft Standard for Quick Frozen Scallop Adductor Meat'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Guidelines for the Utilization of Vegetable Proteins in Foods' ,
                                                                                    'Proposed Draft General Guidelines for the Utilization of Vegetable Proteins in Foods') )] =    'Proposed Draft General Guidelines for the Utilization of Vegetable Proteins in Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Residue Limits for Veterinary Drugs', 
                                                                                   'Proposed Draft Maximum Residues Limits (mrls) for Veterinary Drugs',
                                                                                   'Proposed Draft Maximum Residue Limits and Proposed Draft Revised Maximum Residue Limits for Veterinary Drugs',
                                                                                   'Proposed Draft Maximum Residue Limits for Veterinary Drugs in Foods') )]  =  'Proposed Draft Maximum Residues Limits (MRLS) for Veterinary Drugs'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standards for Canned Mangoes') ) ] = 'Proposed Draft Standards for Canned Mango Products' 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Revised Code of Ethics for International Trade in Foods') ) ] = 'Proposed Draft Code of Ethics for International Trade in Food' 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Regional Standard for Non-fermented Soybean Products' ) )] ='Proposed Draft Standard for Non-fermented Soybean Products'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Previous Cargoes') )]  = 'Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Cargoes'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Provisional Standard for Dried Edible Fungi') )] =   'Proposed Draft Provisional Standard for Edible Fungi and Fungus Products'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft General Standard for Vegetable Juices and Vegetable Nectars') ) ]  =   'Proposed Draft General Standard for Vegetable Juices'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Codex Standard for Durum Wheat') )]  =   'Proposed Draft Codex Standards for Wheat and Durum Wheat'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Appendices to the Guidelines on the Judgement of Equivalence of Sanitary Measures Associated with Food Inspection and Certification') )]  =   'Proposed Draft Guidelines on the Judgement of Equivalence of Sanitary Measures Associated with Food Inspection and Certification Systems'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revisions to the Codex International Numbering System for Food Additives') )] =  'Proposed Draft Revision of the Codex Class Names and the International Numbering System for Food Additives'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft List of Acceptable Previous Cargoes') ) ] = 'Proposed Draft List and Draft Lists of Acceptable Previous Cargoes'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Principles for the Application of Traceability/product Tracing in the Context of Food Import and Export Inspection and Certification Systems') )] =        'Proposed Draft Principles for Traceability/product Tracing As A Tool Within A Food Import and Export Inspection and Certification System'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Recommended International Code of Practice for the Handling and Processing of Quick Frozen Foods',
                                                                                   'Proposed Draft Code of Practice for the Processing and Handling of Quick-frozen Foods',
                                                                                   'Proposed Draft Revised Code of Practice for the Processing and Handling of Quick Frozen Foods',
                                                                                   'Proposed Draft Standards for Quick Frozen Foods') ) ] = 'Proposed Draft Recommended International Code of Practice for the Processing and Handling of Quick Frozen Foods'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Previous Cargoes',
                                                                                   'Proposed Draft Code of Practice for the Storage and Transport of Edible Oils and Fats in Bulk') ) ]  =   'Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Cargoes'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Hygienic Practice for the Inspection of Game') )]  = 'Proposed Draft Code of Hygienic Practice for Game'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Levels for Aflatoxins in Almonds, Hazelnuts and Pistachios') )]  =  'Proposed Draft Maximum Level for Total Aflatoxins in Unprocessed Almonds, Hazelnuts and Pistachios'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Practice for the Prevention and Reduction of Tin Contamination in Foods') )] = 'Proposed Draft Code of Practice for the Prevention and Reduction of Inorganic Tin Contamination in Canned Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Levels for Cadmium in Various Commodities') )] =   'Proposed Draft Maximum Levels for Cadmium' 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Model Certificate for Fish and Fishery Products') )] = 'Proposed Draft Model Certificate for Fish and Fishery Products (other Certificates)' 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft St Andard for Chocolate' ) )]  ='Proposed Draft Standard for Chocolate'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft St Andard for Fructose' ) )]  ='Proposed Draft Standard for Fructose'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Code of Hygienic Practice for Foods for Inf Ants and Children' ) )]  ='Proposed Draft Code of Hygienic Practice for Foods for Infants and Children'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standards for Processed Foods for Infants and Children Based on Cereals',
                                                                                   'Proposed Draft Amendments to the Codex Standard for Processed Cereal Based Foods for Infants and Children',
                                                                                   'Proposed Draft Revised Standard for Cereal-based Foods for Infants and Young Children',
                                                                                   'Proposed Draft Revised Standard for Processed Cerealbased Foods for Infants and Young Children' ) ) ]  =  'Proposed Draft Standard for Processed Cerealbased Foods for Infants and Young Children' 

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standards for the Labelling of and Claims for Pre-packaged Foods Claimed to Be Suitable for Incorporation in A Dietary Regimen for Diabetics (returned to Step 3), Low Energy and Reduced Energy Foods (returned to Step 3) and Draft Guidelines for [medical] Foods.',
                                                                                   'Proposed Draft Standard for the Labelling of and Claims for Prepackaged Foods Claimed to Be Suitable for Incorporation Into A Prescribed Dietary Regimen for Diabetes',
                                                                                   'Proposed Draft Standard for the Labelling of and Claims for Prepackaged Foods Claimed to Be Suitable for Incorporation Into A Prescribed Dietary Regimen for Diabetes,') )]  = 'Proposed Draft Standard for the Labelling of and Claims For, Pre-packaged Foods Claimed to Be Suitable for Diabetics'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Hygienic Practice for the Collecting, Processing and Marketing of Natural Mineral Waters') )]  = 'Proposed Draft Revision of the Recommended International Code of Hygienic Practice for Collecting, Processing and Marketing of Natural Mineral Waters (cac/rcp 33-1985)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Cocoa (cacao) Beans Cocoa (cacao) Nib, Cocoa Cacao Mass Cocoa Press Cake and Cocoa Dust Cocoa Fines for Use in the Manufacture of Cocoa An Chocolate Products') )]  =   'Proposed Draft Revised Standard for Cocoa (cacao) Mass (cocoa/chocolate Liquor) and Cocoa Cake, for the Use in the Manufacture of Cocoa and Chocolate Products'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Guidelines for the Use of Health and Nutrition Claims in Food Product Labelling',
                                                                                         'Proposed Draft Guidelines on Nutrition and Health Claims for Food Labelling.') )]  =  'Proposed Draft Guidelines on Nutrition and Health Claims for Food Labelling'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Bottled/packaged Waters Other Than Natural Mineral Water' ))   ]  =  'Proposed Draft General Standard for Bottled/packaged Drinking Waters (other Than Natural Mineral Waters)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendment to the Codex General Standard for Cheese (description)',
                                                                                         'Proposed Draft Amendment to the Codex General Standard for Cheese: Appendix') )]  = 'Proposed Draft Amendment to the Codex General Standard for Cheese'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Annex to the Codex Guideline for the Conduct of Food Safety Assessment of Foods Derived From Recombinant-dna Plants (cac/gl 45-2003)') )]  = 'Proposed Draft Guideline for the Conduct of Food Safety Assessment of Foods Derived From Recombinant-dna Plants'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Codex Advisory Specifications for the Identity and Purity of Food Additives' ) )]  =     'Proposed Draft Specifications for the Identity and Purity of Food Additives (category I)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Minimum Brix Level for Reconstituted Juice and Reconstituted Purée and Minimum Juice And/or Purée Content for Fruit Nectars (% V/v)' ) )]  =         'Proposed Draft Minimum Brix Level for Reconstituted Juice and Reconstituted Purée and Minimum Juice And/or Purée Content for Fruit Nectars (%v/v) - Orange, Lemon, Lime and Pineapple Juices/nectars'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Amendment to the Guidelines for the Production, Processing, Marketing and Labelling of Organically Produced Foods - Annex 2 (permitted Substances for the Production of Organic Foods)',
                                                                                   'Proposed Draft Amendment to the Guidelines for Organically Produced Foods (Ethylene)',
                                                                                   'Proposed Draft Amendment to the Guidelines for the Production, Processing, Labelling and Marketing of Organically Produced Foods: Annex 2 – Permitted Substances : Table 1 (natural Sodium Nitrate)',
                                                                                   'Proposed Draft Amendment to the Guidelines for the Production, Processing, Labelling and Marketing of Organically Produced Foods (table 1:substances Used in Soil Fertilizing and Conditioning)',
                                                                                   'Proposed/draft Amendment to the Guidelines for the Production, Processing, Labelling and Marketing of Organically Produced Foods (cac/gl 32-1999): Use of Ethylene for Ripening of Fruit (step 8) and Inclusion of New Substances (step 5a)'))  ]  = 'Proposed Draft Amendment to the Guidelines for the Production, Processing, Labelling and Marketing of Organically Produced Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revised Guideline Levels for Radionuclides in Foods Contaminated Following A Nuclear Or Radiological Emergency for Use in International Trade16')) ]  ='Proposed Draft Guideline Levels for Radionuclides in Food for Use in International Trade'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Appendix to the Code of Practice for the Prevention and Reduction of Aflatoxin Contamination in Tree Nuts to Address Additional Measures for the Prevention and Reduction of Aflatoxins in Brazil Nuts'))  ]  ='Proposed Draft Code of Practice for the Prevention and Reduction of Aflatoxin Contamination in Tree Nuts'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revision of the Code of Practice to Minimize and Contain Antimicrobial Resistance (cac/rcp 61-2005)')) ]  ='Proposed Draft Code of Practice to Minimize and Contain Antimicrobial Resistance'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean_clean %in% c('Proposed Draft Principles and Guidelines for National Food Control System') ) ]  ='Proposed Draft Principles and Guidelines for National Food Control Systems (introduction, Sections 1-3)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean_clean %in% c('Proposed Draft and Draft Mrls for Different Combinations of Pesticide/commodity(ies) That Were Withdrawn',
                                                                                         'Proposed Draft and Draft Mrls for Different Combinations of Pesticide/commodity(ies) That Were Withdrawn (discontinued) By Ccpr50') )]  ='Proposed Draft and Draft Mrls for Different Combinations of Pesticide/commodity(ies) That Were Withdrawn'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Definition of Trans-fatty Acids (amendment to the General Standard for the Labelling of Prepackaged Foods and the The Guidelines on Nutrition Labelling) - Accelerated Procedure',
                                                                                   'Proposed Draft Amendment to the General Standard for the Labelling of Prepackaged Foods (quantitative Declaration of Ingredients)')) ]  ='Proposed Draft Amendment to the General Standard for the Labelling of Prepackaged Foods (quantitative Declaration of Ingredients)'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Guidelines for the Judgement of Equivalence of Sanitary Measures Associated with Food Inspection and Certification Systems') )] =  'Proposed Draft Guidelines on the Judgement of Equivalence of Sanitary Measures Associated with Food Inspection and Certification Systems'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft St Andard for Cocoa Butters'))  ]  = 'Proposed Draft Revised Standard for Cocoa Butters'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Practice - General Principles of Food Hygiene'))]= 'Proposed Draft (revised) International Code of Practice: General Principles of Food Hygiene'  

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standards for Fruit Juices and Nectars',
                                                                              'Proposed Draft Codex General Standard for Fruit Juices and Nectars') )] =    'Proposed Draft Codex General Standard for Fruit Juices and Nectars'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft European Regional Standard for Mayonnaise') ) ]  ='Proposed Draft Standard for Mayonnaise'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Maximum Residue Limits (MRLs) for Pesticides Proposed Draft Revised Maximum Residue Limits (MRLs) for Pesticides Proposed Draft Revised Extraneous Maximum Residue Limit (EMRL)') )] = 'Proposed Draft Maximum Residue Limits and Extraneous Maximum Residue Limits for Pesticides'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Labelling of and Claims for Low-energy and Reduced-energy Foods',
                                                         'Proposed Draft Standard for the Labelling of and Claims for "low Energy" and "reduced Energy" Foods') )]  = 'Proposed Draft Standard for Labelling of and Claims for Low-energy and Reduced-energy Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Standard for the Labelling of and Claims for "low Energy" and "reduced Energy" Foods') ) ]  ='Proposed Draft Standard for Labelling of and Claims for Low-energy and Reduced-energy Foods'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Amendments to the Recommended International Code of Practice for Salted Fish') ) ]  ='Proposed Draft Code for Salted Fish'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Sampling Plans for Aflatoxins in Almonds, Brazil Nuts, Hazelnuts and Pistachios',
                                                                              'Proposed Draft Maximum Level for Total Aflatoxins in Almonds, Hazelnuts and Pistachios for “ready to Eat”') ) ]  ='Proposed Draft Maximum Level for Total Aflatoxins in Unprocessed Almonds, Hazelnuts and Pistachios'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Standard for Hot Pepper Fermented Soybean Paste (gochujang)') ) ]  ='Proposed Draft Standard for Gochujang'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Provisional Standard for Fresh Fungus Chanterelle',
                                                                                    'Proposed Draft Revised Regional Standard for Chanterelles') ) ]  ='Proposed Draft Standard for Chanterelles'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Generic Model Official Certicate (annex to the Guidelines for Design, Production, Issuance and Use of Generic Official Certificates (cac/gl 38-2001))') ) ]  ='Proposed Draft Revision to the Guidelines for Generic Official Certificate Formats and Design, Production, Issuance and Use of Certificates'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft and Draft Mrls for Different Combinations of Pesticide/commodity(ies) That Were Withdrawn (discontinued) By Ccpr50'))] = 'Proposed Draft and Draft Mrls for Different Combinations of Pesticide/commodity(ies) That Were Withdrawn'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revised Standard for Infant Formula [and Formulas for Special Medical Purposes Intended for Infants]',
                                                         'Proposed Draft Revised Standard for Infant Formula and Formulas for Special Medical Purposes Intended for Infants (section B)'))] = 'Proposed Draft Revised Standard for Infant Formula [and Formulas for Special Medical Purposes Intended for Infants'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Guidelines for Risk-based Inspection of Imported Foods'))] ='Proposed Draft Principles and Guidelines for Imported Food Inspection Based on Risk'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Standard for Fermented Soybean Paste (doenjang)'))] = 'Proposed Draft Regional Standard for Fermented Soybean Paste'

proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Specifications for the Identity and Purity of Food Additives (category I)'))] = 'Proposed Draft Codex Advisory Specifications for the Identity and Purity of Food Additives'


proposed$standards_extracted_clean[which(proposed$standards_extracted_clean %in% c('Proposed Draft Minimum Brix Level for Reconstituted Juice and Reconstituted Purée and Minimum Juice And/or Purée Content for Fruit Nectars (%v/v) - Orange, Lemon, Lime and Pineapple Juices/nectars'))] = 'Proposed Draft Minimum Brix Level for Reconstituted Juice and Reconstituted Purée and Minimum Juice And/or Purée Content for Fruit Nectars (% V/v)'


# ----------------- Discrepancies that Cindy resolved but should ask Seb
# looked in CAC 24 and looks like entry for "Proposed Draft Guideline for the Conduct of Food Safety Assessment of Recombinant-dna Microorganisms' is not mentioned there, so removed it
proposed = proposed[-which(proposed$standards_extracted_clean %in% c( 'Proposed Draft Guideline for the Conduct of Food Safety Assessment of Recombinant-dna Microorganisms') ),]

# looks like 'Proposed Draft Code of Practice for Refrigerated Packaged Foods with Extended Shelf- Life' is a part of the 'Code of Hygienic Practice for Refrigerated Packaged Foods with Extended Shelf Life' which was passed in 1999
proposed[which(proposed$standards_extracted_clean %in% c('Proposed Draft Code of Practice for Refrigerated Packaged Foods with Extended Shelf- Life' ) ), c('standards_extracted_clean', 'Title', 'Reference')]  = c('Proposed Draft Code of Hygienic Practice for Refrigerated Packaged Foods with Extended Shelf-life', 'Code of Hygienic Practice for Refrigerated Packaged Foods with Extended Shelf Life', 'CAC/RCP 46-1999')

# looks like 'Proposed Draft List of Class Titles for Food Additives' is a part of the 'Class Names and the International Numbering System for Food Additives' which was last revised in 2018
proposed[which(proposed$standards_extracted %in% c('Proposed Draft List of Class Titles for Food Additives') ), c('Title', 'Reference')] = c('Class Names and the International Numbering System for Food Additives', 'CAC/GL 36-1989')
proposed[which(proposed$standards_extracted %in% c('Revised Proposed Draft List of Class Titles for Food Additives') ), 'standards_extracted_clean'] = 'Class Names and the International Numbering System for Food Additives'
 
# looks like 'Proposed Draft Revised Standard for Gluten-Free Foods' is linked to the 'Standard for Foods for Special Dietary Use for Persons Intolerant to Gluten' and 'incorrectly' duplicated in 22, so remove the incorrect duplicate
proposed = proposed[-which(proposed$standards_extracted %in% c( 'Proposed Draft Revised Standard for Gluten-Free Foods') ),]  

# according to CAC 19, Proposed Draft Codex Codes of Practice for the Packaging. Transport, Control and Inspection of Tropical Fresh Fruits and Vegetables 
# was subsequently split into one code which dealt with packaging and transport and another which deals with control and inspection
# it seems to me that it would be sensible to link Proposed Draft Codex Codes of Practice for the Packaging. Transport, Control and Inspection of Tropical Fresh Fruits and Vegetables
# to both standards (actually doesn't look like we have data on control and insepction standard) instead of considering it a failed proposal
proposed[proposed$standards_extracted_clean == 'Proposed Draft Codex Codes of Practice for the Packaging. Transport, Control and Inspection of Tropical Fresh Fruits and Vegetables', c('Title', 'Reference')] = c('Code of Practice for the Packaging and Transport of Fresh Fruit and Vegetables', 'CAC/RCP 44-1995')
 
# from CAC 24, looks like 'Proposed Draft List of Acceptable Previous Cargoes and Banned Immediate Previous Cargoes for inclusion in the Code of Practice for the Storage and Transport of Fats and Oils in Bulk' 
# is associated with the codex code : 'Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk'
proposed[which(proposed$standards_extracted_clean %in% c('Proposed Draft List of Acceptable Previous Cargoes and Banned Immediate Previous Cargoes for Inclusion in the Code of Practice for the Storage and Transport of Fats and Oils in Bulk') ), c('standards_extracted_clean', 'Title', 'Reference')]   = c('Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Cargoes', 'Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk', 'CAC/RCP 36-1987')

# from CAC 28, looks like 'Proposed Draft Revision of the Codex Guidelines for Generic Official Certificate Formats and the Production and Issuance of Certificates'
# is associated with the codex 'Guidelines for Design, Production, Issuance and Use of Generic Official Certificates' 
proposed[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revision of the Codex Guidelines for Generic Official Certificate Formats and the Production and Issuance of Certificates') ), c('standards_extracted_clean', 'Title', 'Reference')]  = c('Proposed Draft Revision to the Guidelines for Generic Official Certificate Formats and Design, Production, Issuance and Use of Certificates', 'Guidelines for Design, Production, Issuance and Use of Generic Official Certificates', 'CAC/GL 38-2001')

# from CAC 14 and CAC15 looks like Proposed Draft Standard for Pulpy Mango Nectar preserved exclusively by physical means
# can be linked to Proposed Draft standards for Pulpy Mango Nectar
proposed$standards_extracted_clean[which(proposed$standards_extracted_clean == 'Proposed Draft Standard for Pulpy Mango Nectar Preserved Exclusively By Physical Means')] = 'Proposed Draft standards for Pulpy Mango Nectar'

# from CAC 15, draft for mango juice indeed looks like its discontinued, have chosen to make the standard_extracted_clean variable 'Discontinuation of work...' to note that explictely
proposed$standards_extracted_clean[which(proposed$standards_extracted_clean == 'Proposed Draft standards for Mango Juice')] = 'Discontinuation of Work on Proposed Draft Standard for Mango Juice'


# this extracted standard refers to CODEX STAN 292-2008 in its name so adjusted 'Title' and 'Reference' variables accordingly
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Performance Criteria for Reference and Confirmatory Methods for Marine Biotoxins (section I-8.6 Determination of Marine Biotoxins) in the Standard for Live and Raw Bivalve Molluscs (codex Stan 292-2008)'),
    c('standards_extracted_clean', 'Title', 'Reference')] = c('Proposed Draft Standard for Live and Raw Bivalve Molluscs','Standard for Live and Raw Bivalve Molluscs',  'CODEX STAN 292-2008') 

# from CAC 17, looks like draft standard for dried shark fins and quick frozen squid are separate
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Standards for Quick Frozen Squid and Dried Shark Fins'),c('standards_extracted_clean', 'Title', 'Reference')] = c('Proposed Draft Standard for Quick Frozen Squid','Standard for Quick Frozen Raw Squid',  'CODEX STAN 191-1995') 

proposed = rbind(proposed, data.frame(meetingCount_cac = 18, meeting = 'CAC18', standards_extracted = 'Proposed Draft Standards for Dried Shark Fins', Reference = 'CODEX STAN 189-1993', Title = 'Standard for Dried Shark Fin', year = 1989, standards_extracted_clean = 'Proposed Draft Standard for Shark Fins'))

# from CAC 14, looks like draft standard 'Proposed Draft Code of Practice for the Handling of Quick Frozen Foods During Transport' is connected to code 'Code of Practice for the Processing and Handling of Quick Frozen Foods'

proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Code of Practice for the Handling of Quick Frozen Foods During Transport'), c('standards_extracted_clean', 'Title', 'Reference') ] = c('Proposed Draft Recommended International Code of Practice for the Processing and Handling of Quick Frozen Foods', 'Code of Practice for the Processing and Handling of Quick Frozen Foods', 'CAC/RCP 8-1976')

# from CAC 36, looks like Proposed Draft Maximum Level for Deoxynevalenol (don) in Cereal-based Foods for Infants and Young Children linked to Standard for Processed Cereal-Based Foods for Infants and Young Children
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Maximum Level for Deoxynevalenol (don) in Cereal-based Foods for Infants and Young Children'), c('standards_extracted_clean', 'Title', 'Reference') ] = c('Proposed Draft Standard for Processed Cerealbased Foods for Infants and Young Childrens', 'Standard for Processed Cereal-Based Foods for Infants and Young Children', 'CODEX STAN 74-1981')

# needed change evident from standard_extracted title
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Microbiological Criteria for Powdered Follow-up Formulae and Formulae for Special Medical Purposes for Young Children (Annex II to the Code of Hygienic Practice for Powdered Formulae for Infants and Young Children (CAC/RCP 66-2008))'), c('standards_extracted_clean', 'Title', 'Reference')] = c('Proposed Draft Code of Hygienic Practice for Powdered Formulae for Infants and Young Children', 'Code of Hygienic Practice for Powdered Formulae for Infants and Young Children', 'CAC/RCP 66-2008') 

# not 100 certain, but from CAC 19 and CAC36 Proposed Draft Nutrient Reference Values for Food Labelling Purposes seems to be related to Guidelines on Nutrition Labelling
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Nutrient Reference Values for Food Labelling Purposes seems to be related to Guidelines on Nutrition Labelling'), c('standards_extracted_clean', 'Title', 'Reference') ] = c('Proposed Draft Additional or Revised Nutrient Reference Values for Labelling Purposes in the Codex Guidelines on Nutrition Labelling', 'Guidelines on Nutrition Labelling', 'CAC/GL 2-1985')

# no clear link from CAC 26 or CAC 30 but seems sensible
proposed[which(proposed$standards_extracted_clean == 'Proposed Draft Standard for Live and Processed Bivalve Molluscs'), c('standards_extracted_clean', 'Title', 'Reference') ] = c('Proposed Draft Standard for Live and Raw Bivalve Molluscs', 'Standard for Live and Raw Bivalve Molluscs', 'CODEX STAN 292-2008')

# Note that from CAC 36,  'Proposed Draft Revision of the Procedure for the Inclusion of Additional Species in Standards for Fish and Fishery Products' belongs in the Procedurual Manual, Section II: Elaboration of Codex Standards and Related Texts: Guidelines for the Inclusion of Specific Provisions in Codex Standards and Related Texts



# 'Proposed Draft Standard for Raw Sugar' ; 'Proposed Draft Standard for Raw Cane Sugar'
# 'Proposed Draft Code of Hygienic Practice for Fresh Meat', 'Proposed Draft Code of Hygienic Practice for Meat'
# 'Proposed Draft Codex Standard for Milled Rice', 'Proposed Draft Codex Standard for Rice'
# 'Proposed Draft Standard for Low Fat Spreads', 'Proposed Draft Standard for Low-fat Dairy Spreads';Proposed Draft Standard for Fat Spreads
# 'Proposed Draft Guidelines for Testing Safety and Nutritional Quality of Vegetable Protein Products','Proposed Draft Guidelines for the Utilization of Vegetable Proteins in Foods' ,
# Proposed Draft Code of Hygienic Practice for Spices and Herbs ; Proposed Draft Code of Hygienic Practice for Spices and Condiments
# 'Proposed Draft Standard for Live and Processed Bivalve Molluscs', 'Proposed Draft Standard for Live and Raw Bivalve Molluscs'
# proposed[which(proposed$standards_extracted %in% c('Proposed Draft Definition of Trans-fatty Acids (amendment to the General Standard for the Labelling of Prepackaged Foods and the The Guidelines on Nutrition Labelling) - Accelerated Procedure','Proposed Draft Definition of Trans-fatty Acids (amendment to the Guidelines on Nutrition Labelling)') ),]  

# !!  'Proposed Draft Guideline for the Conduct of Food Safety Assessment of Recombinant-dna Microorganisms in Food', 'Proposed Draft Guideline for the Conduct of Food Safety Assessment of Recombinant-dna Microorganisms'
# !! 'Proposed Draft Code of Hygienic Practice for Refrigerated Packaged Foods with Extended Shelf-life', 'Proposed Draft Code of Practice for Refrigerated Packaged Foods with Extended Shelf- Life'
# !! proposed[which(proposed$standards_extracted %in% c('Revised Proposed Draft List of Class Titles for Food Additives', 'Proposed Draft List of Class Titles for Food Additives') ),]
# !! proposed[which(proposed$standards_extracted %in% c('Proposed Draft Standard for Gluten-Free Foods', 'Proposed Draft Revised Standard for Gluten-Free Foods') ),]  
# !! proposed[which(proposed$standards_extracted %in% c('Proposed Draft Codex Codes of Practice for the Packaging. Transport, Control and Inspection of Tropical Fresh Fruits and Vegetables', 'Proposed Draft Code of Practice for the Packaging and Transport of Tropical Fresh Fruits and Vegetables'))  ,      ]  
# !! proposed[which(proposed$standards_extracted_clean %in% c('Code of Practice for the Storage and Transport of Edible Fats and Oils in Bulk: Draft and Proposed Draft Lists of Acceptable Cargoes','Proposed Draft List of Acceptable Previous Cargoes and Banned Immediate Previous Cargoes for Inclusion in the Code of Practice for the Storage and Transport of Fats and Oils in Bulk') ),]  
# !! proposed[which(proposed$standards_extracted %in% c('Proposed Draft Amendments to the Code of Practice for the Transport of Edible Fats and Oils in Bulk (list of Acceptable Previous Cargoes and List of Immediate Previous Banned Cargoes)', 'Proposed Draft List of Acceptable Previous Cargoes and Banned Immediate Previous Cargoes for Inclusion in the Code of Practice for the Storage and Transport of Fats and Oils in Bulk'))  ,      ]  
# !! proposed[which(proposed$standards_extracted_clean %in% c('Proposed Draft Revision of the Codex Guidelines for Generic Official Certificate Formats and the Production and Issuance of Certificates','Proposed Draft Revision to the Guidelines for Generic Official Certificate Formats and Design, Production, Issuance and Use of Certificates') ),]  


#  "Discontinuation of work on draft and proposed draft Food Additive Provisions of the GSFA"
# "Discontinuation of Work on Proposed Draft Standard for Mango Juice" 


# remove duplicates
proposed = proposed[-which(duplicated(proposed)),]



# attach information on when standards were adopted, amended or revised, in each cac
std = std[order(std$year, std$Reference),]
std$meetingCount_cac = dates_cac$meetingCount_cac[match( std$year_cac, dates_cac$year)]
std[which(is.na(std$meetingCount_cac)),'year_cac'] = std[which(is.na(std$meetingCount_cac)),'year_cac']-1
std$meetingCount_cac = dates_cac$meetingCount_cac[match( std$year_cac, dates_cac$year)] %>% as.numeric()
std$meeting = paste0('CAC', std$meetingCount_cac)
 
proposed = merge(proposed, std[, c('year', 'Reference', 'adopted', 'revised', 'amended', 'Title', 'meetingCount_cac', 'meeting')], by = c('year', 'Reference', 'Title', 'meetingCount_cac', 'meeting'), all = TRUE)
proposed = proposed[order(proposed$Reference, proposed$year),]

 
proposedAgg =  do.call(rbind, lapply(split(proposed , proposed$Reference)[-c(1)], function(x){
  if (all(is.na(x$adopted)) == FALSE){
    slice = x[1:which(x$adopted ==1), -which(names(x) %in% c('revised', 'amended'))]}
  else  {
    slice = x[, -which(names(x) %in% c('revised', 'amended'))]}
  slice$startYear = slice$year[1]
  slice$endYear = slice$year[length(slice$year)] 
  slice$startCac = slice$meetingCount_cac[1]
  slice$endCac = slice$meetingCount_cac[length(slice$meetingCount_cac)] 
  slice$yearsAfterProposal = slice$endYear - slice$startYear
  slice$cacAfterProposal = slice$endCac - slice$startCac
  return(slice[dim(slice)[1], -1])                                
}))


 
unadoptedProposals = do.call(rbind,split(proposed , proposed$Reference)[c(1)])
dim(unadoptedProposals)
dim(proposedAgg)



intersect(proposed[which(proposed$Reference == 0), 'standards_extracted'] %>% unique(), proposed[which(proposed$Reference != 0), 'standards_extracted'] %>% unique())
head(proposed[which(proposed$Reference == 0), 'standards_extracted']%>% unique() )
head( proposed[which(proposed$Reference != 0), 'standards_extracted'] %>% unique())

head(proposed)
head(proposed, 20)
proposed[which(proposed$Reference != 0), 'standards_extracted'] %>% unique() %>%  head()
proposed[which(proposed$Reference == 0), 'standards_extracted'] %>% unique() %>% head()

proposed[grep('Meat', proposed$standards_extracted),]
install.packages('tidystringdist')
library(tidystringdist)



p
summary(proposedNearDupeDist)
test = proposedNearDupeDist[proposedNearDupeDist$V1 == 'Proposed Draft Codification of Carcases of the Species Ovis', ] 
head(test)
unique(proposed$standards_extracted)[-which(unique(proposed$standards_extracted)=='0')]


head(proposedNearDupeDist)
head(proposedNearDupe )
library(RecordLinkage)
compare.dedup(proposed[,'standards_extracted'] %>% data.frame())
?compare.dedup
proposed[grep('Proposed Draft Maximum Residue Limits for Pesticides', proposed$standards_extracted),]
# Ask Seb: CAC 31 Report, list of adopted standards.... #Practice for Fish and Fishery Products
# std[grep('Practice for Fish and Fishery Products', std$Title),]
# proposed[grep('Standard for Chilli Peppers', proposed$Title),]

proposed[grep('Pepper', proposed$Title),]

std[grep('Standard for Chilli Peppers', std$Title),]

unique(std$Title)
length(unique(std$Reference))
setdiff( unique(proposed$Reference), unique(std$Reference))

unique(std$Reference)
length(unique(proposed$Reference))
dim(proposed)
head(proposed, 20)
#boo[which(is.na(boo$adopted) & is.na(boo$revised) & is.na(boo$amended)),]

head(boo)
boo = merge(proposed, codex[-which(is.na(codex$adopted) & is.na(codex$revised) & is.na(codex$amended)), c('year', 'Reference', 'adopted', 'revised', 'amended')], by = c('year', 'Reference'), all = TRUE)

 

test = codex[-which(is.na(codex$adopted) & is.na(codex$revised) & is.na(codex$amended)),]
dim(test)
head(test[, c(1:5, 50:53, 58:62)])
head(boo, 20)
dim(std)
boo2  = boo[-which(is.na(boo$adopted) & is.na(boo$revised) & is.na(boo$amended)),]

dim(boo2)
head(boo2)

dim(proposed)
head(boo)
dim(boo)

head(codex[, c(1:5, 50:53, 58:62)])
dates_cac[order(dates_cac$year),]
 


head(boo)
dim(boo)
dim(proposed)
dim(boo)



unique(proposed$standards_extracted) %>% sort()

head(proposed)
head(dates)

proposed = proposed[order(proposed$Reference, proposed$meetingCount_cac),]
head((proposed))
unique(proposed$Title)

foo2 = proposed[which(proposed$Reference ==0),]
unique(foo2$standards_extracted) %>% sort()
foo2$standards_extracted[which(duplicated(foo2$standards_extracted))]
dim(foo2)
foo = split(proposed[,c('meetingCount_cac', 'standards_extracted', 'Reference')],proposed$Reference)

foo[which(unlist(lapply(foo, function(x) length(unique(x$standards_extracted))>1))*1==1)]

poo = split(proposed[,c('meetingCount_cac', 'standards_extracted', 'Reference')],proposed$standards_extracted)

poo[which(unlist(lapply(poo, function(x) dim(x)[1]>1))*1==1)]

poo[c(2)]
dim(proposed)
length(poo)
foo[c(1:10)]
length(foo)

# Ask Seb: titles to resolve
# setdiff(unique(proposed$Title), unique(std$Title))

# need to first match based on proposal stage at the committee level
# adoption stage is then matched at the CAC level
# the complicating factor is that the proposal data is only available at the CAC level
# so need to create a concordance between CAC and specific committees where possible


# note also that the same standard is proposed and drafted multple times as it is proposed
# and then subsequently revised
# so the more appropriate model would be one in which 


head(proposed, 20)

proposed$event_short = std$event_short[match(proposed$Title, std$Title)]

head(std)
setdiff(unique(proposed$Title), unique(std$Title))

head(proposed)



head(proposed)
dim(proposed)

head(std)
head(proposed)

proposed[which(proposed$Title == ''),]

setdiff(unique(std$Title), unique(proposed$Title) ) %>% length()


setdiff(unique(proposed$Reference), unique(std$Reference))
setdiff(unique(std$Reference), unique(proposed$Reference)) %>% length()


std$Title[grep('esticide', std$Title)] %>% unique()



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

save(codex, file = paste0(pathData, '/codexData.rda'))



 