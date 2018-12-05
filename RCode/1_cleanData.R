if(Sys.info()['user'] == 'cindycheng'){
	source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R)'
}

 
# Codex Alimentarius in July
# meetings always adopted in CAC

# fix month variable

# -------------------------
# Standards Data
# --------------------------
# load std data
std = read.dta13(paste0(pathData, '/participation_development/codex_standard_development.dta'))
std = std[order(std$event_short, std$year),]

# note have to change some commmittee names to be consistent as some were renamed and reestablished
std$event_short_raw = std$event_short

# Fixed/Renamed: CCCF and CCFA are now sepaate committees but prior to 2007 were effectively combined into one committee,  CCFAC
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
std$id = paste0(std$RefID, std$year)


# -------------------------
# Participant Data
# --------------------------
# load and clean particip data
particip = read.dta13(paste0(pathData, '/participation_development/codex_participation.dta'))


# create variable of number of delegations
particip$delegations = ifelse(particip$delegates !=0, 1, 0)


# create variable that meausures the square of number of delegates weighted by total number of delegates for that committee (delegateWt2) in order to later create herfindahl index of delegations (delegConc)
delegTotal = particip %>% group_by(year, event_short) %>% dplyr:::summarise(delegates = sum(delegates, na.rm = TRUE)) %>% data.frame()
particip$delegatesTotal = delegTotal$delegates[match(paste0(particip$event_short, particip$year), paste0(delegTotal$event_short, delegTotal$year))]
particip$delegateWt2 = c(particip$delegates/particip$delegatesTotal)^2


# match participant type
participType = read.csv(paste0(pathData, '/participation_development/list_IGOs_NGOs_states.csv'), stringsAsFactors = FALSE)
particip[which(particip$actor_name == 'IAFFM'), 'actor_name'] = 'Marine Ingredients Organisation'

particip$headquarters = participType$headquarters[match(particip$actor_name, participType$name)] 
particip$region_group = participType$region_group[match(particip$actor_name, participType$name)] 
particip$income_group = participType$income_group[match(particip$actor_name, participType$name)] 
particip$interest_group = participType$interest_group[match(particip$actor_name, participType$name)] 


# create measure of number of delegates per region, incom and interest grouping
particip = dummy_cols(particip, select_columns = c('region_group', 'income_group', 'interest_group'))
 
disaggDelegates =   particip$delegates * as.matrix(particip[, 16:39])  
particip[, 16:39] = disaggDelegates
names(particip)[16:39] = paste0('delegates_', names(particip)[16:39])
particip = particip[, -grep('n/a', names(particip))]
particip = particip[, -which(names(particip) == 'delegates_interest_group_')]


# ask seb: looks like a lot of IGOs are not classified for the interest group measure
particip[which(particip$interest_group == ''), 'actor_type'] %>% table()
particip[which(particip$interest_group == '' & particip$actor_type == 'IGO'), 'actor_name'] %>% table()
particip[which(particip$interest_group == 'IGO' & particip$actor_type == 'IGO'), 'actor_name'] %>% table()


# aggregate data
participAgg = particip %>% 
				group_by(year, event_short) %>% 
				summarise_at( vars(matches('deleg')), sum, na.rm = TRUE) %>% data.frame()
names(participAgg)[which(names(participAgg) == 'delegateWt2')] = 'delegConc'				 
participAgg = participAgg[, - which(names(participAgg) == 'delegatesTotal')]
names(participAgg) = gsub('\\.', '', names(participAgg))

 
# assume that if there are no delegates, the committee didn't/doesn't exist
participAgg =participAgg[-which(participAgg$delegates == 0),]  
 
# check assumption
split(participAgg$year, participAgg$event_short)

participAgg = participAgg[order(participAgg$event_short, participAgg$year),]
participAgg$meetingCount = lapply(split(participAgg$year, participAgg$event_short), function(x){
	1:length(x)
}) %>% unlist()

# check
split(participAgg[, c('year', 'meetingCount')], participAgg$event_short)

# ---------------------------------------
# Merge Data Based on CAC Committee
# ---------------------------------------

cacAgg = participAgg[which(participAgg$event_short == 'CAC'),]
names(cacAgg) = paste0(names(cacAgg), '_cac')

 
 
cacDirectMatch = merge(std, cacAgg, by = c('year_cac'), all = FALSE)


cacMissMatch = std[which(std$id %in% setdiff(std$id, cacDirectMatch$id)),]



# cacMissMatch = cacMissMatch[cacMissMatch$year_cac<2016,]
cacMissMatch$year_cac = cacMissMatch$year_cac -1
cacByOneMatch = merge(cacAgg, cacMissMatch, by = c('year_cac'), all = FALSE) 

codexCAC = rbind(cacDirectMatch, cacByOneMatch)


 

# ---------------------------------------
# Merge Data Based on Specific Committees 
# ---------------------------------------

# find matches for which there is a direct year-committee match
directMatch = merge(participAgg, std, by = c('event_short', 'year'), all = FALSE)



# find standards for which there is no direct year-committee match with the participants data
stdMissMatch = std[which(std$id %in% setdiff(std$id, directMatch$id)),]

 
# identify standards for which lag time between last meeting and time to standard adoption/revision/amendment more than 1 year
stdMissMatch$year = stdMissMatch$year-1
ByOneMatch = merge(participAgg, stdMissMatch, by = c('event_short', 'year'), all = FALSE)
stdMatch = stdMissMatch[which(stdMissMatch$id %in% ByOneMatch$id),]

# combine ....
std2 = rbind(std[which(std$id %in% directMatch$id),],  # standards that have a direct match, 
			stdMatch, # standarads that have match difference by one year,
			std[-which(std$id %in% c(directMatch$id, ByOneMatch$id)),] #  unmatched standards
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


codex = merge(participAgg, std2, by = c('event_short', 'year'), all = TRUE)

# the executive committee doesn't pass standards; cac gets its own variables (i.e. 'delegates_cac') so remove them from sample
codex = codex[-which(codex$event_short %in% c('CCEXEC', 'CAC')),]


# codex$delegates_cac = codexCAC$delegates_cac[match(codex$id, codexCAC$id)]
# codex$delegations_cac = codexCAC$delegations_cac[match(codex$id, codexCAC$id)]
# codex$delegConc_cac = codexCAC$delegConc_cac[match(codex$id, codexCAC$id)]

 
codex = merge(codex, codexCAC[, c(12, grep('deleg', names(codexCAC)))], by = 'id', all.x = TRUE)
 
codex$delegates[which(is.na(codex$delegates) & !is.na(codex$delegates_cac))] = codex$delegates_cac[which(is.na(codex$delegates) & !is.na(codex$delegates_cac))]
codex$delegations[which(is.na(codex$delegations) & !is.na(codex$delegations_cac))] = codex$delegations_cac[which(is.na(codex$delegations) & !is.na(codex$delegations_cac))] 
codex$delegConc[which(is.na(codex$delegConc) & !is.na(codex$delegConc_cac))] = codex$delegConc_cac[which(is.na(codex$delegConc) & !is.na(codex$delegConc_cac))]

 

# --------------------
## Check
# stdMissMatch2 = std2[which(std2$id %in% setdiff(std2$id, codex$id)),]

# !! note still need to resolve these standards!! will be able to do so when seb updates the codexset to go through 2017 :
# 4 of them would be resolved by getting 2017 codex:
	# CCFH revision in 2017, previous meeting 2015 (does codex for 2017 exist) --- YES; get meeting codex for 2016, 2017
	# CCFO revision in 2017, previous meeting 2015 (does codex for 2017 exist) --- YES
	# CCNFSDU revision in 2017, previous meeting 2015(does codex for 2017 exist) --- YES
	# CCFP revision in 2017, previous meeting 2015 (do we have this codex?)

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

# create dummy for committees that were adjounred/abolished and subsequently had revisions to their standards 
codex$activeCommittee = ifelse(is.na(codex$meetingCount), 0, 1)
 
# set NAs equal to 0
codex$adopted[which(is.na(codex$adopted))] = 0
codex$revised[which(is.na(codex$revised))] = 0
codex$amended[which(is.na(codex$amended))] = 0
 

# create log number of delegates variable
 # create log number of delegations variable
log0 = function(variable){
	ifelse(variable == 0, 0, log(variable))}
ldeleg = apply(codex[, grep('delegates|delegations', names(codex))], 2, log0) %>% data.frame()
names(ldeleg) = paste0('l', names(ldeleg))
codex = cbind(codex, ldeleg) 

# create delegation size variable
codex$delegationSize = codex$delegates/codex$delegations
codex$delegationSize_cac = codex$delegates_cac/codex$delegations_cac

# create share of developed countries
codex$delegates_developedShare = rowSums(codex[, c('delegates_income_group_Highincome', 'delegates_income_group_Uppermiddleincome')] )/codex$delegates 
codex$delegates_developedShare_cac = rowSums(codex[, c('delegates_income_group_Highincome_cac', 'delegates_income_group_Uppermiddleincome_cac')] )/codex$delegates_cac 


# create variable of number of times a standard has been amended or revised
codex$bothAmendedOrRevised = rowSums(codex[, c('amended', 'revised')], na.rm = TRUE)
 
 
# check
split(codex[, c('year', 'meetingCount')], codex$event_short)

 
# create dummy variable for if the WTO is is existence
codex$wtoDummy = ifelse(codex$year>=1995, 1, 0)

save(codex, file = paste0(pathData, '/codexData.rda'))
# exclude 2017 data as sebastian will be collecting this shortly 

