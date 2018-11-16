if(Sys.info()['user'] == 'cindycheng'){
	pathData = '/Users/cindycheng/Dropbox/Documents/Papers/Codex'
}

loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  library(lib, character.only=TRUE)
	}
}

# load packages
packages = c('readstata13', 'sampleSelection', 'dplyr', 'magrittr')
loadPkg(packages)


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

# make `year_enact` variable consistent with the year a standard is adopted, revised or amended
std$year_enact = std$year

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


# aggregate data
participAgg = particip %>% group_by(year, event_short) %>% dplyr:::summarise(delegates = sum(delegates, na.rm = TRUE),
																			delegations = sum(delegations, na.rm = TRUE),
																			delegConc = sum(delegateWt2, na.rm = TRUE)) %>% data.frame()

 
# assume that if there are no delegates, the committee didn't/doesn't exist
participAgg = participAgg[-which(participAgg$delegates == 0),] 
 
# check assumption
split(participAgg$year, participAgg$event_short)



# -------------------------
# Merge Data and Create New Variables
# --------------------------

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





dataAgg = merge(participAgg, std2, by = c('event_short', 'year'), all.x = TRUE)
 
 

# --------------------
## Check
# still missing 96 standards
stdMissMatch2 = std2[which(std2$id %in% setdiff(std2$id, dataAgg$id)),]


# 4 of them would be resolved by getting 2017 data:
	# CCFH revision in 2017, previous meeting 2015 (does data for 2017 exist) --- YES; get meeting data for 2016, 2017
	# CCFO revision in 2017, previous meeting 2015 (does data for 2017 exist) --- YES
	# CCNFSDU revision in 2017, previous meeting 2015(does data for 2017 exist) --- YES
	# CCFP revision in 2017, previous meeting 2015 (do we have this data?)

# the rest pertain to cases in which the relevant committee was adjourned/abolished .... should think about what to do with these; if they should be modeled separately
	# Outstanding: CCCPC reported revising or amending standards from 2003 to 2016, however it stopped meeting in 2001
	# Outstanding: CCCPL reported adopting or ameding standards in 2003, 2013, 2016, but last meeting report we have for them is 1994
	# Oustanding, CCFFP reported revising/amending standards in 1979, 2017
	# Outstanding CCMMP revision 2013, 2014, 2016, adjouned as a committee/last meeting.. in  2010; find out when actually adjou
	# CCNMW amended 2011, adjounred as a committee/last meeting 2008
	# CCPMPP revised 2014, 2015, abolished/last met in 1990
	# CCSB adopted, revised 1981, 2001, 2015, committee abolished/last met 1977
	# CCVP revised in 2001, committed adjourned prob 1989
	# TFFBT amended 2011, dissolved 2008
	# TFPHQFF adopted, revised 1976, 1978, 1983, first meeting recorded is 2008, since dissolved

# --------------------
 
# create dummy for committees that were adjounred/abolished and subsequently had revisions to their standards 
dataAgg$activeCommittee = ifelse( dataAgg$event_short %in% c('CCCPC',
													 'CCCPL',
													 'CCFFP',
													 'CCMMP',
													 'CCNMW',
													 'CCPMPP',
													 'CCSB',
													 'CCVP',
													 'TFFBT',
													 'TFPHQFF'), 0, 1)



# the executive committee and the commission don't pass standards, so remove them from sample
dataAgg = dataAgg[-which(dataAgg$event_short %in% c('CCEXEC', 'CAC')),]

# set NAs equal to 0
dataAgg[is.na(dataAgg)] = 0
dataAgg$Reference[which(dataAgg$Reference == 0)] = NA

# create log number of delegates variable
dataAgg$ldelegates = ifelse(dataAgg$delegates == 0, 0, log(dataAgg$delegates))

# create log number of delegations variable
dataAgg$ldelegations = ifelse(dataAgg$delegations == 0, 0, log(dataAgg$delegations))

# create delegation size variable
dataAgg$delegationSize = dataAgg$delegates/dataAgg$delegations

# create variable of number of times a standard has been amended or revised
dataAgg$amendedOrRevised = rowSums(dataAgg[, c('amended', 'revised')], na.rm = TRUE)


# create measure of meeting counts
dataAgg= dataAgg[order(dataAgg$event_short, dataAgg$year ),]
dataAgg$meetingCount = lapply(split(dataAgg$year, dataAgg$event_short), function(x){
	1:length(x)
}) %>% unlist()

# check
split(dataAgg[, c('year', 'meetingCount')], dataAgg$event_short)

 
# create dummy variable for if the WTO is is existence
dataAgg$wtoDummy = ifelse(dataAgg$year>=1995, 1, 0)


# -------------------------------------
#  Analyze Data 
# -------------------------------------

# run heckman selection models, where first stage is whether a certain standard becomes adopted and 
# the second stage is whether a standard is amended or revised
# unit of analysis: standard-commitee-year 



# check how model performs using 4 different specifications of measure of delegate representation
# In next few days, will also create variable for number or share of country delegates vs other types of delegations, esp. private)

iv = c('ldelegates', # log number of delegates
		'ldelegations', # log number of delegations
		'delegConc',   # herfindahl index of delegates
		'delegationSize') # average size of each delegation


dataSets = list(dataAgg,
				dataAgg[which(dataAgg$activeCommittee == 1),])

dataSetsNames = c('All', 'ActiveOnly')
 

# run models controlling for wtoDummy, committee and meeting count in selection stage, and controlling for wtoDummy and meeting count in second stage
results = list()
for (d in 1:length(dataSets)){
	for (i in 1:length(iv)){
		results[[dataSetsNames[d]]][[i]]=selection(as.formula(paste0('adopted ~', paste(c(iv[i], 'wtoDummy', 'event_short', 'meetingCount' ), collapse = '+'))), 
	            as.formula(paste0('amendedOrRevised ~', paste(c(iv[i], 'wtoDummy',   'meetingCount'  ), collapse = '+'))),
	            data=dataSets[[d]],
	            , method = "2step")

}}

lapply(results[['All']], summary)
lapply(results[['ActiveOnly']], summary)

# run models with interaction term between delegate variable and whether WTO in existence across both stages
resultsInt = list()
for (d in 1:length(dataSets)){
	for (i in 1:length(iv)){
		resultsInt[[dataSetsNames[d]]][[i]]=selection(as.formula(paste0('adopted ~', paste(c( paste0(iv[i], '*wtoDummy'),  'event_short','meetingCount' ), collapse = '+'))), 
	            as.formula(paste0('amendedOrRevised ~', paste(c(paste0(iv[i], '*wtoDummy'), 'meetingCount'   ), collapse = '+'))),
	            data=dataSets[[d]],
	            , method = "2step")}
}
lapply(resultsInt[['All']], summary)
lapply(resultsInt[['ActiveOnly']], summary)



# note also tried it when using reference as a fixed effect, no sig results for any individual references and some of the models didn't converge




# thing to think about; 
# try a survival model for adoption?
# joint delegate membership across meetings?; can we see horse trading going on? --- can create a measure of degree of networkedness of each committee to other committees... can get a finer level of disaggregation if we match back actual participants... 
# create dummy for when committee renamed and reestablished?



# could think about using this specification in future models, but not doing so now
# stdAgg = std %>% group_by(year, event_short) %>% summarise(adopted = sum(adopted, na.rm = TRUE),
# 															revised = sum(revised, na.rm =TRUE),
# 															amended = sum(amended, na.rm = TRUE)) %>% data.frame()


# m2=selection(adopted ~ ldelegates +delegConc+event_short  , 
#             amendedOrRevised ~ ldelegates +delegConc+year  ,
#             data=dataAgg,
#             , method = "2step")

