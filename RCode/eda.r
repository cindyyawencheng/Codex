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


# Codex Alimentarius in July
# meetings always adopted in CAC

 

load(paste0(pathData, '/codexData.rda'))

# restrict to analysis before 2016 for now, as seb is still collecting this data
codex = codex[which(codex$year<2016),]

# restrict to years in which the codex was actually in existence
codex = codex[which(codex$year>1962),]



# -------------------------------------
#  Analyze Data 
# -------------------------------------

# run heckman selection models, where first stage is whether a certain standard becomes adopted and 
# the second stage is whether a standard is amended or revised
# unit of analysis: standard-commitee-year 



# check how model performs using 4 different specifications of measure of delegate representation
# In next few days, will also create variable for number or share of country delegates vs other types of delegations, esp. private)

ivList = list( c( 'ldelegates', # log number of delegates
		'ldelegations', # log number of delegations
		'delegConc',   # herfindahl index of delegates
		'delegationSize', # average size of each delegation
		'delegates_developedShare'), 
		
		c('ldelegates_cac', # log number of delegates in the cac
		'ldelegations_cac', # log number of delegations in the cac
		'delegConc_cac',   # herfindahl index of delegates
		'delegationSize_cac',
		'delegates_developedShare_cac' )) 

cntrlVarList = list(c('wtoDummy', 'event_short', 'meetingCount'),
				c('wtoDummy', 'meetingCount'))

 
selectionDV = c('revised', 'amended', 'amendedOrRevised')

dataSets = list(codex,
				codex[which(codex$activeCommittee == 1),])

dataSetsNames = c('All', 'ActiveOnly')
 

# run models controlling for wtoDummy, committee and meeting count in selection stage, and controlling for wtoDummy and meeting count in second stage
results = list()
for (d in 1:length(dataSets)){
	for ( dv in 1:length(selectionDV)){
	for (ivL in 1:length(ivList)){
		for ( iv in 1:length(ivList[[ivL]]) ){

		results[[dataSetsNames[d]]][[paste0(selectionDV[dv], ivList[[ivL]][iv]) ] ]=selection(as.formula(paste0('adopted ~', paste(c(ivList[[ivL]][iv],  cntrlVarList[[ivL]]), collapse = '+'))), 
	            as.formula(paste0(selectionDV[dv], '~', paste(c(ivList[[ivL]][iv], 'wtoDummy',   'meetingCount'  ), collapse = '+'))),
	            data=dataSets[[d]],
	            , method = "2step")

 }}

}
}


names(codex)

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

