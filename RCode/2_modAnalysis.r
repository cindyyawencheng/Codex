if(Sys.info()['user'] == 'cindycheng'){
	source('/Users/cindycheng/Documents/Papers/Codex/RCode/setup.R')
}

 

# Codex Alimentarius in July
# meetings always adopted in CAC

# european union participation ; on what topic the EU votes, and on what topic the member countries vote  

load(paste0(pathData, '/codexData.rda'))


# restrict to years in which the codex was actually in existence
codex = codex[which(codex$year>1962),]


 
# -------------------------------------
#  Analyze Data Part I
# -------------------------------------
# run heckman selection models, where first stage is whether a proposed certain standard makes it the CAC stage
# the second stage is whether a standard is whether the accepted, proposed standard then becomes adopted
# unit of analysis: standard-commitee-year 




# -------------------------------------
#  Analyze Data Part II
# -------------------------------------
# run heckman selection models, where first stage is whether a certain standard becomes adopted and 
# the second stage is whether a standard is amended or revised
# unit of analysis: standard-commitee-year 



# check how model performs using 4 different specifications of measure of delegate representation
# In next few days, will also create variable for number or share of country delegates vs other types of delegations, esp. private)

# select most common interest groups
# apply(codex[, grep('interest_group', names(codex))], 2, mean, na.rm = TRUE) %>% sort()

regionGroups =  c('region_group_EuropeCentralAsia',
				'region_group_NorthAmerica',
				'region_group_LatinAmericaCaribbean',
				'region_group_MiddleEastNorthAfrica',
				'region_group_SouthAsia',
				"region_group_SubSaharanAfrica",
				'region_group_EastAsiaPacific')

incomeGroups = c("income_group_Highincome",               
		 		"income_group_Uppermiddleincome",    
				"income_group_Lowermiddleincome",
				"income_group_Lowincome" )


interestGroups = c("interest_group_Industry",               
				"interest_group_Mixed",                  
				"interest_group_Consumer",               
#				"nterest_group_Standardization",        
#				"interest_group_IGO",                    
#				"interest_group_Professional",           
#				"interest_group_Environment"            
				"interest_group_Research"               
#				"interest_group_Media")
				)


ivList = list( 
		c( 'ldelegates', # log number of delegates
		'ldelegations', # log number of delegations
		'delegConc',   # herfindahl index of delegates
		'delegationSize', # average size of each delegation
		'delegates_developedShare',
		paste(paste0('ldelegates_', regionGroups), collapse = '+'),	
		paste(paste0('ldelegates_', incomeGroups) , collapse = '+'),
		paste(paste0('ldelegates_', interestGroups) , collapse = '+'),
		paste(paste0('delegates_', regionGroups, '_share'), collapse = '+'),	
		paste(paste0( 'delegates_', incomeGroups, '_share') , collapse = '+'),
		paste(paste0('delegates_', interestGroups, '_share') , collapse = '+')),


		c('ldelegates_cac', # log number of delegates in the cac
		'ldelegations_cac', # log number of delegations in the cac
		'delegConc_cac',   # herfindahl index of delegates
		'delegationSize_cac',
		'delegates_developedShare_cac',
		 paste(paste0('ldelegates_', regionGroups, '_cac'), collapse = '+'),	
		paste(paste0('ldelegates_', incomeGroups, '_cac') , collapse = '+'),
		paste(paste0('ldelegates_', interestGroups, '_cac') , collapse = '+'),
		paste(paste0('delegates_', regionGroups, '_cac_share'), collapse = '+'),	
		paste(paste0( 'delegates_',incomeGroups, '_cac_share') , collapse = '+'),
		paste(paste0('delegates_', interestGroups, '_cac_share') , collapse = '+'))
		 ) 


cntrlVarList = list(
				list(c('wtoDummy', 'event_short', 'meetingCount'),
					c('wtoDummy', 'event_short', 'meetingCount', names(codex)[grep('chair_', names(codex))]),
					c('wtoDummy', 'event_short', 'meetingCount', names(codex)[grep('chair_', names(codex))], names(codex)[grep('obs_', names(codex))])
				),
				list(c('wtoDummy', 'meetingCount')))

 
cntrlNames = list(
				list(c('Base'),
					c('Base+Chair',
					c('Base+Chair+Obs'))),
				list(c('Base'))
	)


selectionDV = c('revised', 'amended', 'bothAmendedOrRevised')
 

dataSets = list(codex,
				codex[which(codex$activeCommittee == 1),],
				codex[which(codex$activeCommittee == 0),] )

dataSetsNames = c('All', 'ActiveOnly', 'inActive' )
 

# run models controlling for wtoDummy, committee and meeting count in selection stage, and controlling for wtoDummy and meeting count in second stage
results = list()
for (d in 1:length(dataSets)){
	for ( dv in 1:length(selectionDV)){
	for (ivL in 1:length(ivList)){
		for ( iv in 1:length(ivList[[ivL]]) ){

			for(cntrl in 1:length(cntrlVarList[[ivL]])){

		results[[dataSetsNames[d]]][[paste0(selectionDV[dv], ivList[[ivL]][iv], cntrlNames[[ivL]][cntrl] ) ] ]=tryCatch(selection(as.formula(paste0('adopted ~', paste(c(ivList[[ivL]][iv],  cntrlVarList[[ivL]][cntrl]), collapse = '+'))), 
	            as.formula(paste0(selectionDV[dv], '~', paste(c(ivList[[ivL]][iv], 'wtoDummy',   'meetingCount'  ), collapse = '+'))),
	            data=dataSets[[d]],
	            , method = "2step"),error=function(e) NA)

 }}
}
}
}
results[[1]]
save(results, file = paste0(pathResults, '/prelimResults_Dec12_2018.rda'))



 
screenreg(all_individual_amended[1])
lapply(results[['All']][[8]], summary)
lapply(all_cac_amended[1:6], summary )
lapply(inactive_individual_both, class )

all_individual_amended = extractTables('All', 'individualCommittees', 'amended')
all_individual_revised = extractTables('All', 'individualCommittees', 'revised')
all_individual_both = extractTables('All', 'individualCommittees', 'both')

all_cac_amended = extractTables('All', 'cac', 'amended')
all_cac_revised = extractTables('All', 'cac', 'revised')
all_cac_both = extractTables('All', 'cac', 'both')


active_individual_amended = extractTables('ActiveOnly', 'individualCommittees', 'amended')
active_individual_revised = extractTables('ActiveOnly', 'individualCommittees', 'revised')
active_individual_both = extractTables('ActiveOnly', 'individualCommittees', 'both')

active_cac_amended = extractTables('ActiveOnly', 'cac', 'amended')
active_cac_revised = extractTables('ActiveOnly', 'cac', 'revised')
active_cac_both = extractTables('ActiveOnly', 'cac', 'both')


inactive_individual_amended = extractTables('inActive', 'individualCommittees', 'amended')
inactive_individual_revised = extractTables('inActive', 'individualCommittees', 'revised')
inactive_individual_both = extractTables('inActive', 'individualCommittees', 'both')

inactive_cac_amended = extractTables('inActive', 'cac', 'amended')
inactive_cac_revised = extractTables('inActive', 'cac', 'revised')
inactive_cac_both = extractTables('inActive', 'cac', 'both')




select_coef_order = c(str_split(ivList[[1]], '\\+') %>% unlist(), 'wtoDummy', 'meetingCount',lapply(all_individual_amended[1], function(x) names(coef(x))[grep('event', names(coef(x)))]) %>% unlist())
outcome_coef_order = c(c(str_split(ivList[[1]], '\\+') %>% unlist()), 'wtoDummy', 'meetingCount')
coef_order = c(paste0('S: ' ,select_coef_order), paste0('O: ', outcome_coef_order ))

mainIV_names = c('Log Delegates',
				  'Log Delegations',
				  'Delegate Concentration',
				  'Avg Delegates per Delegation',
				  'Share of Developed Country Delegates',
				  'Log Delegates, Europe and Central Asia',
				  'Log Delegates, North America',
				  'Log Delegates, Latin American and Caribbean',
				  'Log Delegates, MENA',
				  'Log Delegates South Asia',
				  'Log Delegates Sub Saharan Africa',
				  'Log Delegates East Asia and Pacific',
				  'Log Delegates, High Income',
				  'Log Delegates, Upper middle Income',
				  'Log Delegates, Low middle income',
				  'Log Delegates, Low income',
				  'Log Delegates, Industry',
				  'Log Delegates, Mixed',
				  'Log Delegates, Consumer',
				  'Log Delegates, Standardization',
				  'Log Delegates, IGO',
				  'Log Delegates, Professional',
				  'Log Delegates, Environment',
				  'Log Delegates, Research',
				  'Log Delegates, Media')

	cntrlVars			 c( 'WTO Dummy',
				  'Meeting Count',
				  sort(unique(codex$event_short))[-1]  )


coef_names = c(paste0(mainIV_names, ' (s)'),
				 'WTO Dummy (s)',
				  'Meeting Count (s)',
				  paste0(sort(unique(codex$event_short))[-1], ' (s)'), 
				  paste0(mainIV_names, ' (o)'),
				  'WTO Dummy (o)',
				  'Meeting Count (o)')

coef_map =  paste(paste0("'", coef_order,"'", " = ", "'",coef_names, "'"), collapse = ' , ')

screenreg(all_cac_amended[1:6],  
        custom.coef.map = coef_map_list_cac,
        custom.model.names = c(paste0('Model ', 1:6))
        )


save(texreg(all_individual_amended[-c(8)]), file = paste0(pathResults, '/all_individual_amended.tex'))
load(paste0(pathResults, 'all_individual_amended.tex'))
 

screenreg( all_individual_amended,  custom.coef.map = coef_map_list,
									custom.model.names = c(paste0('Model ', 1:8)))

all_individual_revised

paste0('Model ', 1:8)

c("model 1", "modle 2")
lapply(all_individual_revised, class)

 class()

names(results[['All']])[grep('^both', names(results[['All']]))]
 
results[['All']][grep('^both', names(results[['All']]))][[1]]

list(results[['All']][grep('^both', names(results[['All']]))][[1]],
	results[['All']][grep('^both', names(results[['All']]))][[2]])

screenreg(list(results[['All']][grep('^both', names(results[['All']]))][[1]],
	results[['All']][grep('^both', names(results[['All']]))][[2]]))

 



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

